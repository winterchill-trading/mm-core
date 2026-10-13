(* Reference copy of the pure lib/risk.ml from before the allocation work; the
   bisimulation test checks the library against it. *)

open! Core
open Exchange_core
open Mm_core

module Reason = struct
  type t =
    | Max_position of { position : Size.t }
    | Max_drawdown of
        { pnl : Usd.t
        ; peak : Usd.t
        }
    | Max_action_rate of { actions : int }
    | No_market_data
    | Stale_market_data of { age_ms : int }
  [@@deriving sexp, compare, equal]

  let is_sticky = function
    | Max_position _ | Max_drawdown _ | Max_action_rate _ -> true
    | No_market_data | Stale_market_data _ -> false
  ;;
end

module Action_rate = struct
  (* Oldest first: (time, actions), with their running sum. *)
  type t =
    { entries : (int * int) Fdeque.t
    ; total : int
    }
  [@@deriving sexp_of]

  let empty = { entries = Fdeque.empty; total = 0 }
  let window = 1_000_000_000

  let rec prune t ~now =
    match Fdeque.peek_front t.entries with
    | Some (at, actions) when now - at >= window ->
      prune { entries = Fdeque.drop_front_exn t.entries; total = t.total - actions } ~now
    | Some _ | None -> t
  ;;

  let record t ~now ~actions =
    let t = prune t ~now in
    if actions <= 0
    then t
    else
      { entries = Fdeque.enqueue_back t.entries (now, actions)
      ; total = t.total + actions
      }
  ;;

  let last_second t ~now = (prune t ~now).total
end

type t =
  | Ok
  | Halt of Reason.t
[@@deriving sexp, compare, equal]

let check ?actions_last_second (p : Params.t) ~now ~peak_pnl market inventory =
  let position = Inventory.position inventory in
  let drawdown =
    Option.bind (Ref_market_state.mid market) ~f:(fun mark ->
      let pnl = Inventory.total_pnl inventory ~mark in
      let peak = Usd.max peak_pnl pnl in
      Option.some_if Usd.(peak - pnl > p.max_drawdown) (Reason.Max_drawdown { pnl; peak }))
  in
  let action_rate =
    Option.bind actions_last_second ~f:(fun actions ->
      Option.some_if
        (actions > p.max_actions_per_sec)
        (Reason.Max_action_rate { actions }))
  in
  if Size.(abs position > p.max_position)
  then Halt (Max_position { position })
  else (
    match Option.first_some drawdown action_rate with
    | Some reason -> Halt reason
    | None ->
      (match Ref_market_state.mid market, Ref_market_state.age_ms market ~now with
       | None, _ | _, None -> Halt No_market_data
       | Some _, Some age_ms when Time_ns.Span.(of_int_ms age_ms > p.max_data_age_ms) ->
         Halt (Stale_market_data { age_ms })
       | Some _, Some _ -> Ok))
;;

let cancel_all (p : Params.t) ~open_orders =
  List.filter_map open_orders ~f:(fun (o : Open_order.t) ->
    Option.some_if (Symbol.equal o.symbol p.symbol) (Action.Cancel o.order_id))
;;

let flatten (p : Params.t) ~generation ~open_orders market inventory =
  let position = Inventory.position inventory in
  let close =
    match Ref_market_state.mid market with
    | Some mid when not (Size.is_zero position) ->
      let side : Side.t = if Size.is_positive position then Sell else Buy in
      let limit_px =
        match side with
        | Buy -> Fx.price_ceil (Fx.shift_bps mid p.flatten_slippage_bps) ~tick:p.tick
        | Sell ->
          Fx.price_floor (Fx.shift_bps mid (-.p.flatten_slippage_bps)) ~tick:p.tick
      in
      [ Action.Place
          (Order_request.limit
             ~tif:Ioc
             ~reduce_only:true
             ~client_order_id:
               (Reconciler.cloid
                  ~symbol:p.symbol
                  ~side
                  ~level:Reconciler.flatten_level
                  ~generation)
             p.symbol
             side
             ~px:limit_px
             ~size:(Size.abs position))
      ]
    | _ -> []
  in
  cancel_all p ~open_orders @ close
;;
