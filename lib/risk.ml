open! Core
open Exchange_core

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
  (* Newest first: (time, actions). *)
  type t = (int * int) list [@@deriving sexp_of]

  let empty = []
  let window = 1_000_000_000
  let prune t ~now = List.take_while t ~f:(fun (at, _) -> now - at < window)

  let record t ~now ~actions =
    if actions <= 0 then prune t ~now else (now, actions) :: prune t ~now
  ;;

  let last_second t ~now = List.sum (module Int) (prune t ~now) ~f:snd
end

type t =
  | Ok
  | Halt of Reason.t
[@@deriving sexp, compare, equal]

let check ?actions_last_second (p : Params.t) ~now ~peak_pnl market inventory =
  let position = Inventory.position inventory in
  let drawdown =
    Option.bind (Market_state.mid market) ~f:(fun mark ->
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
      (match Market_state.mid market, Market_state.age_ms market ~now with
       | None, _ | _, None -> Halt No_market_data
       | Some _, Some age_ms when age_ms > p.max_data_age_ms ->
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
    match Market_state.mid market with
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
