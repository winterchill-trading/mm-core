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
    | Reference_divergence of
        { divergence_bps : float
        ; held_ms : int
        }
    | Reference_missing of { age_ms : int option }
    | Toxic_flow of { signal : float }
  [@@deriving sexp, compare, equal]

  let is_sticky = function
    | Max_position _ | Max_drawdown _ | Max_action_rate _ -> true
    | No_market_data
    | Stale_market_data _
    | Reference_divergence _
    | Reference_missing _
    | Toxic_flow _ -> false
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

  (* [peek_front_exn] behind [is_empty] rather than [peek_front]: no [Some] per call. *)
  let rec prune t ~now =
    if Fdeque.is_empty t.entries
    then t
    else (
      let at, actions = Fdeque.peek_front_exn t.entries in
      if now - at >= window
      then
        prune
          { entries = Fdeque.drop_front_exn t.entries; total = t.total - actions }
          ~now
      else t)
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

let check_or_null ~actions_last_second (p : Params.t) ~now ~peak_pnl market inventory =
  let position = Inventory.position inventory in
  if Size.(abs position > p.max_position)
  then Halt (Max_position { position })
  else (
    let mid = Market_state.mid_or_null market in
    let drawdown : Reason.t or_null =
      match mid with
      | Null -> Null
      | This mark ->
        let pnl = Inventory.total_pnl inventory ~mark in
        let peak = Usd.max peak_pnl pnl in
        if Usd.(peak - pnl > p.max_drawdown)
        then This (Max_drawdown { pnl; peak })
        else Null
    in
    match drawdown with
    | This reason -> Halt reason
    | Null ->
      (match actions_last_second with
       | This actions when actions > p.max_actions_per_sec ->
         Halt (Max_action_rate { actions })
       | This _ | Null ->
         (match mid, Market_state.age_ms_or_null market ~now with
          | Null, _ | _, Null -> Halt No_market_data
          | This _, This age_ms when Time_ns.Span.(of_int_ms age_ms > p.max_data_age_ms)
            -> Halt (Stale_market_data { age_ms })
          | This _, This _ -> Ok)))
;;

let check ?actions_last_second p ~now ~peak_pnl market inventory =
  check_or_null
    ~actions_last_second:
      (match actions_last_second with
       | None -> Null
       | Some n -> This n)
    p
    ~now
    ~peak_pnl
    market
    inventory
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
