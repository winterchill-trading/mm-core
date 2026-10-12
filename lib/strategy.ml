open! Core
open Exchange_core

module Mode = struct
  type t =
    | Running
    | Pulled of Risk.Reason.t
    | Halted of Risk.Reason.t
  [@@deriving sexp, compare, equal]
end

type t =
  { params : Params.t
  ; market : Market_state.t
  ; inventory : Inventory.t
  ; open_orders : Open_order.t list
  ; mode : Mode.t
  ; generation : int
  ; peak_pnl : Usd.t
  ; last_plan_at : int option
  ; last_quoted_mid : Price.t option
  ; last_quoted_position : Size.t option
  ; awaiting_ack_since : int option
  ; action_rate : Risk.Action_rate.t
  ; bump_used : Size.t
  }
[@@deriving sexp_of]

let create params =
  let params = Params.validate params |> ok_exn in
  { params
  ; market = Market_state.create ~vol_window:params.vol_window
  ; inventory = Inventory.empty
  ; open_orders = []
  ; mode = Pulled No_market_data
  ; generation = 0
  ; peak_pnl = Usd.zero
  ; last_plan_at = None
  ; last_quoted_mid = None
  ; last_quoted_position = None
  ; awaiting_ack_since = None
  ; action_rate = Risk.Action_rate.empty
  ; bump_used = Size.zero
  }
;;

let ours t symbol = Symbol.equal symbol t.params.symbol
let ms n = n * 1_000_000

let on_book t (book : L2.t) =
  if ours t book.symbol then { t with market = Market_state.on_book t.market book } else t
;;

(* How far a move from [before] to [after] goes past the soft limit, beyond the furthest
   point of [before] on the same side. *)
let bump ~soft ~before ~after =
  let base =
    if Fx.sign_of_size before = Fx.sign_of_size after
    then Size.max soft (Size.abs before)
    else soft
  in
  Size.max Size.zero Size.(abs after - base)
;;

let on_fill t (fill : Fill.t) =
  if ours t fill.symbol
  then (
    let before = Inventory.position t.inventory in
    let inventory = Inventory.on_fill t.inventory fill in
    let bump_used =
      match t.params.soft_max_position with
      | None -> t.bump_used
      | Some soft ->
        Size.(t.bump_used + bump ~soft ~before ~after:(Inventory.position inventory))
    in
    { t with inventory; bump_used })
  else t
;;

let on_open_orders t open_orders =
  { t with
    open_orders = List.filter open_orders ~f:(fun (o : Open_order.t) -> ours t o.symbol)
  ; awaiting_ack_since = None
  }
;;

let on_account t state =
  { t with inventory = Inventory.sync t.inventory state ~symbol:t.params.symbol }
;;

let interval_elapsed t ~now =
  match t.last_plan_at with
  | None -> true
  | Some at -> now - at >= ms t.params.requote_interval_ms
;;

let should_requote t ~now ~was_running =
  (not was_running)
  || interval_elapsed t ~now
  || (match t.last_quoted_position with
      | None -> true
      | Some p -> not (Size.equal p (Inventory.position t.inventory)))
  ||
  match t.last_quoted_mid, Market_state.mid t.market with
  | Some last, Some mid ->
    Float.(abs (Fx.bps_between ~base:last mid) >= t.params.requote_threshold_bps)
  | _ -> true
;;

(* Records a plan: returns it only if it changes anything. *)
let emit t ~now plan =
  if not (List.exists plan ~f:Action.is_change)
  then t, []
  else (
    let cancelled =
      List.filter_map plan ~f:(function
        | Action.Cancel oid -> Some oid
        | Keep _ | Place _ -> None)
      |> Order_id.Set.of_list
    in
    let placed =
      List.exists plan ~f:(function
        | Action.Place _ -> true
        | Keep _ | Cancel _ -> false)
    in
    ( { t with
        open_orders =
          List.filter t.open_orders ~f:(fun (o : Open_order.t) ->
            not (Set.mem cancelled o.order_id))
      ; generation = (if placed then t.generation + 1 else t.generation)
      ; awaiting_ack_since = Some now
      ; action_rate =
          Risk.Action_rate.record
            t.action_rate
            ~now
            ~actions:(List.count plan ~f:Action.is_change)
      }
    , plan ))
;;

let tick t ~now =
  let t =
    match Market_state.mid t.market with
    | Some mark ->
      { t with peak_pnl = Usd.max t.peak_pnl (Inventory.total_pnl t.inventory ~mark) }
    | None -> t
  in
  match t.awaiting_ack_since with
  | Some since when now - since < ms t.params.requote_interval_ms -> t, []
  | _ ->
    let unacknowledged = Option.is_some t.awaiting_ack_since in
    let was_running = Mode.equal t.mode Running in
    let mode : Mode.t =
      match t.mode with
      | Halted _ as halted -> halted
      | Running | Pulled _ ->
        (match
           Risk.check
             ~actions_last_second:(Risk.Action_rate.last_second t.action_rate ~now)
             t.params
             ~now
             ~peak_pnl:t.peak_pnl
             t.market
             t.inventory
         with
         | Ok -> Running
         | Halt reason when Risk.Reason.is_sticky reason -> Halted reason
         | Halt reason -> Pulled reason)
    in
    let t = { t with mode } in
    (match mode with
     | Halted _ ->
       if interval_elapsed t ~now
       then (
         let plan =
           Risk.flatten
             t.params
             ~generation:t.generation
             ~open_orders:t.open_orders
             t.market
             t.inventory
         in
         let t, plan = emit t ~now plan in
         if List.is_empty plan then t, [] else { t with last_plan_at = Some now }, plan)
       else t, []
     | Pulled _ -> emit t ~now (Risk.cancel_all t.params ~open_orders:t.open_orders)
     | Running ->
       if not (should_requote t ~now ~was_running)
       then t, []
       else (
         let bump_left = Size.(max zero (t.params.bump_budget - t.bump_used)) in
         let desired = Quoter.quote ~bump_left t.params t.market t.inventory in
         let plan =
           Reconciler.reconcile
             t.params
             ~now
             ~generation:t.generation
             ~open_orders:t.open_orders
             ~desired
         in
         let plan =
           if unacknowledged
           then
             List.filter plan ~f:(function
               | Action.Place _ -> false
               | Keep _ | Cancel _ -> true)
           else plan
         in
         let t =
           { t with
             last_plan_at = Some now
           ; last_quoted_mid = Market_state.mid t.market
           ; last_quoted_position = Some (Inventory.position t.inventory)
           }
         in
         emit t ~now plan))
;;

let update_params t (params : Params.t) =
  let%bind.Or_error params = Params.validate params in
  if not (Symbol.equal params.symbol t.params.symbol)
  then
    Or_error.error_s
      [%message
        "update_params cannot change the symbol"
          ~from:(t.params.symbol : Symbol.t)
          ~to_:(params.symbol : Symbol.t)]
  else (
    let market =
      if params.vol_window = t.params.vol_window
      then t.market
      else Market_state.with_vol_window t.market ~vol_window:params.vol_window
    in
    Ok { t with params; market; last_plan_at = None })
;;

let params t = t.params
let mode t = t.mode
let market t = t.market
let inventory t = t.inventory
let open_orders t = t.open_orders
let generation t = t.generation
let peak_pnl t = t.peak_pnl
let bump_used t = t.bump_used
