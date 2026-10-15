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
  ; resynced_at : int option
  ; halted_at : int option
  ; bump_used : Size.t
      (* Left out of the sexp while unused, so a strategy without a gate or guard prints
         as before and the bisimulation against [test/ref_strategy.ml] still compares
         states. *)
  ; toxicity : Toxicity_gate.Signal.t option [@sexp.option]
  ; gate : Toxicity_gate.Decision.t [@sexp_drop_if Toxicity_gate.Decision.equal Normal]
  (** at the last tick *)
  ; last_quoted_gate : Toxicity_gate.Decision.t
       [@sexp_drop_if Toxicity_gate.Decision.equal Normal]
  ; guard : Reference_guard.t option [@sexp.option]
  (** present iff [params.reference_guard] is *)
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
  ; resynced_at = None
  ; halted_at = None
  ; bump_used = Size.zero
  ; toxicity = None
  ; gate = Normal
  ; last_quoted_gate = Normal
  ; guard = Option.map params.reference_guard ~f:Reference_guard.create
  }
;;

let ours t symbol = Symbol.equal symbol t.params.symbol
let ns span = Time_ns.Span.to_int_ns span

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

let on_resync t ~now = { t with resynced_at = Some now }

let on_toxicity t ~now ~signal ~imbalance =
  { t with
    toxicity = Some { Toxicity_gate.Signal.value = signal; imbalance; time = now }
  }
;;

let on_reference t ~now reference =
  match t.guard with
  | None -> t
  | Some guard ->
    { t with guard = Some (Reference_guard.on_reference guard ~now reference) }
;;

(* The guard looked at the current mid; [t] itself while its status does not change. *)
let check_guard t ~now =
  match t.guard with
  | None -> t
  | Some guard ->
    let checked =
      Reference_guard.check guard ~now ~venue_mid:(Market_state.mid_or_null t.market)
    in
    if phys_equal checked guard then t else { t with guard = Some checked }
;;

(* The pull reason of the guard and the gate, if any. A reason already in [mode] is kept
   while it holds, so the mode does not change (and get logged) on every tick. *)
let plausibility_reason t ~now ~(gate : Toxicity_gate.Decision.t) : Risk.Reason.t or_null =
  let guard_reason : Risk.Reason.t or_null =
    match t.guard with
    | None -> Null
    | Some guard ->
      (match Reference_guard.status guard, t.mode with
       | Diverged _, Pulled (Reference_divergence _ as kept) -> This kept
       | Diverged { since; divergence_bps; _ }, _ ->
         This
           (Reference_divergence { divergence_bps; held_ms = (now - since) / 1_000_000 })
       | Reference_missing _, Pulled (Reference_missing _ as kept) -> This kept
       | Reference_missing { age_ms }, _ -> This (Reference_missing { age_ms })
       | (Unchecked | Within_band | Diverging _), _ -> Null)
  in
  match guard_reason with
  | This _ -> guard_reason
  | Null ->
    (match gate, t.mode with
     | Pull _, Pulled (Toxic_flow _ as kept) -> This kept
     | Pull { signal }, _ -> This (Toxic_flow { signal })
     | (Normal | Widen _), _ -> Null)
;;

(* Parameters and reservation shift the gate quotes with. *)
let gated_params t =
  match t.gate with
  | Normal | Pull _ -> t.params, None
  | Widen { extra_half_spread_bps; reservation_shift_bps; size_factor; intensity = _ } ->
    let p = t.params in
    let units =
      Float.iround_down_exn (Float.of_int (Size.to_units p.order_size) *. size_factor)
    in
    ( { p with
        base_half_spread_bps = p.base_half_spread_bps +. extra_half_spread_bps
      ; order_size = Size.max p.lot (Fx.size_floor (Size.of_units units) ~lot:p.lot)
      }
    , Some reservation_shift_bps )
;;

let in_grace t ~now =
  match t.resynced_at with
  | None -> false
  | Some at -> now - at < ns t.params.action_rate_grace_ms
;;

(* A [Max_action_rate] halt clears after the cooldown, when there is one. *)
let cooled_down t ~now =
  match t.mode, t.halted_at, t.params.action_rate_cooldown_ms with
  | Halted (Max_action_rate _), Some at, Some cooldown -> now - at >= ns cooldown
  | _ -> false
;;

let interval_elapsed t ~now =
  match t.last_plan_at with
  | None -> true
  | Some at -> now - at >= ns t.params.requote_interval_ms
;;

let should_requote t ~now ~was_running =
  (not was_running)
  || interval_elapsed t ~now
  || (not (Toxicity_gate.Decision.equal t.gate t.last_quoted_gate))
  || (match t.last_quoted_position with
      | None -> true
      | Some p -> not (Size.equal p (Inventory.position t.inventory)))
  ||
  match t.last_quoted_mid, Market_state.mid_or_null t.market with
  | Some last, This mid ->
    let moved =
      Stdlib_upstream_compatible.Float_u.to_float (Fx.bps_between_u ~base:last mid)
    in
    Float.(abs moved >= t.params.requote_threshold_bps)
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
          (if in_grace t ~now
           then t.action_rate
           else
             Risk.Action_rate.record
               t.action_rate
               ~now
               ~actions:(List.count plan ~f:Action.is_change))
      }
    , plan ))
;;

(* A tick that sends nothing returns [t] itself (or a copy only where a field changed) and
   allocates nothing; [tick] boxes the pair. *)
let tick_u t ~now =
  let t =
    match Market_state.mid_or_null t.market with
    | This mark ->
      let pnl = Inventory.total_pnl t.inventory ~mark in
      if Usd.(pnl > t.peak_pnl) then { t with peak_pnl = pnl } else t
    | Null -> t
  in
  match t.awaiting_ack_since with
  | Some since when now - since < ns t.params.requote_interval_ms -> #(t, [])
  | _ ->
    let unacknowledged = Option.is_some t.awaiting_ack_since in
    let t =
      if cooled_down t ~now
      then
        { t with
          mode = Pulled No_market_data
        ; halted_at = None
        ; action_rate = Risk.Action_rate.empty
        }
      else t
    in
    let t = check_guard t ~now in
    let gate =
      match t.params.toxicity_gate with
      | None -> Toxicity_gate.Decision.Normal
      | Some config -> Toxicity_gate.decide config ~now t.toxicity
    in
    let t = if Toxicity_gate.Decision.equal gate t.gate then t else { t with gate } in
    let was_running = Mode.equal t.mode Running in
    let mode : Mode.t =
      match t.mode with
      | Halted _ as halted -> halted
      | Running | Pulled _ ->
        (match
           Risk.check_or_null
             ~actions_last_second:
               (if in_grace t ~now
                then Null
                else This (Risk.Action_rate.last_second t.action_rate ~now))
             t.params
             ~now
             ~peak_pnl:t.peak_pnl
             t.market
             t.inventory
         with
         | Ok ->
           (match plausibility_reason t ~now ~gate:t.gate with
            | Null -> Running
            | This reason -> Pulled reason)
         | Halt reason when Risk.Reason.is_sticky reason -> Halted reason
         | Halt reason -> Pulled reason)
    in
    let halted_at =
      match t.mode, mode with
      | Halted _, _ -> t.halted_at
      | (Running | Pulled _), Halted _ -> Some now
      | _, (Running | Pulled _) -> None
    in
    let t =
      if phys_equal mode t.mode && phys_equal halted_at t.halted_at
      then t
      else { t with mode; halted_at }
    in
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
         if List.is_empty plan
         then #(t, [])
         else #({ t with last_plan_at = Some now }, plan))
       else #(t, [])
     | Pulled _ ->
       let t, plan = emit t ~now (Risk.cancel_all t.params ~open_orders:t.open_orders) in
       #(t, plan)
     | Running ->
       if not (should_requote t ~now ~was_running)
       then #(t, [])
       else (
         let bump_left = Size.(max zero (t.params.bump_budget - t.bump_used)) in
         let params, shift_bps = gated_params t in
         let desired = Quoter.quote ~bump_left ?shift_bps params t.market t.inventory in
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
           ; last_quoted_gate = t.gate
           }
         in
         let t, plan = emit t ~now plan in
         #(t, plan)))
;;

let tick t ~now =
  let #(t, plan) = tick_u t ~now in
  t, plan
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
    let guard =
      match params.reference_guard, t.guard with
      | None, _ -> None
      | Some config, Some guard -> Some (Reference_guard.with_config guard config)
      | Some config, None -> Some (Reference_guard.create config)
    in
    Ok { t with params; market; guard; last_plan_at = None })
;;

let params t = t.params
let mode t = t.mode
let market t = t.market
let inventory t = t.inventory
let open_orders t = t.open_orders
let generation t = t.generation
let peak_pnl t = t.peak_pnl
let bump_used t = t.bump_used
let toxicity_decision t = t.gate
let reference_guard t = t.guard

module Event = struct
  type t =
    | Reference of Reference_guard.Event.t
    | Toxicity of Toxicity_gate.Event.t
  [@@deriving sexp, compare, equal]
end

let events_between ~before ~after ~now =
  let reference =
    match before.guard, after.guard with
    | Some b, Some a ->
      List.map (Reference_guard.events_between ~before:b ~after:a ~now) ~f:(fun e ->
        Event.Reference e)
    | _ -> []
  in
  let signal = Option.value_map after.toxicity ~default:Float.nan ~f:(fun s -> s.value) in
  match Toxicity_gate.Event.of_change ~before:before.gate ~after:after.gate ~signal with
  | None -> reference
  | Some e -> reference @ [ Toxicity e ]
;;

let tick_with_events t ~now =
  let after, plan = tick t ~now in
  after, plan, events_between ~before:t ~after ~now
;;
