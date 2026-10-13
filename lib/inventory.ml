open! Core
open Exchange_core

type t =
  { position : Size.t
  ; avg_entry : Price.t option
  ; realized_pnl : Usd.t
  ; fees : Usd.t
  ; fill_count : int
  ; last_sync_time : int option
  ; seen_tids : Trade_id.Set.t [@sexp_drop_if fun _ -> true]
  }
[@@deriving sexp_of]

let empty =
  { position = Size.zero
  ; avg_entry = None
  ; realized_pnl = Usd.zero
  ; fees = Usd.zero
  ; fill_count = 0
  ; last_sync_time = None
  ; seen_tids = Trade_id.Set.empty
  }
;;

let apply_trade t ~(side : Side.t) ~(px : Price.t) ~(sz : Size.t) =
  let x = t.position in
  let d = Fx.signed side sz in
  let sx = Fx.sign_of_size x in
  let sd = Fx.sign_of_size d in
  let x' = Size.(x + d) in
  if sd = 0
  then t
  else if sx = 0 || sx = sd
  then (
    let ax = Size.to_float (Size.abs x)
    and ad = Size.to_float (Size.abs d) in
    let entry = Option.value_map t.avg_entry ~default:0. ~f:Price.to_float in
    let avg = ((ax *. entry) +. (ad *. Price.to_float px)) /. (ax +. ad) in
    { t with position = x'; avg_entry = Some ((Price.of_float [@alert "-lossy"]) avg) })
  else (
    let closed = Size.min (Size.abs x) (Size.abs d) in
    let entry = Option.value t.avg_entry ~default:px in
    let gain = Fx.notional Price.(px - entry) closed in
    let gain = if sx > 0 then gain else Usd.neg gain in
    let avg_entry =
      match Fx.sign_of_size x' with
      | 0 -> None
      | s when s = sx -> t.avg_entry
      | _ -> Some px
    in
    { t with position = x'; avg_entry; realized_pnl = Usd.(t.realized_pnl + gain) })
;;

let on_fill t (fill : Fill.t) =
  if Set.mem t.seen_tids fill.trade_id
  then t
  else (
    let t =
      { t with
        seen_tids = Set.add t.seen_tids fill.trade_id
      ; fees = Usd.(t.fees + fill.fee)
      ; fill_count = t.fill_count + 1
      }
    in
    match t.last_sync_time with
    | Some synced when fill.time <= synced ->
      let closed = Option.value fill.closed_pnl ~default:Usd.zero in
      { t with realized_pnl = Usd.(t.realized_pnl + closed) }
    | _ -> apply_trade t ~side:fill.side ~px:fill.px ~sz:fill.size)
;;

let sync t (state : Account_view.t) ~symbol =
  let position, avg_entry =
    match Account_view.find_position state symbol with
    | None -> Size.zero, None
    | Some p -> p.size, if Size.is_zero p.size then None else Some p.entry_px
  in
  { t with position; avg_entry; last_sync_time = Some state.time }
;;

let position t = t.position
let avg_entry t = t.avg_entry
let realized_pnl t = t.realized_pnl
let fees t = t.fees
let fill_count t = t.fill_count

let unrealized_pnl t ~mark =
  match t.avg_entry with
  | None -> Usd.zero
  | Some entry ->
    let u = Fx.notional Price.(mark - entry) (Size.abs t.position) in
    if Size.is_negative t.position then Usd.neg u else u
;;

let total_pnl t ~mark = Usd.(t.realized_pnl + unrealized_pnl t ~mark - t.fees)
