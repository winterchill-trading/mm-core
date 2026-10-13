(* Reference copy of the pure lib/market_state.ml from before the allocation work; the
   bisimulation test checks the library against it. *)

open! Core
open Exchange_core

type t =
  { book : L2.t option
  ; mid : Price.t option
  ; microprice : Price.t option
  ; var : float
  ; vol_samples : int
  ; alpha : float
  ; updated_at : int option
  }
[@@deriving sexp_of]

let alpha ~vol_window = 2. /. Float.of_int (Int.max 1 vol_window + 1)

let create ~vol_window =
  { book = None
  ; mid = None
  ; microprice = None
  ; var = 0.
  ; vol_samples = 0
  ; alpha = alpha ~vol_window
  ; updated_at = None
  }
;;

let with_vol_window t ~vol_window = { t with alpha = alpha ~vol_window }

let mid_and_micro (book : L2.t) =
  match L2.best_bid book, L2.best_ask book with
  | Some bid, Some ask ->
    let b = Price.to_float bid.px
    and a = Price.to_float ask.px
    and bs = Size.to_float bid.sz
    and as_ = Size.to_float ask.sz in
    let mid = Price.of_units ((Price.to_units bid.px + Price.to_units ask.px) / 2) in
    let micro =
      if Float.(bs +. as_ <= 0.)
      then mid
      else Price.of_float (((b *. as_) +. (a *. bs)) /. (bs +. as_))
    in
    Some mid, Some micro
  | _ -> None, None
;;

let on_book t (book : L2.t) =
  let mid, microprice = mid_and_micro book in
  let var, vol_samples =
    match t.mid, mid with
    | Some prev, Some next when Price.is_positive prev && Price.is_positive next ->
      let r = Float.log (Price.to_float next /. Price.to_float prev) in
      let r2 = r *. r in
      if t.vol_samples = 0
      then r2, 1
      else ((1. -. t.alpha) *. t.var) +. (t.alpha *. r2), t.vol_samples + 1
    | _ -> t.var, t.vol_samples
  in
  { t with
    book = Some book
  ; mid
  ; microprice
  ; var
  ; vol_samples
  ; updated_at = Some book.time
  }
;;

let book t = t.book
let mid t = t.mid
let microprice t = t.microprice

let best_bid t =
  Option.bind t.book ~f:L2.best_bid |> Option.map ~f:(fun (l : L2.Level.t) -> l.px)
;;

let best_ask t =
  Option.bind t.book ~f:L2.best_ask |> Option.map ~f:(fun (l : L2.Level.t) -> l.px)
;;

let vol_bps t = Float.sqrt t.var *. 10_000.
let vol_samples t = t.vol_samples

let with_vol_bps t vol_bps =
  let v = vol_bps /. 10_000. in
  { t with var = v *. v; vol_samples = Int.max 1 t.vol_samples }
;;

let updated_at t = t.updated_at
let age_ms t ~now = Option.map t.updated_at ~f:(fun u -> (now - u) / 1_000_000)
