open! Core
open Exchange_core
module Float_u = Stdlib_upstream_compatible.Float_u

(* One flat block per update: the two floats are stored unboxed and the optional fields
   are [or_null], so [on_book] allocates the record and nothing else. *)
type t =
  { book : L2.t or_null
  ; mid : Price.t or_null
  ; microprice : Price.t or_null
  ; var : Float_u.t
  ; vol_samples : int
  ; alpha : Float_u.t
  ; updated_at : int or_null
  }

let to_option = function
  | Null -> None
  | This x -> Some x
;;

(* The same shape as the record of options this replaced, so logs and the reference
   comparison in the tests read the same. *)
let sexp_of_t t =
  [%sexp
    { book : L2.t option = to_option t.book
    ; mid : Price.t option = to_option t.mid
    ; microprice : Price.t option = to_option t.microprice
    ; var : float = Float_u.to_float t.var
    ; vol_samples : int = t.vol_samples
    ; alpha : float = Float_u.to_float t.alpha
    ; updated_at : int option = to_option t.updated_at
    }]
;;

let alpha ~vol_window = Float_u.of_float (2. /. Float.of_int (Int.max 1 vol_window + 1))

let create ~vol_window =
  { book = Null
  ; mid = Null
  ; microprice = Null
  ; var = #0.
  ; vol_samples = 0
  ; alpha = alpha ~vol_window
  ; updated_at = Null
  }
;;

let with_vol_window t ~vol_window = { t with alpha = alpha ~vol_window }

let mid_of (bid : L2.Level.t) (ask : L2.Level.t) =
  Price.of_units ((Price.to_units bid.px + Price.to_units ask.px) / 2)
;;

let micro_of (bid : L2.Level.t) (ask : L2.Level.t) ~mid =
  let b = Price.to_float bid.px
  and a = Price.to_float ask.px
  and bs = Size.to_float bid.sz
  and as_ = Size.to_float ask.sz in
  if Float.(bs +. as_ <= 0.)
  then mid
  else (Price.of_float [@alert "-lossy"]) (((b *. as_) +. (a *. bs)) /. (bs +. as_))
;;

let on_book t (book : L2.t) =
  match book.bids, book.asks with
  | bid :: _, ask :: _ ->
    let mid = mid_of bid ask in
    let microprice = micro_of bid ask ~mid in
    (match t.mid with
     | This prev when Price.is_positive prev && Price.is_positive mid ->
       let r = Float.log (Price.to_float mid /. Price.to_float prev) in
       let r2 = Float_u.of_float (r *. r) in
       let var =
         if t.vol_samples = 0
         then r2
         else Float_u.(add (mul (sub #1. t.alpha) t.var) (mul t.alpha r2))
       in
       { t with
         book = This book
       ; mid = This mid
       ; microprice = This microprice
       ; var
       ; vol_samples = t.vol_samples + 1
       ; updated_at = This book.time
       }
     | This _ | Null ->
       { t with
         book = This book
       ; mid = This mid
       ; microprice = This microprice
       ; updated_at = This book.time
       })
  | _ ->
    { t with
      book = This book
    ; mid = Null
    ; microprice = Null
    ; updated_at = This book.time
    }
;;

let book t = to_option t.book
let mid t = to_option t.mid
let microprice t = to_option t.microprice
let mid_or_null t = t.mid
let microprice_or_null t = t.microprice

let best_bid_or_null t =
  match t.book with
  | This { bids = l :: _; _ } -> This l.px
  | This _ | Null -> Null
;;

let best_ask_or_null t =
  match t.book with
  | This { asks = l :: _; _ } -> This l.px
  | This _ | Null -> Null
;;

let best_bid t = to_option (best_bid_or_null t)
let best_ask t = to_option (best_ask_or_null t)
let vol_bps_u t = Float_u.(mul (sqrt t.var) #10_000.)
let vol_bps t = Float_u.to_float (vol_bps_u t)
let vol_samples t = t.vol_samples

let with_vol_bps t vol_bps =
  let v = vol_bps /. 10_000. in
  { t with var = Float_u.of_float (v *. v); vol_samples = Int.max 1 t.vol_samples }
;;

let updated_at t = to_option t.updated_at

let age_ms_or_null t ~now =
  match t.updated_at with
  | Null -> Null
  | This u -> This ((now - u) / 1_000_000)
;;

let age_ms t ~now = to_option (age_ms_or_null t ~now)
