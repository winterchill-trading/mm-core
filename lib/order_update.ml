open! Core
open Exchange_core

module Status = struct
  type t =
    | New
    | Partially_filled
    | Filled
    | Cancelled
    | Rejected
  [@@deriving sexp, compare, equal, enumerate]

  let is_final = function
    | New | Partially_filled -> false
    | Filled | Cancelled | Rejected -> true
  ;;
end

type t =
  { order_id : Order_id.t
  ; client_order_id : Client_order_id.t option
  ; symbol : Symbol.t
  ; side : Side.t
  ; px : Price.t
  ; status : Status.t
  ; filled : Size.t
  ; remaining : Size.t
  ; time : int
  }
[@@deriving sexp, compare, equal]
