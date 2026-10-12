open! Core
open Exchange_core

type t =
  | Keep of Order_id.t
  | Cancel of Order_id.t
  | Place of Order_request.t
[@@deriving sexp, compare, equal]

let is_change = function
  | Keep _ -> false
  | Cancel _ | Place _ -> true
;;
