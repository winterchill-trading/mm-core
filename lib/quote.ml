open! Core
open Exchange_core

type t =
  { side : Side.t
  ; level : int
  ; px : Price.t
  ; sz : Size.t
  }
[@@deriving sexp, compare, equal]
