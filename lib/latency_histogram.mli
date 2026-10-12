(** A histogram of latencies with two significant digits of resolution: a sample is
    counted in the bucket of its value rounded up to two significant digits (in
    nanoseconds), so a reported percentile is never below the true one and at most 10%
    above it. The maximum is kept exactly. Memory is bounded by the number of distinct
    buckets (about 90 per decade). *)

open! Core

type t [@@deriving sexp_of]

val create : unit -> t

(** Negative spans count as zero. *)
val record : t -> Time_ns.Span.t -> unit

val count : t -> int

(** The bucket bound below which a fraction [q] (in [[0, 1]]) of the samples fall. [None]
    when empty. *)
val percentile : t -> float -> Time_ns.Span.t option

module Stats : sig
  type t =
    { count : int
    ; p50 : Time_ns.Span.t
    ; p99 : Time_ns.Span.t
    ; max : Time_ns.Span.t
    }
  [@@deriving sexp_of]

  (** ["n=12 p50=1.2ms p99=3.4ms max=3.41ms"]. *)
  val to_string_hum : t -> string
end

(** [None] when empty. *)
val stats : t -> Stats.t option
