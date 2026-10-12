open! Core

type t =
  { buckets : int Int.Table.t (* bucket upper bound in ns -> samples *)
  ; mutable count : int
  ; mutable max_ns : int
  }
[@@deriving sexp_of]

let create () = { buckets = Int.Table.create (); count = 0; max_ns = 0 }

(* [ns] rounded up to two significant digits. *)
let bucket ns =
  if ns < 100
  then ns
  else (
    let rec scale ns pow =
      if ns >= 100 then scale ((ns + 9) / 10) (pow * 10) else ns, pow
    in
    let mantissa, pow = scale ns 1 in
    mantissa * pow)
;;

let record t span =
  let ns = Int.max 0 (Time_ns.Span.to_int_ns span) in
  Hashtbl.incr t.buckets (bucket ns);
  t.count <- t.count + 1;
  t.max_ns <- Int.max t.max_ns ns
;;

let count t = t.count

let percentile t q =
  if t.count = 0
  then None
  else (
    let rank = Int.max 1 (Float.iround_up_exn (q *. Float.of_int t.count)) in
    let sorted = Hashtbl.to_alist t.buckets |> List.sort ~compare:[%compare: int * int] in
    let rec find seen = function
      | [] -> t.max_ns
      | (bound, n) :: rest -> if seen + n >= rank then bound else find (seen + n) rest
    in
    (* A bucket bound can exceed the exact maximum; never report more than it. *)
    Some (Time_ns.Span.of_int_ns (Int.min t.max_ns (find 0 sorted))))
;;

module Stats = struct
  type t =
    { count : int
    ; p50 : Time_ns.Span.t
    ; p99 : Time_ns.Span.t
    ; max : Time_ns.Span.t
    }
  [@@deriving sexp_of]

  let to_string_hum t =
    sprintf
      "n=%d p50=%s p99=%s max=%s"
      t.count
      (Time_ns.Span.to_short_string t.p50)
      (Time_ns.Span.to_short_string t.p99)
      (Time_ns.Span.to_short_string t.max)
  ;;
end

let stats t =
  match percentile t 0.5, percentile t 0.99 with
  | Some p50, Some p99 ->
    Some { Stats.count = t.count; p50; p99; max = Time_ns.Span.of_int_ns t.max_ns }
  | _ -> None
;;
