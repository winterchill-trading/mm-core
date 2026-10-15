open! Core

module Config = struct
  type t =
    { widen_from : float
    ; pull_at : float
    ; max_extra_half_spread_bps : float
    ; skew_bps : float
    ; size_cut : float
    ; max_signal_age_ms : Time_ns.Span.t
    }
  [@@deriving sexp, compare, equal]

  let default =
    { widen_from = 0.8
    ; pull_at = 0.95
    ; max_extra_half_spread_bps = 5.
    ; skew_bps = 2.
    ; size_cut = 0.5
    ; max_signal_age_ms = Time_ns.Span.minute
    }
  ;;

  let validate t =
    let finite =
      List.for_all
        [ t.widen_from; t.pull_at; t.max_extra_half_spread_bps; t.skew_bps; t.size_cut ]
        ~f:Float.is_finite
    in
    let errors =
      List.filter_map
        [ not finite, "every float must be finite"
        ; Float.(t.widen_from >= t.pull_at), "widen_from must be below pull_at"
        ; Float.(t.max_extra_half_spread_bps < 0.), "max_extra_half_spread_bps < 0"
        ; Float.(t.skew_bps < 0.), "skew_bps < 0"
        ; Float.(t.size_cut < 0. || t.size_cut >= 1.), "size_cut outside [0, 1)"
        ; not (Time_ns.Span.is_positive t.max_signal_age_ms), "max_signal_age_ms <= 0"
        ]
        ~f:(fun (bad, msg) -> Option.some_if bad msg)
    in
    match errors with
    | [] -> Ok t
    | errors ->
      Or_error.error_s
        [%message "invalid toxicity gate" ~_:(errors : string list) ~config:(t : t)]
  ;;
end

module Signal = struct
  type t =
    { value : float
    ; imbalance : float
    ; time : int
    }
  [@@deriving sexp, compare, equal]
end

module Decision = struct
  type t =
    | Normal
    | Widen of
        { intensity : float
        ; extra_half_spread_bps : float
        ; reservation_shift_bps : float
        ; size_factor : float
        }
    | Pull of { signal : float }
  [@@deriving sexp, compare, equal]
end

let decide (c : Config.t) ~now (signal : Signal.t option) : Decision.t =
  match signal with
  | None -> Normal
  | Some s when now - s.time > Time_ns.Span.to_int_ns c.max_signal_age_ms -> Normal
  | Some s when Float.(s.value >= c.pull_at) -> Pull { signal = s.value }
  | Some s when Float.(s.value < c.widen_from) || Float.is_nan s.value -> Normal
  | Some s ->
    let intensity =
      Float.clamp_exn
        ((s.value -. c.widen_from) /. (c.pull_at -. c.widen_from))
        ~min:0.
        ~max:1.
    in
    let imbalance = Float.clamp_exn s.imbalance ~min:(-1.) ~max:1. in
    Widen
      { intensity
      ; extra_half_spread_bps = intensity *. c.max_extra_half_spread_bps
      ; reservation_shift_bps = intensity *. c.skew_bps *. imbalance
      ; size_factor = 1. -. (intensity *. c.size_cut)
      }
;;

module Event = struct
  type t =
    | Widening of { signal : float }
    | Pulled of { signal : float }
    | Cleared
  [@@deriving sexp, compare, equal]

  let of_change ~(before : Decision.t) ~(after : Decision.t) ~signal =
    match before, after with
    | Normal, Normal | Widen _, Widen _ | Pull _, Pull _ -> None
    | _, Widen _ -> Some (Widening { signal })
    | _, Pull _ -> Some (Pulled { signal })
    | _, Normal -> Some Cleared
  ;;
end
