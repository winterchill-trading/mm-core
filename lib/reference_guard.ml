open! Core
open Exchange_core
module Float_u = Stdlib_upstream_compatible.Float_u

module Stale_reference = struct
  type t =
    | Pull
    | Ignore
  [@@deriving sexp, compare, equal]
end

module Config = struct
  type t =
    { band_bps : float
    ; grace_ms : Time_ns.Span.t
    ; max_jump_bps : float option
    ; max_reference_age_ms : Time_ns.Span.t
    ; stale_reference : Stale_reference.t
    }
  [@@deriving sexp, compare, equal]

  let default =
    { band_bps = 50.
    ; grace_ms = Time_ns.Span.of_int_sec 2
    ; max_jump_bps = None
    ; max_reference_age_ms = Time_ns.Span.of_int_sec 30
    ; stale_reference = Pull
    }
  ;;

  let positive x = Float.is_finite x && Float.( > ) x 0.

  let validate t =
    let errors =
      List.filter_map
        [ not (positive t.band_bps), "band_bps must be positive and finite"
        ; Time_ns.Span.is_negative t.grace_ms, "grace_ms < 0"
        ; ( Option.exists t.max_jump_bps ~f:(fun x -> not (positive x))
          , "max_jump_bps must be positive and finite" )
        ; ( not (Time_ns.Span.is_positive t.max_reference_age_ms)
          , "max_reference_age_ms <= 0" )
        ]
        ~f:(fun (bad, msg) -> Option.some_if bad msg)
    in
    match errors with
    | [] -> Ok t
    | errors ->
      Or_error.error_s
        [%message "invalid reference guard" ~_:(errors : string list) ~config:(t : t)]
  ;;
end

module Status = struct
  type t =
    | Unchecked
    | Within_band
    | Diverging of
        { since : int
        ; divergence_bps : float
        }
    | Diverged of
        { since : int
        ; divergence_bps : float
        ; venue_mid : Price.t
        ; reference_mid : Price.t
        }
    | Reference_missing of { age_ms : int option }
  [@@deriving sexp, compare, equal]

  let pulls = function
    | Diverged _ | Reference_missing _ -> true
    | Unchecked | Within_band | Diverging _ -> false
  ;;
end

module Jump = struct
  type t =
    { from_ : Price.t
    ; to_ : Price.t
    ; jump_bps : float
    }
  [@@deriving sexp_of]
end

type t =
  { config : Config.t
  ; reference : Price.t option
  ; reference_at : int
  ; jumps : int (** reference jumps seen so far *)
  ; last_jump : Jump.t option
  ; jump_pending : bool (** a jump not yet followed by a mid back inside the band *)
  ; status : Status.t
  }
[@@deriving sexp_of]

let create config =
  { config = Config.validate config |> ok_exn
  ; reference = None
  ; reference_at = 0
  ; jumps = 0
  ; last_jump = None
  ; jump_pending = false
  ; status = Unchecked
  }
;;

let config t = t.config
let status t = t.status
let reference t = t.reference

let with_config t config =
  { t with config = Config.validate config |> ok_exn; jump_pending = false }
;;

let on_reference t ~now price =
  let jump =
    match t.reference, t.config.max_jump_bps with
    | Some prev, Some max_jump ->
      let bps = Float_u.to_float (Fx.bps_between_u ~base:prev price) in
      if Float.( > ) (Float.abs bps) max_jump
      then Some { Jump.from_ = prev; to_ = price; jump_bps = bps }
      else None
    | _ -> None
  in
  match jump with
  | None -> { t with reference = Some price; reference_at = now }
  | Some _ ->
    { t with
      reference = Some price
    ; reference_at = now
    ; jumps = t.jumps + 1
    ; last_jump = jump
    ; jump_pending = true
    }
;;

let set t (status : Status.t) =
  match t.status, status with
  | Unchecked, Unchecked | Within_band, Within_band -> t
  | _ -> { t with status }
;;

let check t ~now ~(venue_mid : Price.t or_null) =
  let ns = Time_ns.Span.to_int_ns in
  let fresh =
    match t.reference with
    | Some _ -> now - t.reference_at <= ns t.config.max_reference_age_ms
    | None -> false
  in
  if not fresh
  then (
    match t.config.stale_reference with
    | Ignore -> set t Unchecked
    | Pull ->
      let age_ms =
        match t.reference with
        | None -> None
        | Some _ -> Some ((now - t.reference_at) / 1_000_000)
      in
      (match t.status with
       | Reference_missing { age_ms = a } when [%equal: int option] a age_ms -> t
       | _ -> { t with status = Reference_missing { age_ms } }))
  else (
    match venue_mid, t.reference with
    | Null, _ | _, None -> set t Unchecked
    | This mid, Some reference ->
      let divergence = Fx.bps_between_u ~base:reference mid in
      if Float_u.compare (Float_u.abs divergence) (Float_u.of_float t.config.band_bps)
         <= 0
      then (
        let t = if t.jump_pending then { t with jump_pending = false } else t in
        set t Within_band)
      else (
        let divergence_bps = Float_u.to_float divergence in
        let since =
          match t.status with
          | Diverging { since; _ } | Diverged { since; _ } -> since
          | Unchecked | Within_band | Reference_missing _ -> now
        in
        if t.jump_pending || now - since >= ns t.config.grace_ms
        then
          { t with
            status =
              Diverged
                { since; divergence_bps; venue_mid = mid; reference_mid = reference }
          }
        else { t with status = Diverging { since; divergence_bps } }))
;;

module Event = struct
  type t =
    | Diverged of
        { divergence_bps : float
        ; held_ms : int
        ; venue_mid : Price.t
        ; reference_mid : Price.t
        }
    | Reference_jumped of
        { from_ : Price.t
        ; to_ : Price.t
        ; jump_bps : float
        }
    | Reference_missing of { age_ms : int option }
    | Resumed
  [@@deriving sexp, compare, equal]
end

let events_between ~before ~after ~now =
  let jumped =
    match after.last_jump with
    | Some { Jump.from_; to_; jump_bps } when after.jumps > before.jumps ->
      [ Event.Reference_jumped { from_; to_; jump_bps } ]
    | _ -> []
  in
  let moved : Event.t list =
    match before.status, after.status with
    | Diverged _, Diverged _ | Reference_missing _, Reference_missing _ -> []
    | _, Diverged { since; divergence_bps; venue_mid; reference_mid } ->
      [ Diverged
          { divergence_bps
          ; held_ms = (now - since) / 1_000_000
          ; venue_mid
          ; reference_mid
          }
      ]
    | _, Reference_missing { age_ms } -> [ Reference_missing { age_ms } ]
    | (Diverged _ | Reference_missing _), (Unchecked | Within_band | Diverging _) ->
      [ Resumed ]
    | (Unchecked | Within_band | Diverging _), (Unchecked | Within_band | Diverging _) ->
      []
  in
  jumped @ moved
;;
