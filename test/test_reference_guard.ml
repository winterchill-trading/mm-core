open! Core
open Exchange_core
open Mm_core

let ms n = n * 1_000_000
let px = Price.of_string

(* Feeds a script of reference prices and venue mids at times in milliseconds; prints the
   status after each step and the events of the step. *)
let run ?(config = Reference_guard.Config.default) steps =
  ignore
    (List.fold steps ~init:(Reference_guard.create config) ~f:(fun guard (at, step) ->
       let now = ms at in
       let after =
         match step with
         | `Reference p -> Reference_guard.on_reference guard ~now (px p)
         | `Mid p -> Reference_guard.check guard ~now ~venue_mid:(This (px p))
         | `No_mid -> Reference_guard.check guard ~now ~venue_mid:Null
       in
       let events = Reference_guard.events_between ~before:guard ~after ~now in
       print_s
         [%message
           ""
             ~t:(at : int)
             ~status:(Reference_guard.status after : Reference_guard.Status.t)
             ~events:(events : Reference_guard.Event.t list)];
       after)
     : Reference_guard.t)
;;

let%expect_test "divergence past the grace period pulls, convergence resumes" =
  run
    [ 0, `Mid "100000"
    ; 0, `Reference "100000"
    ; 0, `Mid "100010"
    ; 1_000, `Mid "101000"
    ; 2_500, `Mid "101000"
    ; 3_000, `Mid "100900"
    ; 3_500, `Reference "100000"
    ; 4_000, `Mid "100400"
    ; 5_000, `No_mid
    ];
  [%expect
    {|
    ((t 0) (status (Reference_missing (age_ms ())))
     (events ((Reference_missing (age_ms ())))))
    ((t 0) (status (Reference_missing (age_ms ()))) (events ()))
    ((t 0) (status Within_band) (events (Resumed)))
    ((t 1000) (status (Diverging (since 1000000000) (divergence_bps 100)))
     (events ()))
    ((t 2500) (status (Diverging (since 1000000000) (divergence_bps 100)))
     (events ()))
    ((t 3000)
     (status
      (Diverged (since 1000000000) (divergence_bps 90) (venue_mid 100900)
       (reference_mid 100000)))
     (events
      ((Diverged (divergence_bps 90) (held_ms 2000) (venue_mid 100900)
        (reference_mid 100000)))))
    ((t 3500)
     (status
      (Diverged (since 1000000000) (divergence_bps 90) (venue_mid 100900)
       (reference_mid 100000)))
     (events ()))
    ((t 4000) (status Within_band) (events (Resumed)))
    ((t 5000) (status Unchecked) (events ()))
    |}]
;;

let%expect_test "a reference jump skips the grace period" =
  run
    ~config:{ Reference_guard.Config.default with max_jump_bps = Some 100. }
    [ 0, `Reference "100000"
    ; 0, `Mid "100000"
    ; 100, `Reference "102000"
    ; 100, `Mid "100000"
    ; 200, `Mid "101900"
    ];
  [%expect
    {|
    ((t 0) (status Unchecked) (events ()))
    ((t 0) (status Within_band) (events ()))
    ((t 100) (status Within_band)
     (events ((Reference_jumped (from_ 100000) (to_ 102000) (jump_bps 200)))))
    ((t 100)
     (status
      (Diverged (since 100000000) (divergence_bps -196.078431372549)
       (venue_mid 100000) (reference_mid 102000)))
     (events
      ((Diverged (divergence_bps -196.078431372549) (held_ms 0)
        (venue_mid 100000) (reference_mid 102000)))))
    ((t 200) (status Within_band) (events (Resumed)))
    |}]
;;

let%expect_test "a reference that stops updating" =
  let steps = [ 0, `Reference "100000"; 10_000, `Mid "100000"; 31_000, `Mid "100000" ] in
  run steps;
  run ~config:{ Reference_guard.Config.default with stale_reference = Ignore } steps;
  [%expect
    {|
    ((t 0) (status Unchecked) (events ()))
    ((t 10000) (status Within_band) (events ()))
    ((t 31000) (status (Reference_missing (age_ms (31000))))
     (events ((Reference_missing (age_ms (31000))))))
    ((t 0) (status Unchecked) (events ()))
    ((t 10000) (status Within_band) (events ()))
    ((t 31000) (status Unchecked) (events ()))
    |}]
;;

(* Zero on both platforms: [check] returns its argument while the status stays the same
   constant constructor, and the divergence is computed unboxed. *)
let%expect_test "checking a mid inside the band allocates nothing" =
  let guard =
    Reference_guard.on_reference
      (Reference_guard.create Reference_guard.Config.default)
      ~now:0
      (px "100000")
  in
  let mid = px "100020" in
  let guard = Reference_guard.check guard ~now:1 ~venue_mid:(This mid) in
  let before = Gc.minor_words () in
  let g = ref guard in
  for i = 1 to 1000 do
    g := Reference_guard.check !g ~now:(ms i) ~venue_mid:(This mid)
  done;
  printf "%d words, same value: %b\n" (Gc.minor_words () - before) (phys_equal !g guard);
  [%expect {| 0 words, same value: true |}]
;;
