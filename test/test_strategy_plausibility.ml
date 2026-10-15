open! Core
open Exchange_core
open Mm_core
open Helpers

(* Places rest at once with fresh order ids, cancels remove them. *)
module Venue = struct
  type t =
    { mutable orders : Open_order.t list
    ; mutable next_oid : int
    }

  let create () = { orders = []; next_oid = 1 }

  let apply t ~now actions =
    List.iter actions ~f:(function
      | Action.Keep _ -> ()
      | Cancel oid ->
        t.orders <- List.filter t.orders ~f:(fun o -> not (Order_id.equal o.order_id oid))
      | Place (r : Order_request.t) ->
        t.next_oid <- t.next_oid + 1;
        t.orders
        <- t.orders
           @ [ { symbol = r.symbol
               ; side = r.side
               ; px = Option.value_exn r.px
               ; size = r.size
               ; orig_size = Some r.size
               ; order_id = Order_id.of_int t.next_oid
               ; client_order_id = r.client_order_id
               ; timestamp = now
               }
             ])
  ;;
end

(* Ticks at [at] ms, applies the plan, acknowledges it; prints the mode, the resting
   ladder and the events. *)
let step venue t ~at =
  let now = ms at in
  let t, plan, events = Strategy.tick_with_events t ~now in
  Venue.apply venue ~now plan;
  let t = Strategy.on_open_orders t venue.orders in
  let ladder =
    List.map venue.orders ~f:(fun o ->
      sprintf
        "%s%s@%s"
        (if Side.is_buy o.side then "b" else "s")
        (Size.to_string o.size)
        (Price.to_string o.px))
  in
  print_s
    [%message
      ""
        ~t:(at : int)
        ~mode:(Strategy.mode t : Strategy.Mode.t)
        ~cancels:
          (List.count plan ~f:(function
             | Cancel _ -> true
             | _ -> false)
           : int)
        ~resting:(String.concat ~sep:" " ladder : string)
        ~events:(events : Strategy.Event.t list)];
  t
;;

let book_at ~at mid =
  let m = Price.of_string mid in
  book
    ~time:at
    ~bid:Price.(to_string (m - of_int 10))
    ~ask:Price.(to_string (m + of_int 10))
    ()
;;

let params = { Params.btc with levels = 1 }

let%expect_test "a frozen reference while the book moves: the guard pulls, then resumes" =
  let run params =
    let venue = Venue.create () in
    let t = Strategy.create params in
    let t = Strategy.on_reference t ~now:0 (px "100000") in
    let t = Strategy.on_book t (book_at ~at:0 "100000") in
    let t = step venue t ~at:0 in
    (* The venue book walks 1% up; the reference stays where it was. *)
    let t = Strategy.on_book t (book_at ~at:1_000 "101000") in
    let t = step venue t ~at:1_000 in
    let t = step venue t ~at:2_500 in
    let t = step venue t ~at:3_100 in
    let t = step venue t ~at:4_000 in
    (* The reference catches up. *)
    let t = Strategy.on_reference t ~now:(ms 4_500) (px "100990") in
    let (_ : Strategy.t) = step venue t ~at:5_000 in
    ()
  in
  run { params with reference_guard = Some Reference_guard.Config.default };
  [%expect
    {|
    ((t 0) (mode Running) (cancels 0) (resting "b0.001@99970 s0.001@100030")
     (events ()))
    ((t 1000) (mode Running) (cancels 2) (resting "b0.001@100717 s0.001@101283")
     (events ()))
    ((t 2500) (mode Running) (cancels 0) (resting "b0.001@100717 s0.001@101283")
     (events ()))
    ((t 3100)
     (mode (Pulled (Reference_divergence (divergence_bps 100) (held_ms 2100))))
     (cancels 2) (resting "")
     (events
      ((Reference
        (Diverged (divergence_bps 100) (held_ms 2100) (venue_mid 101000)
         (reference_mid 100000))))))
    ((t 4000)
     (mode (Pulled (Reference_divergence (divergence_bps 100) (held_ms 2100))))
     (cancels 0) (resting "") (events ()))
    ((t 5000) (mode Running) (cancels 0) (resting "b0.001@100717 s0.001@101283")
     (events ((Reference Resumed))))
    |}];
  print_endline "without the guard:";
  run params;
  [%expect
    {|
    without the guard:
    ((t 0) (mode Running) (cancels 0) (resting "b0.001@99970 s0.001@100030")
     (events ()))
    ((t 1000) (mode Running) (cancels 2) (resting "b0.001@100717 s0.001@101283")
     (events ()))
    ((t 2500) (mode Running) (cancels 0) (resting "b0.001@100717 s0.001@101283")
     (events ()))
    ((t 3100) (mode Running) (cancels 0) (resting "b0.001@100717 s0.001@101283")
     (events ()))
    ((t 4000) (mode Running) (cancels 0) (resting "b0.001@100717 s0.001@101283")
     (events ()))
    ((t 5000) (mode Running) (cancels 0) (resting "b0.001@100717 s0.001@101283")
     (events ()))
    |}]
;;

let%expect_test "toxic flow: wider and skewed, then pulled, then back" =
  let venue = Venue.create () in
  let t =
    Strategy.create { params with toxicity_gate = Some Toxicity_gate.Config.default }
  in
  let t = Strategy.on_book t (book_at ~at:0 "100000") in
  let t = Strategy.on_toxicity t ~now:0 ~signal:0.5 ~imbalance:0. in
  let t = step venue t ~at:0 in
  (* Informed buying: the quotes move up and apart, sizes halve at full intensity. *)
  let t = Strategy.on_toxicity t ~now:(ms 100) ~signal:0.9 ~imbalance:1. in
  let t = step venue t ~at:100 in
  let t = Strategy.on_toxicity t ~now:(ms 200) ~signal:0.97 ~imbalance:1. in
  let t = step venue t ~at:200 in
  let t = step venue t ~at:1_300 in
  let t = Strategy.on_toxicity t ~now:(ms 1_400) ~signal:0.3 ~imbalance:0. in
  let t = step venue t ~at:1_400 in
  (* A signal older than a minute no longer counts. *)
  let t = Strategy.on_toxicity t ~now:(ms 1_500) ~signal:0.99 ~imbalance:0. in
  let t = step venue t ~at:2_600 in
  let t = Strategy.on_book t (book_at ~at:62_000 "100000") in
  let (_ : Strategy.t) = step venue t ~at:62_000 in
  [%expect
    {|
    ((t 0) (mode Running) (cancels 0) (resting "b0.001@99970 s0.001@100030")
     (events ()))
    ((t 100) (mode Running) (cancels 2)
     (resting "b0.00066@99949 s0.00066@100077")
     (events ((Toxicity (Widening (signal 0.9))))))
    ((t 200) (mode (Pulled (Toxic_flow (signal 0.97)))) (cancels 2) (resting "")
     (events ((Toxicity (Pulled (signal 0.97))))))
    ((t 1300) (mode (Pulled (Toxic_flow (signal 0.97)))) (cancels 0) (resting "")
     (events ()))
    ((t 1400) (mode Running) (cancels 0) (resting "b0.001@99970 s0.001@100030")
     (events ((Toxicity Cleared))))
    ((t 2600) (mode (Pulled (Toxic_flow (signal 0.99)))) (cancels 2) (resting "")
     (events ((Toxicity (Pulled (signal 0.99))))))
    ((t 62000) (mode Running) (cancels 0) (resting "b0.001@99970 s0.001@100030")
     (events ((Toxicity Cleared))))
    |}]
;;

let%expect_test "update_params adds and removes the guard, keeping its reference" =
  let t = Strategy.create params in
  let t = Strategy.on_reference t ~now:0 (px "100000") in
  print_s [%sexp (Option.is_some (Strategy.reference_guard t) : bool)];
  let with_guard =
    { params with reference_guard = Some Reference_guard.Config.default }
  in
  let t = Strategy.update_params t with_guard |> ok_exn in
  let t = Strategy.on_reference t ~now:0 (px "100000") in
  let t =
    Strategy.update_params
      t
      { with_guard with
        reference_guard = Some { Reference_guard.Config.default with band_bps = 10. }
      }
    |> ok_exn
  in
  print_s
    [%sexp
      (Option.bind (Strategy.reference_guard t) ~f:Reference_guard.reference
       : Price.t option)];
  let t = Strategy.update_params t params |> ok_exn in
  print_s [%sexp (Option.is_some (Strategy.reference_guard t) : bool)];
  print_s
    [%sexp
      (Params.validate
         { params with
           toxicity_gate = Some { Toxicity_gate.Config.default with pull_at = 0.1 }
         }
       |> Result.map ~f:ignore
       : unit Or_error.t)];
  [%expect
    {|
    false
    (100000)
    false
    (Error
     ("invalid params"
      (errors
       ( "toxicity_gate: (\"invalid toxicity gate\" (\"widen_from must be below pull_at\")\
        \n (config\
        \n  ((widen_from 0.8) (pull_at 0.1) (max_extra_half_spread_bps 5) (skew_bps 2)\
        \n   (size_cut 0.5) (max_signal_age_ms 1m))))"))))
    |}]
;;
