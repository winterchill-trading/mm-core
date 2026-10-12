(* Random event scripts through [Live.Make] on a synthetic clock. After every event the
   loop is allowed to settle, then the resting orders must not cross each other, must sit
   on distinct prices, and fully filling either side must keep the position within
   [max_position]. Every order placed must be post-only against the book of that moment
   (the venue rejects it otherwise, and the script fails on any rejection).

   A resting order kept within [requote_threshold_bps] may end up level with the book
   after a jump: pushed books do not include our orders, so that is not checked. *)

open! Core
open! Async
open Exchange_core
open Mm_core
module Live_memory = Live.Make (Memory_venue)

module Event = struct
  type t =
    | Book of { mid_step : int } (** the mid moves by this many dollars *)
    | Fill of { pick : int } (** one of our resting orders is taken *)
    | Wait of { ms : int }
  [@@deriving sexp_of]

  let quickcheck_generator =
    Quickcheck.Generator.weighted_union
      [ ( 4.
        , Quickcheck.Generator.map (Int.gen_incl (-40) 40) ~f:(fun mid_step ->
            Book { mid_step }) )
      ; ( 2.
        , Quickcheck.Generator.map Int.quickcheck_generator ~f:(fun pick -> Fill { pick })
        )
      ; 2., Quickcheck.Generator.map (Int.gen_incl 0 400) ~f:(fun ms -> Wait { ms })
      ]
  ;;
end

let params =
  { Params.btc with
    requote_interval_ms = 100
  ; max_position = Size.of_string "0.004"
  ; max_drawdown = Usd.of_string "1000000"
  ; max_actions_per_sec = 10_000 (* books every millisecond would trip the default *)
  }
;;

let check ~live ~venue ~mid =
  let orders = Memory_venue.orders venue in
  let side s = List.filter orders ~f:(fun (o : Open_order.t) -> Side.equal o.side s) in
  let bids = side Buy
  and asks = side Sell in
  let prices os = List.map os ~f:(fun (o : Open_order.t) -> o.px) in
  let total os = Size.sum (List.map os ~f:(fun (o : Open_order.t) -> o.size)) in
  let best_bid = List.max_elt (prices bids) ~compare:Price.compare
  and best_ask = List.min_elt (prices asks) ~compare:Price.compare in
  let position = Memory_venue.position venue in
  let fail msg =
    raise_s
      [%message
        msg
          (mid : int)
          (position : Size.t)
          (orders : Open_order.t list)
          ~strategy:(Live_memory.strategy live : Strategy.t)]
  in
  (match best_bid, best_ask with
   | Some b, Some a when Price.(b >= a) -> fail "own quotes cross"
   | _ -> ());
  List.iter [ bids; asks ] ~f:(fun os ->
    if List.contains_dup (prices os) ~compare:Price.compare
    then fail "two orders on one price");
  if Size.(position + total bids > params.max_position)
  then fail "bids exceed max position";
  if Size.(position - total asks < neg params.max_position)
  then fail "asks exceed max position";
  if (Live_memory.report live).orders_rejected > 0 then fail "an order was rejected"
;;

let run_script ts events =
  let venue =
    Memory_venue.create ~time_source:(Time_source.read_only ts) Memory_venue.Bugs.none
  in
  let config = { Live.Config.params; summary_interval = Time_ns.Span.of_int_sec 3600 } in
  let settle span =
    let%bind () = Time_source.advance_by_alarms_by ts span in
    Scheduler.yield_until_no_jobs_remain ()
  in
  let push_book mid =
    Memory_venue.push_book
      venue
      { symbol = params.symbol
      ; time = Time_ns.to_int_ns_since_epoch (Time_source.now ts)
      ; bids = [ { px = Price.of_int (mid - 10); sz = Size.one; n = 1 } ]
      ; asks = [ { px = Price.of_int (mid + 10); sz = Size.one; n = 1 } ]
      }
  in
  let%bind live =
    Live_memory.start ~time_source:(Time_source.read_only ts) venue config >>| ok_exn
  in
  let mid = ref 100_000 in
  push_book !mid;
  let%bind () = settle (Time_ns.Span.of_int_ms 10) in
  let%bind () =
    Deferred.List.iteri ~how:`Sequential events ~f:(fun i (event : Event.t) ->
      (match event with
       | Book { mid_step } ->
         mid := !mid + mid_step;
         push_book !mid
       | Fill { pick } ->
         (match Memory_venue.orders venue with
          | [] -> ()
          | orders ->
            let o = List.nth_exn orders (pick % List.length orders) in
            Memory_venue.fill_resting venue o ~tid:(i + 1))
       | Wait _ -> ());
      let span =
        match event with
        | Wait { ms } -> Time_ns.Span.of_int_ms ms
        | Book _ | Fill _ -> Time_ns.Span.of_int_ms 1
      in
      let%bind () = settle span in
      check ~live ~venue ~mid:!mid;
      (* Fresh data keeps the strategy out of [Pulled]. *)
      return ())
  in
  let stopped = Live_memory.stop live in
  let%bind () = settle (Time_ns.Span.of_int_sec 1) in
  let%bind report = stopped in
  return (report, venue)
;;

let%expect_test "random event scripts keep quotes uncrossed and inside max_position" =
  Log.Global.set_output [];
  let gen = Quickcheck.Generator.list_with_length 60 Event.quickcheck_generator in
  let%bind stats =
    Deferred.List.init ~how:`Sequential 40 ~f:(fun seed ->
      let events =
        Quickcheck.Generator.generate gen ~size:30 ~random:(Splittable_random.of_int seed)
      in
      let ts = Time_source.create ~now:(Time_ns.of_int_ns_since_epoch 0) () in
      match%map Monitor.try_with ~extract_exn:true (fun () -> run_script ts events) with
      | Ok (report, venue) -> report.fills, Memory_venue.orders venue |> List.is_empty
      | Error exn ->
        raise_s
          [%message "property failed" (seed : int) (exn : exn) (events : Event.t list)])
  in
  let fills = List.sum (module Int) stats ~f:fst in
  print_s
    [%message
      ""
        ~scripts:(List.length stats : int)
        ~total_fills:(fills : int)
        ~all_cancelled_on_stop:(List.for_all stats ~f:snd : bool)];
  [%expect {| ((scripts 40) (total_fills 633) (all_cancelled_on_stop true)) |}];
  return ()
;;
