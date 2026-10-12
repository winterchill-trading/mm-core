open! Core
open Exchange_core
open Mm_core
open Helpers

(* A minimal venue: rests ALO orders with fresh oids, drops cancelled ones, ignores IOCs. *)
module Fake_venue = struct
  type t =
    { mutable orders : Open_order.t list
    ; mutable next_oid : int
    }

  let create () = { orders = []; next_oid = 100 }

  let apply t ~now actions =
    List.iter actions ~f:(function
      | Action.Keep _ -> ()
      | Cancel oid ->
        t.orders <- List.filter t.orders ~f:(fun o -> not (Order_id.equal o.order_id oid))
      | Place (r : Order_request.t) ->
        (match r.order_type, r.px with
         | Limit Alo, Some px ->
           t.next_oid <- t.next_oid + 1;
           t.orders
           <- t.orders
              @ [ { symbol = r.symbol
                  ; side = r.side
                  ; px
                  ; size = r.size
                  ; orig_size = Some r.size
                  ; order_id = Order_id.of_int t.next_oid
                  ; client_order_id = r.client_order_id
                  ; timestamp = now
                  }
                ]
         | _ -> ()))
  ;;
end

let summarize actions =
  let count f = List.count actions ~f in
  printf
    "keep=%d cancel=%d place=%d\n"
    (count (function
      | Action.Keep _ -> true
      | _ -> false))
    (count (function
      | Action.Cancel _ -> true
      | _ -> false))
    (count (function
      | Action.Place _ -> true
      | _ -> false))
;;

let print_places actions =
  List.iter actions ~f:(function
    | Action.Place r ->
      printf
        "  %s %s @ %s%s\n"
        (if Side.is_buy r.side then "buy " else "sell")
        (Size.to_string r.size)
        (Option.value_map r.px ~default:"-" ~f:Price.to_string)
        (if r.reduce_only then " reduce-only IOC" else "")
    | Keep _ | Cancel _ -> ())
;;

(* Tick, send the plan to the venue and acknowledge it with a snapshot. *)
let step venue t ~now =
  let now = ms now in
  let t, actions = Strategy.tick t ~now in
  Fake_venue.apply venue ~now actions;
  let t = Strategy.on_open_orders t venue.orders in
  summarize actions;
  print_places actions;
  print_s [%sexp (Strategy.mode t : Strategy.Mode.t)];
  t
;;

let%expect_test "lifecycle" =
  let venue = Fake_venue.create () in
  let t = Strategy.create Params.btc in
  let t = step venue t ~now:0 in
  [%expect {|
    keep=0 cancel=0 place=0
    (Pulled No_market_data)
    |}];
  let t = Strategy.on_book t (book ~time:100 ~bid:"99990" ~ask:"100010" ()) in
  let t = step venue t ~now:100 in
  [%expect
    {|
    keep=0 cancel=0 place=6
      buy  0.001 @ 99970
      buy  0.001 @ 99950
      buy  0.001 @ 99930
      sell 0.001 @ 100030
      sell 0.001 @ 100050
      sell 0.001 @ 100070
    Running
    |}];
  (* Same book: nothing to do, even after the requote interval. *)
  let t = Strategy.on_book t (book ~time:1_200 ~bid:"99990" ~ask:"100010" ()) in
  let t = step venue t ~now:1_200 in
  [%expect {|
    keep=0 cancel=0 place=0
    Running
    |}];
  (* Best bid quote fills: the ladder skews down and refills the bid at a lower price. *)
  let t =
    Strategy.on_fill t (fill ~time:1_300 ~tid:1 ~side:Buy ~px:"99970" ~sz:"0.001" ())
  in
  let venue_orders =
    List.filter venue.orders ~f:(fun o ->
      not (Side.equal o.side Buy && Price.equal o.px (px "99970")))
  in
  venue.orders <- venue_orders;
  let t = Strategy.on_open_orders t venue.orders in
  let t = step venue t ~now:1_300 in
  [%expect {|
    keep=5 cancel=0 place=1
      buy  0.001 @ 99966
    Running
    |}];
  (* Data goes stale: quotes are pulled, then restored on a fresh book. *)
  let t = step venue t ~now:7_000 in
  [%expect
    {|
    keep=0 cancel=6 place=0
    (Pulled (Stale_market_data (age_ms 5800)))
    |}];
  let t = Strategy.on_book t (book ~time:7_100 ~bid:"99990" ~ask:"100010" ()) in
  let t = step venue t ~now:7_100 in
  [%expect
    {|
    keep=0 cancel=0 place=6
      buy  0.001 @ 99966
      buy  0.001 @ 99946
      buy  0.001 @ 99926
      sell 0.001 @ 100026
      sell 0.001 @ 100046
      sell 0.001 @ 100066
    Running
    |}];
  ignore (t : Strategy.t)
;;

let%expect_test "waits for an acknowledgement before replanning" =
  let t = Strategy.create Params.btc in
  let t = Strategy.on_book t (book ~time:0 ~bid:"99990" ~ask:"100010" ()) in
  let t, first = Strategy.tick t ~now:(ms 0) in
  summarize first;
  let t = Strategy.on_book t (book ~time:500 ~bid:"100990" ~ask:"101010" ()) in
  let t, second = Strategy.tick t ~now:(ms 500) in
  summarize second;
  (* No snapshot arrived: after the interval it may cancel, but never re-places blind. *)
  let _t, third = Strategy.tick t ~now:(ms 1_000) in
  summarize third;
  [%expect
    {|
    keep=0 cancel=0 place=6
    keep=0 cancel=0 place=0
    keep=0 cancel=0 place=0
    |}]
;;

let%expect_test "drawdown halts permanently and flattens" =
  let venue = Fake_venue.create () in
  let t = Strategy.create Params.btc in
  let t = Strategy.on_book t (book ~time:0 ~bid:"99990" ~ask:"100010" ()) in
  let t = step venue t ~now:0 in
  [%expect
    {|
    keep=0 cancel=0 place=6
      buy  0.001 @ 99970
      buy  0.001 @ 99950
      buy  0.001 @ 99930
      sell 0.001 @ 100030
      sell 0.001 @ 100050
      sell 0.001 @ 100070
    Running
    |}];
  let t =
    Strategy.on_fill t (fill ~time:10 ~tid:1 ~side:Buy ~px:"100000" ~sz:"0.01" ())
  in
  let t = Strategy.on_book t (book ~time:2_000 ~bid:"93990" ~ask:"94010" ()) in
  let t = step venue t ~now:2_000 in
  [%expect
    {|
    keep=0 cancel=6 place=1
      sell 0.01 @ 93530 reduce-only IOC
    (Halted (Max_drawdown (pnl -60) (peak 0)))
    |}];
  (* Still long (the IOC did not fill here): flatten again after the interval. *)
  let t = Strategy.on_book t (book ~time:3_000 ~bid:"99990" ~ask:"100010" ()) in
  let t = step venue t ~now:3_000 in
  [%expect
    {|
    keep=0 cancel=0 place=1
      sell 0.01 @ 99500 reduce-only IOC
    (Halted (Max_drawdown (pnl -60) (peak 0)))
    |}];
  let t =
    Strategy.on_fill t (fill ~time:3_010 ~tid:2 ~side:Sell ~px:"99950" ~sz:"0.01" ())
  in
  let t = step venue t ~now:4_000 in
  [%expect
    {|
    keep=0 cancel=0 place=0
    (Halted (Max_drawdown (pnl -60) (peak 0)))
    |}];
  ignore (t : Strategy.t)
;;

let%expect_test "a runaway requote loop trips the action rate kill switch" =
  (* The mid flips by 10 bps every millisecond, so every tick replaces the whole ladder. *)
  let params = { Params.btc with max_actions_per_sec = 40 } in
  let venue = Fake_venue.create () in
  let rec go t i =
    if i > 20
    then t
    else (
      let now = ms i in
      let bid, ask = if i % 2 = 0 then "99990", "100010" else "100090", "100110" in
      let t = Strategy.on_book t (book ~time:i ~bid ~ask ()) in
      let t, plan = Strategy.tick t ~now in
      Fake_venue.apply venue ~now plan;
      let t = Strategy.on_open_orders t venue.orders in
      match Strategy.mode t with
      | Halted _ ->
        printf "halted at %d ms\n" i;
        t
      | Running | Pulled _ -> go t (i + 1))
  in
  let t = go (Strategy.create params) 0 in
  print_s [%sexp (Strategy.mode t : Strategy.Mode.t)];
  [%expect {|
    halted at 4 ms
    (Halted (Max_action_rate (actions 42)))
    |}]
;;
