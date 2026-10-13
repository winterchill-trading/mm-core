open! Core
open Mm_core
open Helpers

(* Minor heap words allocated per call, averaged over 1000 calls after one warm-up call. A
   change here is a change in the quoter's allocation profile; the core_bench numbers are
   in bench/. *)
let words_per_call f =
  ignore (f () : _);
  let before = Gc.minor_words () in
  for _ = 1 to 1_000 do
    ignore (Sys.opaque_identity (f ()) : _)
  done;
  (Gc.minor_words () - before) / 1_000
;;

let%expect_test "minor words per Quoter.quote" =
  let params = Params.btc in
  let market =
    Market_state.with_vol_bps
      (Market_state.on_book
         (Market_state.create ~vol_window:params.vol_window)
         (book ~bid:"99990" ~ask:"100010" ~bid_sz:"1.5" ~ask_sz:"0.5" ()))
      3.
  in
  let long =
    Inventory.on_fill Inventory.empty (fill ~tid:1 ~side:Buy ~px:"100000" ~sz:"0.004" ())
  in
  List.iter
    [ "flat", Inventory.empty; "long", long ]
    ~f:(fun (name, inventory) ->
      printf
        "%s: %d words, %d quotes\n"
        name
        (words_per_call (fun () -> Quoter.quote params market inventory))
        (List.length (Quoter.quote params market inventory)));
  [%expect {|
    flat: 350 words, 6 quotes
    long: 350 words, 6 quotes
    |}]
;;

let%expect_test "minor words per Market_state.on_book" =
  let market = Market_state.create ~vol_window:20 in
  let market = Market_state.on_book market (book ~bid:"99990" ~ask:"100010" ()) in
  let next = book ~time:1 ~bid:"99991" ~ask:"100011" ~bid_sz:"1.2" ~ask_sz:"0.7" () in
  printf "%d words\n" (words_per_call (fun () -> Market_state.on_book market next));
  [%expect {| 22 words |}]
;;

(* A running strategy that has just quoted and seen its orders rest. *)
let quoted () =
  let open Exchange_core in
  let s =
    Strategy.on_book (Strategy.create Params.btc) (book ~bid:"99990" ~ask:"100010" ())
  in
  let s, plan = Strategy.tick s ~now:0 in
  let resting =
    List.filter_mapi plan ~f:(fun i -> function
      | Action.Place (r : Order_request.t) ->
        Some
          (open_order
             ~oid:i
             ?client_order_id:r.client_order_id
             ~side:r.side
             ~px:(Price.to_string (Option.value_exn r.px))
             ~sz:(Size.to_string r.size)
             ())
      | Keep _ | Cancel _ -> None)
  in
  Strategy.on_open_orders s resting
;;

let%expect_test "minor words per Strategy.tick" =
  let s = quoted () in
  let _, plan = Strategy.tick s ~now:(ms 1) in
  printf
    "no change: %d words, %d actions\n"
    (words_per_call (fun () -> Strategy.tick s ~now:(ms 1)))
    (List.length plan);
  let moved = Strategy.on_book s (book ~time:(ms 2) ~bid:"99890" ~ask:"99910" ()) in
  let _, plan = Strategy.tick moved ~now:(ms 3) in
  printf
    "requote: %d words, %d actions\n"
    (words_per_call (fun () -> Strategy.tick moved ~now:(ms 3)))
    (List.length plan);
  [%expect
    {|
    no change: 53 words, 0 actions
    requote: 3399 words, 12 actions
    |}]
;;
