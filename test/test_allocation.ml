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
    flat: 354 words, 6 quotes
    long: 354 words, 6 quotes
    |}]
;;
