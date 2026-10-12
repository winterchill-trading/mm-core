open! Core
open Exchange_core
open Mm_core
open Helpers

let show m =
  print_s
    [%message
      ""
        ~mid:(Market_state.mid m : Price.t option)
        ~micro:(Market_state.microprice m : Price.t option)
        ~vol_bps:(Float.round_decimal (Market_state.vol_bps m) ~decimal_digits:3 : float)
        ~samples:(Market_state.vol_samples m : int)]
;;

let%expect_test "mid and microprice" =
  let m = Market_state.create ~vol_window:10 in
  show m;
  [%expect {| ((mid ()) (micro ()) (vol_bps 0) (samples 0)) |}];
  (* More size on the bid: the microprice leans towards the ask. *)
  let m = Market_state.on_book m (book ~bid:"100000" ~ask:"100010" ~bid_sz:"3" ()) in
  show m;
  [%expect {| ((mid (100005)) (micro (100007.5)) (vol_bps 0) (samples 0)) |}];
  let m = Market_state.on_book m (book ~bid:"100000" ~ask:"100010" ~ask_sz:"3" ()) in
  show m;
  [%expect {| ((mid (100005)) (micro (100002.5)) (vol_bps 0) (samples 1)) |}]
;;

let%expect_test "volatility from mid returns" =
  let m =
    List.fold
      [ "100000"; "100010"; "100000"; "100010"; "100000" ]
      ~init:(Market_state.create ~vol_window:3)
      ~f:(fun m bid ->
        let ask = Price.(px bid + px "2") |> Price.to_string in
        Market_state.on_book m (book ~bid ~ask ()))
  in
  show m;
  [%expect {| ((mid (100001)) (micro (100001)) (vol_bps 1) (samples 4)) |}];
  print_s [%sexp (Market_state.age_ms m ~now:(ms 1500) : int option)];
  [%expect {| (1500) |}]
;;
