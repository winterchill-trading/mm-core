open! Core
open Exchange_core
open Mm_core

let%expect_test "btc defaults validate" =
  print_s [%sexp (Params.validate Params.btc |> Or_error.is_ok : bool)];
  [%expect {| true |}]
;;

let%expect_test "invalid params list every problem" =
  let t = { Params.btc with tick = Price.zero; levels = 0; base_half_spread_bps = 0. } in
  print_s [%sexp (Params.validate t : Params.t Or_error.t)];
  [%expect
    {|
    (Error
     ("invalid params"
      (errors
       ("tick must be positive" "levels must be in [1, 255]"
        "base_half_spread_bps must be positive"))))
    |}]
;;

let instrument =
  Instrument.create
    (Symbol.of_string "BTCUSDT")
    ~size_decimals:3
    ~price_tick:(Price.of_string "0.1")
    ~max_leverage:20
;;

let show_size ?min_notional mid =
  let mid = Price.of_string mid in
  let params = Params.for_instrument ?min_notional instrument ~mid in
  print_s
    [%message
      ""
        ~order_size:(params.order_size : Size.t)
        ~notional:(Fx.notional mid params.order_size : Usd.t)]
;;

let%expect_test "order size without a venue minimum is about 100 USD" =
  show_size "100000";
  [%expect {| ((order_size 0.001) (notional 100)) |}];
  show_size "60000";
  [%expect {| ((order_size 0.002) (notional 120)) |}]
;;

let%expect_test "order size clears the venue minimum with a margin, rounded up" =
  (* 1.2 x 100 USD = 120 USD: 0.0012 BTC, rounded up to the 0.001 lot *)
  show_size ~min_notional:(Usd.of_int 100) "100000";
  [%expect {| ((order_size 0.002) (notional 200)) |}];
  show_size ~min_notional:(Usd.of_int 100) "120000";
  [%expect {| ((order_size 0.001) (notional 120)) |}];
  show_size ~min_notional:(Usd.of_int 100) "119999";
  [%expect {| ((order_size 0.002) (notional 239.998)) |}];
  (* a small minimum leaves the ~100 USD default alone *)
  show_size ~min_notional:(Usd.of_int 5) "100000";
  [%expect {| ((order_size 0.001) (notional 100)) |}]
;;

let%expect_test "a params sexp without max_actions_per_sec takes the default" =
  let sexp =
    match Params.sexp_of_t Params.btc with
    | List fields ->
      Sexp.List
        (List.filter fields ~f:(function
          | List (Atom "max_actions_per_sec" :: _) -> false
          | _ -> true))
    | atom -> atom
  in
  print_s [%sexp ((Params.t_of_sexp sexp).max_actions_per_sec : int)];
  [%expect {| 100 |}]
;;

let%expect_test "NaN or infinity in any float field fails validation" =
  let setters =
    [ ("base_half_spread_bps", fun t x -> { t with Params.base_half_spread_bps = x })
    ; ("level_spacing_bps", fun t x -> { t with Params.level_spacing_bps = x })
    ; ("skew_bps", fun t x -> { t with Params.skew_bps = x })
    ; ("requote_threshold_bps", fun t x -> { t with Params.requote_threshold_bps = x })
    ; ("gamma", fun t x -> { t with Params.gamma = x })
    ; ("max_vol_bps", fun t x -> { t with Params.max_vol_bps = x })
    ; ("flatten_slippage_bps", fun t x -> { t with Params.flatten_slippage_bps = x })
    ]
  in
  List.iter setters ~f:(fun (name, set) ->
    List.iter [ Float.nan; Float.infinity ] ~f:(fun x ->
      match Params.validate (set Params.btc x) with
      | Ok _ -> printf "%s = %f accepted\n" name x
      | Error error ->
        let errors =
          match Error.sexp_of_t error with
          | List [ _; List [ _; List errors ] ] -> List.map errors ~f:Sexp.to_string
          | sexp -> [ Sexp.to_string sexp ]
        in
        printf "%s = %f: %s\n" name x (String.concat ~sep:"; " errors)));
  [%expect
    {|
    base_half_spread_bps = nan: "base_half_spread_bps must be finite"; "base_half_spread_bps must be positive"
    base_half_spread_bps = inf: "base_half_spread_bps must be finite"
    level_spacing_bps = nan: "level_spacing_bps must be finite"; "level_spacing_bps must be non-negative"
    level_spacing_bps = inf: "level_spacing_bps must be finite"
    skew_bps = nan: "skew_bps must be finite"; "skew_bps must be non-negative"
    skew_bps = inf: "skew_bps must be finite"
    requote_threshold_bps = nan: "requote_threshold_bps must be finite"; "requote_threshold_bps must be non-negative"
    requote_threshold_bps = inf: "requote_threshold_bps must be finite"
    gamma = nan: "gamma must be finite"; "gamma must be non-negative"
    gamma = inf: "gamma must be finite"
    max_vol_bps = nan: "max_vol_bps must be finite"; "max_vol_bps must be non-negative"
    max_vol_bps = inf: "max_vol_bps must be finite"
    flatten_slippage_bps = nan: "flatten_slippage_bps must be finite"; "flatten_slippage_bps must be positive"
    flatten_slippage_bps = inf: "flatten_slippage_bps must be finite"
    |}]
;;

let%expect_test "action rate grace and cooldown: printed only when not the default" =
  let fields t =
    match Params.sexp_of_t t with
    | List fields ->
      List.filter fields ~f:(function
        | List (Atom ("action_rate_grace_ms" | "action_rate_cooldown_ms") :: _) -> true
        | _ -> false)
    | Atom _ -> []
  in
  print_s [%sexp (fields Params.btc : Sexp.t list)];
  [%expect {| () |}];
  let t =
    { Params.btc with
      action_rate_grace_ms = Time_ns.Span.of_int_ms 2_000
    ; action_rate_cooldown_ms = Some (Time_ns.Span.of_int_ms 30_000)
    }
  in
  print_s [%sexp (fields t : Sexp.t list)];
  [%expect {| ((action_rate_grace_ms 2s) (action_rate_cooldown_ms 30s)) |}];
  print_s [%sexp (Params.equal t (Params.t_of_sexp (Params.sexp_of_t t)) : bool)];
  [%expect {| true |}];
  print_s
    [%sexp
      (Params.validate
         { Params.btc with
           action_rate_grace_ms = Time_ns.Span.of_int_ms (-1)
         ; action_rate_cooldown_ms = Some (Time_ns.Span.of_int_ms 0)
         }
       |> Result.map ~f:(fun (_ : Params.t) -> ())
       : unit Or_error.t)];
  [%expect
    {|
    (Error
     ("invalid params"
      (errors
       ("action_rate_grace_ms must be non-negative"
        "action_rate_cooldown_ms must be positive"))))
    |}]
;;
