open! Core
open Exchange_core
open Mm_core
open Mm_core_replay
module Md = Market_data

(* Trades and books of a quiet market, then a run of buys: market-data's VPIN buckets feed
   the strategy's toxicity gate, which widens, skews and then pulls the quotes. *)
let%expect_test "VPIN from market-data drives the toxicity gate" =
  let instrument = Md.Instrument.create Binance "BTC" in
  let epoch = Time_ns.of_string_with_utc_offset "2026-01-01 00:00:00Z" in
  let at ms = Time_ns.add epoch (Time_ns.Span.of_int_ms ms) in
  let trade i side : Md.Trade.t =
    { instrument
    ; exch_ts = at (i * 100)
    ; recv_ts = at (i * 100)
    ; price = Price.of_int 100_000
    ; size = Size.of_string "0.01"
    ; side
    ; trade_id = Int.to_string i
    }
  in
  let toxicity =
    Md.Toxicity.create
      instrument
      { bucket_volume = Size.of_string "0.1"
      ; window = 5
      ; zscore_window = 50
      ; classification = Aggressor
      }
  in
  let params =
    { Params.btc with levels = 1; toxicity_gate = Some Toxicity_gate.Config.default }
  in
  let book i : L2.t =
    { symbol = params.symbol
    ; time = Time_ns.to_int_ns_since_epoch (at (i * 100))
    ; bids = [ { px = Price.of_int 99_990; sz = Size.one; n = 1 } ]
    ; asks = [ { px = Price.of_int 100_010; sz = Size.one; n = 1 } ]
    }
  in
  let strategy = ref (Strategy.create params) in
  let last = ref "" in
  for i = 0 to 899 do
    (* Balanced flow (buy, sell alternating in pairs), and only buys from trade 600. *)
    let side = if i >= 600 || i % 4 < 2 then Side.Buy else Sell in
    List.iter
      (Md.Toxicity.add toxicity (trade i side))
      ~f:(fun e -> strategy := Toxicity_input.apply !strategy e);
    strategy := Strategy.on_book !strategy (book i);
    let now = Time_ns.to_int_ns_since_epoch (at (i * 100)) in
    let s, plan = Strategy.tick !strategy ~now in
    let s = Strategy.on_open_orders s [] in
    strategy := s;
    let places =
      List.filter_map plan ~f:(function
        | Place r ->
          Some
            (sprintf
               "%s@%s"
               (Size.to_string r.size)
               (Price.to_string (Option.value_exn r.px)))
        | Keep _ | Cancel _ -> None)
    in
    let line =
      match Strategy.toxicity_decision s with
      | Normal -> "normal"
      | Pull { signal } -> sprintf "pulled at %.2f" signal
      | Widen { extra_half_spread_bps; reservation_shift_bps; size_factor; intensity = _ }
        ->
        sprintf
          "widened +%.1f bps, shift %+.1f bps, size x%.2f"
          extra_half_spread_bps
          reservation_shift_bps
          size_factor
    in
    if not (String.equal line !last)
    then (
      last := line;
      printf
        "trade %d, vpin %.2f: %s %s\n"
        i
        (Md.Toxicity.vpin toxicity)
        line
        (String.concat ~sep:" " places))
  done;
  (* The percentile ranks VPIN against its own recent history: under one-sided flow that
     lasts, VPIN stays at 1, the history fills with 1s and the gate relaxes back to
     normal. A sustained toxic regime needs a threshold on VPIN itself. *)
  [%expect
    {|
    trade 0, vpin nan: normal 0.001@99970 0.001@100030
    trade 610, vpin 0.36: pulled at 1.00
    trade 660, vpin 1.00: pulled at 0.99
    trade 670, vpin 1.00: pulled at 0.98
    trade 680, vpin 1.00: pulled at 0.97
    trade 690, vpin 1.00: pulled at 0.96
    trade 700, vpin 1.00: pulled at 0.95
    trade 710, vpin 1.00: widened +4.7 bps, shift +1.9 bps, size x0.53 0.00053@99941 0.00053@100096
    trade 720, vpin 1.00: widened +4.3 bps, shift +1.7 bps, size x0.57 0.00056@99943 0.00056@100091
    trade 730, vpin 1.00: widened +4.0 bps, shift +1.6 bps, size x0.60 0.00059@99945 0.00059@100087
    trade 740, vpin 1.00: widened +3.7 bps, shift +1.5 bps, size x0.63 0.00063@99947 0.00063@100082
    trade 750, vpin 1.00: widened +3.3 bps, shift +1.3 bps, size x0.67 0.00066@99949 0.00066@100077
    trade 760, vpin 1.00: widened +3.0 bps, shift +1.2 bps, size x0.70 0.0007@99951 0.0007@100073
    trade 770, vpin 1.00: widened +2.7 bps, shift +1.1 bps, size x0.73 0.00073@99953 0.00073@100068
    trade 780, vpin 1.00: widened +2.3 bps, shift +0.9 bps, size x0.77 0.00076@99955 0.00076@100063
    trade 790, vpin 1.00: widened +2.0 bps, shift +0.8 bps, size x0.80 0.0008@99957 0.0008@100059
    trade 800, vpin 1.00: widened +1.7 bps, shift +0.7 bps, size x0.83 0.00083@99959 0.00083@100054
    trade 810, vpin 1.00: widened +1.3 bps, shift +0.5 bps, size x0.87 0.00086@99961 0.00086@100049
    trade 820, vpin 1.00: widened +1.0 bps, shift +0.4 bps, size x0.90 0.0009@99963 0.0009@100045
    trade 830, vpin 1.00: widened +0.7 bps, shift +0.3 bps, size x0.93 0.00093@99965 0.00093@100040
    trade 840, vpin 1.00: widened +0.3 bps, shift +0.1 bps, size x0.97 0.00096@99967 0.00096@100035
    trade 850, vpin 1.00: widened +0.0 bps, shift +0.0 bps, size x1.00 0.001@99970 0.001@100030
    trade 860, vpin 1.00: normal 0.001@99970 0.001@100030
    |}]
;;
