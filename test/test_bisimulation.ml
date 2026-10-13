open! Core
open Exchange_core
open Mm_core
open Helpers

(* The library's strategy against the pure reference copy in [Ref_strategy]: the same
   random event script must give the same plans, the same state (compared as sexps) and
   the same quotes and reasons after every event. *)

module Event = struct
  type t =
    | Book of
        { mid_step : int
        ; spread : int
        ; bid_lots : int
        ; ask_lots : int
        ; one_sided : bool
        }
    | Fill of
        { side : Side.t
        ; lots : int
        }
    | Ack
    | Resync
    | Tick of { ms : int }
    | Retune of
        { vol_window : int
        ; half_spread_bps : float
        }
  [@@deriving sexp_of]

  let quickcheck_generator =
    let open Quickcheck.Let_syntax in
    Quickcheck.Generator.weighted_union
      [ ( 4.
        , let%map mid_step = Int.gen_incl (-30) 30
          and spread = Int.gen_incl 1 20
          and bid_lots = Int.gen_incl 0 2_000
          and ask_lots = Int.gen_incl 0 2_000
          and one_sided =
            Quickcheck.Generator.weighted_union [ 9., return false; 1., return true ]
          in
          Book { mid_step; spread; bid_lots; ask_lots; one_sided } )
      ; ( 1.
        , let%map side = Quickcheck.Generator.of_list [ Side.Buy; Sell ]
          and lots = Int.gen_incl 1 60 in
          Fill { side; lots } )
      ; 2., return Ack
      ; 0.2, return Resync
      ; 4., Quickcheck.Generator.map (Int.gen_incl 0 3_000) ~f:(fun ms -> Tick { ms })
      ; ( 0.2
        , let%map vol_window = Int.gen_incl 1 50
          and half_spread_bps = Float.gen_incl 0.5 20. in
          Retune { vol_window; half_spread_bps } )
      ]
  ;;
end

let gen_params =
  let open Quickcheck.Let_syntax in
  let lots n = Size.scale Params.btc.lot ~by:n in
  let%bind max_lots = Int.gen_incl 5 200 in
  let%map levels = Int.gen_incl 1 5
  and max_actions_per_sec = Int.gen_incl 2 40
  and soft = Option.quickcheck_generator (Int.gen_incl 1 max_lots)
  and cooldown = Option.quickcheck_generator (Int.gen_incl 100 5_000)
  and drawdown_cents = Int.gen_incl 1 99 in
  { Params.btc with
    symbol = btc
  ; levels
  ; order_size = lots 10
  ; max_position = lots max_lots
  ; max_actions_per_sec
  ; soft_max_position = Option.map soft ~f:lots
  ; bump_budget = lots 5
  ; max_drawdown = Usd.of_string (sprintf "0.%02d" drawdown_cents)
  ; action_rate_cooldown_ms = Option.map cooldown ~f:Time_ns.Span.of_int_ms
  }
;;

type run =
  { mutable lib : Strategy.t
  ; mutable reference : Ref_strategy.t
  ; mutable now : int
  ; mutable mid : int
  ; mutable tid : int
  ; mutable oid : int
  ; mutable resting : Open_order.t list
  ; mutable last_plan : Action.t list
  }

let check_same run ~params ~event =
  let lib = [%sexp (run.lib : Strategy.t)]
  and reference = [%sexp (run.reference : Ref_strategy.t)] in
  if not (Sexp.equal lib reference)
  then
    raise_s
      [%message "state differs" (event : Event.t) (lib : Sexp.t) (reference : Sexp.t)];
  let quotes, reasons =
    Quoter.quote_with_reasons
      params
      (Strategy.market run.lib)
      (Strategy.inventory run.lib)
  and ref_quotes, ref_reasons =
    Ref_quoter.quote_with_reasons
      params
      (Ref_strategy.market run.reference)
      (Ref_strategy.inventory run.reference)
  in
  let reasons = [%sexp (reasons : (Side.t * Quoter.Unquoted.t) list)]
  and ref_reasons = [%sexp (ref_reasons : (Side.t * Ref_quoter.Unquoted.t) list)] in
  if not ([%equal: Quote.t list] quotes ref_quotes && Sexp.equal reasons ref_reasons)
  then
    raise_s
      [%message
        "quotes differ"
          (event : Event.t)
          (quotes : Quote.t list)
          (ref_quotes : Quote.t list)
          (reasons : Sexp.t)
          (ref_reasons : Sexp.t)]
;;

let step run ~params (event : Event.t) =
  match event with
  | Book { mid_step; spread; bid_lots; ask_lots; one_sided } ->
    run.now <- run.now + 1_000_000;
    run.mid <- Int.max 1_000 (run.mid + mid_step);
    let lot = Params.btc.lot in
    let level px lots : L2.Level.t =
      { px = Price.of_int px; sz = Size.scale lot ~by:lots; n = 1 }
    in
    let book : L2.t =
      { symbol = btc
      ; time = run.now
      ; bids = [ level (run.mid - spread) bid_lots ]
      ; asks = (if one_sided then [] else [ level (run.mid + spread) ask_lots ])
      }
    in
    run.lib <- Strategy.on_book run.lib book;
    run.reference <- Ref_strategy.on_book run.reference book
  | Fill { side; lots } ->
    run.tid <- run.tid + 1;
    let fill : Fill.t =
      { trade_id = Trade_id.of_int run.tid
      ; order_id = Order_id.of_int run.tid
      ; client_order_id = None
      ; symbol = btc
      ; side
      ; px = Price.of_int run.mid
      ; size = Size.scale Params.btc.lot ~by:lots
      ; fee = Usd.of_string "0.01"
      ; is_maker = true
      ; time = run.now
      ; closed_pnl = None
      }
    in
    run.lib <- Strategy.on_fill run.lib fill;
    run.reference <- Ref_strategy.on_fill run.reference fill
  | Ack ->
    let cancelled =
      List.filter_map run.last_plan ~f:(function
        | Action.Cancel oid -> Some oid
        | Keep _ | Place _ -> None)
    in
    let placed =
      List.filter_map run.last_plan ~f:(function
        | Action.Place ({ order_type = Limit Alo; px = Some px; _ } as r) ->
          run.oid <- run.oid + 1;
          Some
            { Open_order.symbol = r.symbol
            ; side = r.side
            ; px
            ; size = r.size
            ; orig_size = Some r.size
            ; order_id = Order_id.of_int run.oid
            ; client_order_id = r.client_order_id
            ; timestamp = run.now
            }
        | Place _ | Keep _ | Cancel _ -> None)
    in
    run.resting
    <- List.filter run.resting ~f:(fun o ->
         not (List.mem cancelled o.order_id ~equal:Order_id.equal))
       @ placed;
    run.last_plan <- [];
    run.lib <- Strategy.on_open_orders run.lib run.resting;
    run.reference <- Ref_strategy.on_open_orders run.reference run.resting
  | Resync ->
    run.lib <- Strategy.on_resync run.lib ~now:run.now;
    run.reference <- Ref_strategy.on_resync run.reference ~now:run.now
  | Tick { ms } ->
    run.now <- run.now + (ms * 1_000_000);
    let lib, plan = Strategy.tick run.lib ~now:run.now in
    let reference, ref_plan = Ref_strategy.tick run.reference ~now:run.now in
    if not ([%equal: Action.t list] plan ref_plan)
    then
      raise_s
        [%message
          "plans differ"
            (event : Event.t)
            (plan : Action.t list)
            (ref_plan : Action.t list)];
    run.lib <- lib;
    run.reference <- reference;
    if not (List.is_empty plan) then run.last_plan <- plan
  | Retune { vol_window; half_spread_bps } ->
    let next =
      { params with Params.vol_window; base_half_spread_bps = half_spread_bps }
    in
    (match
       Strategy.update_params run.lib next, Ref_strategy.update_params run.reference next
     with
     | Ok lib, Ok reference ->
       run.lib <- lib;
       run.reference <- reference
     | Error _, Error _ -> ()
     | _ -> raise_s [%message "update_params differs" (event : Event.t)])
;;

let%test_unit "strategy matches the pure reference on random event scripts" =
  let gen =
    Quickcheck.Generator.both
      gen_params
      (Quickcheck.Generator.list_with_length 80 Event.quickcheck_generator)
  in
  Quickcheck.test
    gen
    ~trials:300
    ~sexp_of:[%sexp_of: Params.t * Event.t list]
    ~f:(fun (params, events) ->
      let run =
        { lib = Strategy.create params
        ; reference = Ref_strategy.create params
        ; now = 0
        ; mid = 100_000
        ; tid = 0
        ; oid = 0
        ; resting = []
        ; last_plan = []
        }
      in
      List.iter events ~f:(fun event ->
        let params = Strategy.params run.lib in
        step run ~params event;
        check_same run ~params:(Strategy.params run.lib) ~event))
;;

(* The script above reaches every mode; this pins how often, so a generator change that
   stops exercising halts shows up here. *)
let%expect_test "modes reached by the random scripts" =
  let counts = Hashtbl.create (module String) in
  Quickcheck.iter
    ~seed:(`Deterministic "bisimulation")
    ~trials:100
    (Quickcheck.Generator.both
       gen_params
       (Quickcheck.Generator.list_with_length 80 Event.quickcheck_generator))
    ~f:(fun (params, events) ->
      let run =
        { lib = Strategy.create params
        ; reference = Ref_strategy.create params
        ; now = 0
        ; mid = 100_000
        ; tid = 0
        ; oid = 0
        ; resting = []
        ; last_plan = []
        }
      in
      List.iter events ~f:(fun event ->
        step run ~params:(Strategy.params run.lib) event;
        let name (sexp : Sexp.t) =
          match sexp with
          | Atom name | List (Atom name :: _) -> name
          | List _ -> Sexp.to_string sexp
        in
        let mode =
          match Strategy.mode run.lib with
          | Running -> "Running"
          | Pulled reason -> "Pulled " ^ name [%sexp (reason : Risk.Reason.t)]
          | Halted reason -> "Halted " ^ name [%sexp (reason : Risk.Reason.t)]
        in
        Hashtbl.incr counts mode));
  print_s
    [%sexp
      (Hashtbl.to_alist counts |> List.sort ~compare:[%compare: string * int]
       : (string * int) list)];
  [%expect
    {|
    (("Halted Max_action_rate" 102) ("Halted Max_drawdown" 362)
     ("Halted Max_position" 2323) ("Pulled No_market_data" 929)
     ("Pulled Stale_market_data" 728) (Running 3556))
    |}]
;;

(* The quoter alone over a wider space than the strategy scripts reach: tiny prices (so
   far rungs go non-positive), one-sided books, soft limits and bump budgets. *)
let%test_unit "quoter matches the pure reference" =
  let open Quickcheck.Let_syntax in
  let lots n = Size.scale Params.btc.lot ~by:n in
  let gen =
    let%bind max_lots = Int.gen_incl 1 3_000 in
    let%map mid = Int.gen_incl 1 300_000
    and spread = Int.gen_incl 0 50
    and one_sided = Bool.quickcheck_generator
    and levels = Int.gen_incl 1 6
    and order_lots = Int.gen_incl 1 500
    and position = Int.gen_incl (-max_lots - 10) (max_lots + 10)
    and soft = Option.quickcheck_generator (Int.gen_incl 1 max_lots)
    and bump_left = Option.quickcheck_generator (Int.gen_incl 0 50)
    and half_spread = Float.gen_incl 0. 3_000.
    and spacing = Float.gen_incl 0. 3_000.
    and skew = Float.gen_incl 0. 100.
    and vol_bps = Float.gen_incl 0. 80.
    and units = Int.gen_incl 1 1_000_000 in
    let params =
      { Params.btc with
        symbol = btc
      ; levels
      ; order_size = lots order_lots
      ; max_position = lots max_lots
      ; soft_max_position = Option.map soft ~f:lots
      ; base_half_spread_bps = half_spread
      ; level_spacing_bps = spacing
      ; skew_bps = skew
      ; tick = Price.of_units units
      }
    in
    let level px : L2.Level.t = { px = Price.of_units px; sz = lots 7; n = 1 } in
    let book : L2.t =
      { symbol = btc
      ; time = 0
      ; bids = [ level (mid * 10_000) ]
      ; asks = (if one_sided then [] else [ level ((mid + spread) * 10_000) ])
      }
    in
    params, book, position, vol_bps, Option.map bump_left ~f:lots
  in
  Quickcheck.test
    gen
    ~trials:20_000
    ~sexp_of:[%sexp_of: Params.t * L2.t * int * float * Size.t option]
    ~f:(fun (params, book, position, vol_bps, bump_left) ->
      let inventory =
        if position = 0
        then Inventory.empty
        else
          Inventory.on_fill
            Inventory.empty
            { trade_id = Trade_id.of_int 1
            ; order_id = Order_id.of_int 1
            ; client_order_id = None
            ; symbol = btc
            ; side = (if position > 0 then Buy else Sell)
            ; px = Price.of_int 1
            ; size = lots (abs position)
            ; fee = Usd.zero
            ; is_maker = true
            ; time = 0
            ; closed_pnl = None
            }
      in
      let market =
        Market_state.with_vol_bps
          (Market_state.on_book (Market_state.create ~vol_window:10) book)
          vol_bps
      and ref_market =
        Ref_market_state.with_vol_bps
          (Ref_market_state.on_book (Ref_market_state.create ~vol_window:10) book)
          vol_bps
      in
      let quotes, reasons =
        Quoter.quote_with_reasons ?bump_left params market inventory
      in
      let ref_quotes, ref_reasons =
        Ref_quoter.quote_with_reasons ?bump_left params ref_market inventory
      in
      [%test_eq: Quote.t list] quotes ref_quotes;
      [%test_eq: Quote.t list] (Quoter.quote ?bump_left params market inventory) quotes;
      [%test_eq: Sexp.t]
        [%sexp (reasons : (Side.t * Quoter.Unquoted.t) list)]
        [%sexp (ref_reasons : (Side.t * Ref_quoter.Unquoted.t) list)])
;;
