open! Core
open! Async
open Exchange_core

module Report = struct
  type t =
    { elapsed : Time_ns.Span.t
    ; fills : int
    ; maker_fills : int
    ; volume : Usd.t
    ; realized_pnl : Usd.t
    ; unrealized_pnl : Usd.t
    ; fees : Usd.t
    ; total_pnl : Usd.t
    ; position : Size.t
    ; mid : Price.t option
    ; account_value : Usd.t option
    ; plans : int
    ; orders_placed : int
    ; orders_rejected : int
    ; rejections : (string * int) list
    ; orders_cancelled : int
    ; reconnects : int
    ; mode : Strategy.Mode.t
    }
  [@@deriving sexp_of]

  let usd x = Usd.to_string (Usd.round_nearest x ~decimals:4)

  let to_string_hum t =
    let opt f = Option.value_map ~default:"-" ~f in
    let lines =
      [ sprintf "elapsed          %.1fs" (Time_ns.Span.to_sec t.elapsed)
      ; sprintf "fills            %d (%d maker)" t.fills t.maker_fills
      ; sprintf "volume           %s USD" (usd t.volume)
      ; sprintf "realized pnl     %s USD" (usd t.realized_pnl)
      ; sprintf "unrealized pnl   %s USD" (usd t.unrealized_pnl)
      ; sprintf "fees             %s USD" (usd t.fees)
      ; sprintf "total pnl        %s USD" (usd t.total_pnl)
      ; sprintf "final position   %s" (Size.to_string t.position)
      ; sprintf "last mid         %s" (opt Price.to_string t.mid)
      ; sprintf "account value    %s USD" (opt usd t.account_value)
      ; sprintf "plans            %d" t.plans
      ; sprintf
          "orders           %d placed, %d rejected, %d cancelled"
          t.orders_placed
          t.orders_rejected
          t.orders_cancelled
      ]
      @ List.map t.rejections ~f:(fun (msg, n) -> sprintf "  rejected %4d x %s" n msg)
      @ [ sprintf "reconnects       %d" t.reconnects
        ; sprintf "mode             %s" (Sexp.to_string (Strategy.Mode.sexp_of_t t.mode))
        ]
    in
    String.concat ~sep:"\n" lines
  ;;
end

module Config = struct
  type t =
    { params : Params.t
    ; summary_interval : Time_ns.Span.t
    }
  [@@deriving sexp_of]
end

let describe_reject (reason : Reject_reason.t) =
  match reason with
  | Unsupported msg -> msg
  | Post_only_would_cross { best_bid; best_ask } ->
    let show = Option.value_map ~default:"none" ~f:Price.to_string in
    sprintf "post-only order would cross, bbo %s@%s" (show best_bid) (show best_ask)
  | Unknown_symbol
  | Invalid_size
  | Invalid_price
  | Duplicate_client_order_id
  | Reduce_only_would_increase
  | Below_min_value _
  | Insufficient_margin
  | No_liquidity
  | Self_trade
  | Order_not_found -> Sexp.to_string [%sexp (reason : Reject_reason.t)]
;;

let stop_signal ?(time_source = Time_source.wall_clock ()) ?run_for () =
  let stop = Ivar.create () in
  Signal.handle [ Signal.int; Signal.term ] ~f:(fun signal ->
    if Ivar.is_full stop
    then (
      Log.Global.error_s [%message "second signal, exiting" (signal : Signal.t)];
      Shutdown.shutdown 130)
    else (
      Log.Global.info_s [%message "signal received, shutting down" (signal : Signal.t)];
      Ivar.fill_if_empty stop ()));
  Option.iter run_for ~f:(fun span ->
    upon (Time_source.after time_source span) (Ivar.fill_if_empty stop));
  Ivar.read stop
;;

let side_name : Side.t -> string = function
  | Buy -> "buy"
  | Sell -> "sell"
;;

let describe_order (o : Order_request.t) =
  sprintf
    "%s %s@%s%s"
    (side_name o.side)
    (Size.to_string o.size)
    (Option.value_map o.px ~default:"market" ~f:Price.to_string)
    (if o.reduce_only then " (reduce-only)" else "")
;;

module Make (V : Venue.S) = struct
  type t =
    { mutable config : Config.t
    ; venue : V.t
    ; time_source : Time_source.t
    ; sequencer : unit Throttle.Sequencer.t
    ; started_at : Time_ns.t
    ; seen_trades : Trade_id.Hash_set.t
    ; timers_stop : unit Ivar.t
    ; mutable polls_stop : unit Ivar.t
    ; stopped : Report.t Ivar.t
    ; latency : Latency_histogram.t
    ; mutable book_waiting_since : Time_ns.t option
    ; mutable strategy : Strategy.t
    ; mutable account : Account_view.t option
    ; mutable stopping : bool
    ; mutable step_queued : bool
    ; mutable sync_queued : bool
    ; mutable volume : Usd.t
    ; mutable maker_fills : int
    ; mutable plans : int
    ; mutable orders_placed : int
    ; mutable orders_rejected : int
    ; mutable rejections : int String.Map.t
    ; mutable orders_cancelled : int
    ; mutable reconnects : int
    ; mutable resync_hooks : (unit -> unit) list
    }

  let params t = t.config.params
  let symbol t = (params t).symbol
  let strategy t = t.strategy
  let now t = Time_source.now t.time_source
  let now_ns t = Time_ns.to_int_ns_since_epoch (now t)
  let usd = Report.usd

  let report t =
    let inventory = Strategy.inventory t.strategy in
    let mid = Market_state.mid (Strategy.market t.strategy) in
    let unrealized_pnl =
      Option.value_map mid ~default:Usd.zero ~f:(fun mark ->
        Inventory.unrealized_pnl inventory ~mark)
    in
    let realized_pnl = Inventory.realized_pnl inventory in
    let fees = Inventory.fees inventory in
    { Report.elapsed = Time_ns.diff (now t) t.started_at
    ; fills = Hash_set.length t.seen_trades
    ; maker_fills = t.maker_fills
    ; volume = t.volume
    ; realized_pnl
    ; unrealized_pnl
    ; fees
    ; total_pnl = Usd.(realized_pnl + unrealized_pnl - fees)
    ; position = Inventory.position inventory
    ; mid
    ; account_value = Option.map t.account ~f:(fun (a : Account_view.t) -> a.equity)
    ; plans = t.plans
    ; orders_placed = t.orders_placed
    ; orders_rejected = t.orders_rejected
    ; rejections =
        Map.to_alist t.rejections
        |> List.sort ~compare:(fun (_, a) (_, b) -> Int.descending a b)
    ; orders_cancelled = t.orders_cancelled
    ; reconnects = t.reconnects
    ; mode = Strategy.mode t.strategy
    }
  ;;

  (* Runs [f] on the sequencer, logging (not raising) its failures. *)
  let enqueue t ~name f =
    Throttle.enqueue t.sequencer (fun () ->
      match%map Monitor.try_with ~extract_exn:true f with
      | Ok () -> ()
      | Error exn -> Log.Global.error_s [%message "live loop job failed" name (exn : exn)])
  ;;

  (* {2 Snapshots} *)

  let sync t =
    let%map open_orders = V.open_orders t.venue (symbol t)
    and account = V.account t.venue in
    (match account with
     | Ok account ->
       t.account <- Some account;
       t.strategy <- Strategy.on_account t.strategy account
     | Error error -> Log.Global.error_s [%message "account" (error : Error.t)]);
    match open_orders with
    | Ok orders -> t.strategy <- Strategy.on_open_orders t.strategy orders
    | Error error -> Log.Global.error_s [%message "open orders" (error : Error.t)]
  ;;

  (* {2 Plan execution} *)

  let reject t reason =
    let msg = describe_reject reason in
    t.orders_rejected <- t.orders_rejected + 1;
    t.rejections
    <- Map.update t.rejections msg ~f:(fun n -> 1 + Option.value n ~default:0);
    msg
  ;;

  let send_cancels t oids =
    match%map V.cancel t.venue (symbol t) oids with
    | Ok () -> ()
    | Error error -> Log.Global.error_s [%message "cancel" (error : Error.t)]
  ;;

  let send_places t orders =
    match%map V.place t.venue orders with
    | Ok statuses ->
      (match List.zip orders statuses with
       | Unequal_lengths ->
         Log.Global.error_s
           [%message
             "place: one status per order expected" (statuses : Order_status.t list)]
       | Ok pairs ->
         List.iter pairs ~f:(fun (order, status) ->
           match status with
           | Resting _ -> t.orders_placed <- t.orders_placed + 1
           | Filled { total; avg_px; _ } ->
             t.orders_placed <- t.orders_placed + 1;
             Log.Global.info
               "order %s filled immediately: %s@%s"
               (describe_order order)
               (Size.to_string total)
               (Price.to_string avg_px)
           | Rejected reason ->
             let msg = reject t reason in
             Log.Global.info "order %s rejected: %s" (describe_order order) msg))
    | Error error -> Log.Global.error_s [%message "place" (error : Error.t)]
  ;;

  let execute t plan =
    let cancels =
      List.filter_map plan ~f:(function
        | Action.Cancel oid -> Some oid
        | Keep _ | Place _ -> None)
    in
    let places =
      List.filter_map plan ~f:(function
        | Action.Place order -> Some order
        | Keep _ | Cancel _ -> None)
    in
    let keeps = List.length plan - List.length cancels - List.length places in
    t.plans <- t.plans + 1;
    Log.Global.info
      "plan #%d gen %d: cancel %d, keep %d, place [%s]"
      t.plans
      (Strategy.generation t.strategy)
      (List.length cancels)
      keeps
      (String.concat ~sep:" " (List.map places ~f:describe_order));
    let%bind () = if List.is_empty cancels then return () else send_cancels t cancels in
    if List.is_empty places then return () else send_places t places
  ;;

  (* {2 Steps} *)

  let tick_and_execute t =
    if t.stopping
    then return ()
    else (
      let book_at = t.book_waiting_since in
      t.book_waiting_since <- None;
      let mode_before = Strategy.mode t.strategy in
      let strategy, plan = Strategy.tick t.strategy ~now:(now_ns t) in
      t.strategy <- strategy;
      let mode = Strategy.mode strategy in
      if not (Strategy.Mode.equal mode mode_before)
      then
        Log.Global.info_s
          [%message
            "mode" ~from:(mode_before : Strategy.Mode.t) ~to_:(mode : Strategy.Mode.t)];
      if List.is_empty plan
      then return ()
      else (
        Option.iter book_at ~f:(fun at ->
          Latency_histogram.record t.latency (Time_ns.diff (now t) at));
        let%bind () = execute t plan in
        sync t))
  ;;

  (* Coalesces requests: at most one tick waits on the sequencer at a time. *)
  let step t =
    if (not t.stopping) && not t.step_queued
    then (
      t.step_queued <- true;
      don't_wait_for
        (enqueue t ~name:"tick" (fun () ->
           t.step_queued <- false;
           tick_and_execute t)))
  ;;

  let request_sync t =
    if (not t.stopping) && not t.sync_queued
    then (
      t.sync_queued <- true;
      don't_wait_for
        (enqueue t ~name:"sync" (fun () ->
           t.sync_queued <- false;
           if t.stopping then return () else sync t));
      step t)
  ;;

  (* {2 Feeds} *)

  let on_fill t (fill : Fill.t) =
    if Symbol.equal fill.symbol (symbol t)
       && not (Hash_set.mem t.seen_trades fill.trade_id)
    then (
      Hash_set.add t.seen_trades fill.trade_id;
      t.volume <- Usd.(t.volume + Fx.notional fill.px fill.size);
      if fill.is_maker then t.maker_fills <- t.maker_fills + 1;
      t.strategy <- Strategy.on_fill t.strategy fill;
      let r = report t in
      Log.Global.info
        "fill %s %s@%s %s fee %s | pos %s realized %s unrealized %s fees %s pnl %s"
        (side_name fill.side)
        (Size.to_string fill.size)
        (Price.to_string fill.px)
        (if fill.is_maker then "maker" else "taker")
        (Usd.to_string fill.fee)
        (Size.to_string r.position)
        (usd r.realized_pnl)
        (usd r.unrealized_pnl)
        (usd r.fees)
        (usd r.total_pnl);
      step t)
  ;;

  let on_book t (book : L2.t) =
    if Option.is_none t.book_waiting_since then t.book_waiting_since <- Some (now t);
    t.strategy <- Strategy.on_book t.strategy book;
    step t
  ;;

  let on_order_update t (u : Order_update.t) =
    Log.Global.debug_s [%message "order update" (u : Order_update.t)];
    if Symbol.equal u.symbol (symbol t)
    then (
      if Order_update.Status.equal u.status Cancelled
      then t.orders_cancelled <- t.orders_cancelled + 1;
      (* The strategy only takes full snapshots; an order that left the book is picked up
         by the next poll, requested now. *)
      if Order_update.Status.is_final u.status then request_sync t)
  ;;

  (* {2 Summary} *)

  let log_summary t =
    let r = report t in
    let quotes side =
      Strategy.open_orders t.strategy
      |> List.filter ~f:(fun (o : Open_order.t) -> Side.equal o.side side)
      |> List.sort ~compare:(fun (a : Open_order.t) b ->
        match side with
        | Buy -> Price.descending a.px b.px
        | Sell -> Price.ascending a.px b.px)
      |> List.map ~f:(fun (o : Open_order.t) ->
        sprintf "%s@%s" (Size.to_string o.size) (Price.to_string o.px))
      |> String.concat ~sep:" "
    in
    let market = Strategy.market t.strategy in
    let opt = Option.value_map ~default:"-" ~f:Price.to_string in
    Log.Global.info
      "summary: mode %s | mid %s (bid %s ask %s, vol %.2fbps) | pos %s entry %s | fills \
       %d volume %s | realized %s unrealized %s fees %s pnl %s | bids [%s] asks [%s]"
      (Sexp.to_string (Strategy.Mode.sexp_of_t r.mode))
      (opt r.mid)
      (opt (Market_state.best_bid market))
      (opt (Market_state.best_ask market))
      (Market_state.vol_bps market)
      (Size.to_string r.position)
      (opt (Inventory.avg_entry (Strategy.inventory t.strategy)))
      r.fills
      (usd r.volume)
      (usd r.realized_pnl)
      (usd r.unrealized_pnl)
      (usd r.fees)
      (usd r.total_pnl)
      (quotes Buy)
      (quotes Sell)
  ;;

  (* {2 Lifecycle} *)

  (* Polls every [requote_interval_ms] until [stop] or the next [start_polls]. *)
  let start_polls t =
    Ivar.fill_if_empty t.polls_stop ();
    let polls_stop = Ivar.create () in
    t.polls_stop <- polls_stop;
    Time_source.every
      ~stop:(Deferred.any [ Ivar.read polls_stop; Ivar.read t.timers_stop ])
      ~continue_on_error:true
      t.time_source
      (Time_ns.Span.of_int_ms (params t).requote_interval_ms)
      (fun () -> request_sync t)
  ;;

  let update_params t (next : Params.t) =
    if t.stopping
    then Or_error.error_s [%message "update_params: the loop is stopping"]
    else (
      let current = params t in
      match Strategy.update_params t.strategy next with
      | Error _ as error -> error
      | Ok strategy ->
        t.strategy <- strategy;
        t.config <- { t.config with params = next };
        Log.Global.info_s [%message "params updated" ~params:(next : Params.t)];
        if current.requote_interval_ms <> next.requote_interval_ms then start_polls t;
        step t;
        Ok ())
  ;;

  let start ?(time_source = Time_source.wall_clock ()) venue (config : Config.t) =
    let params = config.params in
    match Params.validate params with
    | Error _ as error -> return error
    | Ok params ->
      let t =
        { config
        ; venue
        ; time_source
        ; sequencer = Throttle.Sequencer.create ~continue_on_error:true ()
        ; started_at = Time_source.now time_source
        ; seen_trades = Trade_id.Hash_set.create ()
        ; timers_stop = Ivar.create ()
        ; polls_stop = Ivar.create ()
        ; stopped = Ivar.create ()
        ; latency = Latency_histogram.create ()
        ; book_waiting_since = None
        ; strategy = Strategy.create params
        ; account = None
        ; stopping = false
        ; step_queued = false
        ; sync_queued = false
        ; volume = Usd.zero
        ; maker_fills = 0
        ; plans = 0
        ; orders_placed = 0
        ; orders_rejected = 0
        ; rejections = String.Map.empty
        ; orders_cancelled = 0
        ; reconnects = 0
        ; resync_hooks = []
        }
      in
      Log.Global.info_s [%message "starting" (params : Params.t)];
      V.on_reconnect venue ~f:(fun () ->
        t.reconnects <- t.reconnects + 1;
        t.strategy <- Strategy.on_resync t.strategy ~now:(now_ns t);
        Log.Global.info "venue reconnected; resyncing open orders and position";
        List.iter (List.rev t.resync_hooks) ~f:(fun f -> f ());
        request_sync t);
      (* The first snapshot must be in before quoting, so that resting orders from an
         earlier run are reconciled (cancelled) rather than duplicated. *)
      let%bind () = sync t in
      don't_wait_for (Pipe.iter_without_pushback (V.fills venue) ~f:(on_fill t));
      don't_wait_for
        (Pipe.iter_without_pushback (V.order_updates venue) ~f:(on_order_update t));
      let%bind books = V.books venue params.symbol in
      don't_wait_for (Pipe.iter_without_pushback books ~f:(on_book t));
      start_polls t;
      let stop = Ivar.read t.timers_stop in
      Time_source.every
        ~start:(Time_source.after time_source config.summary_interval)
        ~stop
        ~continue_on_error:true
        time_source
        config.summary_interval
        (fun () -> log_summary t);
      return (Ok t)
  ;;

  let cancel_all t =
    let rec attempt n =
      match%bind V.open_orders t.venue (symbol t) with
      | Error error ->
        Log.Global.error_s [%message "open orders at shutdown" (error : Error.t)];
        return ()
      | Ok open_orders ->
        t.strategy <- Strategy.on_open_orders t.strategy open_orders;
        if List.is_empty open_orders
        then return ()
        else if n = 0
        then (
          Log.Global.error
            "%d orders still resting after cancel-all"
            (List.length open_orders);
          return ())
        else (
          Log.Global.info
            "shutdown: cancelling %d resting orders"
            (List.length open_orders);
          let%bind () =
            match%map V.cancel_all t.venue (symbol t) with
            | Ok () -> ()
            | Error error -> Log.Global.error_s [%message "cancel all" (error : Error.t)]
          in
          attempt (n - 1))
    in
    attempt 3
  ;;

  let stop t =
    if t.stopping
    then Ivar.read t.stopped
    else (
      t.stopping <- true;
      Ivar.fill_if_empty t.timers_stop ();
      Log.Global.info "stopping";
      let%bind () = enqueue t ~name:"shutdown" (fun () -> cancel_all t) in
      let%bind () = sync t in
      (* Let fills and cancel confirmations for the last orders arrive. *)
      let%bind () = Time_source.after t.time_source (Time_ns.Span.of_int_ms 250) in
      let%bind () = V.close t.venue in
      let report = report t in
      Ivar.fill_if_empty t.stopped report;
      return report)
  ;;

  let tick_to_order t = Latency_histogram.stats t.latency
  let on_resync t ~f = t.resync_hooks <- f :: t.resync_hooks

  let first_mid
    ?(time_source = Time_source.wall_clock ())
    ?(timeout = Time_ns.Span.of_int_sec 10)
    venue
    symbol
    =
    let%bind books = V.books venue symbol in
    let rec next () =
      match%bind Pipe.read books with
      | `Eof ->
        return (Or_error.error_s [%message "book feed closed" (symbol : Symbol.t)])
      | `Ok book ->
        (match L2.best_bid book, L2.best_ask book with
         | Some bid, Some ask -> return (Ok (Price.div_int Price.(bid.px + ask.px) 2))
         | _ -> next ())
    in
    let%map result =
      match%map Time_source.with_timeout time_source timeout (next ()) with
      | `Result result -> result
      | `Timeout ->
        Or_error.error_s
          [%message "no two-sided book" (symbol : Symbol.t) (timeout : Time_ns.Span.t)]
    in
    Pipe.close_read books;
    result
  ;;

  let run ?time_source venue config ~stop:stop_signal =
    match%bind start ?time_source venue config with
    | Error _ as error ->
      let%map () = V.close venue in
      error
    | Ok t ->
      let%bind () = stop_signal in
      let%map report = stop t in
      Ok report
  ;;

  let connect_and_start ?time_source config ~symbol ~params ~summary_interval =
    let%bind.Deferred.Or_error venue = V.connect config in
    let prepare =
      let open Deferred.Or_error.Let_syntax in
      let%bind instrument = V.instrument venue symbol in
      let%bind mid = first_mid ?time_source venue symbol in
      Deferred.return (Or_error.try_with_join (fun () -> params instrument ~mid))
    in
    let%bind started =
      match%bind prepare with
      | Error _ as error -> return error
      | Ok params -> start ?time_source venue { Config.params; summary_interval }
    in
    match started with
    | Ok _ as ok -> return ok
    | Error _ as error ->
      let%map () = V.close venue in
      error
  ;;

  let connect_and_run
    ?time_source
    config
    ~symbol
    ~params
    ~summary_interval
    ~stop:stop_signal
    =
    match%bind
      connect_and_start ?time_source config ~symbol ~params ~summary_interval
    with
    | Error _ as error -> return error
    | Ok t ->
      let%bind () = stop_signal in
      let%map report = stop t in
      Ok report
  ;;
end
