open! Core
open Exchange_core
open Mm_core
open Helpers

type case =
  { params : Params.t
  ; bid : Price.t
  ; ask : Price.t
  ; bid_sz : Size.t
  ; ask_sz : Size.t
  ; position : Size.t
  ; vol_bps : float
  }
[@@deriving sexp_of]

let gen_case =
  let open Quickcheck.Let_syntax in
  let%bind mid = Int.gen_incl 20_000 200_000
  and spread = Int.gen_incl 1 100
  and bid_sz = Int.gen_incl 1 1_000_000
  and ask_sz = Int.gen_incl 1 1_000_000
  and levels = Int.gen_incl 1 6
  and order_lots = Int.gen_incl 1 500
  and max_lots = Int.gen_incl 1 5_000
  and base_half_spread_bps = Float.gen_incl 0.1 30.
  and level_spacing_bps = Float.gen_incl 0. 20.
  and skew_bps = Float.gen_incl 0. 50.
  and gamma = Float.gen_incl 0. 5.
  and max_notional = Int.gen_incl 10 100_000
  and vol_bps = Float.gen_incl 0. 50. in
  let%bind position_lots = Int.gen_incl (-max_lots) max_lots
  and target_lots = Int.gen_incl (-(max_lots - 1)) (max_lots - 1) in
  let lot = Params.btc.lot in
  let lots n = Size.scale lot ~by:n in
  let params =
    { Params.btc with
      levels
    ; order_size = lots order_lots
    ; max_position = lots max_lots
    ; inventory_target = lots target_lots
    ; base_half_spread_bps
    ; level_spacing_bps
    ; skew_bps
    ; gamma
    ; max_notional = Usd.of_int max_notional
    }
  in
  return
    { params
    ; bid = Price.of_int (mid - spread)
    ; ask = Price.of_int mid
    ; bid_sz = lots bid_sz
    ; ask_sz = lots ask_sz
    ; position = lots position_lots
    ; vol_bps
    }
;;

let quotes_of c =
  let book : L2.t =
    { symbol = c.params.symbol
    ; time = 0
    ; bids = [ { px = c.bid; sz = c.bid_sz; n = 1 } ]
    ; asks = [ { px = c.ask; sz = c.ask_sz; n = 1 } ]
    }
  in
  let market =
    Market_state.with_vol_bps
      (Market_state.on_book (Market_state.create ~vol_window:10) book)
      c.vol_bps
  in
  let inventory =
    if Size.is_zero c.position
    then Inventory.empty
    else
      Inventory.on_fill
        Inventory.empty
        { (fill ~tid:1 ~side:Buy ~px:"1" ~sz:"1" ()) with
          side = (if Size.is_positive c.position then Buy else Sell)
        ; px = c.bid
        ; size = Size.abs c.position
        }
  in
  Quoter.quote c.params market inventory
;;

let side_of side quotes =
  List.filter quotes ~f:(fun (q : Quote.t) -> Side.equal q.side side)
;;

let rec strictly ~cmp = function
  | a :: (b :: _ as rest) -> cmp a b && strictly ~cmp rest
  | [ _ ] | [] -> true
;;

let check c =
  let quotes = quotes_of c in
  let bids = side_of Buy quotes
  and asks = side_of Sell quotes in
  let px (q : Quote.t) = q.px in
  let total qs = Size.sum (List.map qs ~f:(fun (q : Quote.t) -> q.sz)) in
  let fail msg = raise_s [%message msg (quotes : Quote.t list)] in
  (* Never crossed against ourselves or the book (post-only). *)
  (match List.hd bids, List.hd asks with
   | Some b, Some a -> if Price.(b.px >= a.px) then fail "own quotes cross"
   | _ -> ());
  if List.exists bids ~f:(fun q -> Price.(q.px >= c.ask)) then fail "bid crosses the book";
  if List.exists asks ~f:(fun q -> Price.(q.px <= c.bid)) then fail "ask crosses the book";
  (* Ladders move strictly outward. *)
  if not (strictly bids ~cmp:(fun a b -> Price.(px a > px b)))
  then fail "bids not monotone";
  if not (strictly asks ~cmp:(fun a b -> Price.(px a < px b)))
  then fail "asks not monotone";
  (* Every quote filled never takes the position past the limit. *)
  if Size.(c.position + total bids > c.params.max_position)
  then fail "bids exceed max position";
  if Size.(c.position - total asks < neg c.params.max_position)
  then fail "asks exceed max position";
  (* Tick and lot aligned, positive. *)
  List.iter quotes ~f:(fun q ->
    if Price.to_units q.px % Price.to_units c.params.tick <> 0 then fail "off tick";
    if Size.to_units q.sz % Size.to_units c.params.lot <> 0 then fail "off lot";
    if not (Price.is_positive q.px && Size.is_positive q.sz) then fail "non-positive")
;;

let%test_unit "quotes never cross and never exceed max position" =
  Quickcheck.test gen_case ~sexp_of:sexp_of_case ~trials:5_000 ~f:check
;;

let%test_unit "reconciling the ladder against itself is a no-op" =
  Quickcheck.test gen_case ~sexp_of:sexp_of_case ~trials:1_000 ~f:(fun c ->
    let desired = quotes_of c in
    let open_orders =
      List.mapi desired ~f:(fun i (q : Quote.t) ->
        open_order
          ~oid:i
          ~side:q.side
          ~px:(Price.to_string q.px)
          ~sz:(Size.to_string q.sz)
          ())
    in
    let plan = Reconciler.reconcile c.params ~now:0 ~generation:0 ~open_orders ~desired in
    if List.exists plan ~f:Action.is_change
    then raise_s [%message "churn" (plan : Action.t list)])
;;

(* Resting orders around the ladder: some exact, some a few ticks off, some resized, some
   on the wrong side or far away. *)
let gen_resting (c : case) =
  let open Quickcheck.Let_syntax in
  let desired = quotes_of c in
  let%bind n = Int.gen_incl 0 8 in
  let%map orders =
    List.gen_with_length
      n
      (let%map side = Quickcheck.Generator.of_list [ Side.Buy; Sell ]
       and anchor = Quickcheck.Generator.of_list (None :: List.map desired ~f:Option.some)
       and ticks = Int.gen_incl (-40) 40
       and lots = Int.gen_incl 1 200
       and age_s = Int.gen_incl 0 120 in
       let base, side, size =
         match anchor with
         | Some (q : Quote.t) ->
           q.px, q.side, if lots > 150 then Size.scale c.params.lot ~by:lots else q.sz
         | None -> c.ask, side, Size.scale c.params.lot ~by:lots
       in
       let px = Price.(base + scale c.params.tick ~by:ticks) in
       side, (if Price.is_positive px then px else c.params.tick), size, age_s)
  in
  List.mapi orders ~f:(fun i (side, px, size, age_s) ->
    { (open_order ~oid:i ~side ~px:"1" ~sz:"1" ()) with
      px
    ; size
    ; timestamp = ms (120_000 - (age_s * 1_000))
    })
;;

(* What the venue would hold after executing [plan]. *)
let apply ~now plan open_orders =
  let cancelled =
    List.filter_map plan ~f:(function
      | Action.Cancel oid -> Some oid
      | Keep _ | Place _ -> None)
  in
  let kept =
    List.filter open_orders ~f:(fun (o : Open_order.t) ->
      not (List.mem cancelled o.order_id ~equal:Order_id.equal))
  in
  let placed =
    List.filter_mapi plan ~f:(fun i -> function
      | Action.Place (r : Order_request.t) ->
        Some
          ({ symbol = r.symbol
           ; side = r.side
           ; px = Option.value_exn r.px
           ; size = r.size
           ; orig_size = Some r.size
           ; order_id = Order_id.of_int (1_000 + i)
           ; client_order_id = r.client_order_id
           ; timestamp = now
           }
           : Open_order.t)
      | Keep _ | Cancel _ -> None)
  in
  kept @ placed
;;

let%test_unit "reconciling right after applying a plan yields only keeps" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind c = gen_case in
    let%map resting = gen_resting c in
    c, resting
  in
  Quickcheck.test
    gen
    ~sexp_of:[%sexp_of: case * Open_order.t list]
    ~trials:2_000
    ~f:(fun (c, open_orders) ->
      let now = ms 120_000 in
      let desired = quotes_of c in
      let plan = Reconciler.reconcile c.params ~now ~generation:1 ~open_orders ~desired in
      let after = apply ~now plan open_orders in
      let again =
        Reconciler.reconcile c.params ~now ~generation:2 ~open_orders:after ~desired
      in
      if List.exists again ~f:Action.is_change
      then
        raise_s
          [%message "plan not idempotent" (plan : Action.t list) (again : Action.t list)])
;;

let%test_unit "flatten only places reduce-only orders that shrink the position" =
  let gen =
    let open Quickcheck.Let_syntax in
    let%bind c = gen_case in
    let%map resting = gen_resting c
    and has_book =
      Quickcheck.Generator.weighted_union [ 9., return true; 1., return false ]
    in
    c, resting, has_book
  in
  Quickcheck.test
    gen
    ~sexp_of:[%sexp_of: case * Open_order.t list * bool]
    ~trials:2_000
    ~f:(fun (c, open_orders, has_book) ->
      let market =
        if has_book
        then
          Market_state.on_book
            (Market_state.create ~vol_window:10)
            { symbol = c.params.symbol
            ; time = 0
            ; bids = [ { px = c.bid; sz = c.bid_sz; n = 1 } ]
            ; asks = [ { px = c.ask; sz = c.ask_sz; n = 1 } ]
            }
        else Market_state.create ~vol_window:10
      in
      let inventory =
        if Size.is_zero c.position
        then Inventory.empty
        else
          Inventory.on_fill
            Inventory.empty
            { (fill ~tid:1 ~side:Buy ~px:"1" ~sz:"1" ()) with
              side = (if Size.is_positive c.position then Buy else Sell)
            ; px = c.bid
            ; size = Size.abs c.position
            }
      in
      let plan = Risk.flatten c.params ~generation:3 ~open_orders market inventory in
      List.iter plan ~f:(function
        | Keep _ | Cancel _ -> ()
        | Place (r : Order_request.t) ->
          let after = Size.(c.position + Fx.signed r.side r.size) in
          if not r.reduce_only
          then
            raise_s
              [%message "flatten places a non reduce-only order" (r : Order_request.t)];
          if Size.(abs after > abs c.position)
             || Fx.sign_of_size after * Fx.sign_of_size c.position < 0
          then
            raise_s [%message "flatten grows or flips the position" (r : Order_request.t)]))
;;
