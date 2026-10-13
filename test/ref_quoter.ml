(* Reference copy of the pure lib/quoter.ml from before the allocation work; the
   bisimulation test checks the library against it. *)

open! Core
open Exchange_core
open Mm_core

module Model = struct
  type t =
    { microprice : Price.t
    ; mid : Price.t
    ; vol_bps : float
    ; capacity : Size.t
    ; inventory_ratio : float
    ; reservation : Price.t
    ; half_spread_bps : float
    }
  [@@deriving sexp_of]
end

let capacity (p : Params.t) ~mid =
  Fx.size_floor
    (Size.min p.max_position (Fx.size_of_notional p.max_notional mid))
    ~lot:p.lot
;;

let model (p : Params.t) market inventory =
  match Ref_market_state.microprice market, Ref_market_state.mid market with
  | Some microprice, Some mid ->
    let vol_bps = Float.min (Ref_market_state.vol_bps market) p.max_vol_bps in
    let capacity = capacity p ~mid in
    let excess = Size.(Inventory.position inventory - p.inventory_target) in
    let inventory_ratio =
      if Size.is_positive capacity
      then
        Float.clamp_exn
          (Size.to_float excess /. Size.to_float capacity)
          ~min:(-1.)
          ~max:1.
      else 0.
    in
    let v = p.gamma *. vol_bps *. vol_bps in
    let reservation = Fx.shift_bps microprice (-.inventory_ratio *. (p.skew_bps +. v)) in
    let half_spread_bps = p.base_half_spread_bps +. (v /. 2.) in
    Some
      { Model.microprice
      ; mid
      ; vol_bps
      ; capacity
      ; inventory_ratio
      ; reservation
      ; half_spread_bps
      }
  | _ -> None
;;

(* Sizes for each level given the room left on a side. *)
let level_sizes (p : Params.t) ~room =
  let rec go level room acc =
    if level >= p.levels
    then List.rev acc
    else (
      let sz = Fx.size_floor (Size.min p.order_size room) ~lot:p.lot in
      if Size.is_positive sz
      then go (level + 1) Size.(room - sz) (sz :: acc)
      else List.rev acc)
  in
  if Size.is_positive room then go 0 room [] else []
;;

module Rung = struct
  type t =
    { side : Side.t
    ; level : int
    ; distance_bps : float
    ; target : Price.t
    ; rounded : Price.t
    ; px : Price.t
    ; sz : Size.t
    }
  [@@deriving sexp_of]
end

module Unquoted = struct
  type t =
    | No_book
    | At_limit of
        { position : Size.t
        ; capacity : Size.t
        }
    | Room_below_lot of { room : Size.t }
    | Non_positive_price
    | Soft_limit of
        { position : Size.t
        ; soft_max_position : Size.t
        ; bump_left : Size.t
        }
  [@@deriving sexp, compare, equal]
end

(* The ladder of one side: each rung's price terms, before dropping non-positive prices. *)
let rungs (p : Params.t) (m : Model.t) ~best_bid ~best_ask ~sizes (side : Side.t) =
  let distance level = m.half_spread_bps +. (Float.of_int level *. p.level_spacing_bps) in
  List.folding_mapi sizes ~init:None ~f:(fun level prev sz ->
    let distance_bps = distance level in
    let rung =
      match side with
      | Buy ->
        let target = Fx.shift_bps m.reservation (-.distance_bps) in
        let rounded = Fx.price_floor target ~tick:p.tick in
        let px =
          Option.fold best_ask ~init:rounded ~f:(fun px ask ->
            Price.min px Price.(ask - p.tick))
        in
        let px =
          Option.fold prev ~init:px ~f:(fun px prev -> Price.min px Price.(prev - p.tick))
        in
        { Rung.side; level; distance_bps; target; rounded; px; sz }
      | Sell ->
        let target = Fx.shift_bps m.reservation distance_bps in
        let rounded = Fx.price_ceil target ~tick:p.tick in
        let px =
          Option.fold best_bid ~init:rounded ~f:(fun px bid ->
            Price.max px Price.(bid + p.tick))
        in
        let px =
          Option.fold prev ~init:px ~f:(fun px prev -> Price.max px Price.(prev + p.tick))
        in
        { Rung.side; level; distance_bps; target; rounded; px; sz }
    in
    Some rung.px, rung)
;;

let room (m : Model.t) ~position (side : Side.t) =
  match side with
  | Buy -> Size.(m.capacity - position)
  | Sell -> Size.(m.capacity + position)
;;

(* Room under the soft limit plus what is left of the bump budget, if there is one. *)
let soft_room (p : Params.t) ~bump_left ~position (side : Side.t) =
  Option.map p.soft_max_position ~f:(fun soft ->
    let limit = Size.(soft + bump_left) in
    match side with
    | Buy -> Size.(limit - position)
    | Sell -> Size.(limit + position))
;;

(* Quotes of one side, or why there are none. *)
let side_ladder (p : Params.t) (m : Model.t) market ~bump_left ~position side =
  let hard_room = room m ~position side in
  let soft_room = soft_room p ~bump_left ~position side in
  let room = Option.fold soft_room ~init:hard_room ~f:Size.min in
  let sizes = level_sizes p ~room in
  let rungs =
    rungs
      p
      m
      ~best_bid:(Ref_market_state.best_bid market)
      ~best_ask:(Ref_market_state.best_ask market)
      ~sizes
      side
  in
  let quotes =
    List.filter_map rungs ~f:(fun (r : Rung.t) ->
      Option.some_if
        (Price.is_positive r.px)
        { Quote.side = r.side; level = r.level; px = r.px; sz = r.sz })
  in
  let reason : Unquoted.t option =
    if not (List.is_empty quotes)
    then None
    else if not (Size.is_positive hard_room)
    then Some (At_limit { position; capacity = m.capacity })
    else if Option.exists soft_room ~f:(fun soft -> Size.(soft < p.lot))
    then
      Some
        (Soft_limit
           { position
           ; soft_max_position = Option.value_exn p.soft_max_position
           ; bump_left
           })
    else if List.is_empty rungs
    then Some (Room_below_lot { room })
    else Some Non_positive_price
  in
  rungs, quotes, reason
;;

let quote_with_reasons ?bump_left (p : Params.t) market inventory =
  let bump_left = Option.value bump_left ~default:p.bump_budget in
  match model p market inventory with
  | None -> [], [ Side.Buy, Unquoted.No_book; Sell, No_book ]
  | Some m ->
    let position = Inventory.position inventory in
    let _, bids, bid_reason = side_ladder p m market ~bump_left ~position Buy in
    let _, asks, ask_reason = side_ladder p m market ~bump_left ~position Sell in
    ( bids @ asks
    , List.filter_opt
        [ Option.map bid_reason ~f:(fun r -> Side.Buy, r)
        ; Option.map ask_reason ~f:(fun r -> Side.Sell, r)
        ] )
;;

let quote ?bump_left p market inventory =
  fst (quote_with_reasons ?bump_left p market inventory)
;;

let unquoted ?bump_left p market inventory =
  snd (quote_with_reasons ?bump_left p market inventory)
;;

module For_testing = struct
  let explain ?bump_left (p : Params.t) market inventory =
    let bump_left = Option.value bump_left ~default:p.bump_budget in
    let buf = Buffer.create 1024 in
    let line indent fmt =
      ksprintf
        (fun s -> Buffer.add_string buf (String.make (2 * indent) ' ' ^ s ^ "\n"))
        fmt
    in
    let opt = Option.value_map ~default:"-" ~f:Price.to_string in
    let position = Inventory.position inventory in
    line 0 "quote";
    line
      1
      "position %s (target %s)"
      (Size.to_string position)
      (Size.to_string p.inventory_target);
    line
      1
      "book bid %s ask %s"
      (opt (Ref_market_state.best_bid market))
      (opt (Ref_market_state.best_ask market));
    (match model p market inventory with
     | None -> line 1 "no two-sided book: nothing quoted"
     | Some m ->
       let v = p.gamma *. m.vol_bps *. m.vol_bps in
       line
         1
         "mid %s, microprice %s"
         (Price.to_string m.mid)
         (Price.to_string m.microprice);
       line
         1
         "vol %.4f bps (estimate %.4f, cap %.4f)"
         m.vol_bps
         (Ref_market_state.vol_bps market)
         p.max_vol_bps;
       line
         1
         "capacity %s = min(max_position %s, max_notional %s / mid), floored to lot %s"
         (Size.to_string m.capacity)
         (Size.to_string p.max_position)
         (Usd.to_string p.max_notional)
         (Size.to_string p.lot);
       line 1 "inventory ratio q %.4f" m.inventory_ratio;
       line 1 "risk term v = gamma %.4f * vol^2 = %.4f bps" p.gamma v;
       line
         1
         "reservation %s = microprice shifted by -q * (skew %.4f + v) = %.4f bps"
         (Price.to_string m.reservation)
         p.skew_bps
         (-.m.inventory_ratio *. (p.skew_bps +. v));
       line
         1
         "half-spread %.4f bps = base %.4f + v / 2"
         m.half_spread_bps
         p.base_half_spread_bps;
       List.iter [ Side.Buy; Sell ] ~f:(fun side ->
         let rungs, _, reason = side_ladder p m market ~bump_left ~position side in
         line
           1
           "%s: room %s%s"
           (match side with
            | Buy -> "bids"
            | Sell -> "asks")
           (Size.to_string (room m ~position side))
           (match soft_room p ~bump_left ~position side with
            | None -> ""
            | Some soft ->
              sprintf
                ", soft room %s (soft limit %s + bump left %s)"
                (Size.to_string soft)
                (Size.to_string (Option.value_exn p.soft_max_position))
                (Size.to_string bump_left));
         List.iter rungs ~f:(fun (r : Rung.t) ->
           line
             2
             "L%d %s @ %s: distance %.4f bps, target %s, on tick %s%s"
             r.level
             (Size.to_string r.sz)
             (Price.to_string r.px)
             r.distance_bps
             (Price.to_string r.target)
             (Price.to_string r.rounded)
             (if Price.equal r.rounded r.px
              then ""
              else ", moved off the touch or the level before"));
         Option.iter reason ~f:(fun reason ->
           line 2 "not quoted: %s" (Sexp.to_string [%sexp (reason : Unquoted.t)]))));
    Buffer.contents buf
  ;;
end
