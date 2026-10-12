open! Core
open! Async
open Exchange_core
open Helpers

let rec wait_until ?(timeout = Time_ns.Span.of_int_sec 10) ~what f =
  if f ()
  then return ()
  else if Time_ns.Span.(timeout <= zero)
  then raise_s [%message "timed out waiting" what]
  else (
    let step = Time_ns.Span.of_int_ms 10 in
    let%bind () = Clock_ns.after step in
    wait_until ~timeout:Time_ns.Span.(timeout - step) ~what f)
;;

let live_book ?(bid = "99990") ?(ask = "100010") () =
  { (book ~bid ~ask ()) with time = Time_ns.to_int_ns_since_epoch (Time_ns.now ()) }
;;

let generation_of_cloid cloid =
  Int.of_string ("0x" ^ String.suffix (Client_order_id.to_string cloid) 12)
;;

let print_orders orders =
  List.sort orders ~compare:(fun (a : Open_order.t) b -> Price.descending a.px b.px)
  |> List.iter ~f:(fun (o : Open_order.t) ->
    printf
      "%s %s@%s%s\n"
      (Sexp.to_string [%sexp (o.side : Side.t)])
      (Size.to_string o.size)
      (Price.to_string o.px)
      (Option.value_map o.client_order_id ~default:"" ~f:(fun c ->
         sprintf " gen %d" (generation_of_cloid c))))
;;
