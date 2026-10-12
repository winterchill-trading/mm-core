open! Core
open! Async
open Mm_core
open Mm_core_replay

let replay =
  Command.async_or_error
    ~summary:"Run the strategy over recorded norm/ L2 event logs and print its quotes"
    ~readme:(fun () ->
      "Every L2 update of SYMBOL under the store DIR (all recorded days, merged by \
       recv_ts) is applied to a book; the strategy ticks at each update's recv_ts and \
       the plan it sends is printed. Plans are accepted as sent and nothing fills. \
       Parameters come from -params, or from the built-in instrument catalogue at the \
       first mid.")
    (let%map_open.Command dir =
       flag "-norm" (required string) ~doc:"DIR store root (<DIR>/<venue>/<symbol>/norm/)"
     and symbol =
       flag "-symbol" (required string) ~doc:"S venue:SYMBOL, or a symbol on one venue"
     and params_file =
       flag "-params" (optional string) ~doc:"FILE strategy parameters (a Params.t sexp)"
     and depth =
       flag "-depth" (optional_with_default 20 int) ~doc:"N book levels per side (20)"
     in
     fun () ->
       let open Deferred.Or_error.Let_syntax in
       let%bind instrument, files = Replay.l2_files ~dir ~symbol in
       let%bind fixed =
         match params_file with
         | None -> return None
         | Some path ->
           Deferred.return
             (Or_error.try_with_join (fun () ->
                Params.validate (Sexp.load_sexp_conv_exn path Params.t_of_sexp))
              |> Or_error.map ~f:Option.some)
       in
       let params ~mid =
         match fixed with
         | Some p -> Ok p
         | None -> Replay.catalogue_params instrument ~mid
       in
       let feed = Market_data.Replay_feed.of_files ~speed:0. files in
       let%map (_ : Strategy.t option) =
         Replay.run ~depth feed ~params ~f:(fun tick ->
           print_endline (Replay.Tick.to_string_hum tick))
       in
       ())
;;

let () = Command.group ~summary:"mm-core tools" [ "replay", replay ] |> Command_unix.run
