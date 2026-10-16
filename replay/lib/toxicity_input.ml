open! Core

let signal (e : Market_data.Toxicity.Event.t) =
  Option.map e.cdf ~f:(fun cdf -> cdf, e.imbalance)
;;

let apply strategy (e : Market_data.Toxicity.Event.t) =
  match signal e with
  | None -> strategy
  | Some (signal, imbalance) ->
    Mm_core.Strategy.on_toxicity
      strategy
      ~now:(Time_ns.to_int_ns_since_epoch e.end_ts)
      ~signal
      ~imbalance
;;
