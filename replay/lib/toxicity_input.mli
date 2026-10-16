(** market-data's {!Market_data.Toxicity} as the producer of the signal of mm-core's
    {!Mm_core.Toxicity_gate}: each filled volume bucket becomes a
    {!Mm_core.Strategy.on_toxicity} call.

    The signal is the bucket's VPIN percentile ([cdf], in [[0, 1]]) against the previous
    buckets, which is what the gate's default thresholds (widen from 0.8, pull at 0.95)
    are set for; the imbalance is the bucket's signed order imbalance. A bucket without a
    percentile yet (the first one) gives no signal.

    {[
      let toxicity = Market_data.Toxicity.create instrument config in
      (* for every trade *)
      List.fold
        (Market_data.Toxicity.add toxicity trade)
        ~init:strategy
        ~f:Toxicity_input.apply
    ]} *)

open! Core

(** [(signal, imbalance)] of a bucket, [None] without a percentile. *)
val signal : Market_data.Toxicity.Event.t -> (float * float) option

(** [Strategy.on_toxicity] at the bucket's [end_ts]; the strategy unchanged without a
    signal. *)
val apply : Mm_core.Strategy.t -> Market_data.Toxicity.Event.t -> Mm_core.Strategy.t
