module JsonStar.Schema.Generation.Api

open JsonStar.Schema

let string_s : schema = FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`string))

let nat_s : schema = FStar.Tactics.synth_by_tactic (fun () -> JsonStar.Schema.Generation.gen_schema FStar.Tactics.Goal (`nat))