module JsonStar.Schema.Generation.Helpers

module T = FStar.Tactics
module L = FStar.List.Tot

let tfail (#a: Type) (s:string) : T.Tac a =
    T.debug ("Tactic failure: " ^ s);
    T.fail s

let rec app_head_rev_tail (t: T.term) : T.Tac (T.term * list T.argv) =
    let ins = T.inspect t in
    if T.Tv_App? ins
    then 
        let (T.Tv_App u v) = ins in
        let (x, l) = app_head_rev_tail u in
        (x, v :: l)
    else (t, [])

// Tactic that extracts a term and potentially it's arguments if it's a function application
let app_head_tail (t : T.term) : T.Tac (T.term * list T.argv) =
    let (x,l) = app_head_rev_tail t in
    (x, L.rev l)

let drop_synonym (e : T.env) (t : T.term) : T.Tac (T.term) =
    // delta normalization unfolds names
    T.norm_term_env e [delta] t