module JsonStar.Schema.Generation.Types

module T = FStar.Tactics
module L = FStar.List.Tot

/// Qualified name
type qualified_name = l:list string{Cons? l}
val to_qualified_name: n:list string{Cons? n} -> Tot qualified_name
let to_qualified_name n = n

let lastT (n : list string) : T.Tac string = 
    match n with
    | [] -> T.fail "Can't get last element of an empty list"
    | ns -> L.last ns