module JsonStar.Schema.Dsl

(*
The idea behind this module is to provide easy to use DSL for describing types 
which can later be translated into json-schema. 

NOTE: Functions marked as irreducible can't be replaced with it's implementation for schema generation. 
      We only support detecting the DSL version of the functions below. 
TODO: Does this break automatic F* proves similarly to hiding the implementation behind the interface?
*)

(*
NOTE: The convention here is that the first argument is always the value we're restricting, e.g.
for "s:string{minLength s 5}" it is 's'.
*)

// string options
let minLength (s : string) (n : nat) = String.length s >= n
let maxLength (s : string) (n : nat) = String.length s <= n
// TODO: Stub only - add real pattern matching. 
// Irreducible can be removed when some implementation is provided, as currently F* simplifies it to just C_True
irreducible let pattern (s : string) (p : string) = true
// TODO: How do we handle 'reference'? DSL or attribute?

// number options
let minimum (x : int) (n : int) = x >= n
let maximum (x : int) (n : int) = x <= n

// Object options

// Array options
let minItems (l : list 'a) (n : nat) = List.Tot.length l >= n
let maxItems (l : list 'a) (n : nat) = List.Tot.length l <= n
let uniqueItems (#a:eqtype) (l : list a) = List.Tot.noRepeats l

let rec allow (x : 'a) (l : list ('a -> Tot bool)) =
    match l with
    | [] -> false
    | f :: fs -> f x || allow x fs
let allowed (#t:Type) (l:list (t -> bool)) = x:t{norm [primops; zeta; iota; delta] (allow x l == true)}

let rec disallow (x : 'a) (l : list ('a -> Tot bool)) = 
    match l with 
    | [] -> true
    | f :: fs -> not (f x) && disallow x fs
let disallowed (#t:Type) (l:list (t -> bool)) = x:t{norm [primops; zeta; iota; delta] (disallow x l == true)}

// TODO: Decide how do we want to express "oneOf" and "dependencies"



