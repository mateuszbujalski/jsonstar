module JsonStar.Schema.Dsl

(*
The idea behind this module is to provide easy to use DSL for describing types 
which can later be translated into json-schema. 

NOTE: Functions marked as irreducible can't be replaced with it's implementation for schema generation. 
      We only support detecting the DSL version of the functions below. 
TODO: Does this break automatic F* proves similarly to hiding the implementation behind the interface?
*)

// string options
let minLength (s : string) (n : nat) = String.length s >= n
let maxLength (s : string) (n : nat) = String.length s <= n
// TODO: Stub only - add real pattern matching
let pattern (s : string) (p : string) = true
// TODO: How do we handle 'reference'? DSL or attribute?

// number options
let minimum (x : int) (n : int) = x >= n
let maximum (x : int) (n : int) = x <= n

// Object options

// Array options
irreducible let minItems (l : list 'a) (n : nat) = List.Tot.length l >= n
irreducible let maxItems (l : list 'a) (n : nat) = List.Tot.length l <= n
irreducible let uniqueItems (#a:eqtype) (l : list a) = List.Tot.noRepeats l

// TODO: Decide how do we want to express "oneOf" and "dependencies"



