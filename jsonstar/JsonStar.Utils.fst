module JsonStar.Utils

val pipe_rTot : (x : 'a) -> (f : ('a -> Tot 'b)) -> Tot 'b
let pipe_rTot x f = f x
let op_Bar_Bar_Greater = pipe_rTot

// F* has troubles figuring out "decreases" for inductive types that use functions like map or fold:
// https://github.com/FStarLang/FStar/issues/463 
// Also see ('treeMap' definition)
// https://github.com/FStarLang/FStar/blob/master/examples/termination/termination.fst#L110
// Here with ({x<<xs}) we explicitly say that f accepts only arguments smaller than 'xs'
val list_map : (xs:list 'a) -> (f : (x : 'a{x<<xs} -> 'b)) -> Tot (list 'b)
let rec list_map xs f =
	match xs with 
	| [] -> []
	| x :: xs' -> f x :: list_map xs' f

val list_choose : (xs : list 'a) -> (f : (x : 'a{x<<xs} -> option 'b)) -> Tot (list 'b)
let rec list_choose xs f = 
	match xs with
	| [] -> []
	| x :: xs' -> 
		match f x with 
		| Some a -> a :: list_choose xs' f
		| None   -> list_choose xs' f
