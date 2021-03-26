module JsonStar.Schema.Generation.Helpers

module T = FStar.Tactics
module L = FStar.List.Tot

let rec try_satisfy_any (#a : Type) (fs : list (a -> bool)) (x : a) : Tot (option a) =
  match fs with
  | [] -> None
  | f :: ft -> if f x then Some x else try_satisfy_any #a ft x

val satisfy_all: ('a -> T.Tac bool) -> list 'a -> T.Tac bool
let rec satisfy_all f x = match x with
  | [] -> true
  | a::tl -> if f a then satisfy_all f tl else false

val satisfy_any: ('a -> T.Tac bool) -> list 'a -> T.Tac bool
let rec satisfy_any f x = match x with
  | [] -> false
  | a::tl -> if f a then true else satisfy_any f tl

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

// NOTE: use with care, extensive use can be very slow and eats a lot of RAM
let delta_name (e : T.env) (qname : list string) (t : T.term) : T.Tac T.term = 
  T.norm_term_env e [delta_only qname] t

let printAst (t : T.term) : T.Tac unit =
    let ast = FStar.Tactics.Print.term_to_ast_string t in
    T.print ast

let rec string_of_name (n: T.name) : Tot string =
  match n with
  | [] -> ""
  | [a] -> a
  | a :: b -> a ^ "." ^ string_of_name b

let unfold_fv (t: T.fv) : T.Tac T.term =
  let env = T.cur_env () in
  let n = T.inspect_fv t in
  match T.lookup_typ env n with
  | Some s ->
    begin match T.inspect_sigelt s with
    | T.Sg_Let _ _ _ _ def ->
      let nm = string_of_name n in
      T.debug ("Unfolded definition: " ^ nm);
      def
    | _ ->
      let nm = string_of_name n in
      tfail (nm ^ ": not a non-recursive let definition")
    end
  | _ -> tfail "Definition not found"

let unfold_term (t: T.term) : T.Tac T.term =
  match T.inspect t with
  | T.Tv_FVar v -> unfold_fv v
  | _ -> tfail "Not a global variable"

// Compares if two fully qualified names are equal
let rec fv_eq (fv1 : list string) (fv2 : list string) : bool = 
    match fv1, fv2 with
    | [], [] -> true
    | x :: xs, y :: ys -> if x = y then fv_eq xs ys else false
    | _, _ -> false
    
// NOTE: The build-in T.term_eq doesn't work very reliably. The plan is to build my own fully functional
//       equality on terms (with the definition of equal that makes sense for schema generation) so might not 
//       be very reusable
let rec termeq (t1 : T.term) (t2 : T.term) : T.Tac bool =
    match T.inspect t1, T.inspect t2 with
    | T.Tv_Var bv1, T.Tv_Var bv2 -> begin
        // We only compare type of the argument, not it's name
        match T.inspect_bv bv1, T.inspect_bv bv2 with
        | { T.bv_sort = typ1; }, { T.bv_sort = typ2; } -> termeq typ1 typ2
        end
    | T.Tv_FVar fv1, T.Tv_FVar fv2 -> fv_eq (T.inspect_fv fv1) (T.inspect_fv fv2)
    // We ignore the fact if the arg is implicit or explicit
    | T.Tv_App lt1 (arg1, _), T.Tv_App lt2 (arg2, _) -> begin  
        // TODO: short circuit for && to avoid a tactic getting stuck - I belive this is already improved in master 
        let l_true = termeq lt1 lt2 in 
        if l_true then termeq arg1 arg2 else false
        end
    // false cases - combine them as _, _ -> false when I get confident enough with how this function works
    | T.Tv_App _ _, T.Tv_FVar _ -> false
    | T.Tv_FVar _, T.Tv_Arrow _ _ -> false
    | _, _ -> tfail ("termeq doesn't support " ^ (FStar.Tactics.Print.term_to_ast_string t1) ^ " or " ^ (FStar.Tactics.Print.term_to_ast_string t2) ^ "\n")