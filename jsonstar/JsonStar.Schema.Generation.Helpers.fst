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

let printAst (t : T.term) : T.Tac unit =
    let ast = FStar.Tactics.Print.term_to_ast_string t in
    T.print ast

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
    | _, _ -> tfail ("termeq doesn't support " ^ (T.term_to_string t1) ^ " or " ^ (T.term_to_string t2) ^ "\n")