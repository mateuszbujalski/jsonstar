module JsonStar.Schema.Generation.Formula

(*
    We need a way to reason about complicated refinements that drive the schema generation. 
    Based on FStar.Reflection.Formula from F* standard library.
*)

module T = FStar.Tactics
module P = FStar.Printf

// TODO: Copied from 'Reader' module - refactor to some common module
// Adds "Mk" to last segment of the qualified name
let rec record_constructor_name (n : list string{Cons? n}) : Tot (list string) = 
    match n with
    | [ x ] -> [ "Mk" ^ x ]
    | x :: xs -> x :: record_constructor_name xs
// Records are syntactic sugar over inductive types
let isRecord (env : T.env) (fv : T.fv) : T.Tac bool = 
    //T.print (P.sprintf "IsRecord: %s" (T.flatten_name (T.inspect_fv fv)));
    let qname = T.inspect_fv fv in
    match T.lookup_typ env qname with
    | Some s -> begin
        // Check if sig in an enum definition
        match T.inspect_sigelt s with
        | T.Sg_Let _ _ _ _ _ -> false
        | T.Sg_Inductive _ _ _ _ cts -> begin
            // A record should have exactly one constructor with at least one field
            // and the name of that constructor should be "Mk{record_type}"
            match cts with
            | [ (ctname, ct) ] -> 
                begin match T.inspect ct with 
                | T.Tv_Arrow _ _ -> 
                    if Cons? qname 
                        then (T.implode_qn ctname) = (T.implode_qn (record_constructor_name qname))
                        else false 
                | _ -> false
                end
            | _ -> false
            end
        | T.Sg_Val _ _ _ -> false
        | T.Unk -> false
        end
    | None -> false
// Constructor name to (potential) record name
let rec ctr_to_record_name (n : list string) : Tot (list string) = 
    match n with
    | [] -> []
    | [ x ] -> begin
        match String.list_of_string x with
        | 'M' :: 'k' :: rest -> [ String.string_of_list rest ]
        | _ -> [ x ]
        end
    | x :: xs -> x :: ctr_to_record_name xs

noeq type comparison =
  | Eq     of option T.typ  (* Propositional equality (eq2), maybe annotated *)
  | BoolEq of option T.typ  (* Decidable, boolean equality (eq), maybe annotated *)
  | Lt | Le | Gt | Ge     (* Orderings, at type `int` (and subtypes) *)

noeq type target = 
    /// Restriction applied to the refined type
    | Id : target
    /// Restriction applied to the field of a refined type
    | Field : target -> T.bv -> target
    /// A value to project a value from, a constructor name, an argument name
    | Project : target -> T.fv -> T.bv -> target

let rec target_to_string (x : target) : Tot string = 
    match x with
    | Id -> "Id"
    | Field r n -> (T.bv_to_string n) ^ " " ^ (target_to_string r)
    | Project r n tn -> (T.flatten_name (T.inspect_fv n)) ^ " " ^ (T.bv_to_string tn) ^ " " ^ (target_to_string r)

noeq type refinement_formula =
  | True_  : refinement_formula
  | False_ : refinement_formula
  | Comp   : comparison -> refinement_formula -> refinement_formula -> refinement_formula
  | Is     : T.fv (* Name of recognized value *) -> target (* A projection, e.g. 'Cons?' *) -> refinement_formula
  | And    : refinement_formula -> refinement_formula -> refinement_formula
  | Or     : refinement_formula -> refinement_formula -> refinement_formula
  | Not    : refinement_formula -> refinement_formula
  | Implies: refinement_formula -> refinement_formula -> refinement_formula
  | Iff    : refinement_formula -> refinement_formula -> refinement_formula
  | Forall : T.bv -> refinement_formula -> refinement_formula
  | Exists : T.bv -> refinement_formula -> refinement_formula
  | App    : refinement_formula -> refinement_formula -> refinement_formula
  | Name   : T.bv -> refinement_formula
  | FV     : T.fv -> refinement_formula
  | IntLit : int -> refinement_formula
  // If we can't recognize some term as formula, let's just leave it unchanged
  | UnknownTerm : T.term -> refinement_formula 

let rec pat_to_string (p : T.pattern) : T.Tac string = 
    match p with
    | T.Pat_Constant c -> T.term_to_string (T.pack (T.Tv_Const c))
    | T.Pat_Cons fv args -> begin
        let qname = T.inspect_fv fv in
        let args_s = T.map (fun (p, is_imp) -> P.sprintf "(%s, %s)" (pat_to_string p) (T.term_to_string (T.pack (T.Tv_Const (if is_imp then T.C_True else T.C_False))))) args in
        P.sprintf "Pat_Cons %s [%s]" (T.implode_qn qname) (FStar.String.concat ", " args_s)
        end
    | T.Pat_Var bv -> P.sprintf "Pat_Var (%s)" (T.bv_to_string bv)
    | T.Pat_Wild bv -> P.sprintf "Pat_Wild (%s)" (T.bv_to_string bv)
    | T.Pat_Dot_Term bv t -> P.sprintf "Pat_Dot_Term (%s, %s)" (T.bv_to_string bv) (T.term_to_string t)

let rec targetFromTerm (env : T.env) (t : T.term) : T.Tac target =
    //T.print (T.term_to_ast_string t);
    match T.inspect t with
    | T.Tv_Var n -> Id
    | T.Tv_Match c pats -> begin
        //T.print (T.term_to_ast_string t);
        let inner_target = targetFromTerm env c in
        //T.iter (fun (p, _) -> T.print (pat_to_string p)) pats;
        match pats with
        | [ (T.Pat_Cons fv args, tt) ] -> begin
            // TODO: Check if fv is a record type or validate that 
            // args is about extracting a record field
            match T.inspect tt with
            | T.Tv_Var n -> begin
                // NOTE: Check if we're dealing with a record-type constructor or not
                if isRecord env (T.pack_fv (ctr_to_record_name (T.inspect_fv fv)))
                    then Field inner_target n
                    else begin
                        // TODO: A subschema selection is kind of complicated, as it's a combination of FieldSelection + Projection and we need to handle it as a special case
                        match inner_target with
                        | Field it n2 -> Project it fv n // TODO: Check that inner target is about selecting an enum field where one of the possible values is the Variant we want to choose at this level
                        | _ -> Field inner_target n
                    end
                end
            | _ -> T.fail (P.sprintf "Refinement target not recognized: %s" (T.term_to_ast_string t))
            end
        | _ -> T.fail (P.sprintf "Refinement target not recognized: %s" (T.term_to_ast_string t))
        end
    | _ -> T.fail (P.sprintf "Refinement target not recognized: %s" (T.term_to_ast_string t))

let rec mk_Forall (env : T.env) (ty : T.term) (pred : T.term) : T.Tac refinement_formula =
    let b = T.pack_bv ({ T.bv_ppname = "x";
                       T.bv_sort = ty;
                       T.bv_index = 0; }) in
    Forall b (fromTerm env (T.pack_ln (T.Tv_App pred (T.pack_ln (T.Tv_BVar b), T.Q_Explicit))))

and mk_Exists (env : T.env) (ty : T.term) (pred : T.term) : T.Tac refinement_formula =
    let b = T.pack_bv ({ T.bv_ppname = "x";
                       T.bv_sort = ty;
                       T.bv_index = 0; }) in
    Exists b (fromTerm env (T.pack_ln (T.Tv_App pred (T.pack_ln (T.Tv_BVar b), T.Q_Explicit))))

and fromTerm (env : T.env) (t : T.term) : T.Tac refinement_formula =
    match T.inspect_ln t with
    | T.Tv_Var n -> Name n
    | T.Tv_FVar fv ->
        let qn = T.inspect_fv fv in
        if qn = T.true_qn then True_
        else if qn = T.false_qn then False_
        else FV fv
    // TODO: l_Forall
    // ...or should we just try to drop all squashes?
    // TODO: b2t at this point ?
    | T.Tv_App h0 t -> begin
        let (h, ts) = T.collect_app h0 in
        match T.inspect_ln h, ts@[t] with
        | T.Tv_FVar fv, [(a1, T.Q_Implicit); (a2, T.Q_Explicit); (a3, T.Q_Explicit)] ->
            let qn = T.inspect_fv fv in
            if      qn = T.eq2_qn then Comp (Eq     (Some a1)) (fromTerm env a2) (fromTerm env a3)
            else if qn = ["Prims"; "equals"] then Comp (BoolEq (Some a1)) (fromTerm env a2) (fromTerm env a3)
            else if qn = T.eq1_qn then Comp (BoolEq (Some a1)) (fromTerm env a2) (fromTerm env a3)
            else if qn = T.lt_qn  then Comp Lt (fromTerm env a2) (fromTerm env a3)
            else if qn = T.lte_qn then Comp Le (fromTerm env a2) (fromTerm env a3)
            else if qn = T.gt_qn  then Comp Gt (fromTerm env a2) (fromTerm env a3)
            else if qn = T.gte_qn then Comp Ge (fromTerm env a2) (fromTerm env a3)
            else App (fromTerm env h0) (fromTerm env (fst t))
        | T.Tv_FVar fv, [(a1, T.Q_Explicit); (a2, T.Q_Explicit)] ->
            let qn = T.inspect_fv fv in
            if qn = T.imp_qn then Implies (fromTerm env a1) (fromTerm env a2)
            else if qn = T.and_qn then And (fromTerm env a1) (fromTerm env a2)
            else if qn = ["Prims"; "op_AmpAmp"] then And (fromTerm env a1) (fromTerm env a2)
            else if qn = T.iff_qn then Iff (fromTerm env a1) (fromTerm env a2)
            else if qn = T.or_qn  then Or (fromTerm env a1) (fromTerm env a2)
            else if qn = ["Prims"; "op_BarBar"] then Or (fromTerm env a1) (fromTerm env a2)
            // Non-annotated comparisons
            else if qn = ["Prims"; "equals"] then Comp (BoolEq None) (fromTerm env a1) (fromTerm env a2)
            else if qn = T.eq2_qn then Comp (Eq     None) (fromTerm env a1) (fromTerm env a2)
            else if qn = T.eq1_qn then Comp (BoolEq None) (fromTerm env a1) (fromTerm env a2)
            else App (fromTerm env h0) (fromTerm env (fst t))
        | T.Tv_FVar fv, [(a1, T.Q_Implicit); (a2, T.Q_Explicit)] ->
            let qn = T.inspect_fv fv in
                 if qn = T.forall_qn then mk_Forall env a1 a2
            else if qn = T.exists_qn then mk_Exists env a1 a2
            else App (fromTerm env h0) (fromTerm env (fst t))
        | T.Tv_FVar fv, [(a, T.Q_Explicit)] ->
            let qn = T.inspect_fv fv in
            if qn = ["Prims"; "op_Negation"] then Not (fromTerm env a)
            else if qn = T.not_qn then Not (fromTerm env a)
            else App (fromTerm env h0) (fromTerm env (fst t))
        | _ ->
            App (fromTerm env h0) (fromTerm env (fst t))
        end
    | T.Tv_Const (T.C_Int i) ->
        IntLit i
    | T.Tv_Const (T.C_True) -> True_
    | T.Tv_Const (T.C_False) -> False_
    // Recognizing projections, which are usually taking following form: 'match x with | qualified_name -> true | _ -> false'
    // which should get translated into 'Is qualified_name'
    | T.Tv_Match cond [T.Pat_Cons v _, tt; T.Pat_Wild _, tf] -> begin
        match T.inspect tt, T.inspect tf with
        | T.Tv_Const T.C_True, T.Tv_Const T.C_False -> Is v (targetFromTerm env cond)
        | _ -> UnknownTerm t
        end
    // If we don't recognize something, we just leave it for the user as raw term
    | _ -> UnknownTerm t

let rec refinement_formula_to_string (f : refinement_formula) : T.Tac string = 
    match f with
    | True_ -> "True_"
    | False_ -> "False_"
    | Comp (Eq mt) l r -> "Eq" ^
                        (match mt with
                         | None -> ""
                         | Some t -> " (" ^ T.term_to_string t ^ ")") ^
                        " (" ^ refinement_formula_to_string l ^ ") (" ^ refinement_formula_to_string r ^ ")"
    | Comp (BoolEq mt) l r -> "BoolEq" ^
                        (match mt with
                         | None -> ""
                         | Some t -> " (" ^ T.term_to_string t ^ ")") ^
                        " (" ^ refinement_formula_to_string l ^ ") (" ^ refinement_formula_to_string r ^ ")"
    | Comp Lt l r -> "Lt (" ^ refinement_formula_to_string l ^ ") (" ^ refinement_formula_to_string r ^ ")"
    | Comp Le l r -> "Le (" ^ refinement_formula_to_string l ^ ") (" ^ refinement_formula_to_string r ^ ")"
    | Comp Gt l r -> "Gt (" ^ refinement_formula_to_string l ^ ") (" ^ refinement_formula_to_string r ^ ")"
    | Comp Ge l r -> "Ge (" ^ refinement_formula_to_string l ^ ") (" ^ refinement_formula_to_string r ^ ")"
    | Is fv t -> P.sprintf "Is %s [%s]" (T.flatten_name (T.inspect_fv fv)) (target_to_string t)
    | And p q -> "And (" ^ refinement_formula_to_string p ^ ") (" ^ refinement_formula_to_string q ^ ")"
    | Or  p q ->  "Or (" ^ refinement_formula_to_string p ^ ") (" ^ refinement_formula_to_string q ^ ")"
    | Implies p q ->  "Implies (" ^ refinement_formula_to_string p ^ ") (" ^ refinement_formula_to_string q ^ ")"
    | Not p ->  "Not (" ^ refinement_formula_to_string p ^ ")"
    | Iff p q ->  "Iff (" ^ refinement_formula_to_string p ^ ") (" ^ refinement_formula_to_string q ^ ")"
    | Forall bs t -> P.sprintf "Forall <%s> (%s)" (T.bv_to_string bs) (refinement_formula_to_string t)
    | Exists bs t -> P.sprintf "Exists <%s> (%s)" (T.bv_to_string bs) (refinement_formula_to_string t)
    | App p q ->  "App (" ^ refinement_formula_to_string p ^ ") (" ^ refinement_formula_to_string q ^ ")"
    | Name bv ->  "Name (" ^ T.bv_to_string bv ^ ")"
    | FV fv -> P.sprintf "FV (%s)" (T.flatten_name (T.inspect_fv fv))
    | IntLit i -> "Int " ^ string_of_int i
    | UnknownTerm t -> P.sprintf "<%s>" (T.term_to_ast_string t)