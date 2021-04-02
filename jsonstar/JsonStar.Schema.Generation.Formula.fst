module JsonStar.Schema.Generation.Formula

(*
    We need a way to reason about complicated refinements that drive the schema generation. 
    Based on FStar.Reflection.Formula from F* standard library.
*)

module T = FStar.Tactics
module P = FStar.Printf

noeq type comparison =
  | Eq     of option T.typ  (* Propositional equality (eq2), maybe annotated *)
  | BoolEq of option T.typ  (* Decidable, boolean equality (eq), maybe annotated *)
  | Lt | Le | Gt | Ge     (* Orderings, at type `int` (and subtypes) *)

noeq type refinement_formula =
  | True_  : refinement_formula
  | False_ : refinement_formula
  | Comp   : comparison -> refinement_formula -> refinement_formula -> refinement_formula
  | Is     : T.fv (* Name of recognized value *) -> T.term (* A projection, e.g. 'Cons?' *) -> refinement_formula
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
        | T.Tv_Const T.C_True, T.Tv_Const T.C_False -> Is v t
        | _ -> UnknownTerm t
        end
    // If we don't recognize something, we just leave it for the user as raw term
    | _ -> 
        UnknownTerm t

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
    | Is fv _ -> P.sprintf "Is %s" (T.flatten_name (T.inspect_fv fv))
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