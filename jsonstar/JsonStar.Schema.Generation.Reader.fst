/// This module provides a functionality to transform a Meta-F* term into our simplified view defined in JsonStar.Schema.Ast.typ
module JsonStar.Schema.Generation.Reader

// ..\packages\FStar.Windows.Ocaml.Unofficial.0.4.0\tools\bin\fstar.exe  --error_contexts true --use_hints --record_hints JsonStar.Schema.Generation.Reader.fst

module L = FStar.List.Tot
module AST = JsonStar.Schema.Ast.Types
module T = FStar.Tactics
module P = FStar.Printf

// TODO: 
// 1. Complete the below to the point where it's on-par with existing code
// 2. Rewrite JsonStar.Schema.Generation to use Reader module
// 3. Make sure schema generation tests are passing
// 4. Add support for dependencies and such here
// 5. Add tests for transforming terms into AST
// 6. Add schema generation support for dependencies, arrays etc
// 7. Fill in missing stuff needed to generate schema for types we need.

/// Qualified name
type qualified_name = l:list string{Cons? l}
val to_qualified_name: n:list string{Cons? n} -> Tot qualified_name
let to_qualified_name n = n

let lastT (n : list string) : T.Tac string = 
    match n with
    | [] -> T.fail "Can't get last element of an empty list"
    | ns -> L.last ns

/// Helpers
// Adds "Mk" to last segment of the qualified name
let rec record_constructor_name (n : list string{Cons? n}) : Tot (list string) = 
    match n with
    | [ x ] -> [ "Mk" ^ x ]
    | x :: xs -> x :: record_constructor_name xs

let isOption (fv : T.fv) : T.Tac bool = Helpers.fv_eq (T.inspect_fv fv) (T.explode_qn (`%option))

/// drop top-level "= true" from the refinement
let dropTopLevelEqTrue (t : T.term) : T.Tac T.term =
    match T.term_as_formula_total t with
    | T.Comp (T.Eq _) l _ -> l
    | _ -> t

// A helper functions for dropping arguments from DSL functions except the one which represents the
// restricted value (assumed to always be the first argument in DSL function).
// `dsl_fun should result in something like:
// Abs (Abs (App (App ref ref_val)))
// where ref is the implementation of our DSL function, and ref_val is the argument we are refining.
// We want to retain "App ref ref_val" and use it for comparisons as it's the biggest unchanging part 
// within the Tv_Refine we're getting as input. 
let dropExtraArgs (t : T.term) : T.Tac T.term =
    let rec dropArgsAux (t : T.term) (n : nat) : T.Tac T.term = 
        match T.inspect t with
        | T.Tv_Abs _ t1 -> dropArgsAux t1 (n + 1)
        | T.Tv_App t _ -> if n > 2 then dropArgsAux t (n-1) else t
        | _ -> T.fail ("Expected either Abs or App. Got " ^ (T.term_to_string t))
    in
    dropArgsAux t 0

// For DSL functions we need to first expand their name and then drop extra arguments as we only need to recognize 
// that a certain function was used in the refinement.
let dslToRef (t : T.term) : T.Tac T.term = dropExtraArgs (T.norm_term_env (T.cur_env()) [delta] t)

/// Enum helpers - perhaps move to a separate file
let pack_enum_constructor (name : qualified_name) : T.Tac T.term =
    T.pack (T.Tv_FVar (T.pack_fv name))

let unpack_enum_constructor (t : T.term) : T.Tac qualified_name =
    match T.inspect t with
    | T.Tv_FVar fv -> begin
        match T.inspect_fv fv with
        | [] -> T.fail (P.sprintf "Impossible - enum constructor name empty in term %s" (T.term_to_string t))
        | n -> to_qualified_name n
        end
    | _ -> T.fail (P.sprintf "Expected enum constructor name, got %s" (T.term_to_string t))

let get_enum_constructors (env : T.env) (fv : T.fv) : T.Tac (list qualified_name) = 
    let qname = T.inspect_fv fv in
    match T.lookup_typ env qname with
    | Some s -> begin
        // Check if sig in an enum definition
        match T.inspect_sigelt s with
        | T.Sg_Inductive _ _ _ _ cts -> begin
            T.map 
                (fun (name, _) -> 
                    match name with
                    | [] -> T.fail (P.sprintf "Impossible - empty constructor name in %s" (T.implode_qn qname))
                    | _ -> to_qualified_name name)
                cts
            end
        | _ -> T.fail (P.sprintf "Expected enum, got %s" (T.term_to_string (T.pack (T.Tv_FVar fv))))
        end
    | None -> T.fail (P.sprintf "Could not find type %s" (T.implode_qn qname))
/// Enum helpers end

/// Refinement helpers - perhaps move to a separate file
let intFromTerm (t : T.term) : T.Tac int = 
    match T.inspect t with
    | T.Tv_Const (T.C_Int x) -> x
    | _ -> T.fail (P.sprintf "Expected integer. Got %s" (T.term_to_string t))

let tryAndRefinementFromTerm (t : T.term) : T.Tac (option (T.term * T.term)) = 
    match T.inspect t with
    // second arg to 'and' is inside the arg to the application
    | T.Tv_App l (x2, _) -> begin
        match T.inspect l with
        | T.Tv_App op (x1, _) ->
            // TODO: Use Helpers.termeq consistently
            if op `T.term_eq` (`Prims.op_AmpAmp)
                then Some (x1, x2)
                else None
        | _ -> None
        end
    | _ -> None

let rec complexRefinementFromTerm (env : T.env) (refinementFromTerm : T.env -> T.term -> T.Tac AST.refinement_type) (t : T.term) : T.Tac AST.refinement_type =
    let t = dropTopLevelEqTrue t in
    // Check if this is a complex refinement
    match tryAndRefinementFromTerm t with
    | Some (t1, t2) -> begin
        let refs_left = complexRefinementFromTerm env refinementFromTerm t1 in
        let refs_right = complexRefinementFromTerm env refinementFromTerm t2 in
        AST.And refs_left refs_right
        end
    | None -> 
        // fallback to single refinement
        refinementFromTerm env t

// Extracts the operator
let numberRefinementFromTerm (env : T.env) (ref : T.term) : T.Tac AST.number_refinement =
    // Number refinements take two arguments. The first one is assumed to always be the refined number
    // and the second a value by which we restrict it.
    match T.inspect ref with
    | T.Tv_App op_with_arg (value, _) -> begin
        let v = intFromTerm value in
        // We need to drop the first argument now
        match T.inspect op_with_arg with
        | T.Tv_App op _ -> begin
            // Number refinements are very simple so there's no point in adding extra support for irreducible case
            if op `Helpers.termeq` (`Prims.op_GreaterThanOrEqual) then AST.Minimum v
            else if op `Helpers.termeq` (`Prims.op_GreaterThan) then AST.Minimum (v + 1)
            else if op `Helpers.termeq` (`Prims.op_LessThanOrEqual) then AST.Maximum v
            else if op `Helpers.termeq` (`Prims.op_LessThan) then AST.Maximum (v - 1)
            else T.fail (P.sprintf "Unrecognized operator: %s" (T.term_to_string op))
            end
        | _ -> T.fail (P.sprintf "Expected operator with argument. Got %s" (T.term_to_string op_with_arg))
        end
    | _ -> T.fail (P.sprintf "%s expected to be of form Tv_App op value where op describes refinement and value is the value that should be used." (T.term_to_string ref))

// NOTE: We can easily match against DSL, if we forbid F* from unfolding it. Unfortunately,
//       marking the DSL functions as "irreducible" or hiding them behind the interface has an 
//       unpleasant side effect. F* typechecker can't see the implementation anymore, so if 
//       the type of the function doesn't carry the same amount of information as the entire 
//       implementation, then proving the client code becomes less automatic. 
//       
//       For this reason we try to match against unfolded term representation. Unfortunately 
//       it might require adjustments each time DSL implementation changes. In cases, where it's 
//       not easy to match against unfolded representation, it's possible to mark a certain function 
//       as irreducible and carry on with recognizing the name as below (DSL section). But one needs to 
//       keep in mind it will become harder to use on the client side. 
let stringRefinementFromTerm (env : T.env) (ref : T.term) : T.Tac AST.string_refinement =
    // In the irreducible case "ref" comes as a function name and it's arguments. There's one 
    // Tv_App per argument, e.g. 
    // Tv_App (Tv_App (Tv_FVar JsonStar.Schema.Dsl.pattern, Tv_Var (s:Prims.string)), C_String YYYY-MM-DD)
    // String refinements take two arguments. The first one is assumed to always be the refined string
    // and the second a value by which we restrict it.
    match T.inspect ref with
    | T.Tv_App op_with_arg (value, _) -> begin
        match T.inspect op_with_arg with
        | T.Tv_App op_dsl _ -> begin
            // Try matching against DSL first (irreducible functions)
            if op_dsl `Helpers.termeq` (`JsonStar.Schema.Dsl.maxLength) then AST.MaxLength (T.unquote value)
            else if op_dsl `Helpers.termeq` (`JsonStar.Schema.Dsl.minLength) then AST.MinLength (T.unquote value)
            else if op_dsl `Helpers.termeq` (`JsonStar.Schema.Dsl.pattern) then AST.Pattern (T.unquote value)
            else begin
                if op_with_arg `Helpers.termeq` (dslToRef (`JsonStar.Schema.Dsl.maxLength)) then AST.MaxLength (T.unquote value)
                else if op_with_arg `Helpers.termeq` (dslToRef (`JsonStar.Schema.Dsl.minLength)) then AST.MinLength (T.unquote value)
                else if op_with_arg `Helpers.termeq` (dslToRef (`JsonStar.Schema.Dsl.pattern)) then AST.Pattern (T.unquote value)
                else T.fail (P.sprintf "Unrecognized string operator: %s" (T.term_to_string op_with_arg))
                end
            end
        | _ -> T.fail (P.sprintf "Expected operator with argument. Got %s" (T.term_to_string op_with_arg))
        end
    | _ -> T.fail (P.sprintf "%s expected to be of form Tv_App op value where op describes refinement and value is the value that should be used." (T.term_to_string ref))


let enumRefinementFromTerm (env : T.env) (t : T.term) : T.Tac AST.enum_refinement = 
    let t = dropTopLevelEqTrue t in
    // Check if this is a complex refinement
    match tryAndRefinementFromTerm t with
    | Some (t1, t2) -> T.fail "Only single refinement on enum is supported."
    | None -> begin
        // In the irreducible case "ref" comes as a function name and it's arguments. There's one 
        // Tv_App per argument, e.g. 
        // Tv_App (Tv_App (Tv_FVar JsonStar.Schema.Dsl.allow, Tv_Var (x:some_enum_type)), list_of_projectors)
        // Enum refinements take two arguments. The first one is assumed to always be the refined enum
        // and the second a list of constructors which we use to restrict it.

        // For enums we're interested only in figuring out that SchemaDsl.allow is used. 
        // With a refinement like: "x:enum_onetwothree{ SchemaDsl.allow x _}" where "_" is some list of allowed constructors we get
        // (Tv_App (Tv_FVar JsonStar.Schema.Dsl.allow, Tv_FVar JsonStar.Schema.Generation.Api.enum_onetwothree), Tv_Var (x:JsonStar.Schema.Generation.Api.enum_onetwothree))
        // and we need to extract "Tv_FVar JsonStar.Schema.Dsl.allow".
        // We can't match against "SchemaDsl.allow x" as in e.g. string refinements, as allow/disallow are implemented as visible recursive definitions 
        // and delta normalization doesn't have any effect, and normalizing with zeta replaces the function with it's implementation in terms of match statement.  
        match T.inspect t with
        | T.Tv_App op_with_arg (value, _) -> begin
            // 1. Extract constructors from the enum type
            // 2. Use value to get a list of selectors
            // 3. Evaluate constructors on a list of selectors - filter those for which some selector returns true
            // 4. Convert constructors to their names
            // 5. Return as value of the restriction

            match T.inspect op_with_arg with
            | T.Tv_App op_dsl _ -> begin
                // Drop extra App
                match T.inspect op_dsl with
                | T.Tv_App op (enum_typ, _) -> begin
                    let constructors = 
                        match T.inspect enum_typ with
                        | T.Tv_FVar fv -> begin
                            let ctrs = get_enum_constructors env fv in
                            T.map (fun ctr -> pack_enum_constructor ctr) ctrs
                            end
                        | _ -> T.fail (P.sprintf "Expected an enum type. Got %s" (T.term_to_string enum_typ))
                    in

                    let selected_constructors = 
                        let ty : Type = T.unquote enum_typ in
                        let filtered_constructor_term : list ty = 
                            T.filter_map (fun ctr -> Helpers.try_satisfy_any (T.unquote #(list (ty -> bool)) value) (T.unquote #ty ctr)) constructors in
                        let filtered_constructor_qn = T.map (fun t -> unpack_enum_constructor (quote t)) filtered_constructor_term in
                        T.map (fun n -> L.last n) filtered_constructor_qn
                    in

                    if op `Helpers.termeq` (`JsonStar.Schema.Dsl.allow) then AST.Allow selected_constructors
                    else if op `Helpers.termeq` (`JsonStar.Schema.Dsl.disallow) then AST.Disallow selected_constructors
                    else T.fail (P.sprintf "Unrecognized enum operator: %s" (T.term_to_string op_with_arg))
                    end
                | _ -> T.fail (P.sprintf "Expected Tv_App. Got: %s" (T.term_to_string op_dsl))
                end
            | _ -> T.fail (P.sprintf "Expected operator with argument. Got %s" (T.term_to_string op_with_arg))
            end
        | _ -> T.fail (P.sprintf "%s expected to be of form Tv_App op value where op describes refinement and value is the value that should be used." (T.term_to_string t))
        end
// Refinement helpers - end

/// Recognizers
let isEnum (env : T.env) (fv : T.fv) : T.Tac bool = 
    let qname = T.inspect_fv fv in
    match T.lookup_typ env qname with
    | Some s -> begin
        // Check if sig in an enum definition
        match T.inspect_sigelt s with
        | T.Sg_Let _ _ _ _ _ -> false
        | T.Sg_Inductive _ _ _ _ cts -> begin
            // check if all constructor types are simply the type of the enum
            Helpers.satisfy_all (fun (_, ct) -> Helpers.termeq (T.pack (T.Tv_FVar fv)) ct) cts
            end
        | T.Sg_Val _ _ _ -> false // TODO: How should interface declarations be handled?
        | T.Unk -> false // TODO: What's Unk? Should we fail here?
        end
    | None -> false

// Records are syntactic sugar over inductive types
let isRecord (env : T.env) (fv : T.fv) : T.Tac bool = 
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

/// OneOf represents a discriminated union where every constructor has at most one 
/// argument which is of record type
let isOneOf (env : T.env) (fv : T.fv) : T.Tac bool = 
    let hasNoArguments (ctr : T.ctor) : T.Tac bool = 
        let _, ct = ctr in
        Helpers.termeq (T.pack (T.Tv_FVar fv)) ct
    in
    let hasRecordArg (ctr : T.ctor) : T.Tac bool = 
        let _, ct = ctr in
        match T.inspect ct with
        // TODO: Check that comp is Tv_FVar with a DU type name (i.e. 
        //       there are no other arguments in the constructor)
        | T.Tv_Arrow binder comp -> begin
            let (b, _) = T.inspect_binder binder in
            match (let open FStar.Tactics in T.inspect (T.inspect_bv b).bv_sort) with
            | T.Tv_FVar fv -> isRecord env fv
            | _ -> false
            end
        | _ -> false
    in
    let qname = T.inspect_fv fv in
    match T.lookup_typ env qname with
    | Some s -> begin
        match T.inspect_sigelt s with
        | T.Sg_Inductive _ _ _ _ cts -> begin
            let recordOrEmptyArgs = Helpers.satisfy_all (fun ct -> 
                // NOTE: I had to write it this way as otherwise I'm getting:
                // '(Error 58) Expected a ghost expression (arguments to short circuiting operators must be pure or ghost); got an expression "hasRecordArg ct" with effect "Tac"'
                let x = hasRecordArg ct in
                let y = hasNoArguments ct in
                x || y) cts in
            let atLeastOneRecordArg = Helpers.satisfy_any (fun ctr -> hasRecordArg ctr) cts in
            recordOrEmptyArgs && atLeastOneRecordArg
            end
        | _ -> false
        end
    | _ -> false

/// Translation from term to AST
let primitives = [ "Prims.string"; "Prims.int"; "Prims.bool"; "FStar.Real.real"; ]
let primitiveFromTerm (t : T.term) : T.Tac AST.primitive = 
    if Helpers.termeq t (`string) then AST.String
    else if Helpers.termeq t (`int) then AST.Int
    else if Helpers.termeq t (`bool) then AST.Boolean
    //else if Helpers.termeq t (`real) then AST.Primitive AST.Real
    else T.fail (P.sprintf "%s is not a primitive type" (T.term_to_string t))

let enumFromTerm (env : T.env) (t : T.term) : T.Tac AST.enum = 
    let tv : T.term_view = T.inspect t in
    match tv with 
    | T.Tv_FVar fv -> begin
        // TODO: Read attributes
        let attrs = AST.Mkvalue_attributes None None None None in
        // TODO: Better syntax available when/if https://github.com/FStarLang/FStar/pull/2149 is merged
            // {
            //     AST._description  = None; 
            //     AST._defaultValue = None; 
            //     AST._readOnly     = None; 
            //     AST._format       = None; 
            // }
        let qual_ctrs = get_enum_constructors env fv in
        let ctrs_names = L.map L.last qual_ctrs in
        {
            AST.enum_values = ctrs_names;
            AST.enum_attributes = attrs;
        }
    end
    | _ -> T.fail (P.sprintf "Expected an enum, got %s" (T.term_to_string t))
    
let rec unpack_field_type (env : T.env) (t : T.term) : T.Tac (bool * AST.typ) = 
    match T.inspect t with
    | T.Tv_App x (arg,_) -> begin
        match T.inspect x with
        | T.Tv_FVar fv -> begin
            if isOption fv 
                // NOTE: In case of option, we drop the 'option' from the term before translating into ast
                then true, fromTerm env arg
                else false, fromTerm env t
            end
        | _ -> false, fromTerm env t
        end
    | _ -> false, fromTerm env t

and unpack_field (_env : T.env) (b : T.binder) : T.Tac AST.record_field = 
    let (bv, (_, attrs)) = T.inspect_binder b in
    let attr_opt = match attrs with | [] -> None | _ -> Some (FStar.List.Tot.hd attrs) in
    let bvv = T.inspect_bv bv in
    // Need to make fields of bv_view 
    let open FStar.Tactics in
    let isOptional, ft = unpack_field_type _env bvv.bv_sort in
    {    
        AST._name = bvv.bv_ppname; 
        AST._typ = ft;
        AST._isOptional = isOptional;
        // TODO: Read attributes from attr_opt
        AST._attributes = AST.Mkvalue_attributes None None None None;
    }

and unpack_fields (env : T.env) (qname : list string) (ty : T.term) : T.Tac (list AST.record_field) = 
    // type of the constructor should contain an Arrow type (there's at least one field in a record)
    match T.inspect_ln ty with
    | T.Tv_Arrow binder comp -> begin
        let f = unpack_field env binder in
        match T.inspect_comp comp with
        | T.C_Total ty2 _ -> f :: (unpack_fields env qname ty2)
        | _ -> T.fail "Unsupported computation type when unpacking record fields"
        end
    | T.Tv_FVar fv -> begin
        // The most inner part of 'ty' should be the name of the record type
        let qname2 = T.inspect_fv fv in
        if Helpers.fv_eq qname qname2
            then []
            else T.fail (P.sprintf "Expected %s, got %s" (T.implode_qn qname) (T.implode_qn qname2))
        end
    | _ -> T.fail "Expected an arrow type when unpacking record fields"
and recordFromTerm (env : T.env) (t : T.term) : T.Tac AST.record = 
    match T.inspect t with
    | T.Tv_FVar fv -> begin
        let qname = T.inspect_fv fv in
        match T.lookup_typ env qname with
        | Some s -> begin
            match T.inspect_sigelt s with
            | T.Sg_Inductive _ _ _ _ cts -> begin
                if List.Tot.length cts = 1
                    then 
                        let fields = unpack_fields env qname (snd (List.Tot.hd cts)) in
                        {
                            AST._fields = fields;
                            // TODO: Read attributes
                            AST._attributes = AST.Mkobject_attributes None;
                        }
                    else T.fail (P.sprintf "Expected record, got inductive with more than one constructor: %s" (T.term_to_string t))
                end
            | _ -> T.fail (P.sprintf "Expected inductive, got %s" (T.term_to_string (T.pack (T.Tv_FVar (T.pack_fv qname)))))
            end
        | None -> T.fail (P.sprintf "Could not find type %s" (T.implode_qn qname))
        end
    | _ -> T.fail (P.sprintf "Expected record, got %s" (T.term_to_string t))
    
and refinementTypeFromTerm (env : T.env) (baseType : AST.typ) (phi : T.term) : T.Tac (option AST.refinement_type) = 
    match baseType with
    | AST.Primitive AST.String -> Some (complexRefinementFromTerm env (fun env phi -> AST.StringRefinement (stringRefinementFromTerm env phi)) phi)
    | AST.Primitive AST.Int    -> Some (complexRefinementFromTerm env (fun env phi -> AST.NumberRefinement (numberRefinementFromTerm env phi)) phi)
    | AST.Enum _               -> Some (complexRefinementFromTerm env (fun env phi -> AST.EnumRefinement (enumRefinementFromTerm env phi)) phi)
    | AST.TypeDef td           -> refinementTypeFromTerm env (AST.Mktypedef?._base td) phi
    | AST.Refinement r         -> refinementTypeFromTerm env (AST.Mkrefinement?._base r) phi
    // TODO: We don't support refinements for all types yet
    | _                        -> None

and refinementFromTerm (env : T.env) (t : T.term) : T.Tac AST.refinement = 
    match T.inspect t with
    | T.Tv_Refine b phi -> begin
        let b = T.inspect_bv b in
        let baseTerm : T.term = let open FStar.Tactics in b.bv_sort in
        let baseTyp = fromTerm env baseTerm in
        let refinedTyp = refinementTypeFromTerm env baseTyp phi in
        match refinedTyp with
        | Some r -> { AST._base = baseTyp; AST._refinement = r; }
        | None -> T.fail (P.sprintf "Unsupported refinement %s for base type %s" (T.term_to_string phi) (T.term_to_string baseTerm)) //baseTyp
        end
    | _ -> T.fail (P.sprintf "Expected %s to be refinement" (T.term_to_string t))

and oneOfFromTerm (env : T.env) (t : T.term) : T.Tac AST.one_of =
    let tryGetRecordArg (typName : list string) (ctr : T.ctor) : T.Tac (option T.term) = 
        let name,ct = ctr in
        match T.inspect ct with
        | T.Tv_Arrow binder comp -> begin
            // We assume that the first (and only) argument in the constructor is 
            // a record
            let (b, _) = T.inspect_binder binder in
            let open FStar.Tactics in
            Some ((T.inspect_bv b).bv_sort)
            end
        | T.Tv_FVar fv -> begin
            if Helpers.fv_eq (T.inspect_fv fv) typName
                then None
                else T.fail (P.sprintf "Expected constructor %s to have exactly one record argument or no arguments at all. Got %s" (T.implode_qn name) (T.term_to_string ct)) 
            end
        | _ -> T.fail (P.sprintf "Expected constructor %s to have exactly one record argument or no arguments at all. Got %s" (T.implode_qn name) (T.term_to_string ct)) 
    in
    // 1. A DU type name makes an enum type name (fv)
    // 2. Unpack constructors
    // 3. A constructor name is used as a possible enum value in enum and 
    //    a value in dep_schema
    // 4. A record from the first binder is used as a subschema term
    match T.inspect t with
    | T.Tv_FVar fv -> begin
        let qname = (T.inspect_fv fv) in
        let enum_type = lastT qname in
        match T.lookup_typ env qname with
        | Some s -> begin
            match T.inspect_sigelt s with
            | T.Sg_Inductive _ _ _ _ cts -> begin
                let enum_values = T.map (fun (name, _) -> lastT name) cts in
                let deps = 
                    T.map 
                        (fun (name, ct) -> 
                            let ss = tryGetRecordArg qname (name, ct) in
                            let ssTyp = match ss with | Some ss -> Some (fromTerm env ss) | None -> None in
                            (lastT name, ssTyp)
                        ) cts
                in
                // TODO: Handle attributes
                let e = { AST.enum_values = enum_values; AST.enum_attributes = AST.Mkvalue_attributes None None None None; } in
                AST.ByEnum enum_type e deps
                end
            | _ -> T.fail (P.sprintf "Error in get_dependency. Expected inductive type, got %s" (T.term_to_string t))
            end
        | None -> T.fail (P.sprintf "Could not find type %s" (T.implode_qn qname))
        end
    | _ -> T.fail (P.sprintf "Error in get_dependency. Expected a type name, got %s" (T.term_to_string t))

and fromTerm (env : T.env) (t : T.term) : T.Tac AST.typ = 
    let t = Helpers.drop_synonym env t in
    let tv : T.term_view = T.inspect t in
    match tv with 
    | T.Tv_FVar fv -> begin
        if L.mem (T.fv_to_string fv) primitives then 
            AST.Primitive (primitiveFromTerm t)
        else if isEnum env fv then
            AST.Enum (enumFromTerm env t) 
        else if isRecord env fv then
           AST.Record (recordFromTerm env t) 
        else if isOneOf env fv then
            AST.OneOf (oneOfFromTerm env t)
        else T.fail (P.sprintf "Term %s is not recognized" (T.term_to_string t))
        end
    | T.Tv_Refine _ _ -> AST.Refinement (refinementFromTerm env t)
    | _               -> T.fail (P.sprintf "Term %s is not recognized" (T.term_to_string t))

