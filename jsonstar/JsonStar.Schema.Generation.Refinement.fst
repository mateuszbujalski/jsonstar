module JsonStar.Schema.Generation.Refinement

module T = FStar.Tactics
module L = FStar.List.Tot

open JsonStar.Schema

// Split refinement over "and"

// Some common refinements recognizers? Like >=, >, <, <=?
// TODO: I think we have two kinds of refinements:
//       1. Defined as a bool predicate
//       2. Defined as a "Type" predicate
//       We should recognize and unpack both. At that point we should split by "and" and look for known pieces of
//       code. 

type number_refinement = 
    | Minimum : v:int -> number_refinement
    | Maximum : v:int -> number_refinement

type string_refinement = 
    | MinLength : v:nat -> string_refinement
    | MaxLength : v:nat -> string_refinement
    | Pattern : v:string -> string_refinement

type enum_refinement = 
    | Allow : v:list string -> enum_refinement
    | Disallow : v:list string -> enum_refinement

type complex_refinement = 
    // Tv_App (op_And x1, x2)
    | And : x1:T.term -> x2:T.term -> complex_refinement

let intFromTerm (t : T.term) : T.Tac int = 
    match T.inspect t with
    | T.Tv_Const (T.C_Int x) -> x
    | _ -> Helpers.tfail ("Expected integer. Got " ^ (T.term_to_string t) ^ ".\n")

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

// Extracts the operator
let numberRefinementFromTerm (ref : T.term) : T.Tac number_refinement =
    // Number refinements take two arguments. The first one is assumed to always be the refined number
    // and the second a value by which we restrict it.
    match T.inspect ref with
    | T.Tv_App op_with_arg (value, _) -> begin
        let v = intFromTerm value in
        // We need to drop the first argument now
        match T.inspect op_with_arg with
        | T.Tv_App op _ -> begin
            //T.print "NumberRef";
            //Helpers.printAst op;
            //Helpers.printAst (`Prims.op_GreaterThanOrEqual);

            // Number refinements are very simple so there's no point in adding extra support for irreducible case
            if op `Helpers.termeq` (`Prims.op_GreaterThanOrEqual) then Minimum v
            else if op `Helpers.termeq` (`Prims.op_GreaterThan) then Minimum (v + 1)
            else if op `Helpers.termeq` (`Prims.op_LessThanOrEqual) then Maximum v
            else if op `Helpers.termeq` (`Prims.op_LessThan) then Maximum (v - 1)
            else Helpers.tfail ("Unrecognized operator: " ^ (T.term_to_string op) ^ ".\n")
            end
        | _ -> Helpers.tfail ("Expected operator with argument. Got " ^ (T.term_to_string op_with_arg) ^ ".\n")
        end
    | _ -> Helpers.tfail ((T.term_to_string ref) ^ " expected to be of form Tv_App op value where op describes refinement and value is the value that should be used.")

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
let stringRefinementFromTerm (ref : T.term) : T.Tac string_refinement =
    // In the irreducible case "ref" comes as a function name and it's arguments. There's one 
    // Tv_App per argument, e.g. 
    // Tv_App (Tv_App (Tv_FVar JsonStar.Schema.Dsl.pattern, Tv_Var (s:Prims.string)), C_String YYYY-MM-DD)
    // String refinements take two arguments. The first one is assumed to always be the refined string
    // and the second a value by which we restrict it.
    match T.inspect ref with
    | T.Tv_App op_with_arg (value, _) -> begin
        match T.inspect op_with_arg with
        | T.Tv_App op_dsl _ -> begin
            //T.print "stringop_irreducible";
            //Helpers.printAst op_dsl;
            //Helpers.printAst (`JsonStar.Schema.Dsl.maxLength);
            //Helpers.printAst (`JsonStar.Schema.Dsl.minLength);
            //Helpers.printAst (`JsonStar.Schema.Dsl.pattern);

            // Try matching against DSL first (irreducible functions)
            if op_dsl `Helpers.termeq` (`JsonStar.Schema.Dsl.maxLength) then MaxLength (T.unquote value)
            else if op_dsl `Helpers.termeq` (`JsonStar.Schema.Dsl.minLength) then MinLength (T.unquote value)
            else if op_dsl `Helpers.termeq` (`JsonStar.Schema.Dsl.pattern) then Pattern (T.unquote value)
            else begin
                //T.print "stringop";
                //Helpers.printAst op_with_arg;
                //Helpers.printAst (dslToRef (`JsonStar.Schema.Dsl.maxLength));
                //Helpers.printAst (dslToRef (`JsonStar.Schema.Dsl.minLength));
                ////Helpers.printAst (dslToRef (`JsonStar.Schema.Dsl.pattern)); //pattern is irreducible currently, so this line would fail

                if op_with_arg `Helpers.termeq` (dslToRef (`JsonStar.Schema.Dsl.maxLength)) then MaxLength (T.unquote value)
                else if op_with_arg `Helpers.termeq` (dslToRef (`JsonStar.Schema.Dsl.minLength)) then MinLength (T.unquote value)
                else if op_with_arg `Helpers.termeq` (dslToRef (`JsonStar.Schema.Dsl.pattern)) then Pattern (T.unquote value)
                else Helpers.tfail ("Unrecognized string operator: " ^ (T.term_to_string op_with_arg) ^ ".\n")
                end
            end
        end
    | _ -> Helpers.tfail ((T.term_to_string ref) ^ " expected to be of form Tv_App op value where op describes refinement and value is the value that should be used.")

let tryComplexFromTerm (t : T.term) : T.Tac (option complex_refinement) = 
    match T.inspect t with
    // second arg to 'and' is inside the arg to the application
    | T.Tv_App l (x2, _) -> begin
        match T.inspect l with
        | T.Tv_App op (x1, _) ->
            // TODO: Use Helpers.termeq consistently
            if op `T.term_eq` (`Prims.op_AmpAmp)
                then Some (And x1 x2)
                else None
        | _ -> None
        end
    | _ -> None

let dropTopLevelEqTrue (t : T.term) : T.Tac T.term =
    // drop top-level "= true" from the refinement
    match T.term_as_formula_total t with
    | T.Comp (T.Eq _) l _ -> l
    | _ -> t

let rec refinementsFromTerm (refinementFromTerm : T.term -> T.Tac 'a) (t : T.term) : T.Tac (list 'a) =
    //Helpers.printAst t;
    let t = (dropTopLevelEqTrue t) in
    // Check if this is a complex refinement
    match tryComplexFromTerm t with
    | Some (And t1 t2) -> begin
        //T.print "ByAnd";
        //Helpers.printAst t1;
        //Helpers.printAst t2;
        let refs_left = refinementsFromTerm refinementFromTerm t1 in
        let refs_right = refinementsFromTerm refinementFromTerm t2 in
        let refs = List.Tot.append refs_left refs_right in
        refs
        end
    | None -> 
        // fallback to single refinement
        [ refinementFromTerm t ]

let numberRefinementsFromTerm (t : T.term) : T.Tac (list number_refinement) =
    refinementsFromTerm numberRefinementFromTerm t

let stringRefinementsFromTerm (t : T.term) : T.Tac (list string_refinement) = 
    refinementsFromTerm stringRefinementFromTerm t
    
let enumRefinementFromTerm (t : T.term) : T.Tac enum_refinement = 
    let t = (dropTopLevelEqTrue t) in
    // Check if this is a complex refinement
    match tryComplexFromTerm t with
    | Some (And t1 t2) -> Helpers.tfail ("Only single refinement on enum is supported.")
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
                            let ctrs = JsonStar.Schema.Generation.Enum.get_enum_constructors fv in
                            T.map (fun ctr -> Enum.pack_enum_constructor ctr) ctrs
                            end
                        | _ -> T.fail ("Expected an enum type. Got " ^ (T.term_to_ast_string enum_typ))
                    in

                    let selected_constructors = 
                        let ty : Type = T.unquote enum_typ in
                        let filtered_constructor_term : list ty = 
                            T.filter_map (fun ctr -> Helpers.try_satisfy_any (T.unquote #(list (ty -> bool)) value) (T.unquote #ty ctr)) constructors in
                        let filtered_constructor_qn = T.map (fun t -> Enum.unpack_enum_constructor (quote t)) filtered_constructor_term in
                        T.map (fun n -> L.last n) filtered_constructor_qn
                    in

                    if op `Helpers.termeq` (`JsonStar.Schema.Dsl.allow) then Allow selected_constructors
                    else if op `Helpers.termeq` (`JsonStar.Schema.Dsl.disallow) then Disallow selected_constructors
                    else Helpers.tfail ("Unrecognized enum operator: " ^ (T.term_to_string op_with_arg))
                    end
                | _ -> Helpers.tfail ("Expected Tv_App. Got: " ^ (T.term_to_ast_string op_dsl))
                end
            end
        | _ -> Helpers.tfail ((T.term_to_string t) ^ " expected to be of form Tv_App op value where op describes refinement and value is the value that should be used.")
        end
    