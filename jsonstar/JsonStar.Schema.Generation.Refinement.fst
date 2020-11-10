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

type complex_refinement = 
    // Tv_App (op_And x1, x2)
    | And : x1:T.term -> x2:T.term -> complex_refinement

let intFromTerm (t : T.term) : T.Tac int = 
    match T.inspect t with
    | T.Tv_Const (T.C_Int x) -> x
    | _ -> Helpers.tfail ("Expected integer. Got " ^ (T.term_to_string t) ^ ".\n")

// Extracts the operator
let numberRefinementFromTerm (ref : T.term) (value : T.term) : T.Tac number_refinement =
    let v = intFromTerm value in
    match T.inspect ref with
    | T.Tv_App op _ -> begin
        // TODO: Be consistent and use Helpers.termeq instead of T.term_eq everywhere
        if op `T.term_eq` (`Prims.op_GreaterThanOrEqual) then Minimum v
        else if op `T.term_eq` (`Prims.op_GreaterThan) then Minimum (v + 1)
        else if op `T.term_eq` (`Prims.op_LessThanOrEqual) then Maximum v
        else if op `T.term_eq` (`Prims.op_LessThan) then Maximum (v - 1)
        else Helpers.tfail ("Unrecognized operator: " ^ (T.term_to_string op) ^ ".\n")
        end
    | _ -> Helpers.tfail ("Expected operator. Got " ^ (T.term_to_string ref) ^ ".\n")

// We need to extract the refinement from the term representation of a DSL function. 
// This means dropping the abstractions (function arguments) and a top-level application
let rec dslToRef (t : T.term) : T.Tac T.term = 
    match T.inspect t with
    | T.Tv_Abs _ t1 -> dslToRef t1
    | T.Tv_App t _ -> t
    | _ -> t

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
let stringRefinementFromTerm (ref : T.term) (value : T.term) : T.Tac string_refinement =
    //T.print "stringref";
    //Helpers.printAst ref;
    //Helpers.printAst (dslToRef (Helpers.drop_synonym (T.top_env()) (`(JsonStar.Schema.Dsl.maxLength))));
    if ref `Helpers.termeq` (dslToRef (Helpers.drop_synonym (T.top_env()) (`(JsonStar.Schema.Dsl.maxLength))))
        then MaxLength (T.unquote value)
    else if ref `Helpers.termeq` (dslToRef (Helpers.drop_synonym (T.top_env()) (`(JsonStar.Schema.Dsl.minLength))))
        then MinLength (T.unquote value)
    else if ref `Helpers.termeq` (dslToRef (Helpers.drop_synonym (T.top_env()) (`(JsonStar.Schema.Dsl.pattern))))
        then Pattern (T.unquote value)
    else begin
        // try matching DSL
        // TODO: Be consistent and use Helpers.termeq instead of T.term_eq everywhere
        match T.inspect ref with
        | T.Tv_App op _ -> begin
            if op `T.term_eq` (`JsonStar.Schema.Dsl.maxLength) then MaxLength (T.unquote value)
            else if op `T.term_eq` (`JsonStar.Schema.Dsl.minLength) then MinLength (T.unquote value)
            else if op `T.term_eq` (`JsonStar.Schema.Dsl.pattern) then Pattern (T.unquote value)
            else Helpers.tfail ("Unrecognized string operator: " ^ (T.term_to_string op) ^ ".\n")
            end
        | _ -> Helpers.tfail ("Expected string restriction. Got " ^ (T.term_to_string ref) ^ ".\n")
    end

let tryComplexFromTerm (t : T.term) : T.Tac (option complex_refinement) = 
    match T.inspect t with
    // second arg to 'and' is inside the arg to the application
    | T.Tv_App l (x2, _) -> begin
        match T.inspect l with
        | T.Tv_App op (x1, _) ->
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

let rec refinementsFromTerm (refinementFromTerm : T.term -> T.term -> T.Tac 'a) (t : T.term) : T.Tac (list 'a) =
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
    | None -> begin 
        // fallback to single refinement
        match T.inspect t with
        | T.Tv_App op value -> begin
            let ref = refinementFromTerm op (fst value) in
            [ ref ]
            end
        | _ -> Helpers.tfail ((T.term_to_string t) ^ " is not supported refinement.\n")
        end

let rec numberRefinementsFromTerm (t : T.term) : T.Tac (list number_refinement) =
    refinementsFromTerm numberRefinementFromTerm t

let rec stringRefinementsFromTerm (t : T.term) : T.Tac (list string_refinement) = 
    refinementsFromTerm stringRefinementFromTerm t
    
