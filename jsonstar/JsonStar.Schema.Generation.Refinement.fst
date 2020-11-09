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
        if op `T.term_eq` (`Prims.op_GreaterThanOrEqual) then Minimum v
        else if op `T.term_eq` (`Prims.op_GreaterThan) then Minimum (v + 1)
        else if op `T.term_eq` (`Prims.op_LessThanOrEqual) then Maximum v
        else if op `T.term_eq` (`Prims.op_LessThan) then Maximum (v - 1)
        else Helpers.tfail ("Unrecognized operator: " ^ (T.term_to_string op) ^ ".\n")
        end
    | _ -> Helpers.tfail ("Expected operator. Got " ^ (T.term_to_string ref) ^ ".\n")

let rec dropAbs (t : T.term) : T.Tac T.term = 
    match T.inspect t with
    | T.Tv_Abs _ t1 -> dropAbs t1
    | _ -> t

// TODO: How to recognize a dsl with a name after we've expanded names (dropped abbreviations)?
//       The function maxLength is defined differently as standalone term from when it's used within refinement? 
//       It's expressed as lambda. Alternatively we can just pattern match on it's expected term-shape (multi level) ...
//       
//       IDEA: Try hiding DSL behind the interface (or hide the implementation from Meta-F* in some other way), so that it's not able to 
//       expand the names too much.
let stringRefinementFromTerm (ref : T.term) (value : T.term) : T.Tac string_refinement =
    //T.print "stringRefinementFromTerm";
    //Helpers.printAst ref;
    //Helpers.printAst (dropAbs (T.norm_term [] (Helpers.drop_synonym (T.top_env()) (`(JsonStar.Schema.Dsl.maxLength)))));
    //if ref `T.term_eq` (`(JsonStar.Schema.Dsl.maxLength)) then Helpers.tfail "MaxLength !!!"
    //else Helpers.tfail "MaxLength not recognized !!!"
    match T.inspect ref with
    | T.Tv_App op _ -> begin
        if op `T.term_eq` (`JsonStar.Schema.Dsl.maxLength) then MaxLength (T.unquote value)
        else if op `T.term_eq` (`JsonStar.Schema.Dsl.minLength) then MinLength (T.unquote value)
        else if op `T.term_eq` (`JsonStar.Schema.Dsl.pattern) then Pattern (T.unquote value)
        else Helpers.tfail ("Unrecognized string operator: " ^ (T.term_to_string op) ^ ".\n")
        end
    | _ -> Helpers.tfail ("Expected string restriction. Got " ^ (T.term_to_string ref) ^ ".\n")

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
    
