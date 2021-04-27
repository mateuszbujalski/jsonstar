module JsonStar.Schema.Generation.Formula.Dsl

(*
    Recognizers for formulas representing DSL expressions
*)

module T = FStar.Tactics
module P = FStar.Printf
module L = FStar.List.Tot
module F = JsonStar.Schema.Generation.Formula

open JsonStar.Schema.Generation.Types

// NOTE: Replaced Option.mapTot with a locally defined function as it seemed to cause Tactic to get stuck 
//       for an unknown reason. 
// TODO: Try to produce a minimal repro
let option_map (f : 'a -> 'b) (x : option 'a) : Tot (option 'b) =
    match x with
    | Some a -> Some (f a)
    | None -> None

type dsl_formula = 
    | EnumRequired : list (qualified_name * F.target) -> dsl_formula
    | EnumForbidden : list (qualified_name * F.target) -> dsl_formula
    | NotSupported : F.refinement_formula -> dsl_formula

#push-options "--warn_error -242"
let tryEnumRequired (f : F.refinement_formula) : Tot (option dsl_formula) =
    let rec aux f = 
        match f with
        | F.Or (F.Is n t) r -> begin
            match aux r with
            | Some tt -> Some ((T.inspect_fv n, t) :: tt)
            | None -> None
            end
        | F.False_ -> Some []
        | _ -> None
    in option_map EnumRequired (aux f)
#pop-options

#push-options "--warn_error -242"
let tryEnumForbidden (f : F.refinement_formula) : Tot (option dsl_formula) = 
    let rec aux f = 
        match f with
        | F.And (F.Not (F.Is n t)) r -> begin 
            match aux r with 
            | Some tt -> Some ((T.inspect_fv n, t) :: tt)
            | None -> None
            end
        | F.True_ -> Some []
        | _ -> None
    in option_map EnumForbidden (aux f)
#pop-options

let dsl_formulas = 
    [
        tryEnumRequired;
        tryEnumForbidden;
    ]
    
#push-options "--warn_error -290"
let rec findFormula (fs : list (F.refinement_formula -> option dsl_formula)) (x : F.refinement_formula) : Tot dsl_formula = 
    match fs with
    | f :: t -> begin
        match f x with
        | Some r -> r
        | None -> findFormula t x
        end 
    | [] -> NotSupported x
#pop-options

let fromFormula (f : F.refinement_formula) : Tot dsl_formula =
    findFormula dsl_formulas f

// TODO: F* seems to have some problem getting the below to work. It fails with 'Tactic got stuck'. 
//       Might also be related to options, as it seemed to have issues recognizing Some / None when e.g. 
//       using Option.mapTot inside this module. 
// let rec tryApply (fs : list ('a -> option 'b)) (x : 'a) : Tot (option 'b) = 
//     match fs with
//     | f :: t -> begin
//         match f x with
//         | Some r -> r
//         | None -> tryApply t x
//         end 
//     | [] -> None

// let fromFormula (f : F.refinement_formula) : Tot dsl_formula =
    // match tryApply dsl_formulas f with
    // | Some x -> x
    // | None -> NotSupported f
    