module JsonStar.Schema.Generation.Refinement

module T = FStar.Tactics
module L = FStar.List.Tot

open JsonStar.Schema

// Refinement recognizer
let isRefinement (tv : T.term_view) : Tot bool = 
    match tv with
    | T.Tv_Refine _ _ -> true
    | _           -> false

// Split refinement over "and"

// Some common refinements recognizers? Like >=, >, <, <=?
// TODO: I think we have two kinds of refinements:
//       1. Defined as a bool predicate
//       2. Defined as a "Type" predicate
//       We should recognize and unpack both. At that point we should split by "and" and look for known pieces of
//       code. 

type refinement = 
    | Minimum : v:int -> refinement
    | Maximum : v:int -> refinement

// nat
//Tv_Refine 
//  (
//    (i:Prims.int), 
//    Tv_App 
//      (
//        Tv_App 
//          (
//            Tv_App 
//              (
//                Tv_FVar Prims.eq2, 
//                Tv_FVar Prims.bool
//              ), 
//                Tv_App (Tv_App (Tv_FVar Prims.op_GreaterThanOrEqual, Tv_Var (i:Prims.int)), C_Int 0)
//          ), C_True
//      )
//  )