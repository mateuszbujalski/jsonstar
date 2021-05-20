module JsonStar.Serialization.Demo4

// ..\packages\FStar.Windows.Ocaml.Unofficial.0.4.0\tools\bin\fstar.exe  --error_contexts true --use_hints --record_hints --include "../jsonstar" --codegen FSharp --extract "+JsonStar.Serialization.Demo4" JsonStar.Serialization.Demo4.fst

open JsonStar.Json
open FStar.Tactics
open FStar.Tactics.Typeclasses

module P = FStar.Printf
module LessInterestingHelpers = JsonStar.Schema.Generation.Reader
module Helpers = JsonStar.Schema.Generation.Helpers

type result (a : Type) = 
    | Error of string
    | Valid of a
    
let result_return (a : Type) (x : a) : Tot (result a) = Valid x

[@@ noextract_to "FSharp"]
type primitive = 
    | Int
    | String
    | Boolean

[@@ noextract_to "FSharp"]
noeq type constructor_argument = 
    | Primitive : (name:string) -> (ty:primitive) -> constructor_argument
    // TODO: Dependencies on other fields are not supported yet, I assume that refinement_fun has type unquote ty -> bool
    //       The plan is to extend this with a list of binders that represent expected arguments and them 
    //       match them with constructor arguments calculated earlier
    // TODO: We ignore nested refinements
    | Refined : (name:string) -> (pty:primitive) -> (rty:term) -> (refinement_fun:term) -> constructor_argument

[@@ noextract_to "FSharp"]
let constructor_argument_name (ctr_arg : constructor_argument) : Tot string = 
    match ctr_arg with
    | Primitive n _ -> n
    | Refined n _ _ _ -> n

[@@ noextract_to "FSharp"]
let is_record_type (e : env) (typ : term) : Tac bool = 
    match inspect typ with
    | Tv_FVar fv -> 
        // This is just more pattern matching on term representation to see if 
        // 'typ' represents a record type (which is a discriminated union with a single constructor called 'Mk{record_type_name}')
        LessInterestingHelpers.isRecord e fv
    | _ -> false

// Adds "Mk" to last segment of the qualified name
[@@ noextract_to "FSharp"]
let rec record_constructor_name (n : list string{Cons? n}) : Tot (list string) = 
    match n with
    | [ x ] -> [ "Mk" ^ x ]
    | x :: xs -> x :: record_constructor_name xs

[@@ noextract_to "FSharp"]
let typ_to_string (x : typ) : Tac (n : list string{Cons? n}) = 
    match inspect x with
    | Tv_FVar fv -> begin
        match inspect_fv fv with
        | [] -> fail "Type name can't be empty"
        | n -> n
        end
    | _ -> fail "Can't extract typename from typ"

[@@ noextract_to "FSharp"]
let call (f : term) (arg : term) : Tac term = pack (Tv_App f (arg, Q_Explicit))

[@@ noextract_to "FSharp"]
let drop_b2t (t : term) : Tac term = 
    match inspect t with
    | Tv_App f (arg, _) -> begin 
        if Helpers.termeq f (`Prims.b2t)
            then arg
            else t
        end
    | _ -> t

[@@ noextract_to "FSharp"]
let mk_function (b : binder) (body : term) = pack (Tv_Abs b body)

[@@ noextract_to "FSharp"]
let primitive_from_typ (ty : term) : Tac primitive = 
    match ty with
    | Tv_Refine b _ -> fail (P.sprintf "Nested refinements are not supported: '%s'" (term_to_ast_string ty))
    | Tv_FVar fv -> begin
        let lid = inspect_fv fv in
        if Helpers.fv_eq lid string_lid then String
        else if Helpers.fv_eq lid int_lid then Int
        else if Helpers.fv_eq lid bool_lid then Boolean
        else fail (P.sprintf "'%s' is not supported primitive type" (term_to_ast_string ty))
        end
    | _ -> fail (P.sprintf "'%s' is not a primitive type" (term_to_ast_string ty))

[@@ noextract_to "FSharp"]
let create_argument (e : env) (bi : binder) : Tac constructor_argument = 
    let (b, _) = inspect_binder bi in
    let bvv = inspect_bv b in
    let prop_name = bvv.bv_ppname in
    let prop_type = bvv.bv_sort in
    
    match inspect prop_type with
    | Tv_Refine b phi -> begin
        Refined prop_name (primitive_from_typ (type_of_bv b)) prop_type (mk_function (pack_binder b Q_Explicit []) (drop_b2t phi))
        end
    | Tv_FVar fv -> Primitive prop_name (primitive_from_typ prop_type)
    | _ -> fail (P.sprintf "Field '%s' has unsupported type '%s'" prop_name (term_to_ast_string prop_type))

[@@ noextract_to "FSharp"]
let rec create_arguments (e : env) (record_qname : list string) (ctr : term) : Tac (list constructor_argument) = 
    match inspect ctr with
    | Tv_Arrow bi ct -> begin
        let args = 
            match inspect_comp ct with
            | C_Total rt _ -> create_arguments e record_qname rt
            | _ -> fail (P.sprintf "Unsupported computation type while creating record constructor arguments for '%s'" (flatten_name record_qname))
        in
        let arg = create_argument e bi in
        arg :: args        
        end
    | Tv_FVar fv -> 
        // The last part of the record constructor type should be the record type name, i.e.:
        // Mkrecord_type_name : arg1 -> arg2 -> arg3 -> record_type_name
        let qname2 = inspect_fv fv in
        if Helpers.fv_eq record_qname qname2
            then []
            else fail (P.sprintf "Expected '%s' to be the last part of record constructor of '%s'" (flatten_name qname2) (flatten_name record_qname))
    | _ -> fail (P.sprintf "Expected record constructor of %s, got %s" (flatten_name record_qname) (term_to_ast_string ctr))

[@@ noextract_to "FSharp"]
let rec create_callable_ctr_f (e : env) (record_qname : list string{Cons? record_qname}) (ctr : term) : Tac term = 
    let (bs, c) = collect_arr_bs ctr in
    let ctr_name = 
        match inspect_comp c with
        | C_Total rt _ -> begin
            match inspect rt with
            | Tv_FVar fv -> begin
                // The last part of the record constructor type should be the record type name, i.e.:
                // Mkrecord_type_name : arg1 -> arg2 -> arg3 -> record_type_name
                let qname2 = inspect_fv fv in
                if Helpers.fv_eq record_qname qname2
                    then
                        let ctr_name = record_constructor_name record_qname in 
                        (pack (Tv_FVar (pack_fv ctr_name)))
                    else fail (P.sprintf "Expected '%s' to be the last part of record constructor of '%s'" (flatten_name qname2) (flatten_name record_qname))
                end
            | _ -> fail (P.sprintf "Expected the last part of record constructor to be '%s', got '%s'" (flatten_name record_qname) (term_to_ast_string rt))
            end
        | _ -> fail (P.sprintf "Unsupported computation type while creating callable constructor for '%s'" (flatten_name record_qname))
    in
    // We need to produce a function that looks like this:
    // (fun arg1 arg2 -> JsonStar.Serialization.Demo3.Mksample_record_2 arg1 arg2)
    // This means we need to produce abstractions in a different order than we're applying the constructor to arguments
    let calls = 
        fold_left (fun c bi ->
            let (b, _) = inspect_binder bi in
            call c (pack (Tv_Var b))) ctr_name bs
    in
    fold_left (fun t bi -> (mk_function bi t)) calls (List.Tot.rev bs)

[@@ noextract_to "FSharp"]
let get_record_arg (e : env) (json_arg : term) (ctr_arg : constructor_argument) : Tac term = 
    
    let arg_f = 
        match ctr_arg with
        | Primitive prop_name prop_type -> fail "Primitive types not supported yet"
        | Refined prop_name base_type ref_type refinement_fun -> begin
            let refinement_fun_string = term_to_string refinement_fun in
            let refinement_fun_string_term = quote refinement_fun_string in
            let prop_name_string_term = quote prop_name in
            match base_type with
            | Int -> begin
                let arg_f : term = //: option json -> result (unquote ref_type)
                    `(fun (prop_j : option json) ->
                        match prop_j with
                        | Some p -> begin
                            match p with
                            | JNumber num JInt -> begin
                                let num_v = JsonStar.Parser.int_of_string num in
                                let prop_ref : int -> bool = `#refinement_fun in
                                if prop_ref num_v 
                                    then Valid (num_v <: `#ref_type)
                                    else Error (P.sprintf "Invalid value for property '%s': '%d' does not satisfy '%s'" (`#prop_name_string_term) num_v (`#refinement_fun_string_term))
                                end
                            | _ -> Error (P.sprintf "Property '%s' has invalid type. Expected int." (`#prop_name_string_term))
                            end
                        | _ -> Error (P.sprintf "Property '%s' does not exist" (`#prop_name_string_term))
                        )
                in
                arg_f 
                end
            | String -> begin
                let arg_f : term = //: option json -> result (unquote ref_type)
                    `(fun (prop_j : option json) ->
                        match prop_j with
                        | Some p -> begin
                            match p with
                            | JString s -> begin
                                let prop_ref : string -> bool = `#refinement_fun in
                                if prop_ref s 
                                    then Valid (s <: `#ref_type)
                                    else Error (P.sprintf "Invalid value for property '%s': '%s' does not satisfy '%s'" (`#prop_name_string_term) s (`#refinement_fun_string_term))
                                end
                            | _ -> Error (P.sprintf "Property '%s' has invalid type. Expected string." (`#prop_name_string_term))
                            end
                        | _ -> Error (P.sprintf "Property '%s' does not exist" (`#prop_name_string_term))
                        )
                in
                arg_f 
                end
            | Boolean -> begin
                let arg_f : term = //: option json -> result (unquote ref_type)
                    `(fun (prop_j : option json) ->
                        match prop_j with
                        | Some p -> begin
                            match p with
                            | JBoolean s -> begin
                                let prop_ref : bool -> bool = `#refinement_fun in
                                if prop_ref s 
                                    then Valid (s <: `#ref_type)
                                    else Error (P.sprintf "Invalid value for property '%s': '%b' does not satisfy '%s'" (`#prop_name_string_term) s (`#refinement_fun_string_term))
                                end
                            | _ -> Error (P.sprintf "Property '%s' has invalid type. Expected boolean." (`#prop_name_string_term))
                            end
                        | _ -> Error (P.sprintf "Property '%s' does not exist" (`#prop_name_string_term))
                        )
                in
                arg_f 
                end
            | _ -> fail "Not implemented yet"
            end
    in
    let prop_name = constructor_argument_name ctr_arg in
    let prop_json_option_term = call (call (quote JsonStar.Json.getProp) (quote prop_name)) json_arg in
    call arg_f prop_json_option_term

[@@ noextract_to "FSharp"]
let apply_record_constructor (e : env) (json_arg : term) (ctr_callable : term) (arg_f : term) : Tac term = 
    `( 
        match `#arg_f with
        | Valid a -> begin 
            match `#ctr_callable with
            | Valid c -> Valid (c a)
            | Error msg -> Error msg
            end
        | Error msg -> Error msg
     )

[@@ noextract_to "FSharp"]
let create_deserializer_f (e : env) (t : Type) (json_arg_binder : binder) : Tac term = 
    let real_arg = pack (Tv_Var (bv_of_binder json_arg_binder)) in

    let qname = typ_to_string (quote t) in

    let s = lookup_typ e qname in
    match s with
    | Some s -> begin
        match inspect_sigelt s with
        | Sg_Inductive _ _ _ _ cts -> begin
            if List.Tot.length cts = 1
                then begin
                    // We can assume that cts is a single-element list as we've checked that with 'is_record_type'
                    let ctr = snd (List.Tot.hd cts) in
                    let ctr_callable_term = create_callable_ctr_f e qname ctr in
                    let ctr_result = `(result_return (`#ctr) (`#ctr_callable_term)) in

                    let ctr_args = create_arguments e qname ctr in
                    let record_args = map (fun ctr_arg -> get_record_arg e real_arg ctr_arg) ctr_args in
                    let ctr_applied = fold_left (fun c a -> apply_record_constructor e real_arg c a) ctr_result record_args in

                    let final = mk_function json_arg_binder ctr_applied in

                    print "Generated deserialization function:";
                    print (term_to_string final);

                    final
                    end
                else fail (P.sprintf "Expected record type '%s' to have a single constructor." (implode_qn qname))
            end
        | _ -> fail (P.sprintf "Expected record, got %s" (term_to_string (pack (Tv_FVar (pack_fv qname)))))
        end
    | None -> fail (P.sprintf "Could not find type %s" (implode_qn qname))

/////////// Function generation part end

// This has to produce a function: (json -> result t)
// Note the usage of 'fail' for typechecking time errors (when we're dealing with bad usage or unsupported kinds of typ_term as the 
// structure of the type to deserialize is known during typechecking)
[@@ noextract_to "FSharp"]
let deserialize_impl (t : Type) (typ_term : term) : Tac unit = 
    let e = cur_env() in
    match inspect (cur_goal ()) with
    | Tv_Arrow b c -> begin
        if is_record_type e typ_term
            then begin
                let deserializer_f = create_deserializer_f e t b in
                exact deserializer_f
                end
            else fail "We only support deserialization of a record, sorry!"
        end
    | _ -> fail "Invalid usage of deserialize_impl. It should be used to generate a function: 'json -> result t'"


class serializable a = {
    //serialize_gen: a -> result json; 
    deserialize_gen: (j:json{JObject? j}) -> result a
}

//let serialize   #a {| serializable a |} v = serialize_gen   #a v
let deserialize #a {| serializable a |} v = deserialize_gen #a v

/// Example record type that we would want to deserialize //
type sample_record_1 =                                                  //
    {                                                     //
        l : x:int{x >= 5};                                //
    }                                                     //
////////////////////////////////////////////////////////////

type sample_record_2 = 
    {
        _x : (x:int{x >= 5});
        _y : (y:string{String.length y <= 10});
        _z : (z:bool{z = true});
    }

// NOTE1: 
//     We can't inspect type at runtime with F*, so we need some boilerplate to 
//     have a nice, generic serialization function. For each type we need to define an instance of 
//     a type class.
// NOTE2: 
//     Unfortunately we can't simply pass json argument to 'deserialize_impl' and pattern match on it. 
//     We actually need to produce a function type with Meta-F* that takes a json argument and does something with it. 
instance sample_record_1_serializable: serializable sample_record_1 = {
   deserialize_gen = synth_by_tactic (fun () -> deserialize_impl sample_record_1 (`sample_record_1));
}

instance sample_record_2_serializable: serializable sample_record_2 = {
   deserialize_gen = synth_by_tactic (fun () -> deserialize_impl sample_record_2 (`sample_record_2));
} 

// We could simplify it a little and generate these instances with a line similar to the following:
// %splice[] (generate_serializable (`r))
// where generate_serializable is a Meta-F* function that realizes the type class instance. 
// Still it's an extra line per type to make it json-serializable. 
// The good part is that we should be able to add these instances on top of library types as long 
// as their definition is visible/accessible. 

let process_sample_record_1_from_json (j : json{JObject? j}) : Tot (result (x:int{x >= 5})) = 
    let r_deserialized : result sample_record_1 = deserialize j in
    match r_deserialized with
    | Valid rv -> Valid rv.l
    | Error msg -> Error msg

let process_sample_record_2_from_json (j : json{JObject? j}) : Tot (result (x:string{String.length x <= 10})) = 
    let r_deserialized : result sample_record_2 = deserialize j in
    match r_deserialized with
    | Valid rv -> Valid rv._y
    | Error msg -> Error msg

let test_sample_record_1 () =
    FStar.IO.print_string "Write json(sample_record_1):\n";
    let json_str = FStar.IO.input_line () in
    let json = JsonStar.Parser.parse json_str in

    if JObject? json
        then begin
            let l_r = process_sample_record_1_from_json json in
            match l_r with
            | Valid l -> FStar.IO.print_string (P.sprintf "Value of l field is: %d\n" l)
            | Error msg -> FStar.IO.print_string (P.sprintf "Value of l field is not available due to: %s\n" msg)
            end
        else FStar.IO.print_string "Provided json does not represent an object\n"

let test_sample_record_2 () = 
    FStar.IO.print_string "Write json (sample_record_2):\n";
    let json_str2 = FStar.IO.input_line () in
    let json2 = JsonStar.Parser.parse json_str2 in
    if JObject? json2
        then begin
            let y_r = process_sample_record_2_from_json json2 in
            match y_r with
            | Valid y -> FStar.IO.print_string (P.sprintf "Value of y field is: %s\n" y)
            | Error msg -> FStar.IO.print_string (P.sprintf "Value of y field is not available due to: %s\n" msg)
            end
        else FStar.IO.print_string "Provided json does not represent an object\n"

let main = 
    test_sample_record_1 ();
    test_sample_record_2 ()