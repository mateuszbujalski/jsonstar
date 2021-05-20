module JsonStar.Serialization.Demo3

open JsonStar.Json
open FStar.Tactics
open FStar.Tactics.Typeclasses

module P = FStar.Printf
module LessInterestingHelpers = JsonStar.Schema.Generation.Reader
module Helpers = JsonStar.Schema.Generation.Helpers

type result (a : Type) = 
    | Error of string
    | Valid of a
    
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
let create_deserializer_f (e : env) (t : Type) (json_arg_binder : binder) : Tac term = 
    let real_arg = pack (Tv_Var (bv_of_binder json_arg_binder)) in

    let qname = typ_to_string (quote t) in
    let ctr_name = record_constructor_name qname in

    let s = lookup_typ e qname in
    match s with
    | Some s -> begin
        match inspect_sigelt s with
        | Sg_Inductive _ _ _ _ cts -> begin
            if List.Tot.length cts = 1
                then begin
                    // We can assume that cts is a single-element list as we've checked that with 'is_record_type'
                    let ctr = snd (List.Tot.hd cts) in

                    match inspect ctr with
                    | Tv_Arrow bi _ -> begin
                        // TODO: Just a single field for now
                        let (b, _) = inspect_binder bi in
                        let bvv = inspect_bv b in
                        let prop_name = bvv.bv_ppname in
                        let prop_type = bvv.bv_sort in
                        let prop_refinement_fun = 
                            match inspect prop_type with
                            | Tv_Refine b phi -> begin
                                mk_function (pack_binder b Q_Explicit []) (drop_b2t phi)
                                end
                            | _ -> fail "Unsupported prop-type"
                        in

                        let prop_json_option_term = call (call (quote JsonStar.Json.getProp) (quote prop_name)) real_arg in 

                        let ctr_callable_term = (mk_function bi (call (pack (Tv_FVar (pack_fv ctr_name))) (pack (Tv_Var b)))) in

                        let match_json = 
                            `(fun (prop_j:option json) ->
                                match prop_j with
                                | Some p -> begin
                                    match p with
                                    | JNumber num JInt -> begin 
                                        let num_v = JsonStar.Parser.int_of_string num in
                                        let prop_ref : int -> bool = `#prop_refinement_fun in
                                        if prop_ref num_v 
                                            then Valid ((`#ctr_callable_term) num_v)
                                            else Error "BBB"
                                        end
                                    | JString s -> Error (P.sprintf "Value string: %s" s)
                                    | JBoolean b -> Error (P.sprintf "Value bool: %b" b)
                                    | _ -> Error "Unsupported value"
                                    end
                                | _ -> Error (P.sprintf "Unsupported value type")
                            )
                        in
                        let body = call match_json prop_json_option_term in
                        let final = mk_function json_arg_binder body in

                        final
                       end
                    | _ -> fail "Not implemented yet"
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
type sample_record_1 =                                    //
    {                                                     //
        l : x:int{x >= 5};                                //
    }                                                     //
////////////////////////////////////////////////////////////

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

// We could simplify it a little and generate these instances with a line similar to the following:
// %splice[] (generate_serializable (`r))
// where generate_serializable is a Meta-F* function that realizes the type class instance. 
// Still it's an extra line per type to make it json-serializable. 
// The good part is that we should be able to add these instances on top of library types as long 
// as their definition is visible/accessible. 

    
let process_r_from_json (j : json{JObject? j}) : Tot (result (x:int{x >= 5})) = 
    let r_deserialized : result sample_record_1 = deserialize j in
    match r_deserialized with
    | Valid rv -> Valid rv.l
    | Error msg -> Error msg

//let main = 
//    FStar.IO.print_string "Write json (sample_record_1):\n";
//    let json_str = FStar.IO.input_line () in
//    let json = JsonStar.Parser.parse json_str in
//    if JObject? json
//        then begin
//            let l_r = process_r_from_json json in
//            match l_r with
//            | Valid l -> FStar.IO.print_string (P.sprintf "Value of l field is: %d\n" l)
//            | Error msg -> FStar.IO.print_string (P.sprintf "Value of l field is not available due to: %s\n" msg)
//            end
//        else FStar.IO.print_string "Provided json does not represent an object\n"
    