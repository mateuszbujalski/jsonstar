module JsonStar.Schema.Generation.Dependencies

module T = FStar.Tactics
module L = FStar.List.Tot

// TODO: Support refinements on DUs that represent dependencies
//       E.g. restrict the DU to a certain case with some extra 
//       constraints on the record argument

type enum = 
    {
        name : string; // name of the enum type
        fields : list string; // possible enum values
    }

type dep_subschema = 
    {
        value : string; // enum value that activates a subschema
        subschema : T.term; // term representation of the subschema
    }

type dependency = 
    | OneOf : enum:enum -> list dep_subschema -> dependency

let isOneOfDU (t : T.term) : T.Tac bool = 
    let hasRecordArg (ctr : T.ctor) : T.Tac bool = 
        let _,ct = ctr in
        match T.inspect ct with
        // TODO: Check that comp is Tv_FVar with a DU type name (i.e. 
        //       there are no other arguments in the constructor)
        | T.Tv_Arrow binder comp -> begin
            let (b, _) = T.inspect_binder binder in
            let open FStar.Tactics in
            match T.inspect (T.inspect_bv b).bv_sort with
            | T.Tv_FVar fv -> JsonStar.Schema.Generation.Recognizers.isRecord fv
            | _ -> false
            end
        | _ -> false
    in
    match T.inspect t with
    | T.Tv_FVar fv -> begin
        let env = T.cur_env () in
        let qname = T.inspect_fv fv in
        match T.lookup_typ env qname with
        | Some s -> begin
            match T.inspect_sigelt s with
            | T.Sg_Inductive _ _ _ _ cts -> begin
                // Constructors should have an argument that is a record or have 
                // no arguments at all.  
                // There has to be at least one constructor with an argument. 
                
                // TODO: Allow constructoors without arguments
                Helpers.satisfy_all (fun ctr -> hasRecordArg ctr) cts
                end
            | _ -> false
            end
        | None -> false
        end
    | _ -> false

// Assumption: "isOneOfDU t" is true, but we can't use this as refinement as it's not 
// a total function 
let get_dependency (env : T.env) (t : T.term(*{isOneOfDU t}*)) : T.Tac dependency = 
    let getRecordArg (ctr : T.ctor) : T.Tac T.term = 
        let name,ct = ctr in
        match T.inspect ct with
        | T.Tv_Arrow binder comp -> begin
            // We assume that the first (and only) argument in the constructor is 
            // a record
            let (b, _) = T.inspect_binder binder in
            let open FStar.Tactics in
            (T.inspect_bv b).bv_sort 
            end
        | _ -> T.fail ("Error in get_dependency. Expected constructor " ^ (T.implode_qn name) ^ " to have exactly one argument. Got " ^ (T.term_to_string ct)) 
    in
    // 1. A DU type name makes an enum type name (fv)
    // 2. Unpack constructors
    // 3. A constructor name is used as a possible enum value in enum and 
    //    a value in dep_schema
    // 4. A record from the first binder is used as a subschema term
    match T.inspect t with
    | T.Tv_FVar fv -> begin
        let qname = (T.inspect_fv fv) in
        let enum_type = L.last qname in
        match T.lookup_typ env qname with
        | Some s -> begin
            match T.inspect_sigelt s with
            | T.Sg_Inductive _ _ _ _ cts -> begin
                let enum_values = T.map (fun (name, _) -> L.last name) cts in
                let deps = 
                    T.map 
                        (fun (name, ct) -> 
                            let ss = getRecordArg (name, ct) in
                            { value = L.last name; subschema = ss }
                        ) cts
                in
                OneOf ({ name = enum_type; fields = enum_values; }) deps
                end
            | _ -> T.fail ("Error in get_dependency. Expected inductive type, got " ^ (T.term_to_string t))
            end
        | None -> T.fail ("Could not find type " ^ (T.implode_qn qname))
        end
    | _ -> T.fail ("Error in get_dependency. Expected a type name, got " ^ (T.term_to_string t))