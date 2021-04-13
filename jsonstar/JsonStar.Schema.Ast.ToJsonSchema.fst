module JsonStar.Schema.Ast.ToJsonSchema

module L = FStar.List.Tot
module AST = JsonStar.Schema.Ast.Types
module P = FStar.Printf

open JsonStar.Schema

let min_opt (x : option number) (y : option number) : option number = 
    match x, y with
    | Some (Int x), Some (Int y) -> Some (Int (FStar.Math.Lib.min x y))
    | Some x, None -> Some x
    | None, Some y -> Some y
    | None, None -> None

let max_opt (x : option number) (y : option number) : option number = 
    match x, y with
    | Some (Int x), Some (Int y) -> Some (Int (FStar.Math.Lib.max x y))
    | Some x, None -> Some x
    | None, Some y -> Some y
    | None, None -> None

let with_default (#a:Type) (def : a) (x : option a) : z:option a{Some? z} = 
    match x with
    | Some a -> Some a
    | None -> Some def

let rec refineString (x : AST.refinement_type) (opt : string_options) : Tot string_options = 
    match x with
    | AST.StringRefinement r -> begin
        match r with
            | AST.MaxLength v -> { opt with maxLength = with_default v (Option.mapTot (fun x -> JsonStar.Math.min_nat x v) opt.maxLength) }
            | AST.MinLength v -> { opt with minLength = with_default v (Option.mapTot (fun x -> JsonStar.Math.max_nat x v) opt.minLength) }
            | AST.Pattern s -> { opt with pattern = Some s} // TODO: Combine patterns for nested refinements?
        end
    | AST.And lr rr -> refineString lr (refineString rr opt)
    // NOTE: The rest of refinement types is not applicable to strings so we ignore them. 
    //       Ideally, we would make them unrepresentable, but I don't know how to define 
    //       AST.typ in a way that allows that as is still practical to use. 
    | _ -> opt

let rec refineNumber (x : AST.refinement_type) (opt : number_options) : Tot number_options = 
    match x with
    | AST.NumberRefinement r -> begin
        match r with
            | AST.Maximum v -> { opt with maximum = min_opt opt.maximum (Some (Int v)) }
            | AST.Minimum v -> { opt with minimum = max_opt opt.minimum (Some (Int v)) }
        end
    | AST.And lr rr -> refineNumber lr (refineNumber rr opt)
    | _ -> opt

let rec refineEnum (x : AST.refinement_type) (vs : list string) : Tot (list string) = 
    match x with
    | AST.EnumRefinement sr -> begin
        match sr with
            | AST.Allow values -> L.filter (fun x -> L.mem x values) vs
            | AST.Disallow values -> L.filter (fun x -> not (L.mem x values)) vs
        end
    | AST.And lr rr -> refineEnum lr (refineEnum rr vs)
    | _ -> vs

let primitiveToJsonSchema (x : AST.primitive) : Tot schema = 
    let _typ = 
        match x with
        | AST.Int -> Number (Mknumber_options None None)
        | AST.String -> String (mkempty_string_options ())
        | AST.Boolean -> Boolean
    in
    {
        _id         = None;
        _schema     = None;
        _type       = _typ;
        description = None;
        title       = None;
        _default    = None;
        readonly    = None;
        format      = None;
        definitions = [];
    }

let enumToJsonSchema (x : AST.enum) : Tot schema = 
    // TODO: Translate attributes into schema properties
    let values = (let open AST in x.enum_values) in
    {
        _id         = None;
        _schema     = None;
        _type       = Enum values;
        description = None;
        title       = None;
        _default    = None;
        readonly    = None;
        format      = None;
        definitions = [];
    }

let rec refineObjectEnumField (x : schema) (p : list AST.path_segment) (r : AST.enum_refinement) : Tot schema =
    match p with
    | [] -> begin
        // Make sure we deal with enum schema
        // Return updated schema
        match x._type with
        | Enum vs -> { x with _type = Enum (refineEnum (AST.EnumRefinement r) vs); }
        | _ -> x
        end
    | AST.Variant c :: pp -> begin
        // Make sure x is an object schema with a subschema for 'c'
        // Recurse into refining subschema for 'c'
        match x._type with
        | Object p d opt -> begin
            // TODO: Can we extend the AST.Variant to include the enum-field name
            //       used for dependency? Or can we somehow figure it out?  
            let new_dep = 
                JsonStar.Utils.list_map
                    d
                    (fun (e, deps) ->
                        e, (JsonStar.Utils.list_map
                            deps
                            (fun (case, dep_schema) ->
                                if c = case
                                    then (case, refineObjectEnumField dep_schema pp r)
                                    else (case, dep_schema))))
            in
            { x with _type = Object p new_dep opt }
            end
        | _ -> x
        end
    | AST.RecordField f tn :: pp -> begin
        // Make sure x is an object schema with property 'f'
        match x._type with
        | Object p d opt -> begin
            let new_props = 
                JsonStar.Utils.list_map 
                    p
                    (fun (prop, prop_s) -> 
                            // tn = prop to handle special case where autogenerated enum field from OneOfDU uses the name of the DU type
                            if f = prop || tn = prop
                                // Recurse into refining schema for 'f'
                                then (prop, refineObjectEnumField prop_s pp r)
                                else (prop, prop_s)) 
            in
            { x with _type = Object new_props d opt }
            end
        | _ -> x
        end

let refineObject (x : schema{Object? x._type}) (ref : AST.record_refinement) : Tot schema = 
    match ref with
    | AST.EnumField p er -> refineObjectEnumField x p er
    
let rec refinementToJsonSchema (x : AST.refinement) : Tot schema = 
    let baseTyp = (let open AST in x._base) in
    let baseSchema = toJsonSchema baseTyp in
    let ref = (let open AST in x._refinement) in
    // NOTE: Ideally we'd change 'AST.refinement' to only allow certain refinement types
    //       depending on the refined type. Unfortunately this is not possible with F* 
    //       at least currently. 
    match baseSchema._type with
    | String opt -> { baseSchema with _type = String (refineString ref opt); }
    | Integer -> baseSchema
    | Number opt -> { baseSchema with _type = Number (refineNumber ref opt); }
    | Enum vs -> { baseSchema with _type = Enum (refineEnum ref vs); }
    | Boolean -> baseSchema
    | Object p d opt -> begin
        // TODO: Better error handling / better design of refinement_type.
        //       For now simply ignore refinements not applicable to object schema
        match ref with
        | AST.RecordRefinement rr -> refineObject baseSchema rr
        | _ -> baseSchema
        end
    // TODO: Apply other refinements
    | _ -> baseSchema
    
and typedefToJsonSchema (x : AST.typedef) : Tot schema = 
    let baseTyp = AST.Mktypedef?._base x in
    let attrs = AST.Mktypedef?._attributes x in
    let baseSchema = toJsonSchema baseTyp in
    {
        baseSchema with
            description = AST.Mkvalue_attributes?._description attrs;
            _default    = AST.Mkvalue_attributes?._defaultValue attrs;
            readonly    = AST.Mkvalue_attributes?._readOnly attrs;
            format      = Option.mapTot AST.format_type_to_string (AST.Mkvalue_attributes?._format attrs);
    }

and recordToJsonSchema (r : AST.record) : Tot schema = 
    let props = 
        JsonStar.Utils.list_map 
            (AST.Mkrecord?._fields r)
            (fun rf ->
                match AST.Mkrecord_field?._typ rf with
                // NOTE: OneOf record field is a special case where we generate an 
                //       enum property and elsewhere a subschema is being put into deps
                | AST.OneOf o -> begin
                    match o with
                    | AST.ByEnum n e _ -> n, (enumToJsonSchema e)
                    end
                | field_typ -> begin
                    // TODO: Support attributes
                    (AST.Mkrecord_field?._name rf), (toJsonSchema field_typ)
                    end
            ) 
    in
    let deps = 
        JsonStar.Utils.list_choose
            (AST.Mkrecord?._fields r)
            (fun rf ->
                match AST.Mkrecord_field?._typ rf with
                | AST.OneOf o -> begin
                    match o with
                    | AST.ByEnum n e st -> begin
                        let ds = 
                            JsonStar.Utils.list_choose
                                st
                                (fun (v, so) -> 
                                    match so with
                                    // TODO: Instead of flattening the subschema, we'd like to wrap it as a field with 
                                    //       object schema. The field name should be equal to the field name of DU field in a record.
                                    //       This should make (de)serialization easier.
                                    | Some s -> Some (v, toJsonSchema s)
                                    | None -> None
                                )
                        in Some (n, ds)
                        end
                    end
                | _ -> None)
    in
    let req =  
        JsonStar.Utils.list_choose 
            (AST.Mkrecord?._fields r)
            (fun rf ->
                match AST.Mkrecord_field?._typ rf with
                // NOTE: OneOf record field is a special case where we generate an 
                //       enum property and it's always marked as required
                | AST.OneOf (AST.ByEnum n _ _) -> Some n
                | _ -> 
                    if (AST.Mkrecord_field?._isOptional rf)
                        then None
                        else Some (AST.Mkrecord_field?._name rf)
            )
    in
    {
        _id         = None;
        _schema     = None;
        _type       = Object props deps ({ required = Some req; additionalProperties = None; });
        description = None;
        title       = None;
        _default    = None;
        readonly    = None;
        format      = None;
        definitions = [];
    }

and toJsonSchema (x : AST.typ) : Tot schema = 
    match x with
    | AST.Primitive p -> primitiveToJsonSchema p
    | AST.Refinement r -> refinementToJsonSchema r
    | AST.TypeDef td -> typedefToJsonSchema td 
    | AST.Enum e -> enumToJsonSchema e
    | AST.Record r -> recordToJsonSchema r
    | AST.OneOf _ -> mkSchemaEmpty Integer // TODO: This should not be allowed by construction
    | AST.Array _ -> mkSchemaEmpty Integer // TODO: Add support for arrays