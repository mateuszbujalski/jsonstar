/// This module provides a simplified view of F* terms we want to translate into json-schema
module JsonStar.Schema.Ast.Types

// ..\packages\FStar.Windows.Ocaml.Unofficial.0.4.0\tools\bin\fstar.exe  --error_contexts true --odir obj\Debug\extracted --codegen FSharp --use_hints --record_hints JsonStar.Schema.Ast.Types.fst --extract "+JsonStar.Schema.Ast.Types"

let description (desc : string) : Tot unit = ()
let defaultValue (v : string) : Tot unit = ()
let readOnly : unit = ()
/// Provided value has to be known at compile-time
let readOnlyIf (v : bool) : Tot unit = ()

type format_type = 
    | Percentage
    | Date

let format_type_to_string (x : format_type) = 
    match x with
    | Percentage -> "percentage"
    | Date       -> "date"

let format (f : format_type) : Tot unit = ()

/// Attributes that can be associated with anything that represents a field in json-schema
type value_attributes = 
    {
        _description  : option string;
        _defaultValue : option string;
        _readOnly     : option bool;
        _format       : option format_type;
    }

/// Attributes that can be associated with anything that represents an object in json-schema
type object_attributes =
    {
        _description : option string;
        // Id, schemaVersion, title, AllowAdditionalProperties ... (?)
    }

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

type refinement_type =
    | NumberRefinement : number_refinement -> refinement_type
    | StringRefinement : string_refinement -> refinement_type
    | EnumRefinement   : enum_refinement   -> refinement_type
    // Some fields are restricted by (or calculated from) other fields
    | Formula          : string            -> refinement_type
    | And : refinement_type -> refinement_type -> refinement_type

/// Primitive types
type primitive = 
    | Int :  primitive
    //| Real : primitive
    | String : primitive
    | Boolean : primitive

type enum = 
    {
        enum_values : list string;
        enum_attributes : value_attributes;
    }

/// A single case discriminated union with a single field where the type name is equal to the case name
/// NOTE: A common workaround to assign some metadata (via attributes) to a type abbreviation
type typedef = 
    {
        _base : typ;
        _attributes : value_attributes;
    }

and refinement = 
    {
        _base : typ;
        _refinement : refinement_type;
    }

and array_type = 
    | List : typ -> array_type
    | Tuple : list typ -> array_type

and array = 
    {
        // Refinements (like Min/Max items) included in the element_type
        element_type : array_type;
        _unique_items : option bool; // attributes?
    }

/// OneOf type is a discriminated union where each case has at most one field of type record
// NOTE: An idea, a post processing step, detecting a refinement that restricts one_of to a particular case results in a flattening?
and one_of = 
    /// An extra enum field where its values are used to select a dependent subschema
    | ByEnum : string -> enum -> list (string * option typ) -> one_of
    // Add support for requiredWhen and similar

and record_field = 
    {
        _name : string;
        _typ : typ;
        _isOptional : bool;
        _attributes : value_attributes; //?
    }

and record =
    {
        _fields : list record_field;
        _attributes : object_attributes;
    }

and typ = 
    | Primitive : primitive -> typ
    | Refinement : refinement -> typ
    | TypeDef : typedef -> typ
    | OneOf : one_of -> typ
    | Array : array -> typ
    | Enum : enum -> typ
    | Record : record -> typ


