module JsonStar.Schema

open JsonStar.Json

// TODO: Recognize regex patterns valid in json-schema according to:
//       https://json-schema.org/understanding-json-schema/reference/regular_expressions.html
let isPattern (x : string) = true
type regex = x:string{isPattern x}

// { type : string }
type string_options = 
    {
        minLength : option nat;
        maxLength : option nat;
        pattern   : option regex;
        // Extension: expect content to be equal to some other
        // $type : "reference", $data : "json-path to some other prop"
        reference : option string;
        // NOTE: I ignore format, uri-template, json-pointer or regex for now at least
    }
let mkempty_string_options () = Mkstring_options None None None None

//// type : string, enum : [ ... ] }
//type enum_options =
//    {
//        // TODO: Should it be part of 'schema' instead?
//        default : option string;
//    }

// { type : number }
type number_options = 
    {
        // NOTE: we represent all numbers with string. 
        // TODO: Think about replacing it with DU to distinguish between various flavors of numbers 
        minimum : option string;
        maximum : option string;
    }

// { type : object }
type object_options = 
    {
        required : option (list string);
        // NOTE: false if not set
        additionalProperties : option bool; // NOTE: Only boolean supported for now at least
    }

// { type : array }
type array_options = 
    {
        minItems : option nat;
        maxItems : option nat;
        uniqueItems : option bool;
    }

type schema_type = 
    | String : options:string_options -> schema_type
    | Enum : enum:list string (*-> options:enum_options*) -> schema_type
    // { type : integer }
    | Integer
    | Number : options:number_options -> schema_type
    // { type : boolean }
    | Boolean
    | Object : props:list (string * schema) -> deps:list (string * schema) -> options:object_options -> schema_type
    // Just list validation, no support for tuples
    | Array : items:schema -> options:array_options -> schema_type
    | Reference : ref:string -> schema_type
    // { type : object }
    | OneOf : items:list schema -> schema_type
    // Let's ignore AllOf, AnyOf, Not for now

and schema =
    {
        _id : option string; // uri
        _schema : option string; // uri
        _type : schema_type;
        description : option string;
        title : option string;
        _default : option string;
        // any named type will go into definitions, otherwise it should be inlined
        definitions : list (string * schema);
        // TODO: Should this be part of common properties of the schema or rather part of Object?
        //dependencies : list (string * schema);
    }

/// Transform json-schema into json
val toJson : schema -> Tot json

// To extract just this module use:
// ..\external\fstar\tools\bin\fstar.exe --warn_error -271 --odir obj\Debug\extracted --codegen FSharp --use_hints --record_hints JsonStar.Schema.fst --extract "+JsonStar.Schema"