module JsonStar.Parser
open FStar.All
open JsonStar.Json

/// Attempts to parse a provided json. Throws an exception if the json is incorrect. 
/// NOTE: This is currently implemented with unverified F# code under the cover.
val parse : string -> ML json

val int_of_string : string -> Tot int