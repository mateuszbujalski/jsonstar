module JsonStar.Utils

val op_Bar_Bar_GreaterThan : (f : ('a -> Tot 'b)) -> (x : 'a) -> Tot 'b
let op_Bar_Bar_GreaterThan f x = f x