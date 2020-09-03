module JsonStar.Utils

val pipe_rTot : (x : 'a) -> (f : ('a -> Tot 'b)) -> Tot 'b
let pipe_rTot x f = f x
let op_Bar_Bar_Greater = pipe_rTot