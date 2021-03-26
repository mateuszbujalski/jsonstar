module Test

open FStar.Real
let x = 1.0R +. 3.14R +. sqrt_2
let _ = assert (sqrt_2 *. sqrt_2 == 2.0R)
