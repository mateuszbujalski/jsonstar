module JsonStar.Math

(* Function: maximum value *)
val max_nat: x:nat -> y:nat -> Tot (z:nat{ (x >= y ==> z = x) /\ (x < y ==> z = y) })
let max_nat x y = if x >= y then x else y

(* Function: minimum value *)
val min_nat: x:nat -> y:nat -> Tot (z:nat{ (x >= y ==> z = y) /\ (x < y ==> z = x) })
let min_nat x y = if x >= y then y else x

// open FStar.Real
// let x = 1.0R +. 3.14R +. sqrt_2
// let _ = assert (sqrt_2 *. sqrt_2 == 2.0R)
