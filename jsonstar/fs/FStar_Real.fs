module FStar_Real

open System.Numerics

type real = decimal

let ( + )  (x:real) (y:real) = x + y
let ( - )  (x:real) (y:real) = x - y
let ( * )  (x:real) (y:real) = x * y

let sqrt_2 (x : real) = System.Math.sqrt(x)