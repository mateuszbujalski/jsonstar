module SchemaDslTests

open Types

open JsonStar.Schema.Dsl

// positive tests
let nat2_p_ex1 : nat2 = 5
let minimum3_p_ex1 : minimum3 = 3
let maximum8_p_ex1 : maximum8 = 8

let min5max9_p_ex1 : min5max9 = 5
let min5max9_p_ex2 : min5max9 = 9



// negative tests
[@@(expect_failure [19])]
let nat2_n_ex1 : nat2 = -1

[@@(expect_failure [19])]
let minimum3_n_ex1 : minimum3 = 2

[@@(expect_failure [19])]
let maximum8_n_ex1 : maximum8 = 9

[@@(expect_failure [19])]
let min5max9_n_ex1 : min5max9 = 4
[@@(expect_failure [19])]
let min5max9_n_ex2 : min5max9 = 10