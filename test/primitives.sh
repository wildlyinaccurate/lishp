describe "Primitives"

it_performs_addition() {
    result=$(bin/lishp "(+ 1 3 9)")
    test "$result" = "13"
}

it_performs_subtraction() {
    result=$(bin/lishp "(- 9 3 1)")
    test "$result" = "5"
}

it_performs_subtraction_below_zero() {
    result=$(bin/lishp "(- 1 3 9)")
    test "$result" = "-11"
}

it_performs_multiplication() {
    result=$(bin/lishp "(* 2 3)")
    test "$result" = "6"
}

it_performs_division() {
    result=$(bin/lishp "(/ 1 2)")
    test "$result" = "0.5"
}
