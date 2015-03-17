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

it_performs_multiplication_with_integers() {
    result=$(bin/lishp "(* 2 3)")
    test "$result" = "6"
}

it_performs_multiplication_with_floats() {
    result=$(bin/lishp "(* 3 1.5)")
    test "$result" = "4.5"
}

it_performs_division_with_integers() {
    result=$(bin/lishp "(/ 6 2)")
    test "$result" = "3"
}

it_performs_division_with_floats() {
    result=$(bin/lishp "(/ 1.0 2)")
    test "$result" = "0.5"
}
