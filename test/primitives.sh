describe "Primitives"

it_performs_addition() {
    result=$($LISHP "(+ 1 3 9)")
    test "$result" = "13"
}

it_performs_addition_with_negative_numbers() {
    result=$($LISHP "(+ -1 3)")
    test "$result" = "2"
}

it_performs_subtraction() {
    result=$($LISHP "(- 9 3 1)")
    test "$result" = "5"
}

it_performs_subtraction_with_negative_numbers() {
    result=$($LISHP "(- -5 2)")
    test "$result" = "-7"
}

it_performs_subtraction_below_zero() {
    result=$($LISHP "(- 1 3 9)")
    test "$result" = "-11"
}

it_performs_multiplication_with_integers() {
    result=$($LISHP "(* 2 3)")
    test "$result" = "6"
}

it_performs_multiplication_with_floats() {
    result=$($LISHP "(* 3 1.5)")
    test "$result" = "4.5"
}

it_performs_division_with_integers() {
    result=$($LISHP "(/ 6 2)")
    test "$result" = "3"
}

it_performs_division_with_floats() {
    result=$($LISHP "(/ 1.0 2)")
    test "$result" = "0.5"
}
