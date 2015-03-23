describe "Primitives"

it_performs_addition() {
    result=$($LISHP "(+ 1 3 9)")
    test "$result" = "13"
}

it_performs_addition_with_negative_numbers() {
    result=$($LISHP "(+ -1 3)")
    test "$result" = "2"
}

it_performs_addition_with_floats() {
    result=$($LISHP "(+ 1.5 3)")
    test "$result" = "4.5"
}

it_performs_subtraction() {
    result=$($LISHP "(- 9 3 1)")
    test "$result" = "5"
}

it_performs_subtraction_with_negative_numbers() {
    result=$($LISHP "(- -5 2)")
    test "$result" = "-7"
}

it_performs_subtraction_with_floats() {
    result=$($LISHP "(- 3 1.5)")
    test "$result" = "1.5"
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
    result=$($LISHP "(/ 8 2 2)")
    test "$result" = "2"
}

it_performs_division_with_floats() {
    result=$($LISHP "(/ 1.0 2)")
    test "$result" = "0.5"
}

it_throws_an_error_on_divide_by_zero() {
    result=$($LISHP "(/ 5 0)")
    test "$result" = "Division by zero!"
}

it_performs_modulus() {
    result=$($LISHP "(mod 4 3)")
    test "$result" = "1"
 }

it_calculates_division_remainder() {
    result=$($LISHP "(remainder 5 2)")
    test "$result" = "1"
 }
