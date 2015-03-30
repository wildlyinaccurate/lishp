describe 'Primitives'

it_performs_addition() {
    test $($LISHP '(+ 1 3 9)') = '13'
    test $($LISHP '(+ -1 3)') = '2'
    test $($LISHP '(+ 1.5 3)') = '4.5'
}

it_performs_subtraction() {
    test $($LISHP '(- 9 3 1)') = '5'
    test $($LISHP '(- -5 2)') = '-7'
    test $($LISHP '(- 3 1.5)') = '1.5'
    test $($LISHP '(- 1 3 9)') = '-11'
}

it_performs_multiplication() {
    test $($LISHP '(* 2 3)') = '6'
    test $($LISHP '(* 3 1.5)') = '4.5'
}

it_performs_division() {
    test $($LISHP '(/ 8 2 2)') = '2'
    test $($LISHP '(/ 1.0 2)') = '0.5'
    test "$($LISHP '(/ 5 0)')" = 'Division by zero!'
}

it_performs_modulus() {
    test $($LISHP '(mod 4 3)') = '1'
 }

it_calculates_division_remainder() {
    test $($LISHP '(remainder 5 2)') = '1'
 }
