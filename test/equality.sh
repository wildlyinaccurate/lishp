it_can_check_multiple_values() {
    test "$($LISHP '(= 2)')" = '#t'
    test "$($LISHP '(= 2 2 2 2)')" = '#t'
}

it_checks_numeric_equality() {
    test "$($LISHP '(= 2 2)')" = '#t'
    test "$($LISHP '(= 2.0 2)')" = '#t'

    test "$($LISHP '(= 2 1)')" = '#f'
    test "$($LISHP '(= 2.0 1)')" = '#f'
}

it_checks_boolean_equality() {
    test "$($LISHP '(= #t #t)')" = '#t'
    test "$($LISHP '(= #f #f)')" = '#t'
    test "$($LISHP '(= #f #t)')" = '#f'
}

it_checks_string_equality() {
    test "$($LISHP '(= "foo" "foo")')" = '#t'
    test "$($LISHP '(= "foo" "bar")')" = '#f'
}

it_compares_with_less_than() {
    test "$($LISHP '(< 2 5)')" = '#t'
    test "$($LISHP '(< 2.0 5)')" = '#t'

    test "$($LISHP '(< 5 2)')" = '#f'
    test "$($LISHP '(< 5.0 2)')" = '#f'
}

it_compares_with_less_than_or_equal() {
    test "$($LISHP '(<= 5 5)')" = '#t'
    test "$($LISHP '(<= 4.0 5)')" = '#t'

    test "$($LISHP '(<= 5 2)')" = '#f'
    test "$($LISHP '(<= 5.0 2)')" = '#f'
}

it_compares_with_greater_than() {
    test "$($LISHP '(> 5 2)')" = '#t'
    test "$($LISHP '(> 5 2.0)')" = '#t'

    test "$($LISHP '(> 2 5)')" = '#f'
    test "$($LISHP '(> 2 5.0)')" = '#f'
}

it_compares_with_greater_than_or_equal() {
    test "$($LISHP '(>= 5 5)')" = '#t'
    test "$($LISHP '(>= 5 2.0)')" = '#t'

    test "$($LISHP '(>= 2 5)')" = '#f'
    test "$($LISHP '(>= 2 5.0)')" = '#f'
}
