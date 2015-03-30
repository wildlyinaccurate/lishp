it_checks_numeric_equality() {
    test "$($LISHP '(= 2 2)')" = '#t'
    test "$($LISHP '(= 2.0 2)')" = '#t'

    test "$($LISHP '(= 2 1)')" = '#f'
    test "$($LISHP '(= 2.0 1)')" = '#f'
}

it_compares_with_less_than() {
    test "$($LISHP '(< 2 5)')" = '#t'
    test "$($LISHP '(< 2.0 5)')" = '#t'

    test "$($LISHP '(< 5 2)')" = '#f'
    test "$($LISHP '(< 5.0 2)')" = '#f'
}

it_compares_with_more_than() {
    test "$($LISHP '(> 5 2)')" = '#t'
    test "$($LISHP '(> 5 2.0)')" = '#t'

    test "$($LISHP '(> 2 5)')" = '#f'
    test "$($LISHP '(> 2 5.0)')" = '#f'
}
