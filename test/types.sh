describe "Basic type parsing"

it_parses_strings() {
    test "$($LISHP '"foo bar"')" = '"foo bar"'
}

it_parses_escaped_strings() {
    test "$($LISHP '"foo \"baz\" bar"')" = '"foo \"baz\" bar"'
}

it_parses_positive_integers() {
    test $($LISHP 1048) = '1048'
}

it_parses_negative_integers() {
    test $($LISHP '-1048') = '-1048'
}

it_parses_floats() {
    test $($LISHP 10.48) = '10.48'
}

it_parses_boolean_true() {
    test $($LISHP '#t') = '#t'
}

it_parses_boolean_false() {
    test $($LISHP '#f') = '#f'
}

it_parses_lists() {
    test "$($LISHP "'(1 2 3)")" = "(1 2 3)"
}

it_parses_nested_lists() {
    test "$($LISHP "'(1 2 (3))")" = "(1 2 (3))"
}
