describe "Basic type parsing"

it_parses_strings() {
    result=$($LISHP '"foo bar"')
    test "$result" = '"foo bar"'
}

it_parses_escaped_strings() {
    result=$($LISHP '"foo \"baz\" bar"')
    test "$result" = '"foo \"baz\" bar"'
}

it_parses_positive_integers() {
    result=$($LISHP 1048)
    test "$result" = "1048"
}

it_parses_negative_integers() {
    result=$($LISHP "-1048")
    test "$result" = "-1048"
}

it_parses_floats() {
    result=$($LISHP 10.48)
    test "$result" = "10.48"
}

it_parses_boolean_true() {
    result=$($LISHP '#t')
    test "$result" = "#t"
}

it_parses_boolean_false() {
    result=$($LISHP '#f')
    test "$result" = "#f"
}

it_parses_lists() {
    result=$($LISHP "'(1 2 3)")
    test "$result" = "(1 2 3)"
}

it_parses_nested_lists() {
    result=$($LISHP "'(1 2 '(3))")
    test "$result" = "(1 2 (quote 3)))"
}
