describe "Basic type parsing"

it_parses_strings() {
    result=$(bin/lishp '"foo bar"')
    test "$result" = '"foo bar"'
}

it_parses_positive_integers() {
    result=$(bin/lishp 1048)
    test "$result" = "1048"
}

it_parses_negative_integers() {
    result=$(bin/lishp "-1048")
    test "$result" = "-1048"
}

it_parses_floats() {
    result=$(bin/lishp 10.48)
    test "$result" = "10.48"
}

it_parses_lists() {
    result=$(bin/lishp "'(1 2 3)")
    test "$result" = "(1 2 3)"
}

it_parses_nested_lists() {
    result=$(bin/lishp "'(1 2 '(3))")
    test "$result" = "(1 2 (quote 3)))"
}
