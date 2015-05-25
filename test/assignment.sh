describe "Variables and assignment"

it_handles_basic_assignment() {
    result=$(
        (
            echo '(define x 10)'
            echo '(* 2 x)'
        ) | $LISHP | tail -n 1
    )

    test "$result" = '20'
}
