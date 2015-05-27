describe "Functions"

ADD_FUNCTION='(define (add x y) (+ x y))'

it_can_define_functions() {
    test "$($LISHP '(define (add x y) (+ x y))')" = '(lambda ("x" "y") ...)'

    result=$(
        (
            echo $ADD_FUNCTION
            echo '(add 10 2.0)'
        ) | $LISHP | tail -n 1
    )

    test "$result" = '12.0'
}

it_can_handle_complex_functions_with_state() {
    result=$(
        (
            echo '(define (counter inc) (lambda (x) (set! inc (+ x inc)) inc))'
            echo '(define my-count (counter 2))'
            echo '(my-count 2)'
            echo '(my-count 2)'
        ) | $LISHP | tail -n 1
    )

    test "$result" = '6'
}

it_validates_argument_length() {
    result=$(
        (
            echo $ADD_FUNCTION
            echo '(add 1 2 3)'
        ) | $LISHP | tail -n 1
    )

    test "$result" = 'Expected 2 args; found values 1 2 3'
    result=$(
        (
            echo $ADD_FUNCTION
            echo '(add 1)'
        ) | $LISHP | tail -n 1
    )

    test "$result" = 'Expected 2 args; found values 1'
}
