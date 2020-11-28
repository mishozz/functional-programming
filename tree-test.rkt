#lang racket/base
(require rackunit rackunit/gui racket/include)

(require "tree.rkt")

(test/gui
(test-suite
 "tree?"
 (test-true "* is valid" (tree? "*"))
 (test-true "{3{2**}*} is valid" (tree? "{3{2**}*}"))
 (test-true "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}} is valid" (tree? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}"))
 (test-true "{2 {4 * *} *} is valid" (tree? "{2 {4 * *} *}"))
 (test-true "{2{4**}*} is valid" (tree? "{2{4**}*}"))
 (test-true "{2     {4     * *}               *} is valid" (tree? "{2     {4     * *}               *}"))
 (test-false "{*} is NOT valid" (tree? "{*}"))
 (test-false "{2*} is NOT valid" (tree? "{2*}"))
 (test-false "{4{**} is NOT valid" (tree? "{4{**}"))
 (test-false "{3{**}} is NOT valid" (tree? "{3{**}}"))
 )

(test-suite
 "string->tree"
 (test-equal? "* -> '()"  (string->tree "*") '())
 (test-equal? "{2**} -> '(2 () ())"  (string->tree "{2**}") '(2 () ()))
 (test-equal? "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}} - > '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))"
              (string->tree "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
              '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))))
 (test-false "{2**}} is NOT VALID" (string->tree "{2**}}"))
 (test-false "{2*3} is NOT VALID" (string->tree "{2*3}"))
 )
(test-suite
 "tree->string"
 (test-equal? "'(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) -> {5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}" (tree->string '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))) "{5 {22 {2 * *} {6 * *}} {1 * {3 {111 * *} *}}}")
 (test-equal? "'() -> *" (tree->string '()) "*")
 (test-equal? "'(2 () ()) -> {2**}" (tree->string '(2 () ())) "{2 * *}")
 (test-false "'(()) is NOT valid" (tree->string '(())))
 (test-false "'(2 ()) is NOT valid" (tree->string '(2 ())))
 )

(test-suite
 "ordered?"
 (test-true "'() is ordered" (ordered? '()))
 (test-true "'(5 (3 () ()) (7 () ())) is ordered" (ordered? '(5 (3 () ()) (7 () ()))))
 (test-false "'(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ()))) is NOT ordered" (ordered? '(5 (22 (2 () ()) (6 () ())) (1 () (3 (111 () ()) ())))))
 (test-false "'(()) is NOT valid" (ordered? '(())))
 (test-false "'(2 ()) is NOT valid" (ordered? '(2 ())))
 )

(test-suite
 "balanced?"
 (test-true "'() is balanced" (balanced? '()))
 (test-true "'(5 (3 () ()) (7 () ())) is balanced" (balanced? '(5 (3 () ()) (7 () ()))))
 (test-false "'(5 (3 (3 () ()) ()) () ) is balanced" (balanced? '(5 (3 (3 () ()) ()) ())))
 (test-false "'(()) is NOT valid" (balanced? '(())))
 (test-false "'(2 ()) is NOT valid" (balanced? '(2 ())))
 )
)
