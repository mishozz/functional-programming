#lang racket
(define (loop-print a b s)
  (cond ((> a b)
        (void))
        (else
         (display s)
         (loop-print (+ a 1) b s)))        
  )

(define(print-middle n s1 s2)
  (define(print-symbol-n-times n s)
  (loop-print 1 n s)
  )
  (display s1)
  (print-symbol-n-times (+ 1 (* 4 (- n 1))) "─")
  (display s2)
  )

(define (print-cols n col s1 s2)
  (cond ((= col n)
         (void))
        (else
         (display s1)
         (display s2)
         (print-cols n (+ 1 col) s1 s2)))       
  )

(define (print-side n side)
  (cond ((= n 1)
         (void))
        ((equal? side "left")
         (print-cols n 1 " " "│"))
        ((equal? side "right")
         (print-cols n 1 "│" " "))
        (else
         (void)))       
  )

(define (square-up-loop a b)
  (cond ((> a b)
         (void))
        (else
         (print-side a "right")
         (print-middle (- (+ b 1) a) "┌" "┐")
         (print-side a "left")
         (displayln "")
         (square-up-loop (+ a 1) b)))
  )

(define (square-down-loop a b)
  (cond ((> a b)
         (void))
        (else
         (print-side (- b (- a 1)) "right")
         (print-middle a "└" "┘")
         (print-side (- b (- a 1)) "left")
         (displayln "")
         (square-down-loop (+ a 1) b)))
  )

(define (square n)
  (square-up-loop 1 n)
  (square-down-loop 1 n)
  )
