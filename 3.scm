#lang racket
(define (isStrNumber s)
  (cond ((not (string->number s))
         #f)
        (else
         #t))
  )

(define (add s c)
  (if (string? c)
      (string-append s c)
      (string-append s (make-string 1 c)))
  )

(define (isEmpty s)
  (= (string-length s) 0)
  )

(define (top stack)
  (string-ref stack (- (string-length stack) 1))
  )

(define (pop stack)
  (substring stack 0 (- (string-length stack) 1))
  )

(define (removeWhiteSpacesHelper s a b acc)
  (cond((= (+ a 1) b)
        (cond ((eq? (string-ref s a) #\ )
              acc)
              (else
               (string-append acc (substring s 0 1)))))
       ((and (eq? (string-ref s a) #\ ) (eq? (string-ref s (+ a 1) ) #\ ))
        (removeWhiteSpacesHelper (substring s (+ a 1)) 0 (string-length (substring s (+ a 1))) acc))
       ((and (eq? (string-ref s a) #\ ) (not (char-numeric? (string-ref s (+ a 1)))))
        (removeWhiteSpacesHelper (substring s (+ a 1)) 0 (string-length (substring s (+ a 1))) acc))
       ((and (eq? (string-ref s a) #\ ) (= (string-length acc) 0))
        (removeWhiteSpacesHelper (substring s (+ a 1)) 0 (string-length (substring s (+ a 1))) acc))
       ((and (eq? (string-ref s a) #\ ) (char-numeric? (string-ref acc (- (string-length acc) 1))))
        (removeWhiteSpacesHelper (substring s (+ a 1)) 0 (string-length (substring s (+ a 1))) (string-append acc (substring s 0 1))))
       (else
        (removeWhiteSpacesHelper (substring s (+ a 1)) 0 (string-length (substring s (+ a 1))) (string-append acc (substring s 0 1)))))
  )

(define (removeWhiteSpacesHelper2 s a b acc)
  (cond ((= (+ a 1) b)
        (cond ((eq? (string-ref s a) #\ )
              acc)
              (else
               (string-append acc (substring s a)))))
        ((and (eq? (string-ref s a) #\ ) (if (= a 0) #f (not (char-numeric? (string-ref s (- a 1))))))
         (removeWhiteSpacesHelper2 s (+ a 1) b acc))
        (else
         (removeWhiteSpacesHelper2 s (+ a 1) b (string-append acc (substring s a(+ a 1))))))
  )

(define (removeUselessWhiteSpaces s)
  (define x (removeWhiteSpacesHelper s 0 (string-length s) ""))
  (removeWhiteSpacesHelper2 x 0 (string-length x) "")
  )

(define (prec operator)
  (cond ((eq? operator #\^)
         3)
        ((or (eq? operator #\*) (eq? operator #\/))
         2)
        ((or (eq? operator #\+) (eq? operator #\-))
         1)
        (else
         0))
  )

(define (isOperator operator)
  (or (eq? operator #\-)(or (or (eq? operator #\/) (eq? operator #\+)) (or (eq? operator #\^) (eq? operator #\*))))
  )

(define (emptyStackInResult stack result)
  (cond ((isEmpty stack)
         result)
        (else
         (emptyStackInResult (pop stack) (add result (top stack)))))
  )

(define (infixToPostfixHelper s a b stack result )
  (define (predicate s a stack)
    (and (not (isEmpty stack)) ( <= (prec (string-ref s a)) (prec (top stack))))
    )
  (cond ((= a b)
          (emptyStackInResult stack result))
        (else
         (cond ((isOperator (string-ref s a))
                (cond ((not (predicate s a stack))
                       (infixToPostfixHelper s (+ a 1) b (add stack (string-ref s a)) (add result #\,)))
                      (else
                       (infixToPostfixHelper s a b (pop stack) (add result (top stack))))))
               (else
                (infixToPostfixHelper s (+ a 1) b stack (add result (string-ref s a)))))))
  )

(define (infixToPostfix s)
  (define exp (removeUselessWhiteSpaces s))
  (infixToPostfixHelper exp 0 (string-length exp) "" "")
  )