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
  (cond ((equal? s "")
         "")
        (else
          (removeWhiteSpacesHelper2 (removeWhiteSpacesHelper s 0 (string-length s) "") 0 (string-length (removeWhiteSpacesHelper s 0 (string-length s) "")) "")))
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

(define (lastOccuranceOf s a symbol)
 (cond ((or (not (string-contains? s (make-string 1 symbol))) (isEmpty s))
        -1)
       ((eq? (string-ref s a) symbol)
        a)
       (else
        (lastOccuranceOf s (- a 1) symbol)))
  )

(define (takeElementFromStack stack delimiter)
  (define index (lastOccuranceOf stack (- (string-length stack) 1) delimiter))
  (substring stack (+ index 1))
  )


(define (myabs x)
  (cond ((< x 0) (* x -1))
        ((x >= 0) (* x 1))))

(define (myexpt x y)
        (cond ((< x 0) (* (cond ((odd? (myabs y)) -1)
                                 (else 1))
                          (myexpt (myabs x) y)))
              ((< y 0) (/ 1 (myexpt x (myabs y))))
              ((= y 0) 1)
              ((> y 0) (* x (myexpt x (- y 1))))))

(define (getProcedure operator)
  (cond ((eq? operator #\+)
         +)
        ((eq? operator #\-)
         -)
        ((eq? operator #\*)
         *)
        (else
         /))
  )

  (define (evalStack stack operator)
    (define t1 (takeElementFromStack stack #\,))
    (define st1 (substring stack 0 (lastOccuranceOf stack (- (string-length stack) 1) #\,)))
    (define t2 (takeElementFromStack st1 #\,))
    (define st2 (substring st1 0 (if (= (lastOccuranceOf st1 (- (string-length st1) 1) #\,) -1)
                                       0
                                       (lastOccuranceOf st1 (- (string-length st1) 1) #\,))))
    (cond ((eq? operator #\^)
           (if (= (string-length st2) 0 )
                 (add st2 (number->string(myexpt (string->number t2) (string->number t1))))
                 (string-append st2 "," (number->string(myexpt (string->number t2) (string->number t1))))))
          (else
           (if (= (string-length st2) 0)
           (add st2 (number->string((getProcedure operator) (string->number t1) (string->number t2))))
           (string-append st2 "," (number->string((getProcedure operator) (string->number t1) (string->number t2)))))))
    )



(define (evalHelper s a b stack)
  (cond ((= a b)
         (takeElementFromStack stack #\,))
        ((isOperator (string-ref s a))
         (evalHelper s (+ a 1) b (evalStack stack (string-ref s a))))
        (else
         (evalHelper s (+ a 1) b (add stack (string-ref s a)))))
  )

(define (takeEndingNumberIndex s a b)
  (cond ((or (= a b) (isOperator(string-ref s a)))
         a)
        (else
         (takeEndingNumberIndex s (+ a 1) b)))
  )


;final working functions are below

(define (expr-rp s)
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
  (define exp (removeUselessWhiteSpaces s))
  (cond ((not (expr-valid? s))
         #f)
        (else
         (infixToPostfixHelper exp 0 (string-length exp) "" "")))
  )


(define (expr-valid? x)
  (define (isValidHelper s a b op?)
  (define expr (removeUselessWhiteSpaces s))
  (define b(string-length expr))
  (cond ((= (string-length expr) 0 )
         #t)
        ((or (isOperator(string-ref expr (- b 1))) (or (string-contains? expr " ") (isOperator (string-ref expr 0))))
         #f)
        ((= a b)
         #t)
        ((and (isOperator (string-ref expr a)) op?)
         #f)
        ((and (isOperator (string-ref expr a)) (not op?))
         (isValidHelper expr (+ a 1) b #t))
        (else
         (isValidHelper expr (takeEndingNumberIndex expr a b) b #f)))
  )
  (isValidHelper x 0 (string-length x) #f)
  )


(define (expr-eval x)
  (cond ((= (string-length x) 0)
         0)
        ((not (expr-valid? x))
         #f)
        (else
         (string->number (evalHelper (expr-rp  x) 0 (string-length (expr-rp  x)) ""))))
  )