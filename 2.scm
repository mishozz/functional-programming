#lang racket
(define (binary-to-decimal n)
  (string->number (number->string n) 2)
  )
(define (decimal-to-binary n)
   (string->number (number->string n 2))
  )

(define (accumulate a b op f acc)
  (if (>= a b)
      acc
  (accumulate (+ a 1) b op f (op (f a) acc)))
  )

(define (append-leading-zeroes s n)
  (define (append a b)
    (string-append a b)
    )
  (define zeroes (accumulate 0 n append (lambda (x) "0") ""))
  (string-append zeroes s)
  )

(define (change-symbol-at s n symbol)
  (string-append (substring s 0 (- (string-length s) (+ n 1))) symbol (substring s (- (string-length s) n)))
  )

(define (string-to-decimal set)
  (binary-to-decimal (string->number set))
  )

(define (set-operation-helper set1 set2 a result p)
    (define set1-in-decimal (string-to-decimal set1))
    (define set2-in-decimal (string-to-decimal set2))
    (define length (if (> (string-length set1) (string-length set2))
                     (string-length set1)
                     (string-length set2)))
    (cond ((= a length)
           result)
          ((p a)
           (set-operation-helper set1 set2 (+ a 1) (set-add result a) p))
          (else
           (set-operation-helper set1 set2 (+ a 1) result p)))
    )

(define (set-contains? set element)
  (define s (number->string set 2))
  (define pos (+ element 1))
  (cond ((>= element (string-length s))
        #f)
        ((not (eq? (string-ref s (- (string-length s) pos)) #\1))
         #f)
        (else
         #t))
  )

(define (set-add set elem)
  (define set-string (number->string set 2))
  (define set-mod (append-leading-zeroes set-string (- elem (string-length set-string))))
  (cond ((set-contains? set elem)
         set)
        ((<= (string-length set-string) elem)
         (binary-to-decimal (string->number (string-append "1" set-mod) 10)))
        (else
         (binary-to-decimal (string->number (change-symbol-at set-mod elem "1")))))
  )

(define (set-remove set elem)
  (define set-string (number->string set 2))
  (cond ((not (set-contains? set elem))
         #f)
        (else
         (binary-to-decimal (string->number (change-symbol-at set-string elem "0")))))
  )

(define (set-size set)
  (define set-string (number->string set 2))
  (accumulate 0 (string-length set-string) + (lambda (x) (
                                                          if (eq? (string-ref set-string x) #\1)
                                                             1
                                                             0
                                                          )) 0)
  )
(define (set-empty? set)
  (= (set-size set) 0)
  )

(define (set-union set1 set2)
  (define set1-string (number->string set1 2))
  (define set2-string (number->string set2 2)) 
  (set-operation-helper set1-string set2-string 0 0 (lambda (x) (or (set-contains? set2 x) (set-contains? set1 x))))
  )

(define (set-intersect set1 set2)
  (define set1-string (number->string set1 2))
  (define set2-string (number->string set2 2))
  (set-operation-helper set1-string set2-string 0 0 (lambda (x) (and (set-contains? set2 x) (set-contains? set1 x))))
  )

(define (set-difference set1 set2)
  (define set1-string (number->string set1 2))
  (define set2-string (number->string set2 2))
  (set-operation-helper set1-string set2-string 0 0 (lambda (x) (and (set-contains? set1 x) (not (set-contains? set2 x)))))
  )


 (define (max-set set1 set2 p)
    (define (get-weight set p)
      (define set-string (number->string set 2))
      (accumulate 0 (string-length set-string) + (lambda (x) (
                                                          if (eq? (string-ref set-string x) #\1)
                                                            (p x)
                                                             0
                                                          )) 0)
      )
    (cond ((> (get-weight set1 p) (get-weight set2 p))
           set1)
          (else
           set2))
    )


(define (knapsack c n w p)
  (cond ((or (= n 0) (= c 0))
         0)
        ((> (w (- n 1)) c)
         (knapsack c (- n 1) w p))
        (else
         (max-set 
	       (set-add (knapsack (- c (w (- n 1))) (- n 1) w p) (- n 1))
               (knapsack c (- n 1) w p) p)))
  )