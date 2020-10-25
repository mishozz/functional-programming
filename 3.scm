#lang racket
(define (isStrNumber s)
  (cond ((not (string->number s))
         #f)
        (else
         #t))
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
