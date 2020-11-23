#lang racket
(define (my-filter list pred)
  (cond ((null? list)
        null)
        ((pred (car list))
        (cons (car list) (my-filter (cdr list) pred)))
        (else
         (my-filter (cdr list) pred)))
  )
(define (space? c)
 (eq? c #\space))

(define (star? c)
  (eq? c #\*))

(define (char->num c) (string->number (string c)))

(define (valid? c)
  (if (or (or (or (space? c) (star? c)) (or (eq? c #\{) (eq? c #\}))) (char->num c))
      #t
      #f)
  )

(define (pop stack)
  (reverse (cdr (reverse stack)))
  )

(define (top stack)
  (car (reverse stack))
  )

(define (push stack x)
  (cond ((null? stack)
         (cons x '()))
        (else
         (cons (car stack) (push (cdr stack) x))))
  )

(define (remove-spaces str)
  (define ls (string->list str))
  (list->string (my-filter ls (lambda (x) (not (space? x)))))
  )

(define (find-ending-index str si ei)
  (cond ((> si ei)
         -1)
        ((not (char->num (string-ref str (+ si 1))))
         si)
        (else
         (find-ending-index str (+ si 1) ei)))
  )


(define (parse-to-working-list s)
  (define str (remove-spaces s))
  (define (parse-to-list-helper s si ei result)
  (cond ((> si ei)
         result)
        ((char->num (string-ref str si))
         (parse-to-list-helper str (+ si (- (find-ending-index str si ei) (- si 1))) ei (push result (string->number (substring str si (+ (find-ending-index str si ei) 1))))))
        (else
         (parse-to-list-helper str (+ si 1) ei (push result (string-ref str si)))))
  )
  
  (parse-to-list-helper str 0 (- (string-length str) 1) null)
  )


(define (root tree) (car tree))
(define (left-tree tree) (cadr tree))
(define (right-tree tree) (caddr tree))
(define (make-tree root left right)
  (list root left right))

(define (find-index str si ei stack)
  (cond ((> si ei)
         -1)
        ((eq? (string-ref str si) #\{)
         (find-index str (+ si 1) ei (push stack #\{)))
        ((eq? (string-ref str si) #\})
         (cond ((eq? (top stack) #\{)
                (cond ((null? (pop stack))
                       si)
                      (else
                       (find-index str (+ si 1) ei (pop stack)))))))
        (else
         (find-index str (+ si 1) ei stack)))
  )


(define (parse-tree str)
  (let parse ([tokens (parse-to-working-list str)] [stack '(())])
    (cond [(null? tokens)
           (caar stack)]
          [(eq? (car tokens) #\{)
           (parse (cdr tokens) (cons '() stack))]
          [(eq? (car tokens) #\})
           (parse (cdr tokens) (append-top (car stack) (cdr stack)))]
          [(eq? (car tokens) #\*)
           (parse (cdr tokens) (append-top '() stack))]
          [else
           (parse (cdr tokens) (append-top  (car tokens) stack))])))

(define (list-tree? t)
  (cond ((null? t)
         #t)
        ((not (list? t))
        #f)
        ((not (= (length t) 3))
         #f)
        (else
         (list-tree? (left-tree t))
         (list-tree? (right-tree t))))

  )

(define (valid-list? lst)
  (cond ((null? lst)
         #t)
        ((not (valid? (car lst)))
         #f)
        (else
         (valid-list? (cdr lst))))
  )

(define (brackets-valid? lst)
  (define (brackets-helper lst stack)
    (cond ((null? lst)
           (if (null? stack)
               #t
               #f)
           )
          ((eq? (car lst) #\{)
           (brackets-helper (cdr lst) (push stack #\{)))
          ((eq? (car lst) #\})
           (if (null? stack)
               #f
               (brackets-helper (cdr lst) (pop stack))))
          (else
           (brackets-helper (cdr lst) stack)))
    )

  (brackets-helper lst null)
  )

(define (tree? str)
  (cond ((not (brackets-valid? (string->list str)))
         #f)
        ((not (list-tree? (parse-tree str)))
          #f)
        ((not (valid-list? (string->list str)))
         #f)
        (else
         #t))
  )

(define (append-top ele stack)
  (cons (append (car stack) (list ele))
        (cdr stack)))

(define (string->tree str)
  (let parse ([tokens (parse-to-working-list str)] [stack '(())])
    (cond [(not (tree? str))
           #f]
          [(null? tokens)
           ; solution is at the top of the stack, return it
           (caar stack)]
          [(eq? (car tokens) #\{)
           ; start new sublist at the top of the stack
           (parse (cdr tokens) (cons '() stack))]
          [(eq? (car tokens) #\})
           ; pop top element of the stack, append it to previous
           ; frame, continue with solution where we left it
           (parse (cdr tokens) (append-top (car stack) (cdr stack)))]
          [(eq? (car tokens) #\*)
           (parse (cdr tokens) (append-top '() stack))]
          [else
           ; add current element to top of stack
           (parse (cdr tokens) (append-top  (car tokens) stack))])))
