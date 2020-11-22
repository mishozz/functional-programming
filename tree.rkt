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

(define (remove-spaces-stars str)
  (define ls (string->list str))
  (list->string (my-filter ls (lambda (x) (and (not (star? x)) (not (space? x))))))
  )

(define (remove-closing-brackets str)
  (substring (substring str 1) 0 (- (string-length str) 2))
  )

(define (root tree) (car tree))
(define (left-tree tree) (cadr tree))
(define (right-tree tree) (caddr tree))
(define (make-tree root left right)
  (list root left right))

(define (new-node x)
  (make-tree x '() '())
  )

(define (add-left-tree tree x)
  (cond ((null? tree)
         (new-node x))
        ((make-tree (root tree) (add-left-tree (left-tree tree) x) (right-tree tree))))
  )

(define (add-right-tree tree x)
  (cond ((null? tree)
         (new-node x))
        ((make-tree (root tree) (left-tree tree) (add-right-tree (right-tree tree) x))))
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

(define (s-t-helper s si ei tree)
  (define str (remove-spaces-stars s))
  (define index (find-index str (+ si 1) ei null))
  (cond ((> si ei)
         null)
        ((and (<= (+ si 1) ei) (eq? (string-ref str (+ si 1)) #\{))
         (cond ((not (= index -1))
                (make-tree (root tree) (add-left-tree tree (s-t-helper str (+ si 2) (- index 1) (left-tree tree))) (add-right-tree tree (s-t-helper str (+ index 2) (- ei 1) (right-tree tree))))
                ))))
  )

(define (tree? t)
  (or (null? t) (and (list? t)
           (= (length t) 3))
           (tree? (left-tree t))
           (tree? (right-tree t))))

(define (append-top ele stack)
  (cons (append (car stack) (list ele))
        (cdr stack)))

(define (tree->string str)
  (let parse ([tokens (string->list str)] [stack '(())])
    (cond [(null? tokens)
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
           (parse (cdr tokens) (append-top (string->number (string (car tokens))) stack))])))