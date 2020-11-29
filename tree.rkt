#lang racket/base
(require racket/stream)
(provide (all-defined-out))

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
(define (left-tree tree) (if (null? tree)
                             '()
                          (cadr tree)))
(define (right-tree tree) (if (null? tree)
                              '()
                              (caddr tree)))
(define (make-tree root left right)
  (list root left right))
(define (tree-empty? tree) (null? tree))

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
           (caar stack)]
          [(eq? (car tokens) #\{)
           (parse (cdr tokens) (cons '() stack))]
          [(eq? (car tokens) #\})
           (parse (cdr tokens) (append-top (car stack) (cdr stack)))]
          [(eq? (car tokens) #\*)
           (parse (cdr tokens) (append-top '() stack))]
          [else
           (parse (cdr tokens) (append-top  (car tokens) stack))])))

(define (ordered? tree)
  (define (ordered-helper tree)
  (let ts-minmax ((tree tree) (minv -Inf.0) (maxv +Inf.0))    
    (or (tree-empty? tree)
        (let ((value (root tree)))
          (and (<= minv value maxv)
               (ts-minmax (left-tree tree) minv  value)
               (ts-minmax (right-tree tree) value maxv))))))
  (cond ((not (list-tree? tree))
         #f)
        (else
         (ordered-helper tree)))
  )

(define (height tree)
  (if (null? tree)
        0
        (+ 1 (max (height (left-tree tree)) 
                  (height (right-tree tree))))))

(define (balanced? tree)
  (define (balanced-helper tree lh rh)
    (or (tree-empty? tree)
        (and (<= (abs (- lh rh)) 1)
             (balanced-helper (left-tree tree) (height (left-tree tree)) (height (right-tree tree)))
             (balanced-helper (right-tree tree) (height (left-tree tree)) (height (right-tree tree)))))
    )
  (cond ((not (list-tree? tree))
         #f)
        (else
         (balanced-helper tree (height (left-tree tree)) (height (right-tree tree)))))
  )

(define (tree->string-helper tree)
  (if (null? tree)
      "*"
      (string-append "{" (number->string (root tree))
                     (tree->string-helper (left-tree tree))
                     (tree->string-helper (right-tree tree)) "}"))
  )

(define (to-string lst)
  (define (to-string-helper lst result)
    (if (null? lst)
        result
        (to-string-helper (cdr lst) (string-append result (
                                                           if (char? (car lst))
                                                              (string (car lst))
                                                              (number->string (car lst))
                                                           ))))
    )
    (to-string-helper lst "")
  )

(define (tree->string tree)
  (define (tree->string-helper tree)
    (define (add-spaces str)
      (define (add-spaces-helper lst result)
        (cond ((null? lst)
               result)
              ((and (not (eq? (car lst) #\{)) (not (eq? (car lst) #\})))
               (
                if (and (not (null? (cdr lst))) (not (eq? (cadr lst) #\})))
                   (add-spaces-helper (cdr lst) (append
                                                 (if (list? result)
                                                     result
                                                     (list result))
                                                 (list (car lst))
                                                 (list #\space)))
                   (add-spaces-helper (cdr lst) (append result (list (car lst))))
                   ))
              ((and (not (null? (cdr lst))) (and (eq? (car lst) #\}) (star? (cadr lst))))
               (add-spaces-helper (cdr lst) (append result (list (car lst)) (list #\space))))
              ((and (not (null? (cdr lst))) (and (eq? (car lst) #\}) (eq? (cadr lst) #\{)))
               (add-spaces-helper (cdr lst) (append result (list (car lst)) (list #\space))))
              (else
               (add-spaces-helper (cdr lst) (append result (list (car lst))))))
    )
  (to-string (add-spaces-helper (parse-to-working-list str) '()))
  )
  (if (null? tree)
      "*"
      (add-spaces (string-append "{" (number->string (root tree))
                     (tree->string-helper (left-tree tree))
                     (tree->string-helper (right-tree tree)) "}")))
  )
  (cond ((not (list-tree? tree))
         #f)
        (else
         (tree->string-helper tree)))
  )

(define (preorder-stream tree)  
  (if (null? tree)
      empty-stream
      (stream-append (stream (root tree))
              (preorder-stream (left-tree tree))
              (preorder-stream (right-tree tree)))))

(define (postorder-stream tree)  
  (if (null? tree)
      empty-stream
      (stream-append (postorder-stream (left-tree tree))
              (postorder-stream (right-tree tree))
              (stream (root tree)))))

(define (inorder-stream tree)  
  (if (null? tree)
      empty-stream
      (stream-append (inorder-stream (left-tree tree))
              (stream (root tree))
              (inorder-stream (right-tree tree)))))

(define ‘preorder preorder-stream)
(define ‘inorder inorder-stream)
(define ‘postorder postorder-stream)

(define (tree->stream tree order)
  (if (not (list-tree? tree))
      #f
  (order tree)
  )
  )
