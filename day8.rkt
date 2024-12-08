#lang racket

(require racket/hash
         threading)

(define grid
  (map string->list
       (for/list ([line (in-lines)])
         line)))
(define m (length grid))
(define n (length (car grid)))
(define (valid-point? p)
  (let ([x (real-part p)]
        [y (imag-part p)])
    (and (<= 0 y) (< y m) (<= 0 x) (< x n))))
(define grouped
  (map (lambda (group) (map car group))
       (group-by cdr
                 (filter (lambda (x) (not (null? x)))
                         (for*/list ([(rows y) (in-indexed grid)]
                                     [(col x) (in-indexed rows)])
                           (if (or (char-alphabetic? col) (char-numeric? col))
                               (cons (make-rectangular x y) col)
                               '()))))))
(define (enum-part-one a b)
  (let ([p (+ b (- b a))])
    (if (valid-point? p)
        (list p)
        '())))
(define (enum-part-two a b)
  (define (helper p t)
    (if (valid-point? p)
        (cons p (helper (+ p t) t))
        '()))
  (helper b (- b a)))

(define unique-length (compose length remove-duplicates))

(define (solve method)
  (unique-length (flatten (map (lambda (a-group)
                                 (for*/list ([a (in-list a-group)]
                                             [b (in-list (rest a-group))])
                                   (if (> (index-of a-group b) (index-of a-group a))
                                       (append (method a b) (method b a))
                                       '())))
                               grouped))))

(solve enum-part-one)
(solve enum-part-two)
