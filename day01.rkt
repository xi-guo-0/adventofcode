#lang racket

(define xys
  (for/list ([line (port->lines (current-input-port))])
    (map string->number (string-split line))))
(define xs (map car xys))
(define ys (map cadr xys))
(define (bagify lst)
  (foldl (lambda (key ht) (hash-update ht key add1 0)) #hash() lst))
(define yset (bagify ys))

(apply + (map (compose abs -) (sort xs <) (sort ys <)))
(apply + (map (lambda (x) (* x (hash-ref yset x 0))) xs))
