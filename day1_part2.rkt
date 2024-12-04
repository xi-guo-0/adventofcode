#lang racket

(define (count-in-ys x ys)
  (length (filter (lambda (y) (= x y)) ys)))

(define (calculate-similarity xs ys)
  (apply + (map (lambda (x) (* x (count-in-ys x ys))) xs)))

(define (process-input str)
  (define mat (map (lambda (line) (map string->number (string-split line))) (string-split str "\n")))
  (let* ([xs (map car mat) ]
         [ys (map cadr mat) ])
    (calculate-similarity xs ys)))

(define (main)
  (displayln (process-input (port->string (current-input-port)))))

(main)
