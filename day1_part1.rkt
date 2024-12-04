#lang racket

(define (calculateDistance xs ys)
  (apply + (map (lambda (x y) (abs (- x y))) xs ys)))

(define (process-input str)
  (define mat (map (lambda (line) (map string->number (string-split line))) (string-split str "\n")))
  (let* ([xs (sort (map car mat) <)]
         [ys (sort (map cadr mat) <)])
    (calculateDistance xs ys)))

(define (main)
  (displayln (process-input (port->string (current-input-port)))))

(main)
