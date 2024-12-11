#lang racket

(require memo
         threading)

(define input (map string->number (string-split (port->string (current-input-port)))))

(define (digits n)
  (~> n (log 10) add1 floor inexact->exact))

(define (split n)
  (call-with-values (λ () (quotient/remainder n (expt 10 (quotient (digits n) 2)))) list))

(define/memoize (transform stone n)
                (cond
                  [(zero? n) 1]
                  [(zero? stone) (transform 1 (sub1 n))]
                  [(even? (digits stone)) (for/sum ([st (split stone)]) (transform st (sub1 n)))]
                  [else (transform (* stone 2024) (sub1 n))]))

(define (blink n)
  (for/sum ([stone input]) (transform stone n)))

(blink 25)
(blink 75)
