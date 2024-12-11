#lang racket

(define raw-input (port->string (current-input-port)))

(match-define (list raw-rules raw-updates) (string-split raw-input "\n\n"))

(define rules
  (for/set ([l (string-split raw-rules "\n")])
    (map string->number (string-split l "|"))))

(define updates
  (map (lambda (l) ((curry map string->number) (string-split l ",")))
       (string-split raw-updates "\n")))

(define (rule<? a b)
  (set-member? rules (list a b)))

(define (get-middle-element lst)
  (list-ref lst (floor (/ (length lst) 2))))

(for/sum ([a-update updates])
         (let ([sorted (sort a-update rule<?)])
           (if (equal? a-update sorted)
               (get-middle-element a-update)
               0)))

(for/sum ([a-update updates])
         (let ([sorted (sort a-update rule<?)])
           (if (equal? a-update sorted)
               0
               (get-middle-element sorted))))
