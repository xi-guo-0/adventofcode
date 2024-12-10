#lang racket

(define input (port->string (current-input-port)))

(define (find pattern)
  (interpreter (regexp-match* pattern input #:match-select values)))

(define (interpreter instructions [acc 0] [enabled #t])
  (match* (instructions enabled)
    [('() _) acc]
    [((list* (list "do()" _ _) rest) _) (interpreter rest acc #t)]
    [((list* (list "don't()" _ _) rest) _) (interpreter rest acc #f)]
    [((list* _ rest) #f) (interpreter rest acc #f)]
    [((list* (list _ (app string->number a) (app string->number b)) rest) #t)
     (interpreter rest (+ acc (* a b)) #t)]))

(find #px"mul\\((\\d+),(\\d+)\\)")
(find #px"mul\\((\\d+),(\\d+)\\)|don't\\(\\)|do\\(\\)")
