#lang racket

(define (zip-1-to-n-minus-1 lst)
  (map string-append (cdr lst) (make-list (sub1 (length lst)) "|") (reverse (cdr (reverse lst)))))

(define (get-middle-element lst)
  (define len (length lst))
  (define middle-index (floor (/ len 2)))
  (list-ref lst middle-index))

(define (process-input input)
  (let* ([l (string-split input "\n\n")]
         [first-section (list->set (string-split (car l) "\n"))]
         [second-section (map (lambda (x) (string-split x ",")) (string-split (cadr l) "\n"))])
    (define (is-correct-order xs)
      (not (ormap (lambda (x) (set-member? first-section x)) (zip-1-to-n-minus-1 xs))))
    (for/sum ([a-update second-section])
             (if (is-correct-order a-update)
                 (string->number (get-middle-element a-update))
                 0))))

(define (main)
  (displayln (process-input (port->string (current-input-port)))))

(main)
