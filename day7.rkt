#lang racket

(define (parse-numbers str)
  (map string->number (string-split (string-replace str ":" "") " ")))

(define (|| x y)
  (string->number (string-append (number->string x) (number->string y))))

(define (valid-expression target nums ops)
  (define (helper res nums)
    (if (empty? nums)
        (= target res)
        (let ([x (first nums)]
              [xs (rest nums)])
          (ormap (lambda (op) (helper (op res x) xs)) ops))))
  (helper (first nums) (rest nums)))

(define input
  (for/list ([line (in-lines)])
    line))

(define (have-a-try ops)
  (for/sum ([x input])
           (let* ([xs (parse-numbers x)]
                  [target (first xs)]
                  [nums (rest xs)])
             (if (valid-expression target nums ops) target 0))))

(have-a-try (list + *))
(have-a-try (list + * ||))
