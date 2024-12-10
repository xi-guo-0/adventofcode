#lang racket

(define reports
  (for/list ([line (port->lines (current-input-port))])
    (map string->number (string-split line))))

(define (zip-1-to-n-minus-1 lst)
  (map cons (reverse (cdr (reverse lst))) (cdr lst)))

(define (safe<? . args)
  (andmap (lambda (p) (and (< (car p) (cdr p)) (<= (- (cdr p) (car p)) 3)))
          (zip-1-to-n-minus-1 args)))

(define (safe>? . args)
  (andmap (lambda (p) (and (> (car p) (cdr p)) (<= (- (car p) (cdr p)) 3)))
          (zip-1-to-n-minus-1 args)))

(define (safe? report)
  (or (apply safe<? report) (apply safe>? report)))

(count safe? reports)
(count (lambda (report) (ormap safe? (combinations report (sub1 (length report))))) reports)
