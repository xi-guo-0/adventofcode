#lang racket

(define (traversal mat)
  (define (is-mas ch0 ch4)
    (or (and (char=? ch0 #\M) (char=? ch4 #\S)) (and (char=? ch0 #\S) (char=? ch4 #\M))))
  (define (is-xmas ch0 ch1 ch3 ch4)
    (and (is-mas ch0 ch4) (is-mas ch1 ch3)))

  (define (traversal-with-width-height m n)
    (for/sum ([r (in-range 1 (sub1 m))])
             (for/sum ([c (in-range 1 (sub1 n))])
                      (if (let ([ch0 (list-ref (list-ref mat (sub1 r)) (sub1 c))]
                                [ch1 (list-ref (list-ref mat (sub1 r)) (add1 c))]
                                [ch2 (list-ref (list-ref mat r) c)]
                                [ch3 (list-ref (list-ref mat (add1 r)) (sub1 c))]
                                [ch4 (list-ref (list-ref mat (add1 r)) (add1 c))])
                            (and (char=? ch2 #\A) (is-xmas ch0 ch1 ch3 ch4)))
                          1
                          0))))
  (if (or (empty? mat) (empty? (first mat)))
      0
      (traversal-with-width-height (length mat) (length (first mat)))))

(define (process-input str)
  (define (string->matrix str)
    (map string->list (string-split str "\n")))
  (traversal (string->matrix str)))

(define (main)
  (displayln (process-input (port->string (current-input-port)))))

(main)
