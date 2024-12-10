#lang racket

(define input (map string->list (port->lines (current-input-port))))

(define (transpose rows)
  (for/list ([col (range (length (first rows)))])
    (for/list ([row rows])
      (list-ref row col))))

(define (diagonal mat)
  (let* ([m (length mat)]
         [n (length (first mat))])
    (for/list ([k (range (- 1 n) m)])
      (for/list ([r (range (max 0 k) (min m (+ n k)))])
        (list-ref (list-ref mat r) (- r k))))))

(define (count-xmas chs)
  (match chs
    [(list* #\X #\M #\A #\S rest) (+ 1 (count-xmas rest))]
    [(cons _ rest) (count-xmas rest)]
    ['() 0]))

(define backwards (curry map reverse))

(define (traversal mat)
  (define (is-mas ch00 ch22)
    (or (and (char=? ch00 #\M) (char=? ch22 #\S)) (and (char=? ch00 #\S) (char=? ch22 #\M))))
  (define (ref r c)
    (list-ref (list-ref mat r) c))
  (let ([m (length mat)]
        [n (length (car mat))])
    (for*/sum ([r (in-range 1 (sub1 m))] [c (in-range 1 (sub1 n))])
              (if (and (char=? (ref r c) #\A)
                       (and (is-mas (ref (sub1 r) (sub1 c)) (ref (add1 r) (add1 c)))
                            (is-mas (ref (sub1 r) (add1 c)) (ref (add1 r) (sub1 c)))))
                  1
                  0))))

(for/sum ([method
           (list identity
                 transpose
                 diagonal
                 (compose diagonal backwards)
                 backwards
                 (compose backwards transpose)
                 (compose backwards diagonal)
                 (compose backwards diagonal backwards))])
         (for/sum ([line (method input)]) (count-xmas line)))
(traversal input)
