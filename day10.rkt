#lang racket

(define grid
  (for/list ([line (in-lines)])
    (map (lambda (ch) (- (char->integer ch) (char->integer #\0))) (string->list line))))

(define (find grid m n r c v)
  (define (helper r c v)
    (cond
      [(or (< r 0) (<= m r) (< c 0) (<= n c)) '()]
      [(not (= (list-ref (list-ref grid r) c) v)) '()]
      [(= (list-ref (list-ref grid r) c) 9) (list (make-rectangular r c))]
      [else
       (let ([nv (add1 v)])
         (append (helper (add1 r) c nv)
                 (helper (sub1 r) c nv)
                 (helper r (add1 c) nv)
                 (helper r (sub1 c) nv)))]))
  (helper r c v))

(let ([m (length grid)]
      [n (length (car grid))])
  (for*/sum ([r (range m)] [c (range n)])
            (length (remove-duplicates (flatten (find grid m n r c 0))))))

(let ([m (length grid)]
      [n (length (car grid))])
  (for*/sum ([r (range m)] [c (range n)]) (length (flatten (find grid m n r c 0)))))
