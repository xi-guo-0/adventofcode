#lang racket

(define disk-map
  (map (lambda (ch) (- (char->integer ch) (char->integer #\0)))
       (string->list (string-trim (port->string (current-input-port))))))
(define (odd xs)
  (map (lambda (i) (list-ref xs i)) (range 1 (length xs) 2)))
(define (even xs)
  (map (lambda (i) (list-ref xs i)) (range 0 (length xs) 2)))
(define file-blocks (even disk-map))
(define free-space (odd disk-map))
(define (checksum xs)
  (for/sum ([(v i) (in-indexed xs)])
           (if v
               (* v i)
               0)))

(let* ([free-space-count (apply + free-space)]
       [file-blocks-to-be-move (take (reverse (flatten (for/list ([(v i) (in-indexed file-blocks)])
                                                         (make-list v i))))
                                     free-space-count)])
  (define (helper disk-map to-be-move v is-free)
    (cond
      [(empty? disk-map) to-be-move]
      [(empty? to-be-move) disk-map]
      [is-free
       (append (take to-be-move (first disk-map))
               (helper (rest disk-map) (drop to-be-move (first disk-map)) v (not is-free)))]
      [else
       (append (make-list (first disk-map) v)
               (helper (rest disk-map) to-be-move (add1 v) (not is-free)))]))
  (checksum (take (helper disk-map file-blocks-to-be-move 0 #f) (apply + file-blocks))))

(define disk
  (for/list ([(v i) (in-indexed disk-map)])
    (if (even? i)
        (list (quotient i 2) v)
        (list #f v))))

(define (find-index-by-value alist target-value)
  (call/cc (lambda (k)
             (for ([(v i) (in-indexed alist)])
               (when (eq? (car v) target-value)
                 (k i))))))

(define (find-free alist target-count)
  (call/cc (lambda (k)
             (begin
               (for ([(v i) (in-indexed alist)])
                 (when (and (not (first v)) (<= target-count (second v)))
                   (k i)))
               #f))))

(define (sublist lst i j)
  (if (< i j)
      (take (drop lst i) (- j i))
      '()))

(define (move alist target-value)
  (let* ([idx (find-index-by-value alist target-value)]
         [p (list-ref alist idx)]
         [v (first p)]
         [cnt (second p)]
         [free-idx (find-free (take alist idx) cnt)])
    (if free-idx
        (append (take alist free-idx)
                (list p)
                (if (not (= (second (list-ref alist free-idx)) cnt))
                    (list (list #f (- (second (list-ref alist free-idx)) cnt)))
                    '())
                (sublist alist (add1 free-idx) idx)
                (list (list #f cnt))
                (drop alist (add1 idx)))
        alist)))

(define (times alist n)
  (if (<= n 0)
      alist
      (times (move alist n) (sub1 n))))

(checksum (flatten (for/list ([p (times disk (sub1 (length file-blocks)))])
                     (make-list (second p) (first p)))))
