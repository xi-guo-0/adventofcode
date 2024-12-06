#lang racket

(define (process-input input)
  (let* ([grid (list->vector (map (compose list->vector string->list) (string-split input "\n")))]
         [m (vector-length grid)]
         [n (vector-length (vector-ref grid 0))]
         [p (call/cc (lambda (k)
                       (for* ([r (in-range m)]
                              [c (in-range n)])
                         (let ([ch (vector-ref (vector-ref grid r) c)])
                           (when (not (or (char=? ch #\#) (char=? ch #\.)))
                             (k (+ c (* r 0+i))))))))]
         [get-char (lambda (p) (vector-ref (vector-ref grid (imag-part p)) (real-part p)))]
         [d (hash-ref (make-hash (list (cons #\^ 0-i) (cons #\> 1) (cons #\v 0+i) (cons #\< -1)))
                      (get-char p))])
    (define (is-out p)
      (let ([c (real-part p)]
            [r (imag-part p)])
        (or (< r 0) (<= m r) (< c 0) (<= n c))))
    (define (is-obstruction p)
      (char=? #\# (get-char p)))
    (define (simulate p d l)
      (cond
        [(is-out p) (length (remove-duplicates l))]
        [(is-out (+ p d)) (add1 (simulate (+ p d) d l))]
        [(is-obstruction (+ p d)) (simulate p (* d 0+i) (cons p l))]
        [else (simulate (+ p d) d (cons p l))]))
    (simulate p d (list p))))

(define (main)
  (displayln (process-input (port->string (current-input-port)))))

(main)
