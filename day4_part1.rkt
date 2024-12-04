#lang racket

(define (count-xmas-sequences chs)
  (define (count-xmas-rec chs dp)
    (if (empty? chs)
        (vector-ref dp 3)
        (let ([ch (first chs)]
              [dp0 (vector-ref dp 0)]
              [dp1 (vector-ref dp 1)]
              [dp2 (vector-ref dp 2)]
              [dp3 (vector-ref dp 3)])
          (count-xmas-rec (rest chs)
                          (vector (+ dp0 (if (char=? ch #\X) 1 0))
                                  (+ dp1 (if (char=? ch #\M) dp0 0))
                                  (+ dp2 (if (char=? ch #\A) dp1 0))
                                  (+ dp3 (if (char=? ch #\S) dp2 0)))))))
  (count-xmas-rec chs (vector 0 0 0 0)))

(define (count-xmas chs)
  (define (check-substring start)
    (if (and (char=? (list-ref chs start) #\X)
             (char=? (list-ref chs (+ start 1)) #\M)
             (char=? (list-ref chs (+ start 2)) #\A)
             (char=? (list-ref chs (+ start 3)) #\S))
        1
        0))

  (define (count-helper idx)
    (if (< (+ idx 3) (length chs))
        (+ (check-substring idx) (count-helper (+ idx 1)))
        0))
  (count-helper 0))

(define (string->matrix str)
  (map string->list (string-split str "\n")))

(define (transpose-matrix rows)
  (for/list ([col (in-range (length (first rows)))])
    (for/list ([row rows])
      (list-ref row col))))

(define (main-diagonal-traversal mat)
  (define (main-diagonal-traversal-helper m n k line)
    (define (downright-traversal r c)
      (if (and (< r m) (< c n))
          (cons (list-ref (list-ref mat r) c) (downright-traversal (+ r 1) (+ c 1)))
          '()))
    (if (< line k)
        (cons (let* ([r (max 0 line)]
                     [c (- r line)])
                (downright-traversal r c))
              (main-diagonal-traversal-helper m n k (+ 1 line)))
        '()))
  (if (or (empty? mat) (empty? (first mat)))
      '()
      (let ([m (length mat)]
            [n (length (first mat))])
        (main-diagonal-traversal-helper m n m (- 1 n)))))

(define (anti-diagonal-traversal mat)
  (define (anti-diagonal-traversal-helper m n k line)
    (define (downleft-traversal r c)
      (if (and (<= 0 c) (< r m))
          (cons (list-ref (list-ref mat r) c) (downleft-traversal (+ r 1) (- c 1)))
          '()))
    (if (< line k)
        (cons (let* ([c (min (- n 1) line)]
                     [r (- line c)])
                (downleft-traversal r c))
              (anti-diagonal-traversal-helper m n k (+ 1 line)))
        '()))
  (if (or (empty? mat) (empty? (first mat)))
      '()
      (let ([m (length mat)]
            [n (length (first mat))])
        (anti-diagonal-traversal-helper m n (- (+ m n) 1) 0))))

(define (backwards mat)
  (map reverse mat))

(define traversal-methods
  (list identity
        transpose-matrix
        main-diagonal-traversal
        anti-diagonal-traversal
        backwards
        (compose backwards transpose-matrix)
        (compose backwards main-diagonal-traversal)
        (compose backwards anti-diagonal-traversal)))

(define (process-input str)
  (let ([mat (string->matrix str)])
    (apply + (map (lambda (m) (apply + (map count-xmas (m mat)))) traversal-methods))))

(define (main)
  (displayln (process-input (port->string (current-input-port)))))

(main)
