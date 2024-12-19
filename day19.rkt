#lang racket

(require memo)

(define inputs (string-split (port->string (current-input-port)) "\n\n"))

(define stripes (string-split (string-replace (car inputs) " " "") ","))

(define towels (string-split (cadr inputs) "\n"))

(define (arrange stripes towel)
  (if (zero? (string-length towel))
      #t
      (for/or ([stripe stripes])
        (and (string-prefix? towel stripe)
             (arrange stripes (substring towel (string-length stripe)))))))

(length (filter identity (map (curry arrange stripes) towels)))

(define/memoize (ways stripes towel)
                (if (zero? (string-length towel))
                    1
                    (for/sum ([stripe stripes])
                             (if (string-prefix? towel stripe)
                                 (ways stripes (substring towel (string-length stripe)))
                                 0))))

(apply + (map (curry ways stripes) towels))
