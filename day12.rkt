#lang racket

(require posn ;package: jack-posn
         threading)

(define (string->posn-grid str [f identity])
  (for*/hash ([(row y) (in-indexed (string-split str))]
              [(col x) (in-indexed row)])
    (values (posn x y) (f col))))

(define in-cardinal-directions (list (posn 1 0) (posn -1 0) (posn 0 1) (posn 0 -1)))

(define in-all-directions
  (list (posn -1 0)
        (posn -1 1)
        (posn 0 1)
        (posn 1 1)
        (posn 1 0)
        (posn 1 -1)
        (posn 0 -1)
        (posn -1 -1)))

(define (neighbors-of p directions)
  (map (curry posn-add p) directions))

(define (flood-fill grid stack seen plant)
  (match stack
    ['() seen]
    [(list* next rest)
     #:when (or (set-member? seen next) (not (equal? plant (hash-ref grid next #f))))
     (flood-fill grid rest seen plant)]
    [(list* next rest)
     (flood-fill grid
                 (append rest (neighbors-of next in-cardinal-directions))
                 (set-add seen next)
                 plant)]))
(define (find-contiguous-regions [grid grid] [acc '()])
  (match (hash-iterate-first grid)
    [#f acc]
    [n
     (define-values (pos plant) (hash-iterate-key+value grid n))
     (define region (flood-fill grid (list pos) (set) plant))
     (define trimmed-grid (sequence-fold (λ (acc k) (hash-remove acc k)) grid region))
     (find-contiguous-regions trimmed-grid (cons region acc))]))

(define area set-count)

(define (perimeter region)
  (for/sum ([pos (in-set region)])
           (count (negate (λ (n) (set-member? region n))) (neighbors-of pos in-cardinal-directions))))

(define (sides region)
  (for*/sum ([pos (in-set region)] ;
             #:do [(match-define (list u ur r dr d dl l ul)
                     (map (λ~>> (set-member? region)) (neighbors-of pos in-all-directions)))
                   (define vertices
                     (list (and (not u) (not l))
                           (and (not u) (not r))
                           (and (not d) (not l))
                           (and (not d) (not r))
                           (and u l (not ul))
                           (and u r (not ur))
                           (and d l (not dl))
                           (and d r (not dr))))]
             [vertex (in-list vertices)]
             #:when vertex)
            1))

(define grid (string->posn-grid (port->string (current-input-port))))
(define regions (find-contiguous-regions))

(define (score metric)
  (for/sum ([region (in-list regions)]) (* (area region) (metric region))))

(score perimeter)
(score sides)
