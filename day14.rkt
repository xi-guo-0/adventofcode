#lang racket

(require posn ;package: jack-posn
         2htdp/image)

(define WIDTH 101)
(define HEIGHT 103)

(define (extract-position-velocitie str)
  (define pattern #px"-?\\d+")
  (let ([numbers (map string->number (regexp-match* pattern str))])
    (list (posn (first numbers) (second numbers)) (posn (third numbers) (fourth numbers)))))

(define robots (map extract-position-velocitie (port->lines (current-input-port))))

(define (move p v width height seconds)
  (let ([np (posn-add p (posn-scale seconds v))])
    (posn (modulo (posn-x np) width) (modulo (posn-y np) height))))

(define (simulate width height seconds)
  (let ([mw (quotient width 2)]
        [mh (quotient height 2)]
        [ps (for/list ([position-velocitie robots])
              (move (first position-velocitie) (second position-velocitie) width height seconds))])
    (apply *
           (hash-values
            (foldl (lambda (p ht) (hash-update ht (list (< (posn-x p) mw) (< (posn-y p) mh)) add1 0))
                   #hash()
                   (filter (lambda (p) (and (not (= (posn-x p) mw)) (not (= (posn-y p) mh)))) ps))))))

(simulate 101 103 100)

(define (draw-pixel-art m n points)
  (define background (empty-scene n m))
  (define (color-pixel img x y)
    (place-image (circle 2 "solid" "black") x y img))
  (define final-image
    (foldl (lambda (point img) (color-pixel img (posn-x point) (posn-y point))) background points))
  final-image)

(define (messiness robots)
  (define pos (map car robots))
  (define xs (map posn-x pos))
  (define ys (map posn-y pos))
  (define x-mean (/ (apply + xs) (length xs)))
  (define y-mean (/ (apply + ys) (length ys)))
  (define x-var (apply + (map (λ (x) (expt (- x x-mean) 2)) xs)))
  (define y-var (apply + (map (λ (y) (expt (- y y-mean) 2)) ys)))
  (* x-var y-var))

(define (move-robot robot)
  (let ([np (posn-add (first robot) (second robot))])
    (list (posn (modulo (posn-x np) WIDTH) (modulo (posn-y np) HEIGHT)) (second robot))))

(define (move-robots robots)
  (map move-robot robots))

(define (search-with-break robots n threshold)
  (let ([m (messiness robots)])
    (cond
    [(< m 37481863450) n]
    [(> n threshold) n]
    [else (search-with-break (move-robots robots) (add1 n) threshold)])))

(search-with-break robots 0 7413)
