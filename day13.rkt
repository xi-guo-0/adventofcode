#lang racket

(require math/matrix)

(struct Machine (a b prize) #:transparent)
(struct Dist (x y) #:transparent)

(define BUTTON-REGEX #px"Button .: X\\+(\\d+), Y\\+(\\d+)")
(define PRIZE-REGEX #px"Prize: X=(\\d+), Y=(\\d+)")
(define RECALIBRATION-OFFSET 10000000000000)

(define (parse-coordinates regex line)
  (match-define (list (list (app string->number x) (app string->number y)))
    (regexp-match* regex line #:match-select cdr))
  (Dist x y))

(define (parse-machine machine-text)
  (match-define (list button-a button-b prize) (string-split machine-text "\n"))

  (Machine (parse-coordinates BUTTON-REGEX button-a)
           (parse-coordinates BUTTON-REGEX button-b)
           (parse-coordinates PRIZE-REGEX prize)))

(define (parse-input)
  (for/list ([machine-text (string-split (port->string (current-input-port)) "\n\n")])
    (parse-machine machine-text)))

(define (solve-machine-equation ax ay bx by prize-x prize-y)
  (let* ([solution-matrix (matrix-solve (matrix [[ax bx] [ay by]]) (col-matrix [prize-x prize-y]))]
         [solution (and solution-matrix (matrix->list solution-matrix))])
    (and solution (andmap integer? solution) solution)))

(define (calculate-price solution)
  (+ (* 3 (car solution)) (cadr solution)))

(define (find-price machine)
  (match-let ([(Machine (Dist ax ay) (Dist bx by) (Dist prize-x prize-y)) machine])
    (let ([solution (solve-machine-equation ax ay bx by prize-x prize-y)])
      (if solution
          (calculate-price solution)
          0))))

(define (recalibrate machine)
  (match-let ([(Machine a b (Dist prize-x prize-y)) machine])
    (Machine a b (Dist (+ RECALIBRATION-OFFSET prize-x) (+ RECALIBRATION-OFFSET prize-y)))))

(define MACHINES (parse-input))

(foldl + 0 (map find-price MACHINES))

(foldl + 0 (map (compose find-price recalibrate) MACHINES))
