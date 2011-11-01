#lang racket
(require (planet williams/science/statistics))
(require "../plt-stuff/plot/plot2d.rkt")

(define (epanechnikov u)
  (if (<= (abs u) 1) (* (/ 3 4) (- 1 (expt u 2))) 0))

(define (triweight u)
  (if (<= (abs u) 1) (* (/ 35 32) (expt (- 1 (expt u 2)) 3)) 0))

(define (get-standard-bandwidth data)
    (* 1.06 (expt (standard-deviation data) -1/5)))

(define (make-kd h K xs)
  (define (scale-and-shift t x)
    (/ (K (/ (- t x) h)) h))
  (lambda (t) (/ (foldl + 0 (map (lambda (x) (scale-and-shift t x)) xs)) (length xs))))

(define (kd data #:Kernel (K epanechnikov) #:bandwidth (h (get-standard-bandwidth data)))
  (make-kd h K data))

;(define data
;  '(1 2 2 2 3 4 5 5 5 5 5 5 6 7 8))
;
;(plot2d (function (kd data  #:Kernel triweight #:bandwidth 2)) #:x-min (apply min data) #:x-max (apply max data))

