#lang racket

(require plot)
(require (planet williams/science/statistics))

(provide epanechnikov triweight kd)

(define (epanechnikov u)
  (if (<= (abs u) 1) (* (/ 3 4) (- 1 (expt u 2))) 0))

(define (triweight u)
  (if (<= (abs u) 1) (* (/ 35 32) (expt (- 1 (expt u 2)) 3)) 0))

(define (get-standard-bandwidth data)
  (let ((stddev (standard-deviation data)))
    (* 1.06 (expt stddev -0.2))))

(define (make-kd h K xs)
  (define (scale-and-shift t x)
    (/ (K (/ (- t x) h)) h))
  (lambda (t) (/ (foldl + 0 (map (lambda (x) (scale-and-shift t x)) xs)) (length xs))))

(define (kd data #:Kernel (K triweight) #:bandwidth (h (get-standard-bandwidth data)))
  (make-kd h K data))

;(define data
;  '(1.22 2.33 2.44 2.55 3.55 4.66 5.66 5.66 5.66 5.66 5.66 5.77 6.66 7.66 8.66))
;
;(plot (function (kd data  #:Kernel triweight #:bandwidth 2)) #:x-min (apply min data) #:x-max (apply max data))
;(plot (function (kd data  #:Kernel triweight)) #:x-min (apply min data) #:x-max (apply max data))
;(plot (function (kd data  #:Kernel epanechnikov #:bandwidth 2)) #:x-min (apply min data) #:x-max (apply max data))
;(plot (function (kd data  #:Kernel epanechnikov)) #:x-min (apply min data) #:x-max (apply max data))

