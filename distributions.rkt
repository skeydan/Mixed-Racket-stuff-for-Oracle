#lang racket
(require "../plt-stuff/plot/plot2d.rkt")
(require "../plt-stuff/plot/plot3d.rkt")
(provide (all-defined-out))

(define (make-exponential λ generator)
  (lambda () (* (/ -1 λ) (log (random generator)))))

(define (make-bernoulli p generator)
  (lambda () (let ((U (random generator)))
    (if (< U p) 1 0))))

(define (make-geometric p generator)
  (lambda () (let* ((λ (- (log (- 1 p))))
                    (get-exp (make-exponential λ generator)))
               (ceiling (get-exp)))))

(define (make-uniform low high generator)
  (lambda () (let ((U (random generator)))
               (+ low (* (- high low) U)))))

(define (get-random-variable distribution param n #:generator (generator (current-pseudo-random-generator)) #:round-to (round-to 2))
  (let ((fun (match distribution
               ('exponential (make-exponential param generator))
               ('bernoulli (make-bernoulli param generator))
               ('geometric (make-geometric param generator))
               ('uniform (make-uniform (car param) (cadr param) generator)))))
    (let loop ((lst '()) (cnt 0))
      (if (>= cnt n) (reverse lst)
          (loop (cons (/ (round (* (fun) (expt 10 round-to))) (expt 10 round-to)) lst) (+ 1 cnt))))))

(define (compare-params type data given given-name)
  (let ((avg (exact->inexact (/ (foldl + 0 data) (length data)))))
    (printf "Simulated ~a distribution.~nDefined ~a:\t ~a~nSample mean:\t ~a~nEmpirical ~a:\t ~a~nSample size:\t ~a~n"
            type (symbol->string given-name) given avg (symbol->string given-name)
            (match type
              ('exponential (/ 1 avg))
              ('bernoulli (/ avg (length data)))
              ('geometric (/ 1 avg))
              ('uniform (/ (- (cadr given) (car given)) 2.0)))
            (length data))))

(define (render-exponential λ #:factor (factor 1.5) #:color (color "black"))
  (let ((variance (/ 1 (expt λ 2))))
    (function (lambda (x) (if (< x 0) 0 (* λ (exp (* -1 λ x))))) 0 (* variance factor) #:color color)))

(define (render-geometric p n #:color (color "black"))
  (points (map vector (sequence->list (in-range 1 (+ n 1)))  (build-list n (lambda (x) (* (expt (- 1 p) (- (+ x 1) 1)) p)))) #:color color))

(define (render-pareto a #:min (min 1) #:max (max 10) #:color (color "black"))
  (function (lambda (x) (if (< x 1) 0 (/ a (expt x (+ a 1))))) min max #:color color))

(define (show-examples)
  (let ((data (get-random-variable 'exponential 0.25 50 #:round-to 4)))
    (compare-params 'exponential data 0.25 'λ))
  (newline)
  (let ((data (get-random-variable 'geometric 0.25 50 #:round-to 4)))
    (compare-params 'geometric data 0.25 'p))
  (newline)
  (let ((data (get-random-variable 'bernoulli 0.2 50)))
    (compare-params 'bernoulli data 0.2 'p))
  (newline)
  (let ((data (get-random-variable 'uniform (list 0 5) 20)))
  (compare-params 'uniform data (list 0 5) 'mean))
  (newline)           
  (plot2d (list (render-exponential 0.25 #:color "red") (render-geometric 0.25  20 #:color "yellow") (render-exponential 0.5 #:color "orange") (render-geometric 0.5  20 #:color "red") (render-exponential 0.75 #:color "green") (render-geometric 0.75  20 #:color "black")))
  (plot2d (list (render-pareto 1 #:color "green") (render-pareto 2 #:color "yellow") (render-pareto 3 #:color "red"))))

;(show-examples)

