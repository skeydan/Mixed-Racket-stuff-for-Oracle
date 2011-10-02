#lang racket
(require "../plt-stuff/plot/plot2d.rkt")
(require "distributions.rkt")
(require "calc-queueing.rkt")
(provide (all-defined-out))

(define (plot-rtime-for-arrivals service-time arr-min arr-max servers)
  (plot2d 
   (map (lambda (serv)
          (function (lambda (arr) (calc-response-time service-time (calc-utilization service-time arr serv) serv 1)) 0 arr-max)) servers)))

(plot-rtime-for-arrivals 0.25 0 30 (list 8 9 10 11))
  
