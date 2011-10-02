#lang racket
(require "distributions.rkt")
(require "erlangc.rkt")

(define (simulate service-time-λ arrival-rate-λ servers-per-queue numqueues n #:use-erlangC (use-erlangC #f))
  (let ((avg-service-time (/ (foldl + 0 (get-random-variable 'exponential service-time-λ n)) n))
        (avg-arrival-rate (/ (foldl + 0 (get-random-variable 'exponential arrival-rate-λ n)) n)))
    (printf "Simulating response time calculation based on exponential arrival rate and exponential service time.\nGiven: service time λ = ~a, arrival rate λ = ~a, servers per queue = ~a, no. of queues = ~a.\nSimulating ~a arrivals.\n\nAverage service time is: ~a\nAverage arrival rate is: ~a\n\nNow calling response time calculation...\n\n" service-time-λ arrival-rate-λ servers-per-queue numqueues n avg-service-time avg-arrival-rate)
    (if (eq? use-erlangC #f)
        (print-simple-queueing avg-service-time avg-arrival-rate servers-per-queue numqueues)
        (print-erlangC  avg-service-time avg-arrival-rate servers-per-queue numqueues))))

(simulate 0.8 0.2 16 1 100)
