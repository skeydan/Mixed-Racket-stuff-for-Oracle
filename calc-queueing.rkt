#lang racket
(require "distributions.rkt")
(provide (all-defined-out))

; utilization = (service time * arrival rate) / servers
(define (calc-utilization service-time arrival-rate servers-per-queue)
  (/ (* service-time arrival-rate) servers-per-queue))

(define (calc-service-time arrival-rate utilization servers-per-queue)
  (/ (* utilization servers-per-queue) arrival-rate))

; response time = service time / 1 - utilization (one-queue-per-server case) resp. utilization to the power of the servers
(define (calc-response-time service-time utilization servers-per-queue numqueues)
  (let ((rtime (/ service-time (- 1 (cond ((= numqueues 1) (expt utilization servers-per-queue))
                                          ((= servers-per-queue 1) utilization)
                                          (else (error "calc-response-time: not implemented - need to have either just 1 queue or one server per queue")))))))
    (if (<= rtime 0) +inf.0 rtime)))

(define (calc-rtime-from-qtime service-time queue-time)
  (+ service-time queue-time)) 

(define (calc-qtime-from-rtime response-time service-time)
  (- response-time service-time))

(define (calc-number-in-system arrival-rate response-time)
  (* arrival-rate response-time))

(define (calc-waiting-line-length arrival-rate queue-time)
  (* arrival-rate queue-time))

(define (calc-number-being-served arrival-rate service-time)
  (* arrival-rate service-time))

; servers-per-queue: CPU => num cpus, I/O => 1; numqueues: CPU => 1, I/O: num devices
(define (calc-qtime-erlangC service-time arrival-rate-system servers-per-queue numqueues)
  (define (erlangB qtr)
    (let loop ((result 1) (_m 1))
      (if (> _m servers-per-queue)
          result
          (loop (/ (* result qtr) (+ _m (* result qtr))) (+ _m 1)))))
  (define (erlangC eB ρ)
    (/ eB (+ (- 1 ρ) (* eB ρ))))      
  (let* ((arrival-rate-queue (/ arrival-rate-system numqueues)) 
         (queue-traffic (* arrival-rate-queue service-time)) 
         (utilization (/ (* service-time arrival-rate-queue) servers-per-queue)) 
         (eB (erlangB queue-traffic)) 
         (eC (erlangC eB (/ queue-traffic servers-per-queue))) 
         (queue-time (/ (* eC service-time) (* servers-per-queue (- 1 utilization)))))
    (if (< queue-time 0) +inf.0 queue-time)))

(define (compare-calcs service-time arrival-rate-system servers-per-queue numqueues)
  (let* ((utilization (calc-utilization service-time arrival-rate-system servers-per-queue))
        (rtime-simple (calc-response-time service-time utilization servers-per-queue numqueues))
        (qtime-simple (calc-qtime-from-rtime rtime-simple service-time))
        (nsys-simple (calc-number-in-system arrival-rate-system rtime-simple))
        (wline-simple (calc-waiting-line-length arrival-rate-system qtime-simple))
        (qtime-ec (calc-qtime-erlangC service-time arrival-rate-system servers-per-queue numqueues))
        (rtime-ec (calc-rtime-from-qtime service-time qtime-ec))
        (nsys-ec (calc-number-in-system arrival-rate-system rtime-ec))
        (wline-ec (calc-waiting-line-length arrival-rate-system qtime-ec))
        (num-served (calc-number-being-served arrival-rate-system service-time)))
    (printf "Response time calculation\n\nInput:\nService time:\t\t\t~a\nArrival rate (system):\t\t~a\nServers per queue:\t\t~a\nNo. of queues:\t\t\t~a\n\nOutput:\nUtilization:\t\t\t~a\nResponse time (eC/simple):\t~a / ~a\nQueue time (eC/simple):\t\t~a / ~a\nNo. in system (eC/simple):\t~a / ~a\nWait line length (eC/simple):\t~a / ~a\nNo. being served:\t\t~a\n\n" service-time arrival-rate-system servers-per-queue numqueues utilization rtime-ec rtime-simple qtime-ec qtime-simple nsys-ec nsys-simple wline-ec wline-simple num-served)))
    

;(compare-calcs 0.25 36 8 1)