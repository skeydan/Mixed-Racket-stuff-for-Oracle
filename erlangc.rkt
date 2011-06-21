#lang racket

; Arguments: 
; 1) S: service time (ms/tx)
; 2) λsys: system arrival rate (trx/ms)
; 3) m: number of servers FOR EACH queue (cpu: no. of cpus, I/O: 1)
; 4) Qn: number of queues (cpu: 1, I/O: no. of devices)
(define (calc-queuetime #:St St #:λsys λsys #:m m #:Qn Qn)
  (define (erlangB qtr)
    (let loop ((result 1) (_m 1))
      (if (> _m m)
          result
          (loop (/ (* result qtr) (+ _m (* result qtr))) (+ _m 1)))))
  (define (erlangC eB ρ)
    (/ eB (+ (- 1 ρ) (* eB ρ))))      
  (let* ((λq (/ λsys Qn)) ; queue arrival rate
         (qtr (* λq St)) ; queue traffic
         (u (/ (* St λq) m)) ; utilization
         (eB (erlangB qtr)) ; erlangB
         (eC (erlangC eB (/ qtr m))) ;erlangC
         (Qt (/ (* eC St) (* m (- 1 u)))) ; queue time
         (Rt (+ St Qt)) ;  response time
         (Ql (* λq Qt)) ; queue length
        )
  (printf "Given:~nSystem arrival rate: ~s~nNumber of queues: ~s~nNumber of servers per queue: ~s~nService time: ~s~n~n"
          λsys Qn m St)
  (printf "Result: ~nQueue arrival rate: ~s~nQueue traffic: ~s~nUtilization: ~s~nErlangC: ~s~nQueue time: ~s~nResponse time: ~s~nQueue length: ~s~n"
          λq qtr u eC Qt Rt Ql)))
  
  
(calc-queuetime #:St 4.0 #:λsys 5.0 #:m 24 #:Qn 1)
  
 