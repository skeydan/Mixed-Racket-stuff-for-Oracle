#lang racket
(require ffi/unsafe)
(require (prefix-in sr19: srfi/19))
(provide gettimeofday μsecs->date)

(define libsys (ffi-lib "libsystem"))

(define-cstruct _timeval ((secs _long) (μsecs _int)))
(define-cstruct _timezone ((minswest _int) (dsttime _bool)))

(define gettimeofday (get-ffi-obj "gettimeofday" libsys (_fun (tm : (_ptr o _timeval)) (tz : (_ptr o _timezone)) -> _int -> (values (timeval-secs tm) (timeval-μsecs tm) (timezone-minswest tz) (timezone-dsttime tz)))))

(define (μsecs->date μsecs)
  (sr19:date->string (sr19:time-utc->date (sr19:make-time 'time-utc (remainder μsecs 1000000) (quotient μsecs 1000000))) "~4"))

(define (test)
  (let-values (((secs μsecs tzmins dst) (gettimeofday)))
    (μsecs->date (+ (* secs 1000000) μsecs))))



