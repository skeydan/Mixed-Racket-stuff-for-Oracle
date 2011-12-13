#lang racket

(require plot)
(require "kerneldensity.rkt")
(require "../oci4racket/main.rkt")

(define (metric-description-query metric-name)
  (string-append
   "select distinct metric_unit from v$sysmetric where metric_name = '"
   metric-name
   "'"))

(define (summary-query metric-name)
  (string-append
   "select snap_id, begin_time, average, standard_deviation, minval, maxval, num_interval, intsize --intsize is intsize_csec (~6000 for group 2) * num_interval
   from dba_hist_sysmetric_summary
   where metric_name = '"
   metric-name
   "' order by snap_id"))

(define (history-query metric-name group-id)
  (string-append
   "select snap_id, begin_time, intsize, value -- intsize is intsize_csec (~6000 for group 2, ~1500 for group 3)
   from dba_hist_sysmetric_history
   where metric_name = '"
   metric-name
   "' and group_id= "
   (number->string group-id)
   " order by snap_id"))

(define mbps-summary-query
  "select min(snap_id) snap_id, min (begin_time) begin_time, sum(average) / (1024*1024) average, sum(standard_deviation)  / (1024*1024) standard_deviation -- MOST CERTAINLY THIS IS WRONG
   from dba_hist_sysmetric_summary
   where metric_name in ('Physical Read Total Bytes Per Sec', 'Physical Write Total Bytes Per Sec')
   group by snap_id
   order by snap_id")

(define iops-summary-query
  "select min(snap_id) snap_id, min (begin_time) begin_time, sum(average) average, sum(standard_deviation) standard_deviation -- MOST CERTAINLY THIS IS WRONG
   from dba_hist_sysmetric_summary
   where metric_name in ('Physical Read Total IO Requests Per Sec', 'Physical Write Total Total IO Requests Per Sec')
   group by snap_id
   order by snap_id")

(define (fetch-description metric-name stmt)
  (let ((res (and (executestmt stmt (metric-description-query metric-name)) (getresultset stmt))))
    (and (fetchnext res) (getstring2 res "metric_unit"))))

(define (fetch-summary-data query stmt)
  (let ((res (and (executestmt stmt query) (getresultset stmt))))
    (let loop ((snaps '()) (starts '()) (avgs '()) (stddevs '()))
      (if (eq? #f (fetchnext res))
          (list (reverse snaps) (reverse starts) (reverse avgs) (reverse stddevs))
          (loop (cons (getint2 res "snap_id") snaps) (cons (datetotext (getdate2 res "begin_time") "yyyy-mm-dd hh24:mi:ss" 30) starts) (cons (getdouble2 res "average") avgs) (cons (getdouble2 res "standard_deviation") stddevs))))))

(define (fetch-history-data metric-name group-id stmt)
  (let ((res (and (executestmt stmt (history-query metric-name group-id)) (getresultset stmt))))
    (let loop ((snaps '()) (starts '()) (vals '()))
      (if (eq? #f (fetchnext res))
          (list (reverse snaps) (reverse starts) (reverse vals))
          (loop (cons (getint2 res "snap_id") snaps) (cons (datetotext (getdate2 res "begin_time") "yyyy-mm-dd hh24:mi:ss" 30) starts) (cons (getdouble2 res "value") vals))))))

(define (plot-density data metric-name title #:bandwidth bandwidth)
  (parameterize ((plot-font-size 20))
    (plot-file (function (kd data  #:Kernel triweight #:bandwidth bandwidth) #:label metric-name)
               (string-append (regexp-replace* " " metric-name "") ".png")
               #:x-min (apply min data)
               #:x-max (apply max data)
               #:x-label metric-name 
               #:y-label "density"
               #:title title
               #:width 1200
               #:height 800
               )))

(define (main)
  (let* ((args (current-command-line-arguments))
         (connect-data (regexp-split #rx" +" (first (vector->list args))))
         (metric-name (second (vector->list args)))
         (conn (begin (init) (apply connect (append connect-data (list 'oci_session_default)))))
         (stmt (createstmt conn)))
    (let ((description (fetch-description metric-name stmt))
          (summary-data (fetch-summary-data (summary-query metric-name) stmt))
          (i60sec-data (fetch-history-data metric-name 2 stmt))
          (i15sec-data (fetch-history-data metric-name 3 stmt))
          (mbps-data (fetch-summary-data mbps-summary-query stmt))
          (iops-data (fetch-summary-data iops-summary-query stmt)))      
      (begin (connfree conn)
             (list
              (if (null? (third summary-data)) (printf "No data found in dba_hist_sysmetric_summary for metric: ~a~n" metric-name) (plot-density (third summary-data) metric-name (string-append "Kernel density diagram, sample size = " (number->string (length (first summary-data)))) #:bandwidth 1000000))
              (if (null? (third i60sec-data)) (printf "No data found in dba_hist_sysmetric_history for metric: ~a and interval size 60sec~n" metric-name) (plot-density (third i60sec-data) metric-name (string-append "Kernel density diagram, sample size = " (number->string (length (first i60sec-data)))) #:bandwidth 1000000))
              (if (null? (third i15sec-data)) (printf "No data found in dba_hist_sysmetric_history for metric: ~a and interval size 15sec~n" metric-name) (plot-density (third i15sec-data) metric-name (string-append "Kernel density diagram, sample size = " (number->string (length (first i15sec-data)))) #:bandwidth 1000000))
              (if (null? (third iops-data)) (printf "No data found in dba_hist_sysmetric_summary for calculation of total IOPS~n") (plot-density (third iops-data) "Total IOPS per Second" (string-append "Kernel density diagram, sample size = " (number->string (length (first iops-data)))) #:bandwidth 1000000))
              (if (null? (third mbps-data)) (printf "No data found in dba_hist_sysmetric_summary for calculation of total MBPS~a") (plot-density (third mbps-data) "Total IO Megabytes per Second" (string-append "Kernel density diagram, sample size = " (number->string (length (first mbps-data)))) #:bandwidth 1000000))
              )))))
  
(parameterize ((current-command-line-arguments
                (vector "orcl stat stat" 
                        ;"Redo Generated Per Sec"))
                        ;"I/O Requests per Second"))
                        ;"I/O Megabytes per Second"))
                        "Physical Read Total Bytes Per Sec"))
                        ;"Physical Write Total Bytes Per Sec"))
                        ;"Physical Read Total IO Requests Per Sec"))
                        ;"Physical Write Total IO Requests Per Sec"))
               (logfile "analyze-sysmetrics.log")
               (log-level (bitwise-ior 1 2 4)))
  (main))