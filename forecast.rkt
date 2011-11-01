#lang racket
(require "../plt-stuff/plot/plot2d.rkt")
(require "../oci4racket/main.rkt")
(require "distributions.rkt")
(require "calc-queueing.rkt")
(provide (all-defined-out))

(struct waititem (name value))

(struct waitinfo (events classes total))

(define (print-iteminfos item text)
  (string-append text (format "~a:\t~a\n" (waititem-value item) (waititem-name item))))

(define unitsofwork
  '(("1" . "Logical I/O") ("2" . "user calls") ("3" . "execute count") ("4" . "Physical I/O")))

(define (ask-for-unitofwork)
  (begin (display (string-append "Choose unit of work for service time and arrivals:\n" (foldr (lambda (item res) (string-append (format "~a ~a\n" (car item) (cdr item)) res)) "" unitsofwork)))
         (cdr (assoc (read-line) unitsofwork))))

(define snaps-query
  "select snap_id, end_interval_time from dba_hist_snapshot where snap_flag = 0 order by snap_id -- take automatic, tuning pack enabled snaps only")

(define (get-snaps stmt)
  (let ((res (begin (executestmt stmt snaps-query) (getresultset stmt))))
    (let loop ((snaps '()))
      (if (eq? #f (fetchnext res))
          (foldl (lambda (x y) (string-append x y)) "" snaps)
          (loop (cons (string-append (number->string (getint2 res "snap_id")) " " (timestamptotext (gettimestamp2 res "end_interval_time") "yyyy-mm-dd hh24:mi" 30 0) "\n") snaps))))))

(define (build-stat-query statnames table)
  (let ((q (regexp-replace "&&table"
                           (regexp-replace "&&statnames"
                                           "with input as
(select t.snap_id snap,
t.stat_name,
lag (t.snap_id) over (partition by stat_name order by t.snap_id) prev_snap,
lag (t.value) over (partition by stat_name order by t.snap_id) prev_value,     
t.value value,
s.end_interval_time snap_end,
s.end_interval_time - lag(s.end_interval_time) over (partition by stat_name order by s.snap_id) time_diff,
t.value - lag (t.value) over (partition by stat_name order by t.snap_id) diff
from &&table t join dba_hist_snapshot s 
on t.snap_id=s.snap_id
where t.snap_id between :snap_id - 1 and :snap_id
and t.stat_name in &&statnames)
select sum(diff) val,
avg(extract (hour from time_diff) * 60 * 60 + extract (minute from time_diff) * 60 + extract (second from time_diff)) secs from input" statnames)
                           table))) q))

(define events-query
  "with events as
(select t.snap_id snap,
t.event_name event,
t.wait_class class,
lag (t.snap_id) over (partition by event_name order by t.snap_id) prev_snap,
lag (t.time_waited_micro) over (partition by event_name order by t.snap_id) prev_value,     
t.time_waited_micro time_waited_micro,
t.time_waited_micro - lag (t.time_waited_micro) over (partition by event_name order by t.snap_id) diff
from dba_hist_system_event t join dba_hist_snapshot s 
on t.snap_id=s.snap_id
where t.snap_id between :snap_id - 1 and :snap_id
and wait_class <> 'Idle'
order by (t.time_waited_micro - lag (t.time_waited_micro) over (partition by event_name order by t.snap_id)) desc nulls last),
ordered_events as
(select event, class, diff, sum(diff) over (partition by class) waitclass_total, sum(diff) over() grand_total from events where diff is not null order by diff desc)
select * from ordered_events where rownum < 11")

(define osstat-query
  "with busy as
(select t.snap_id snap,
lag (t.snap_id) over (partition by stat_name order by t.snap_id) prev_snap,
lag (t.value) over (partition by stat_name order by t.snap_id) prev_value,     
t.value value,
t.value - lag (t.value) over (partition by stat_name order by t.snap_id) diff
from dba_hist_osstat t join dba_hist_snapshot s 
on t.snap_id=s.snap_id
where t.snap_id between :snap_id - 1 and :snap_id
and t.stat_name='BUSY_TIME'),
cpus as
(select t.snap_id snap,
t.value value
from dba_hist_osstat t join dba_hist_snapshot s 
on t.snap_id=s.snap_id
where t.snap_id = :snap_id
and t.stat_name='NUM_CPUS')
select busy.diff busy, cpus.value cpus from busy join cpus on busy.snap=cpus.snap")

(define (get-work-and-interval snap unitofwork stmt) ; interval in seconds
  (let ((res (begin (prepare stmt (cond ((string=? unitofwork "Logical I/O") (build-stat-query "('db block gets', 'consistent gets')" "dba_hist_sysstat"))
                                        ((string=? unitofwork "Physical I/O") (build-stat-query "('physical read IO requests')" "dba_hist_sysstat"))
                                        ((ormap (lambda (str) (string=? unitofwork str)) '("user calls" "execute count")) (build-stat-query (string-append "('" unitofwork "')") "dba_hist_sysstat"))
                                        (else (error "not available"))))
                    (bindint stmt ":snap_id" snap) (execute stmt) (getresultset stmt))))
    (let ((work (and (fetchnext res) (getdouble2 res "val")))
          (interval (getint2 res "secs")))
      (values work interval))))

(define (get-total-cpu snap stmt) ; in μs
  (let ((res (begin (prepare stmt (build-stat-query "('DB CPU', 'background cpu time')" "dba_hist_sys_time_model"))  (bindint stmt ":snap_id" snap) (execute stmt) (getresultset stmt))))
    (and (fetchnext res) (getdouble2 res "val"))))

(define (get-osstat snap stmt)
  (let ((res (begin (prepare stmt osstat-query)  (bindint stmt ":snap_id" snap) (execute stmt) (getresultset stmt))))
    (let ((osbusy (and (fetchnext res) (getdouble2 res "busy")))
          (numcpus (getint2 res "cpus")))
      (values osbusy numcpus))))

(define (get-waitinfo snap stmt) ; in ms
  (let ((res (begin (prepare stmt events-query)  (bindint stmt ":snap_id" snap) (execute stmt) (getresultset stmt))))
    (let loop ((events '()) (classes '()) (total 0))
      (if (eq? #f (fetchnext res))
          (waitinfo (reverse events) (reverse classes) total)
          (let ((class (getstring2 res "class")))
            (loop (cons (waititem (getstring2 res "event") (/ (getdouble2 res "diff") 1000)) events)
                  (if (memf (lambda(c) (string=? class c)) (map waititem-name classes)) classes (cons (waititem class (/ (getdouble2 res "waitclass_total") 1000)) classes))
                  (if (= total 0) (/ (getdouble2 res "grand_total") 1000) total)))))))

(define (calc-rtime snap unitofwork stmt)
  (let-values (((work interval-secs) (get-work-and-interval snap unitofwork stmt))
               ((osbusy numcores) (get-osstat snap stmt))) ; in μs
    (let ((total-cpu-ms (/ (get-total-cpu snap stmt) 1000)) ; μs -> ms
          (total-osbusy-ms (* osbusy 10)) ; cs -> ms
          (waits (get-waitinfo snap stmt))) ; already in ms
      (let* ((λ (/ work (* 1000 interval-secs))) ; s -> ms
             (St (/ total-cpu-ms work)) ; ms
             (U-oracle (calc-utilization St λ numcores))
             (U-os (/ total-osbusy-ms (* numcores (* interval-secs 1000))))
             (Rt-from-U (/ St (- 1 (expt U-oracle numcores))))
             (Qt-from-U (- Rt-from-U St))
             (Qt-as-allwaits (waitinfo-total waits))
             (Qt-as-iowaits (foldl (lambda (item res) (+ res (if (or (string=? (waititem-name item) "User I/O") (string=? (waititem-name item) "System I/O")) (waititem-value item) 0))) 0 (waitinfo-classes waits)))   
             (Rt-from-st+allwaits (+ St (/ Qt-as-allwaits work)))
             (Rt-from-st+iowaits (+ St (/ Qt-as-iowaits work))))
        (printf "\n*** CPU Response time calculation ***\n\nUnit of work chosen: ~a\nInterval in seconds: ~a\nNumber of cores : ~a\nTotal work (arrivals): ~a\nArrival rate (work/ms): ~a\nTotal service (cpu) time (ms): ~a\nTotal OS cpu time (ms): ~a\nService time (ms/unit of work): ~a\nOracle utilization: ~a\nOS utilization: ~a\nResponse time from cpu utilization: ~a\nQueue time from cpu utilization: ~a\n\n*** Waits ***\n\nTop 10 wait events (ms):\n~a\nTop wait classes (ms):\n~a\nTotal time waited (ms): ~a\nResponse time as service + queue time: ~a\nTotal I/O waits (ms): ~a\nResponse time as service + queue time, I/O only: ~a\n" unitofwork interval-secs numcores work λ total-cpu-ms total-osbusy-ms St U-oracle U-os Rt-from-U Qt-from-U (foldl print-iteminfos "" (waitinfo-events waits)) (foldl print-iteminfos "" (waitinfo-classes waits)) Qt-as-allwaits Rt-from-st+allwaits Qt-as-iowaits Rt-from-st+iowaits)
        ;(plot-rtime-for-arrivals St 0 (* 10 λ) numcores)
        (plot-rtime-for-arrivals 1.15 0 10 16)))))
        

(define (plot-rtime-for-arrivals St λmin λmax numservers)
  (plot2d (function (lambda (λ) (calc-response-time St (calc-utilization St λ numservers) numservers 1)) λmin λmax)))

;(plot-rtime-for-arrivals 1.15 0 10 16)

(define (main)
  (let-values (((inst user pw) (vector->values (current-command-line-arguments))))
    (let* ((conn (begin (init) (connect inst user pw 'oci_session_default)))
           (stmt (createstmt conn))
           (unitofwork (ask-for-unitofwork))
           (snaps (regexp-split #rx" +" (begin (display (string-append "Enter snapshots to use for calculation:\n\n" (get-snaps stmt))) (read-line)))))
      (for-each (lambda (snap) (calc-rtime (string->number snap) unitofwork stmt)) snaps))))

(current-command-line-arguments (vector "orcl" "hr" "hr"))
(main)