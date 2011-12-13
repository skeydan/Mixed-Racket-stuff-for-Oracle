#lang racket

(require plot)
(require "../perftools/filters.rkt")
(require "../oci4racket/main.rkt")
(plot-tick-size 5)

(plot-new-window? #t)
(plot-font-size 16)

(define stat-tables
  '(("Historical statistics" (1 "dba_hist_sysstat") (2 "dba_hist_osstat") (3 "dba_hist_sys_time_model"))
    ("Historical metrics" (10 "dba_hist_sysmetric_summary"))))

(define firstsnap-query
  "select min(snap_id) first_snap, min(begin_interval_time) first_start, min(end_interval_time) first_end from dba_hist_snapshot where snap_flag = 0 -- take automatic, tuning pack enabled snaps only
and to_char(end_interval_time, 'D') = '2' -- start with Mondays always
and to_char(end_interval_time, 'hh24') = '00' -- first compared-to (= subtracted) snasphot is from 00:00h, so first shown value is for 01:00h")

(define lastsnap-base-query
  "select max(snap_id) last_snap, max(begin_interval_time) last_start, max(end_interval_time) last_end from dba_hist_snapshot where snap_flag = 0 -- take automatic, tuning pack enabled snaps only")

(define lastsnap-endofweek-query
  (string-append lastsnap-base-query 
                 "\nand to_char(end_interval_time, 'D') = '1' -- get snap samples for whole weeks always, ending on Sunday
                  and to_char(end_interval_time, 'hh24') = '23' -- last compared-to (= subtracted) snasphot is from 23:00h, so last shown value is for 00:00h"))
  
(define base-stat-query
  "with input as 
     (select lag (t.snap_id) over (order by t.snap_id) prev_snap,
             lag (t.value) over (order by t.snap_id) prev_value,
             t.snap_id snap,
             t.value value,
             s.end_interval_time snap_end,
             trunc (s.end_interval_time, 'hh24') - trunc(lag(s.end_interval_time) over (order by s.snap_id), 'hh24')  time_diff,
             t.value - lag (t.value) over (order by t.snap_id) diff
      from &&tablename t join dba_hist_snapshot s 
      on t.snap_id=s.snap_id
      where t.snap_id between :first_snap and :last_snap+1
      and t.stat_name = '&&statname'
      order by t.snap_id),
        cleaned_input as
         (select snap,
                 snap_end,
                 diff
        from input
        where diff is not null -- because of lag
        and diff >= 0 -- because of possible instance bounces
        and time_diff = 1/24)")

(define base-metric-query
  "with cleaned_input as 
     (select t.snap_id snap,
             t.average avg_,
             t.standard_deviation stddev_,
             s.end_interval_time snap_end
      from &&tablename t join dba_hist_snapshot s 
      on t.snap_id=s.snap_id
      where t.snap_id between :first_snap and :last_snap+1
      and t.metric_name = '&&statname'
      order by t.snap_id)")

(define (agg-by-hour-query base-query)
  (string-append base-query 
                 "\nselect trim(to_char(snap_end, 'DY')) day,
                           extract(hour from snap_end) time,
                           round(avg(diff),0) val
	            from cleaned_input
	            group by to_char(snap_end, 'DY'), to_char(snap_end, 'D'), extract(hour from snap_end)
	            order by decode(to_char(snap_end, 'D'), 0, 7, to_char(snap_end, 'D')), 2"))


(define (metric-agg-by-hour-query base-query)
  (string-append base-query 
                 "\nselect trim(to_char(snap_end, 'DY')) day,
                           extract(hour from snap_end) time,
                           round(avg(avg_),0) val
	            from cleaned_input
	            group by to_char(snap_end, 'DY'), to_char(snap_end, 'D'), extract(hour from snap_end)
	            order by decode(to_char(snap_end, 'D'), 0, 7, to_char(snap_end, 'D')), 2"))

(define (detail-query base-query)
  (string-append base-query
                 "\nselect trim(to_char(snap_end, 'DY')) day,
                           extract(hour from snap_end) time,
                           diff val
	            from cleaned_input
	            order by snap"))

(define (metric-detail-query base-query)
  (string-append base-query
                 "\nselect trim(to_char(snap_end, 'DY')) day,
                           extract(hour from snap_end) time,
                           avg_ val,
                           stddev_ stddev_
	            from cleaned_input
	            order by snap"))

(define agg-modes
  '((1 "averaged by weekday and hour") (2 "by-hour data for last week") (3 "by-hour data for last 3 days")))

(define (get-value-opts tableno table stmt)
  (let ((cols-query (cond ((= tableno 2) (string-append "select distinct stat_name from " table " where stat_id in (select osstat_id from v$osstat where cumulative='YES') order by stat_name"))
                          ((< tableno 10) (string-append "select distinct stat_name from " table " order by stat_name"))
                          ((< tableno 20) (string-append "select distinct metric_name from " table " order by metric_name")))))
    (let* ((res (begin (executestmt stmt cols-query) (getresultset stmt)))
           (namelst
            (let loop ((res res) (lst '()))
              (if (not (fetchnext res))
                  (reverse lst)
                  (loop res (cons (getstring res 1) lst))))) 
           (namevec (list->vector namelst))
           (prompt 
            (let loop ((lst namelst)
                       (str "")
                       (cnt 0))
              (if (null? (cdr lst))
                  str
                  (loop (cdr lst) (string-append str (string-downcase (first lst)) " [" (number->string cnt) "]\n") (+ cnt 1))))))
    (values namevec (string-append "\n\nChoose a statistic (enter number):\n\n" prompt)))))

(define (format-category cat)
  (format "*** ~a ***~n~a~n" (car cat) (format-cat-tables (cdr cat))))

(define (format-cat-tables tables)
  (foldl (lambda (tab str) (string-append str (format "~a: ~a~n" (car tab) (cadr tab)))) "" tables))
  
(define (get-table-opts)
  (string-append "Choose a statistics table and enter the corresponding number: \n\n"
                          (foldl (lambda (cat str) (string-append str (format-category cat))) "" stat-tables)))

(define (accept prompt)
  (begin (display prompt) (read-line))) 

(define (construct-query query table col)
  (regexp-replace "&&tablename" (regexp-replace "&&statname" query col) table))

(define (build-plot-data resultset)
  (let* ((data (get-data resultset))
         (xs (x-axis-values (car data) (cadr data)))
         (raw-values (caddr data)))
    (vectorize-for-plotting xs raw-values)))

(define (get-data resultset)
  (let loop ((days '()) (hours '()) (vals '()))
    (if (eq? #f (fetchnext resultset))
        (list (reverse days) (reverse hours) (reverse vals))
        (loop (cons (getstring2 resultset "day") days) (cons (getint2 resultset "time") hours) (cons (getdouble2 resultset "val") vals)))))

(define (vectorize-for-plotting x-axis vals)
  (foldr (lambda (x y result) (cons (vector x y) result)) '() x-axis vals))

(define (x-axis-values days hours)
  (foldr (lambda (day hour result)
           (cons (string-append (case hour ((0) (string-append day "")) (else ""))
                                (case hour ((4 8 12 16 20) (format "~a" hour)) (else "")))
                 result))
         '() days hours))
  
(define (main)
  (let* ((conn (begin (init) (connect "orcl" "hr" "hr" 'oci_session_default)))
         (stmt (createstmt conn))
         (tableno (accept (get-table-opts)))
         (table (cadr (assoc (string->number tableno) (foldr append '() (map cdr stat-tables))))))
    (let-values (((namevec prompt) (get-value-opts (string->number tableno) table stmt)))
      (let* ((col (vector-ref namevec (string->number (accept prompt))))
             (base-query (case (string->number tableno) ((1 2 3) base-stat-query) ((10) base-metric-query)))
             (firstsnap-result (begin (executestmt stmt firstsnap-query) (getresultset stmt)))
             (first-snap (and (fetchnext firstsnap-result) (getint2 firstsnap-result "first_snap")))
             (first-snap-time (timestamptotext (gettimestamp2 firstsnap-result "first_end") "yyyy-mm-dd hh24:mi:ss" 30 0))
             (lastsnap-result (begin (executestmt stmt lastsnap-base-query) (getresultset stmt)))
             (last-snap (and (fetchnext lastsnap-result) (getint2 lastsnap-result "last_snap")))
             (lastsnap-endofweek-result (begin (executestmt stmt lastsnap-endofweek-query) (getresultset stmt)))
             (last-endofweek-snap (and (fetchnext lastsnap-endofweek-result) (getint2 lastsnap-endofweek-result "last_snap")))
             (agg-mode (accept (string-append (format "\n\nFirst available snapshot is from ~a.\nHow do you want the data displayed?\n\n" first-snap-time) (foldl (lambda (mode str) (string-append str (format "~a: ~a~n" (car mode) (cadr mode)))) "" agg-modes)))))
        (let ((data
               (cond ((< (string->number tableno) 10)
                      (case (string->number agg-mode)
                        ((1) (build-plot-data (and (prepare stmt (construct-query (agg-by-hour-query base-stat-query) table col)) (bindint stmt ":first_snap" first-snap) (bindint stmt ":last_snap" last-endofweek-snap) (execute stmt) (getresultset stmt))))
                        ((2) (build-plot-data (and (prepare stmt (construct-query (detail-query base-stat-query) table col)) (bindint stmt ":first_snap" (- last-snap (* 24 7))) (bindint stmt ":last_snap" last-snap) (execute stmt) (getresultset stmt))))
                        ((3) (build-plot-data (and (prepare stmt (construct-query (detail-query base-stat-query) table col)) (bindint stmt ":first_snap" (- last-snap (* 24 3))) (bindint stmt ":last_snap" last-snap) (execute stmt) (getresultset stmt))))))
                     ((< (string->number tableno) 20)
                      (case (string->number agg-mode)
                        ((1) (build-plot-data (and (prepare stmt (construct-query (metric-agg-by-hour-query base-metric-query) table col)) (bindint stmt ":first_snap" first-snap) (bindint stmt ":last_snap" last-endofweek-snap) (execute stmt) (getresultset stmt))))
                        ((2) (build-plot-data (and (prepare stmt (construct-query (metric-detail-query base-metric-query) table col)) (bindint stmt ":first_snap" (- last-snap (* 24 7))) (bindint stmt ":last_snap" last-snap) (execute stmt) (getresultset stmt))))
                        ((3) (build-plot-data (and (prepare stmt (construct-query (metric-detail-query base-metric-query) table col)) (bindint stmt ":first_snap" (- last-snap (* 24 3))) (bindint stmt ":last_snap" last-snap) (execute stmt) (getresultset stmt)))))))))
          (begin (and (connfree conn) clean)
                 (plot-file (discrete-histogram data #:label (string-downcase col))
                               (string-append (string-downcase col) ".png")
                               #:width 1400
                               #:height 800
                               #:title (string-downcase col)
                               #:x-label "Hour"
                               #:y-label (string-downcase col))
                 ;data
                 ))))))

(parameterize ((logfile (build-path (current-directory) "plot-awr.log"))
               (log-level (bitwise-ior 1 2 4)))
  (main))