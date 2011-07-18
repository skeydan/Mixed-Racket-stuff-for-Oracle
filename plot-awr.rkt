#lang racket

(require "../oci4racket/main.rkt")

(define stat-tables
  '(("Historical statistics" (1 "dba_hist_sysstat") (2 "dba_hist_osstat") (3 "dba_hist_sys_time_model"))
    ("Historical metrics" (10 "dba_hist_sysmetric_summary"))))

(define firstsnap-query
  "select min(snap_id) first_snap, min(begin_interval_time) first_start, min(end_interval_time) first_end from dba_hist_snapshot where snap_flag = 0 -- take automatic, tuning pack enabled snaps only
and to_char(end_interval_time, 'D') = '2' -- start with Mondays always
and to_char(end_interval_time, 'hh24') = '00' -- first compared-to (= subtracted) snasphot is from 00:00h, so first shown value is for 01:00h")

(define lastsnap_query
  "select max(snap_id) last_snap, max(begin_interval_time) last_start, max(end_interval_time) last_end from dba_hist_snapshot where snap_flag = 0 -- take automatic, tuning pack enabled snaps only
and to_char(end_interval_time, 'D') = '1' -- get snap samples for whole weeks always, ending on Sunday
and to_char(end_interval_time, 'hh24') = '23' -- last compared-to (= subtracted) snasphot is from 23:00h, so last shown value is for 00:00h")

(define (get-value-opts tableno table stmt)
  (let ((cols-query (cond ((< tableno 10) (string-append "select distinct stat_name from " table " order by stat_name"))
                          ((< tableno 20) (string-append "select metric_name from " table " order by metric_name")))))
    (let* ((res (begin (executestmt stmt cols-query) (getresultset stmt)))
           (namelst
            (let loop ((res res) (lst '()))
              (if (not (fetchnext res))
                  (reverse lst)
                  (loop res (cons (getstring res 1) lst))))) 
           (namevec (list->vector namelst))
           (prompt 
            (let loop ((lst namelst) (str (string-append (first namelst) " [0]" )) (cnt 1) (startchar (first (string->list (first namelst)))))
              (if (null? (cdr lst))
                  (string-append str "\n")
                  (let* ((nextstr (first (cdr lst)))
                        (nextstartchar (first (string->list nextstr))))
                    (loop (cdr lst) (string-append str (if (char-ci=? startchar nextstartchar) "  ++  " "\n") nextstr " [" (number->string cnt) "]") (+ cnt 1) nextstartchar))))))
    (values namevec prompt))))

(define (format-category cat)
  (format "*** ~a ***~n~a~n" (car cat) (format-cat-tables (cdr cat))))

(define (format-cat-tables tables)
  (foldl (lambda (tab str) (string-append str (format "~a: ~a~n" (car tab) (cadr tab)))) "" tables))
  
(define (get-table-opts)
  (string-append "Choose a statistics table and enter the corresponding number: \n\n"
                          (foldl (lambda (cat str) (string-append str (format-category cat))) "" stat-tables)))

(define (accept prompt)
  (begin (display prompt) (read-line))) ; port->string needs ctrl-D on command line, read-line returns eof on commandline, read cannot process spaces...

(define (main)
  (let* ((conn (begin (init) (connect "orcl" "hr" "hr" 'oci_session_default)))
         (stmt (createstmt conn))
         (tableno (accept (get-table-opts)))
         (table (cadr (assoc (string->number tableno) (foldr append '() (map cdr stat-tables))))))
    (let-values (((namevec prompt) (get-value-opts (string->number tableno) table stmt)))
      (let* ((col (vector-ref namevec (string->number (accept prompt))))
             (snaps-result (begin (executestmt stmt firstsnap-query) (getresultset stmt)))
             (first-snap (and (fetchnext snaps-result) (getint2 snaps-result "first_snap")))
             (first-snap-time (getdate2 snaps-result "first_end")))
        first-snap-time))))

(main)
 ; TBD after columns: display first available snapshot info and ask what user wants: averaged over hours, 4-hour periods, days, or detail data for last x days