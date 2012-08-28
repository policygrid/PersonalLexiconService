#|----------------------------------------------------------------------------|
 | Copyright 2002, 2006, 2011 John Carroll, Ted Briscoe, Rebecca Watson       |
 |                                                                            |
 | This file is part of RASP.                                                 |
 |                                                                            |
 | RASP is free software: you can redistribute it and/or modify it            |
 | under the terms of the GNU Lesser General Public License as published      |
 | by the Free Software Foundation, either version 3 of the License, or       |
 | (at your option) any later version.                                        |
 |                                                                            |
 | RASP is distributed in the hope that it will be useful,                    |
 | but WITHOUT ANY WARRANTY; without even the implied warranty of             |
 | MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              |
 | GNU Lesser General Public License for more details.                        |
 |                                                                            |
 | You should have received a copy of the GNU Lesser General Public License   |
 | along with RASP.  If not, see <http://www.gnu.org/licenses/>.              |
 |----------------------------------------------------------------------------|#

;;; LR1 parse table recomputation after GDE grammar change and table
;;; inspection tools.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;;

(defstruct item
   cfrule-index dot-number mother-index)


;;; Global

(defvar *productions*)
(defvar *terminals*)
(defvar *non-terminals*)
(defvar *terminal-categories*)
(defvar *null-categories*)
(defvar *item-sets*)
(defvar *index-items*)
(defvar *item-lookahead-sets*)
(defvar *state-actions*)
(defvar *shift-action-vector*)
(defvar *state-action-probs*)

(defvar *sentence-end-marker*)


;;; From GDE

(defvar *id-rules nil)
(defvar *chart-edges)


;;; Timeouts
;;;
;;; (execute-with-timeout (format t "Halted~%") (values 1 (cons 3 2)))

(defparameter +parse-timeout+ 20) ; in seconds cpu time

#+(or allegro sbcl)
(defparameter +parse-storeout+
   ;; 800MB (64 bit) or 400MB (32 bit)
   (if (> most-positive-fixnum (ash 2 32)) 800000000 400000000))


#|

#+openmcl
(defmacro execute-with-timeout (timeout-form &body body)
   `(let*
       (res
        (sem (ccl:make-semaphore))
        (process
           (ccl:process-run-function "timeout-computation"
              #'(lambda (s)
                 (setq res (multiple-value-list (progn ,@body)))
                 (ccl:signal-semaphore s))
              sem)))
       (if (ccl:timed-wait-on-semaphore sem +parse-timeout+)
          (values-list res)
          (progn
             (process-kill process)
             (format *error-output* "~%Timed out at ~A secs~%" +parse-timeout+)
             ,timeout-form))))
|#

#|
http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/portableaserve/portableaserve/acl-compat/acl-mp-mcl.lisp?rev=1.4&content-type=text/vnd.viewcvs-markup

;;; This file implements the process functions for AllegroServe in MCL.
;;; Based on the the work done for cmucl and Lispworks.
;;;
;;; John DeSoi, Ph.D. desoi@mac.com

File: [SourceForge] / portableaserve / portableaserve / acl-compat / acl-mp-mcl.lisp (download)
Revision 1.4 , Tue Apr 30 03:36:31 2002 UTC (3 weeks, 1 day ago) by desoi
Branch: MAIN
CVS Tags: HEAD
Changes since 1.3: +15 -0 lines 

Added new timer implementation from Gary Byers.
|#

#+mcl
(progn
(defmacro execute-with-timeout (timeout-form &body body)
  `(invoke-with-timeout +parse-timeout+ #'(lambda () ,@body)
                        #'(lambda ()
                            (format *error-output* "~%Timed out at ~A secs~%" +parse-timeout+)
                            ,timeout-form)))

(defun invoke-with-timeout (seconds bodyfn timeoutfn)
  (block timeout
    (let* ((timer (ccl::make-timer-request
                    seconds
                    #'(lambda () (return-from timeout (funcall timeoutfn))))))
      (ccl::enqueue-timer-request timer)
      (unwind-protect (funcall bodyfn)
	(ccl::dequeue-timer-request timer)))))

(pushnew
  '(ccl::%install-periodic-task
    'ccl::process-timer-requests
    'ccl::process-timer-requests
    (ccl::ticks-per-second))
  *gde-init-forms* :test #'equalp)
)


#+allegro
(progn
(define-condition parse-timeout (simple-condition) ())
(define-condition parse-storeout (simple-condition) ())
(defmacro execute-with-timeout (timeout-form &body body)
   ;; version of built-in with-timeout macro, roughly equivalent to
   ;; `(mp:with-timeout (+parse-timeout+ ,timeout-form) ,@body)
   ;; but better in that GC time is not counted in timeout, and with
   ;; addition of facility for giving a memory quota, in +parse-storeout+
   `(let* ((.vals. nil)
           (.parse-timeout.
              (if (and (numberp +parse-timeout+) (> +parse-timeout+ 0))
                 (+ (* +parse-timeout+ internal-time-units-per-second)
                    (get-internal-run-time)
                    (- (nth-value 2 (excl::get-internal-run-times)))) ; gc time
                 nil))
           (.parse-storeout.
              (if (and (numberp +parse-storeout+) (> +parse-storeout+ 0))
                 +parse-storeout+
                 nil))
           (.continuation. excl:*gc-after-hook*)
           (excl:*gc-after-hook*
              #'(lambda (global new old efficiency to-be-allocated)
                   (when (and .parse-timeout.
                            (> (- (get-internal-run-time)
                                  (nth-value 2 (excl::get-internal-run-times)))
                               .parse-timeout.))
                      (signal 'parse-timeout))
                   (when .parse-storeout.
                      (decf .parse-storeout. new)
                      (when (< .parse-storeout. 0) (signal 'parse-storeout)))
                   (when .continuation.
                      (funcall .continuation. global new old efficiency to-be-allocated)))))
         (setf (sys:gsgc-switch :hook-after-gc) t)
         (handler-case
             (setq .vals. (multiple-value-list (progn ,@body)))
            (parse-timeout ()
               (setf (sys:gsgc-switch :hook-after-gc) nil)
               (format *error-output* "~%Timed out at ~A secs~%" +parse-timeout+)
               (setq .vals. (multiple-value-list ,timeout-form)))
            (parse-storeout ()
               (setf (sys:gsgc-switch :hook-after-gc) nil)
               (format *error-output* "~%Memory quota of ~A bytes exceeded~%" +parse-storeout+)
               (setq .vals. (multiple-value-list ,timeout-form)))
            (serious-condition (c)
               (setf (sys:gsgc-switch :hook-after-gc) nil)
               (format *error-output* "~%Error: ~A~%  [condition type: ~A]~%"
                  c (type-of c))))
         (setf (sys:gsgc-switch :hook-after-gc) (if excl:*gc-after-hook* t nil))
         (values-list .vals.)))
)


#+sbcl
(progn
(define-condition parse-timeout (simple-condition) ())
(define-condition parse-storeout (simple-condition) ())
(defmacro execute-with-timeout (timeout-form &body body)
   ;; version of built-in with-timeout macro, roughly equivalent to
   ;; `(handler-case (sb-ext:with-timeout +parse-timeout+ ,@body) (timeout () ,timeout-form))
   ;; but better in that it uses actual computation time not real time
   ;; and that GC time is not counted in timeout -- also with
   ;; addition of facility for giving a memory quota, in +parse-storeout+
   `(let ((.vals. nil))
       (handler-case
          (setq .vals.
             (let*
                ((.parse-timeout.
                    (if (and (numberp +parse-timeout+) (> +parse-timeout+ 0))
                       (+ (* +parse-timeout+ internal-time-units-per-second)
                          (- (get-internal-run-time) sb-ext:*gc-run-time*))
                       nil))
                 (.parse-storeout.
                   (if (and (numberp +parse-storeout+) (> +parse-storeout+ 0))
                       (+ +parse-storeout+ (sb-ext:get-bytes-consed))
                       nil))
                 (sb-ext:*after-gc-hooks*
                    (cons
                       #'(lambda ()
                            (when (and .parse-timeout.
                                     (> (- (get-internal-run-time) sb-ext:*gc-run-time*)
                                        .parse-timeout.))
                               (signal 'parse-timeout))
                            (when (and .parse-storeout.
                                     (> (sb-ext:get-bytes-consed) .parse-storeout.))
                               (signal 'parse-storeout)))
                       sb-ext:*after-gc-hooks*)))
                (multiple-value-list (progn ,@body))))
          (parse-timeout ()
             (format *error-output* "~%Timed out at ~A secs~%" +parse-timeout+)
             (setq .vals. (multiple-value-list ,timeout-form)))
          (parse-storeout ()
             (format *error-output* "~%Memory quota of ~A bytes exceeded~%" +parse-storeout+)
             (setq .vals. (multiple-value-list ,timeout-form)))
          (serious-condition (c)
             (format *error-output* "~%computation stopped due to a ~A: ~A~%"
                (type-of c) c)))
       (values-list .vals.)))
)


#-(or openmcl mcl allegro sbcl)
(progn
(defvar *parse-timeout* nil)
(defmacro execute-with-timeout (timeout-form &body body)
   `(let ((*parse-timeout*
            (if (and (numberp +parse-timeout+) (> +parse-timeout+ 0))
               (+ (* +parse-timeout+ internal-time-units-per-second)
                  (get-internal-run-time))
               nil)))
      (let ((.vals. (multiple-value-list (catch '.parse-timeout. ,@body))))
         (if (eq (car .vals.) '.parse-timeout.)
            (progn
               (warn "Timed out at ~A secs" +parse-timeout+)
               ,timeout-form)
            (values-list .vals.)))))
(defmacro g-perform-stack-check nil
   ;; called in cpar/parse.lsp and lr1/dparse.lsp. These files need to be
   ;; recompiled if they haven't seen this new definition
   '(when (and *parse-timeout* (> (get-internal-run-time) *parse-timeout*))
       (throw '.parse-timeout. '.parse-timeout.)))
)


;;;

(defun clear-lr1-parser nil
   (setq *productions* nil)
   (setq *terminals* nil)
   (setq *terminal-categories* nil)
   (setq *non-terminals* nil)
   (setq *null-categories* nil)
   (setf *state-actions* nil)
   (setf *shift-action-vector* nil)
   (dolist (idrule *id-rules)
      (remprop idrule 'lr1-psrule)))


(defun make-lr1-parse-states (remove-cached)
   (install-unification-grammar nil)
   (compute-ps-backbone nil remove-cached)
   (construct-item-sets)
   (compute-items-lookahead)
   (construct-lalr-machine)
   (clear-lalr-intermediate-results))


;;; (find-duplicate-rules)
;;; (find-duplicate-kleene-names)

(defun find-duplicate-rules ()
   (do ((n 0 (1+ n))
         (limit (length *productions*))
         (table (make-hash-table :test #'equal)))
      ((eql n limit) nil)
      (let*
         ((rule (svref *productions* n))
            (key
               (list* (cfrule-name rule)
                  (cfrule-mother rule) (cfrule-daughters rule))))
         (if (gethash key table)
            (print rule)
            (setf (gethash key table) t)))))


(defun find-duplicate-kleene-names ()
   (do ((n 0 (1+ n))
         (limit (length *productions*))
         (found nil))
      ((eql n limit) nil)
      (let*
         ((rule (svref *productions* n))
            (name-str (string (cfrule-name rule)))
            (dash (position #\- name-str :start 2))
            (slash (and dash
                      (> (length name-str) (+ dash 2))
                      (position #\/ name-str :start (+ dash 2)))))
         (when slash
            (if (member (cfrule-name rule) found :test #'eq)
               (print rule)
               (push (cfrule-name rule) found))))))


;;; (count-action-conflicts)
;;; 231413 shift-reduce conflicts
;;; 223766 reduce-reduce conflicts
;;; 2178 states contain at least 1 conflict
;;; median 34 conflicts, max 1851

(defun count-action-conflicts ()
   (let*
       ((n-terminals (length *terminals*))
        (shift-reduce (make-array n-terminals :initial-element 0))
        (reduce-reduce (make-array n-terminals :initial-element 0))
        (reduce-r-n (make-array n-terminals :initial-element 0))
        (sr-vec
         (make-array (length *state-actions*) :initial-element 0))
        (rr-vec
         (make-array (length *state-actions*) :initial-element 0)))
     (dotimes (state-no (length (the vector *state-actions*)))
       (let ((r-vec nil) (r-vecs nil) (s-int nil))
         (dolist (action (cddr (svref *state-actions* state-no)))
           (cond
            ((symbolp (car action))     ; accept
             )
            ((cdr action)               ; reduce
             (push (car action) r-vecs)
             (if r-vec
                 (setq r-vec (bit-ior r-vec (car action)))
               (setq r-vec (car action))))
            (t                          ; shift
             (setq s-int (car action)))))
         (when (and r-vec s-int)
           (dotimes (n (length *shift-action-vector*))
             (unless (eql (sbit r-vec n) 0)
               (dolist (shift-pair (svref *shift-action-vector* n))
                 (when
                     (and (< (car shift-pair) (length s-int))
                          (eql (sbit s-int (car shift-pair)) 1))
                   (incf (svref sr-vec state-no))
                   (incf (svref shift-reduce n)))))))
         (when (> (length r-vecs) 1)
               (dotimes (n n-terminals)
                  (let ((ndup 0))
                     (dolist (v r-vecs)
                        (unless (eql (sbit v n) 0) (incf ndup)))
                     (when (> ndup 1)
                        (incf (svref rr-vec state-no) (1- ndup))
                        (incf (svref reduce-reduce n) (1- ndup))))))))
      (format t "~&~A shift-reduce conflicts~%~A~%
~A reduce-reduce conflicts~%~A~%
~A states contain at least 1 conflict~%~A~%~A~%"
         (reduce '+ shift-reduce) shift-reduce 
         (reduce '+ reduce-reduce) reduce-reduce
         (let ((ns 0))
            (dotimes (n (length sr-vec) ns)
               (when (or (> (svref sr-vec n) 0) (> (svref rr-vec n) 0))
                  (incf ns))))
         sr-vec rr-vec)))


;;; (count-reduce-reduce-conflicts)
;;; 111207 reduce-reduce conflicts with same no. daughters

(defun count-reduce-reduce-conflicts ()
   (let*
      ((n-terminals (length *terminals*))
         (s-vec
            (make-array (length *state-actions*) :initial-element 0)))
      (dotimes (state-no (length (the vector *state-actions*)))
         (let ((r-actions nil))
            (dolist (action (cddr (svref *state-actions* state-no)))
               (cond
                  ((symbolp (car action)) ; accept
                     )
                  ((cdr action) ; reduce
                     (let ((n-daughters
                              (length (cfrule-daughters
                                    (svref *productions* (- (cdr action)))))))
                        (unless (assoc n-daughters r-actions)
                           (push (list n-daughters) r-actions))
                        (push (car action) 
                           (cdr (assoc n-daughters r-actions)))))
                  (t ; shift
                     )))
            (dolist (item r-actions)
               (when (cddr item)
                  (dotimes (n n-terminals)
                     (let ((ndup
                              (count-if
                                 #'(lambda (v) (not (eql (sbit v n) 0)))
                                 (cdr item))))
                        (when (> ndup 1)
                           (incf (svref s-vec state-no) (1- ndup)))))))))
      (format t "~&~A reduce-reduce conflicts with same no. daughters~%~A~%"
         (reduce '+ s-vec) s-vec)))


;;; (check-for-unreachable-rules)

(defun check-for-unreachable-rules ()
   (let*
      ((rule-reduces
            (make-array (length (the vector *productions*))
               :element-type 'bit :initial-element 0))
         (not-used-rules nil))
      (dotimes (state-no (length (the vector *state-actions*)))
         (dolist (action (cddr (svref *state-actions* state-no)))
            (when
               (and (numberp (cdr action)) (< (cdr action) 0))
               (setf (sbit rule-reduces (- (cdr action))) 1))))
      ;; Don't mention top rule
      (setf (sbit rule-reduces 0) 1)
      (dotimes (rule-no (length (the vector rule-reduces)))
         (when (eql (sbit rule-reduces rule-no) 0)
            (push
               (cfrule-name (svref *productions* rule-no))
               not-used-rules)))
      (if not-used-rules
         (format t
            "~&The following rules are not referenced in parse table:
~{   ~A~%~}" not-used-rules)
         (format t
            "~&All rules are referenced in parse table~%"))))


;;; (print-lr-machine)

(defun print-lr-machine (&optional n m)
   (format t "~&Total of ~A states~%" (length (the vector *state-actions*)))
   (do*
      ((state-no (if n n 0) (1+ state-no))
         (end
            (if m (1+ m) (if n (1+ n) (length (the vector *state-actions*))))))
      ((>= state-no end))
      (let ((state (svref *state-actions* state-no)))
         (format t "~%State ~A:~%Goto table: ~{~A ~A  ~}~%Actions:~%~:{~A ~A  ~}~%"
            state-no
            (mapcan #'(lambda (x) (list (car x) (cdr x))) (cadr state))
            (mapcan
               #'(lambda (pair)
                  (cond
                     ((symbolp (car pair))
                        (list (list (car pair) "acc")))
                     ((cdr pair)
                        (do*
                           ((action (format nil "r~A" (- (cdr pair))))
                              (ts *terminals* (cdr ts))
                              (n 0 (1+ n))
                              (res nil))
                           ((null ts) (nreverse res))
                           (when (eql (sbit (car pair) n) 1)
                              (push (list (car ts) action) res))))
                     (t
                        (do*
                           ((ts *terminals* (cdr ts))
                              (n 0 (1+ n))
                              (res nil))
                           ((null ts) (nreverse res))
                           (do* ((shift-pairs
                                    (svref *shift-action-vector* n)
                                    (cdr shift-pairs)))
                              ((null shift-pairs))
                              (when
                                 (and (< (caar shift-pairs) (length (car pair)))
                                    (eql (sbit (car pair) (caar shift-pairs)) 1))
                                 (push
                                    (list (car ts)
                                       (format nil "s~A" (cdar shift-pairs)))
                                    res)))))))
               (cddr (svref *state-actions* state-no)))))))


;;; (display-lr-machine)

(defun display-lr-machine ()
   (do*
      ((state-no 0 (1+ state-no))
         (end (length *state-actions*))
         (n-ts (length *terminals*))
         (t-len
            (max 6
               (1+ (apply #'max
                  (mapcar #'(lambda (s) (length (string s))) *terminals*)))))
         (sep
            (make-array (* t-len (1+ (length *terminals*)))
               :element-type
               #+(or cltl2 x3j13 ansi-cl) 'base-character
               #-(or cltl2 x3j13 ansi-cl) 'string-char
               :initial-element #\-)))
      ((>= state-no end))
      (display-lr-state-actions state-no
         (make-list (length *terminals*) :initial-element nil)
         n-ts t-len sep))
   (do*
      ((state-no 0 (1+ state-no))
         (end (length *state-actions*))
         (n-nts (length *non-terminals*))
         (nt-len
            (max 6
               (1+ (apply #'max
                  (mapcar #'(lambda (s) (length (string s))) 
                     *non-terminals*)))))
         (sep
            (make-array (* nt-len (1+ (length *non-terminals*)))
               :element-type
               #+(or cltl2 x3j13 ansi-cl) 'base-character
               #-(or cltl2 x3j13 ansi-cl) 'string-char
               :initial-element #\-)))
      ((>= state-no end))
      (display-lr-state-goto state-no n-nts nt-len sep)))


(defun display-lr-state-actions (state-no action-vec n-ts t-len sep)
   (mapc
      #'(lambda (pair)
         (cond
            ((symbolp (car pair))
               (push "acc"
                  (nth
                     (position *sentence-end-marker* *terminals* 
                        :test #'eq)
                     action-vec)))
            ((cdr pair)
               (do*
                  ((action (format nil "r~A" (- (cdr pair))))
                     (n 0 (1+ n)))
                  ((>= n n-ts))
                  (when (eql (sbit (car pair) n) 1)
                     (push action (nth n action-vec)))))
            (t
               (do ((n 0 (1+ n)))
                  ((>= n n-ts))
                  (do* ((shift-pairs
                           (svref *shift-action-vector* n)
                           (cdr shift-pairs)))
                     ((null shift-pairs))
                     (when
                        (and (< (caar shift-pairs) (length (car pair)))
                           (eql (sbit (car pair) (caar shift-pairs)) 1))
                        (push
                           (format nil "s~A" (cdar shift-pairs))
                           (nth n action-vec))))))))
      (cddr (svref *state-actions* state-no)))
   (when (eql state-no 0)
      (format t "~%State~{~VT~A~}"
         (let ((col 0))
            (mapcan #'(lambda (term) (list (incf col t-len) term))
               *terminals*))))
   (format t "~%~A" sep)
   (dotimes
      (row (apply #'max (mapcar #'length action-vec)))
      (format t "~%~A~{~VT~A~}"
         (if (eql row 0) state-no " ")
         (do* ((n 0 (1+ n)) (col 0) (res nil))
            ((>= n n-ts) (nreverse res))
            (push (incf col t-len) res)
            (push
               (if (nth n action-vec) (pop (nth n action-vec)) "   ")
               res)))))


(defun display-lr-state-goto (state-no n-nts nt-len sep)
   (declare (ignore n-nts))
   (when (eql state-no 0)
      (format t "~%~%State~{~VT~A~}"
         (let ((col 0))
            (mapcan #'(lambda (nt) (list (incf col nt-len) nt))
               *non-terminals*))))
   (format t "~%~A" sep)
   (format t "~%~A~{~VT~A~}" state-no
      (do*
         ((n 0 (1+ n))
            (nts *non-terminals* (cdr nts))
            (col 0) (res nil)
            (gotos (cadr (svref *state-actions* state-no))))
         ((null nts) (nreverse res))
         (push (incf col nt-len) res)
         (push
            (or (cdr (assoc (car nts) gotos :test #'eq)) " ")
            res))))


;;; (print-lr-probs)

(defun print-lr-probs ()
   (do
      ((state-no 0 (1+ state-no))
         (end (length *state-action-probs*))
         state)
      ((>= state-no end))
      (setq state (svref *state-action-probs* state-no))
      (format t "~%~%State ~A" state-no)
      (format t "~%Shift:~%~:{   ~A ~A~}"
         (mapcar
            #'(lambda (item)
               (list (nth (car item) *terminals*)
                  (format nil "~6,4F" (expt 10 (cdr item)))))
            (state-prob-shift state)))
      (format t "~%Reduce:~%~:{   ~A r~A ~A ~A~}"
         (mapcan
            #'(lambda (la-item)
               (mapcan
                  #'(lambda (rule-pair)
                     (mapcar
                        #'(lambda (pair)
                           (list (nth (car la-item) *terminals*)
                              (car rule-pair)
                              (car pair)
                              (format nil "~6,4F" (expt 10 (cdr pair)))))
                        (cdr rule-pair)))
                  (cdr la-item)))
            (state-prob-reduce state)))
      (format t "~%Action Default: ~6,4F"
         (expt 10 (state-prob-action-default state)))
      (format t "~%Goto Default: ~6,4F"
         (expt 10 (state-prob-goto-default state)))))


;;; Verify that an arc between 2 states in graph-structured stack can only
;;; be occupied by analyses whose top node is the same atomic CF category
;;; (check-lr-to-states)

(defun check-lr-to-states ()
   (do
      ((state-no 0 (1+ state-no))
         (end (length *state-actions*)))
      ((>= state-no end))
      (let ((states nil) (n-ts (length *terminals*)))
   (dolist (goto (cadr (svref *state-actions* state-no)))
      (if (member (cdr goto) states)
         (break "state ~S" state-no)
         (push (cdr goto) states)))
   (mapc
      #'(lambda (pair)
         (cond
            ((or (symbolp (car pair)) (cdr pair)))
            (t
               (do ((n 0 (1+ n)))
                  ((>= n n-ts))
                  (do* ((shift-pairs
                           (svref *shift-action-vector* n)
                           (cdr shift-pairs)))
                     ((null shift-pairs))
                     (when (and (< (caar shift-pairs) (length (car pair)))
                             (eql (sbit (car pair) (caar shift-pairs)) 1))
                        (if (member (cdar shift-pairs) states)
                           (break "state ~S" state-no)
                           (push (cdar shift-pairs) states))))))))
      (cddr (svref *state-actions* state-no)))
   ;(print states)
   )))


;;; End of file
