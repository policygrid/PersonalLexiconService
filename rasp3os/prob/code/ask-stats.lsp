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

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

;;; Global

(defvar *productions*)
(defvar *terminals*)
(defvar *terminal-categories*)
(defvar *state-actions*)
(defvar *sentence-end-marker*)

(defvar *state-action-probs*)
(defvar *lex-probs*)
(defvar *index-category-table)

;;; Bound here

(defvar *terminal-unary-pairs*)
(defvar *terminals-vector*)

(defstruct state-prob
   transitions transition-default)

(defstruct lex-prob
   rules rules-default)

;;; Compare two files, first of actual rule trees derived from parse
;;; histories, second of analysis alternatives from probabalistic
;;; non-deterministic parser and return number of matches of actual
;;; trees against 1st alt, 2nd alt etc. There may be sentences
;;; in actual rule tree file that are not in probabalistic file.
;;;
;;; [Also display: completely matching trees, completely non-matching
;;; trees]
#|
(lr1-compare-analysis-results "internal:lr1:data:all.trees"
   "internal:lr1:data:all.parses" "internal:lr1:data:all.matches")
(lr1-compare-analysis-results "internal:lr1:data:all.trees"
   "internal:lr1:data:all.parses-lrv" nil 2 10)
(lr1-compare-analysis-results "internal:lr1:data:all.trees"
   "internal:lr1:data:some.parses-goto" "internal:lr1:data:some.matches-goto")
(lr1-compare-analysis-results "internal:lr1:data:new.trees"
   "internal:lr1:data:new.parses")
(lr1-compare-analysis-results "internal:lr1:data:new.trees"
   "internal:lr1:data:new.parses" nil 1 1000 #'lr1-compare-analysis-similar)
|#

(defun lr1-compare-analysis-results (in-actual-trees in-prob-parses
      &optional out-matches (lower 1) (upper 1000)
      (compare-fn #'lr1-compare-analysis-equal))
   (let ((res (list nil)))
      (flet
         ((lr1-compare-analysis-fn (match nparses sent actual)
               (let ((len (length sent)))
                  (when (and (>= len lower) (<= len upper))
                     (format t "~S~%"
                        (list (cond (match) (actual 0) (t '?))
                           nparses len sent))))))
         (with-open-file (in-actual in-actual-trees :direction :input)
            (with-open-file (in-prob in-prob-parses :direction :input)
               (lr1-process-analysis-results in-actual in-prob
                  #'lr1-compare-analysis-fn compare-fn))))))


(defun lr1-process-analysis-results (in-actual in-prob analysis-fn compare-fn)
   (let (actual (prob (read in-prob nil 'eof)))
      (loop
         (setq actual (read in-actual nil 'eof))
         (when (or (eq actual 'eof) (eq prob 'eof))
            (unless (eq prob 'eof)
               (warn "No match for prob parse sentence ~A" prob))
            (return nil))
         (cond
            ((equal actual prob) ; input sentences
               (setq actual (read in-actual))
               (let ((nparses (read in-prob)))
                  (cond
                     ((not (numberp nparses))
                        (error "Incorrect prob parse file"))
                     ((eql nparses 0)) ; ignore prob parse failures
                     (t
                        (let ((prob-parses
                                 (do* ((n nparses (1- n))
                                       (lst nil))
                                    ((eql n 0) (nreverse lst))
                                    (push (read in-prob) lst)))
                              (match-res nil) (n 0))
                           (when actual
                              (dolist (p prob-parses)
                                 (incf n)
                                 (multiple-value-bind (f1 f2)
                                    (funcall compare-fn actual p)
                                    (when f1 (setq match-res (or f2 n))
                                       (return)))))
                           (funcall analysis-fn
                              match-res nparses prob actual)))))
               (setq prob (read in-prob nil 'eof)))
            (t
               (read in-actual))))))


(defun lr1-compare-analysis-similar (a1 a2)
   ;; Sampson tree similarity ratio - return values t, ratio
   ;; "For each word input compare chains of node labels between leaf
   ;; and root in each of the two trees and compute the number of labels
   ;; which match each other and occur in the same order in the two chains as 
   ;; a proportion of all labels in both chains; then we average over the 
   ;; words (+ refinement to ensure that only fully identical tree pairs 
   ;; receive 100% scores)"
   ;; !!! NB assumes no gaps, otherwise number of paths may differ
   (let ((a1-paths
            (lr1-compare-find-paths a1 nil nil))
         (a2-paths
            (lr1-compare-find-paths a2 nil nil))
         (score 0.0))
      (mapc
         #'(lambda (p1 p2)
            (incf score
               (/ (float (lr1-compare-analysis-match p1 p2))
                  (+ (length p1) (length p2)))))
         a1-paths a2-paths)
      (values t (/ score (length a1-paths)))))


(defun lr1-compare-find-paths (tree path res)
   (if (atom tree)
      (push path res)
      (let* ((name (car tree))
            (new-path (cons (if (symbolp name) name (intern name)) path)))
         (dolist (d (cdr tree))
            (setq res (lr1-compare-find-paths d new-path res)))))
   res)


(defun lr1-compare-analysis-match (path1 path2)
   (if (and path1 path2)
      (let ((p1 (car path1)) (p2 (car path2)) tail)
         (cond
            ((eq p1 p2)
               (+ 2 (lr1-compare-analysis-match (cdr path1) (cdr path2))))
            ((and (setq tail (member p1 (cdr path2) :test #'eq))
                  (or (null (cdr path1)) (null (cdr tail))
                     (member (cadr path1) (cdr tail))))
               (lr1-compare-analysis-match path1 tail))
            (t
               (lr1-compare-analysis-match (cdr path1) path2))))
      0))


(defun lr1-compare-analysis-labels (a1 a2)
   (cond
      ((or (atom a1) (atom a2))
         (when (symbolp a1) (setq a1 (symbol-name a1)))
         (when (symbolp a2) (setq a2 (symbol-name a2)))
         (or (equal a1 a2)
            (and (stringp a1) (stringp a2)
               (equal (subseq a1 0 (or (position #\/ a1) (length a1)))
                  (subseq a2 0 (or (position #\/ a2) (length a2)))))))
      (t
         (and (lr1-compare-analysis-labels (car a1) (car a2))
            (lr1-compare-analysis-labels (cdr a1) (cdr a2))))))


(defun lr1-compare-analysis-equal (a1 a2)
   (cond
      ((or (atom a1) (atom a2))
         (when (symbolp a1) (setq a1 (symbol-name a1)))
         (when (symbolp a2) (setq a2 (symbol-name a2)))
         (or (equal a1 a2)
            (and (member a1 '("N2-a" "N2-b") :test #'equal)
               (member a2 '("N2-a" "N2-b") :test #'equal))
            (and (member a1 '("VP/MODAL1" "VP/MODAL2") :test #'equal)
               (member a2 '("VP/MODAL1" "VP/MODAL2") :test #'equal))))
      (t
         (and (lr1-compare-analysis-equal (car a1) (car a2))
            (lr1-compare-analysis-equal (cdr a1) (cdr a2))))))


;;; Compare two files, first of actual rule trees derived from parse
;;; histories, second of analysis alternatives from probabalistic
;;; non-deterministic parser and return number of matches vs phrase
;;; length
;;;
#|
(lr1-chart-analysis-results "internal:lr1:data:all.trees"
   "internal:lr1:data:all.parses") ; "internal:lr1:data:all.chart"
(lr1-chart-analysis-results "internal:lr1:data:new.trees"
   "internal:lr1:data:new.parses") ; "internal:lr1:data:new.chart"
|#

(defun lr1-chart-analysis-results (in-actual-trees in-prob-parses
      &optional out-matches (compare-fn #'lr1-compare-analysis-equal))
   (let ((res (list nil)))
      (flet
         ((lr1-chart-analysis-fn (match nparses sent actual)
               (declare (ignore nparses))
               (let ((len (length sent)))
                  (unless (assoc len (car res))
                     (setf (car res)
                        (merge 'list (list (list len)) (car res) #'< :key #'car)))
                  (push 
                     (cond (match) (sent 0) (t '?))
                     (cdr (assoc len (car res)))))))
         (with-open-file (in-actual in-actual-trees :direction :input)
            (with-open-file (in-prob in-prob-parses :direction :input)
               (lr1-process-analysis-results in-actual in-prob
                  #'lr1-chart-analysis-fn compare-fn)
               (dolist (x (car res)) (print x t))
               (car res))))))


(defun lr1-summarise-results-chart (c)
   ;; print summary of analysis results chart and return a score
   (let ((ranks (mapcan #'(lambda (x) (copy-list (cdr x))) c)))
      (format t "~&~A ranked 1st, ~A ranked 2nd, ~A ranked 3rd, ~A worse~%"
         (count 1 ranks) (count 2 ranks) (count 3 ranks) (count 0 ranks))
      (let* ((score
               (+ (* 3 (count 1 ranks)) (* 2 (count 2 ranks))
                  (count 3 ranks))))
         (format t "Score = ~A~%" score)
         score)))


;;; Write out sentences, number of parses and leave space for number of
;;; correct parse to be inserted
#|
(lr1-summarise-analysis-results "internal:lr1:data:new.parses"
   "internal:lr1:data:new.summary")
|#

(defun lr1-summarise-analysis-results (in-prob-parses &optional out-summary)
   (with-open-file (in-prob in-prob-parses :direction :input)
      (if out-summary 
         (with-open-file (out-str out-summary :direction :output :if-exists 
               :supersede :if-does-not-exist :create)
            (lr1-summarise-analysis-results1 in-prob out-str))
         (lr1-summarise-analysis-results1 in-prob t))))


(defun lr1-summarise-analysis-results1 (in-prob out-str)
   (loop
      (let ((prob (read in-prob nil 'eof)) prob-number)
         (when (eq prob 'eof) (return nil))
         (setq prob-number (read in-prob))
         (when (> prob-number 0)
            (format out-str "~S~%" (list '? prob-number prob)))
         (read in-prob) (read in-prob) (read in-prob))))


;;; Read raw parse histories from input file and write out corresponding
;;; rule- and word- labelled parse trees.
;;;
;;; (lr1-history-rule-trees "internal:lr1:data:all.1" "internal:lr1:data:all.trees")
;;; (lr1-history-rule-trees "internal:lr1:data:testdp.1" "internal:lr1:data:testdp.trees")

(defun lr1-history-rule-trees (raw-hist-in rule-tree-out)
   (with-open-file (in-str raw-hist-in :direction :input)
      (with-open-file (out-str rule-tree-out :direction :output
            :if-exists :supersede :if-does-not-exist :create)
         #+procyon (set-right-margin out-str 78)
         (let (hist)
            (loop
               (setq hist (read in-str nil 'eof))
               (when (eq hist 'eof) (return))
               (format out-str "~S" (cadr hist))
               (pprint
                  (lr1-history-rule-tree (caddr hist) (cadr hist))
                  out-str)
               (terpri out-str))))))


(defun lr1-history-rule-tree (history words)
   ;; Return a rule- and word- labelled tree from a raw history list and
   ;; the words parsed.
   (let ((tree nil))
      (mapc
         #'(lambda (action)
            (cond
               ((eq (car action) 'accept))
               ((eq (car action) 'shift)
                  (push (pop words) tree))
               ((find #\@ (string (car action))))
               ((search "NULL/" (string (car action)))
                  (push 'E tree))
               (t 
                  (let*
                     ((n-daughters
                           (or (lr1-history-rule-elide-p (car action))
                              (length
                                 (cfrule-daughters
                                    (find (car action) *productions*
                                       :key #'cfrule-name :test #'eq)))))
                        (rest-tree (nthcdr n-daughters tree)))
                     (setq tree
                        (cons
                           (cons (car action)
                              (do*
                                 ((ds (ldiff tree rest-tree) (cdr ds))
                                    (res nil))
                                 ((null ds) res)
                                 (if (and (consp (car ds))
                                       (lr1-history-rule-elide-p (caar ds)))
                                    (setq res (append (cdar ds) res))
                                    (push (car ds) res))))
                           rest-tree))))))
         (nreverse (mapcan #'copy-list history)))
      (car tree)))


(defun lr1-history-rule-elide-p (name-sym)
   (let ((name (string name-sym)))
      ;; e.g. A1-851/-, A1-851/+
      (and (> (length name) 4)
         (let*
            ((dash
                  (position #\- name :start 1 :end (- (length name) 3)))
               (slash
                  (and dash (position #\/ name :start (+ dash 2)))))
            (and slash
               (do ((n (1+ dash) (1+ n)))
                  ((eql n slash)
                     (return
                        (if (eql (schar name (1- (length name))) #\-) 1 2)))
                  (unless (digit-char-p (schar name n)) (return nil))))))))


;;; --- Probabilities ---

;;; Probabilities for an action in a state given a lookahead, e.g.
;;; (lr1-parse-action-prob 1 47 184)
;;; (pprint (svref *state-action-probs* 8))
;;; (lr1-read-history-probs "internal:lr1:data:all.probs")
;;; (lr1-read-history-probs "internal:lr1:data:testdp.probs")
;;; (lr1-read-history-probs "internal:lr1:data:sent6.probs")
;;; (lr1-read-history-probs "~/lr1/data/all.probs")

(defvar *rare-productions* nil)
(defvar *state-action-floor-prob* nil)

(defparameter +orig-r-p+ t) ;; used by different training models!

(defun lr1-parse-action-prob (from-state action la via-state to-state)
   ;; from-state is integer, action = shift, accept or rule number
   ;; for reduce, to-state is integer
   (declare (ignore via-state) (fixnum la))
   (let*
      ((state (svref *state-action-probs* from-state))
       (la-pair (assoc la (state-prob-transitions state)))
       (res
          (cond
	   ((null la-pair)		; la not seen before
	    ;;(format t "shouldn't be null la-pair!!~%")
	    (state-prob-transition-default state))
             ((typecase action
                 (fixnum
		  (if (and +orig-r-p+
			   (svref *rare-productions* action))
                       *state-action-floor-prob*
                       (locally (declare (fixnum to-state))
                          (cdr
                             (assoc action
                                (cdr (or (assoc to-state (caddr la-pair))
                                         (assoc nil (caddr la-pair)))))))))
                (t (cdddr la-pair))))
             (t ; la seen before, but specific shift/reduce not
	      ;;(format t "shouldn't be null la-pair 2!!~%")
	      (cadr la-pair)))))
;;;       (format t "~&From ~A with ~A by ~A to ~A -> ~6,4F~%" from-state
;;;          (nth la *terminals*)
;;;          (if (integerp action)
;;;             (cfrule-name (svref *productions* action)) action)
;;;          to-state (expt 10 res))
       res))


(defun lr1-read-history-probs (action-prob-in)
   (unless *productions* (error "No grammar loaded"))
   (let ((*read-default-float-format* 'single-float))
      (with-open-file (in-str action-prob-in :direction :input)
         (peek-char #\( in-str)
         (read-char in-str)
         (do ((res nil))
            ((eql (peek-char t in-str) #\))
               (setq *state-action-probs*
                  (make-array (length res) :initial-contents (reverse res))))
            (push (read in-str) res))
         (setq *rare-productions*
            (make-array (length *productions*) :initial-element nil))
         (dotimes (n (length *productions*))
            (when (rare-rule-p (string (cfrule-name (svref *productions* n))))
               (setf (svref *rare-productions* n) t)))
         (setq *state-action-floor-prob*
            (reduce #'min *state-action-probs*
               :key #'state-prob-transition-default :initial-value 0)))))

(defun rare-rule-p (name) ; name ends -r[/+-]*
   (do* ((i (1- (length name)) (1- i))
         c)
      ((< i 0) nil)
      (setq c (schar name i))
      (cond
         ((or (eql c #\/) (eql c #\+) (eql c #\-)))
         ((eql c #\r)
            (if (and (> i 0) (eql (schar name (1- i)) #\-))
               (return t)
               (return nil)))
         (t (return nil)))))


;;; Read raw parse histories from input file and write out corresponding
;;; state action probabilities. Probabilities expressed as base 10 logs.
;;; 
;;; (lr1-history-probs "internal:lr1:data:all.1" "internal:lr1:data:all.probs") 
;;; (lr1-history-probs "internal:lr1:data:testdp.1" "internal:lr1:data:testdp.probs")
;;; (lr1-history-probs "internal:lr1:data:sent6.1" "internal:lr1:data:sent6.probs")
;;;
;;; Within each state normalise transitions to 1 for each lookahead.

(defparameter +titech-normalise-p+ t)
(defparameter +old-normalise-p+ nil)
(defparameter +history-probs-stats-freq+ 200)

;; use the new training version 
;; - ie. gets rid of old bugs and initialises the lr table
;; to include all possible actions before count transitions
(defparameter +new-version-p+ t)

;;; how to bootstrap: --------------------------------------------
;;; only one of the following switches should be t:
;;;
;;; use 1 weight for each transition set for the top tree (bootstrap-1)
(defparameter +weight-1+ nil) 
;;; use 1/n weight for each transition set for tree in n trees
(defparameter +weight-n+ nil) 
;;; use 1/r weight for each transition set for tree ranked r
(defparameter +weight-r+ nil) 
;;; use prob(tree) as weight for each transition set for tree 
(defparameter +weight-prob+ nil) 

;; to consider top ranked tree only and use above weighting schemes:
(defparameter +top-1+ nil)
;; for conll style training: 0.123456789 weight was a frag parse:!
(defparameter +allow-frag-p+ t)

;; the weight (+threshold) options can be used with the above 
;; three in combination or as an alternative.
;; some of the old .trans file created are in old format!

;; use the ioa score (with threshold) for each transition
(defparameter +weight-ioa+ nil)
;; use the ioa num (with threshold) for each transition ie. 90% 0.9 threshold
(defparameter +weight-ioa-num+ nil)

;; used in conjunction with weight-ioa and weight-num
;; note < this is not counted
(defparameter +weight-threshold+ 0.0d0)

;;; the weight to increment action transitions
(defparameter +trans-weight+ 1.0d0)
;;;---------------------------------------------------------------

(defun lr1-history-probs (raw-hist-in action-prob-out)
   (with-open-file (in-str raw-hist-in :direction :input)
      (with-open-file (out-str action-prob-out :direction :output
            :if-exists :supersede :if-does-not-exist :create)
         (setq *state-action-probs*
            (make-array (length *state-actions*)))
         (dotimes (n (length *state-actions*))
            (setf (svref *state-action-probs* n)
               (make-state-prob
		:transitions nil :transition-default nil)))
	 
	 ;; initialise the actions that can occur:
	 ;; only if add-1 is done otherwise it doesn't change alg
	 ;; and will crash as we'll output 0 value as prob.
	 (when (and +new-version-p+) ;; +add-1-normalise-p+)
	   (initialise-state-action-probs)
	   )
	 
	 (lr1-history-probs-statistics-start)
	 (let (hist (prev nil) (nhist 0)
               (*print-level* nil) (*print-length* nil) (*print-pretty* nil))
	   (loop
	     (setq hist (read in-str nil 'eof))
	     ;;(format t "~%-------------~%history: ~%") (pprint hist) (terpri) (format t "--------------------------------~%")
	     (cond
	      ((eq hist 'eof)
	       ;;(format t "normalising ~% ~S~%" *state-action-probs*)
	       (lr1-history-probs-statistics-running nhist)
	       (lr1-history-probs-normalise)
	       (format out-str "~S~%" *state-action-probs*)
	       ;;(format t "normalised ~% ~S~%" *state-action-probs*)
	       (return))
	      ((null (caddr hist))	; maybe previous failure to reproduce parse
	       )
	      ((vectorp (car (caddr hist)))
	       (setq +trans-weight+ 1.0d0)
	       (incf nhist)
	       (dolist (action (caddr hist))
		 (lr1-history-prob-action action)))
	      ((consp (car (caddr hist))) ;; bootstrap trans input
	       (incf nhist (car hist))
	       (let ((trans-sets (caddr hist)))
	     	 (dolist (action-set (if (or +weight-1+ +top-1+)
					 (list (car trans-sets)) trans-sets))
		   (cond (+weight-1+ (setq +trans-weight+ 1))
			 (+weight-n+ (setq +trans-weight+ (car action-set)))
			 (+weight-r+ (setq +trans-weight+ (cadr action-set)))
		       	 (+weight-prob+ (setq +trans-weight+ (caddr action-set))))
		   ;; test for non-frag?
		   ;;(format t "weight:~A ~A ~A~%" (car action-set) (cadr action-set) (caddr action-set))

		   (when (or +allow-frag-p+ (not (= 0.12345679f0 (coerce (caddr action-set) 'single-float))))
		     ;;(format t "added:~%")
		     (dolist (action (cadddr action-set))
		       (when (consp action) ;; ioa style trans file
		       ;; get the normalised ioa score/count:
			 (cond (+weight-ioa+
				(setq +trans-weight+ (car action)))
			       (+weight-ioa-num+
				(setq +trans-weight+ (cadr action))))
		       ;; threshold and don't count actions below threshold:
		       (when (< +trans-weight+ +weight-threshold+)
			 (setq +trans-weight+ 0.0d0))
		       (setq action (caddr action))
		       )
		     (when (> +trans-weight+ 0.0d0)
		       ;;(format t "~&--------------~%")
		       ;;(format t "counting the weight: ~A ~A ~%" 
		       ;;+trans-weight+ action)
		       (lr1-history-prob-action action)
		       ))
		     )
		   )	 
		 ))
	      (t			; old format - needs disambiguating
					; (dolist (action (lr1-disambiguate-history (caddr hist)))
					;    (lr1-history-prob-action-old action prev)
					;    (setq prev action))
	       (error "incorrect format for history: ~S" hist)))
	     (setq hist nil)
	     (when (zerop (mod nhist +history-probs-stats-freq+))
	       (lr1-history-probs-statistics-running nhist)))))))

(defun initialise-state-action-probs ()
  (dotimes (state-no (length *state-actions*))
  	(let ((state-action (svref *state-actions* state-no))
	  (state-prob (svref *state-action-probs* state-no)))
      (dolist (pair (cddr state-action)) ;; was cddr before sbcl?
	(cond
	 ((symbolp (car pair)))
	 ((cdr pair) ;; reduce
	  ;; see if this reduce applies for each terminal
	  (dotimes (ts (length *terminals*)) 
	    (when (eql (sbit (car pair) ts) 1) ;; if reduce for terminal
	      (let ((la-pair (assoc ts (state-prob-transitions state-prob)))
		    (to-state nil))
		(unless la-pair
		  (push (setq la-pair (list* ts 0 nil nil))
			(state-prob-transitions state-prob)))
		(when (not +old-normalise-p+);; can add all the reduces!
		  (setq to-state (assoc nil (third la-pair)))
		  (if to-state
		      (push (cons (- (cdr pair)) 0) (cdr to-state))
		    (setf (third la-pair) (list (list nil (cons (- (cdr pair)) 0))))
		    ))
		)	      
	      )))
	 (t ;; shift
	  (dotimes (ts (length *terminals*)) 
	    (dolist (shift-pairs (svref *shift-action-vector* ts))
	      (when
		  (and (< (car shift-pairs) (length (car pair)))
		       (eql (sbit (car pair) (car shift-pairs)) 1))
		;; shift item applies for this state + ts
		(let ((la-pair (assoc ts (state-prob-transitions state-prob)))
		      (to-state nil))
		  (unless la-pair
		    (push (setq la-pair (list* ts 0 nil 0))
			  (state-prob-transitions state-prob)))
		  		  (unless (cdddr la-pair)
		    (setf (cdddr la-pair) 0))))))))))))

(defparameter +add-1-normalise-p+ t)
(defparameter +shift-1-p+ nil)

(defun lr1-history-prob-action (action)
   (let* ((state (svref *state-action-probs* (svref action 0)))
          (la (svref action 2))
          (la-pair (assoc la (state-prob-transitions state))))
     ;;(format t "state: ~A ~%" state)
     ;;(format t "la-pair: ~A ~%" la-pair)
      (unless la-pair
	(push (setq la-pair (list* la 0 nil nil))
	      ;; la, total smoothed freq of this la in this state, nested
	      ;; reduce alist keyed on to-state then rule, then optionally
	      ;; shift freq
	      (state-prob-transitions state)))
      (if (eql (length action) 3)
         ;; shift
	  (when (not +shift-1-p+)
	    (let
		((n (if (and (zerop (cdddr la-pair)) ;; new-scheme
			     +add-1-normalise-p+) 2 1))) 
	      (incf (second la-pair) n)
	      (if (cdddr la-pair)
		  (incf (cdddr la-pair) n) (setf (cdddr la-pair) n))))
         ;; reduce
         (let* ((to-state (if (not +old-normalise-p+) nil (svref action 1)))
                (to-state-pair (assoc to-state (third la-pair)))
                (rule (svref action 3))
                rule-pair)
            (unless to-state-pair
               (push (setq to-state-pair (cons to-state nil))
                  (third la-pair)))
            (setq rule-pair (assoc rule (cdr to-state-pair)))
            (unless rule-pair
	      (push (setq rule-pair (cons rule 0)) 
		    (cdr to-state-pair)))
            (let
		((n (if (and (zerop (cdr rule-pair)) ;; new scheme - bug fix! 
			     +add-1-normalise-p+) 
			(1+ +trans-weight+) +trans-weight+)))
               (incf (second la-pair) n)
               (incf (cdr rule-pair) n))))
      ;;(format t "la-pair after: ~A ~%" la-pair)
      ;;(format t "state after: ~A ~%" state)
      ))
      
(defun count-unseen (la-pair)
  (let ((unseen-num 0))
    (when (and (cdddr la-pair) (eq 0 (cdddr la-pair)))
      (incf unseen-num))
    ;; find unseen reduces
    (dolist (to-state-pair (caddr la-pair))
      (dolist (rule-pair (cdr to-state-pair))
	(when (eq (cdr rule-pair) 0)
	  (incf unseen-num))))
    ;;(format t "~%ind-state ~A number unseen= ~A~%" ind unseen-num)
    unseen-num
    ))

(defun lr1-history-probs-normalise nil ; nil)
   (flet ((log-10 (x) (log x 10)
          ))
      (let ((r-states
               (and +titech-normalise-p+
                  (lr1-history-probs-r-states *state-actions*))))
         (dotimes (ind (length *state-action-probs*))
	   (let*
               ((state (svref *state-action-probs* ind))
                (r-state-p (member ind r-states))
                (trans-freq-sum 0)
                (base-freq (if +add-1-normalise-p+ 1 0))
		(state-unseen 0))
	     ;;(format t "~%-------~%state before:~A ~A ~%" ind state)
               (dolist (la-pair (state-prob-transitions state))
		 (incf trans-freq-sum 
		       (if +new-version-p+
			   (second la-pair)
			 (+ (second la-pair) base-freq)))
		 (when (and +new-version-p+
			    (or +old-normalise-p+
				(and +titech-normalise-p+ (not r-state-p))))
		   (incf state-unseen (count-unseen la-pair)))
		 )
	       (when (and +new-version-p+
			  (>= state-unseen 0))
		 (incf trans-freq-sum state-unseen))
               ;; if titech flag set: if we're in an S_{r} state (i.e. just done
               ;; a reduce to get here) then normalise action probs to 1 separately
               ;; for each lookahead - otherwise in an S_{s} state normalise
               ;; across all actions in state
               (dolist (la-pair (state-prob-transitions state))
		 (let*
		     ((la-unseen (count-unseen la-pair))
		      (trans-freq
		       (if +new-version-p+ 
			   (if (>= la-unseen 0)
			       (+ (second la-pair) la-unseen)
			     (second la-pair))
			 (+ (second la-pair) base-freq)))
                      (trans-freq-norm
                         (if +titech-normalise-p+
                            (if r-state-p trans-freq trans-freq-sum)
                            (if +old-normalise-p+ trans-freq-sum trans-freq)))
                      (shift-freq (cdddr la-pair)))
		   ;; shift / accept
                     (when shift-freq
		       (setf (cdddr la-pair)
			 (if (eq shift-freq 0)
			     (if +add-1-normalise-p+
				 (log-10 (/ (float 1)
					    trans-freq-norm))
			       (log-10 least-positive-single-float))
			   (log-10 (/ (float shift-freq) trans-freq-norm)))))
                     ;; reduce
                     (dolist (to-state-pair (caddr la-pair))
                        (dolist (rule-pair (cdr to-state-pair))
                           (setf (cdr rule-pair)
			     (if (eq (cdr rule-pair) 0)
				 (if +add-1-normalise-p+
				     (log-10 (/ (float 1) trans-freq-norm))
				   (log-10 least-positive-single-float))
			       (log-10 (/ (float (cdr rule-pair)) trans-freq-norm))))
			  ))
                     ;; default for transition
                     (setf (second la-pair)
		       (if +add-1-normalise-p+ 
			   (log-10 (/ (float 1) trans-freq-norm))
			 (log-10 least-positive-single-float)))))
               ;; default if transition not present
               (setf (state-prob-transition-default state)
		 (if +add-1-normalise-p+ 
		     (log-10 (/ (float 1) (max trans-freq-sum 1)))
		   (log-10 least-positive-single-float)))
	       ;;(format t "state after:~A ~A ~%" trans-freq-sum state)
	       )))))

(defun lr1-history-probs-r-states (state-actions)
   (let ((res nil))
      (dotimes (n (length state-actions))
         (dolist (pair (cadr (svref state-actions n)))
            (pushnew (cdr pair) res)))
      res))


;;; Collect and print stats from possibly a half-finished prob
;;; table

(defun lr1-history-probs-statistics-start nil
   (format t "~&nhist    sh/acc distinct      <=2    reduce distinct      <=2~%~%"))

(defun lr1-history-probs-statistics-running (nhist)
  (let ((shift-total 0) (shift-distinct 0) (shift-2orless 0) (shift-unseen 0)
	(reduce-total 0) (reduce-distinct 0) (reduce-2orless 0) (reduce-unseen 0))
    (dotimes (ind (length *state-action-probs*))
      (let*
	  ((state (svref *state-action-probs* ind))
	   (base-freq (if +add-1-normalise-p+ 1 0)))
	;;(format t "~%BASE-FREQ: ~A~%" base-freq)
	(dolist (la-pair (state-prob-transitions state))
	  (let
	      ((shift-freq (cdddr la-pair)))
	    ;; shift / accept
	    (when (and shift-freq (> shift-freq 0))
	      ;;(format t "~%shift-freq: ~A~%" shift-freq)
	      (incf shift-total (- shift-freq base-freq))
	      (incf shift-distinct)
	      (when (<= (- shift-freq base-freq) 0) (incf shift-unseen))
	      (when (<= (- shift-freq 2 base-freq) 0) (incf shift-2orless)))
	    ;; reduce
	    (dolist (to-state-pair (caddr la-pair))
	      (dolist (rule-pair (cdr to-state-pair))
		(when (> (cdr rule-pair) 0)
		  ;;(format t "~%reduce rule-pair: ~A~%" rule-pair)
		  (incf reduce-total (- (cdr rule-pair) base-freq))
		  (incf reduce-distinct)
		  (when (<= (- (cdr rule-pair) base-freq) 0)
		    (incf reduce-unseen))
		  (when (<= (- (cdr rule-pair) 2 base-freq) 0)
		    (incf reduce-2orless)))))))))
    (format t "~&~7A ~7@A  ~7@A  ~7@A   ~7@A  ~7@A  ~7@A ~7@A  ~7@A~%"
	    nhist shift-total shift-distinct shift-2orless shift-unseen
	    reduce-total reduce-distinct reduce-2orless reduce-unseen)))

;;; End of file
