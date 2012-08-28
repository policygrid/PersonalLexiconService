#|----------------------------------------------------------------------------|
 | Copyright 2006, 2011 John Carroll, Ted Briscoe, Rebecca Watson             |
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

;;; please refer to the article printed in IWPT 2005 for details of
;;; the ioa algorithm implemented herein to extract the weighted
;;; gr output directly from the parse forest.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

;;(set-print-fn 'gw)
;;(setf *print-level* nil)
;;(setf *print-circle* nil)

(setf *cached-converted-categories* nil)
(setq *read-default-float-format* 'double-float)

;; global hash table to store grs:
(defvar *weighted-grs-table)
(defvar *total-prob)

(defun set-gio ()
  (progn
    (setq +analysis-tree-type+ 'extracted-grs)
    (setq +analysis-tree-print-fn+ 'print-extracted-weighted-grs)
    (setq +numbered-words-p+ t) ;;-- for equality test on heads!!
))
;; weight of grs + tags determined by counting parses 
;; (instead of weighted count)
;; doesn't include the tags orig score either!
(defvar +count-parses+)
(setq +count-parses+ nil)

;; used when we output pos tags as well 
;; (pos tagging paper - using parser as tagger!)
(defvar *tags-and-scores)
(defvar +get-tags+)
(setq +get-tags+ nil)

;; use ALL rules in the grammar?
;; (setq +disabled-rules+ nil)

;;;-------------------------------------------------------------------
;;; GENERAL FUNCTIONS 
;;;-------------------------------------------------------------------

(defun rs ()
  (run-system-parc700))
(defun rsg ()
  (run-system-gdc))
(defun rewg ()
  (run-ewg-parc700))

(defun run-system-parc700 ()
  (setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
  (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print-with-weights)
  (setq +parc700+ t)
  (setq +n-best-retained+ 1)
  (TIME (LR1-PARSE-ANALYSIS-TREES "./prob/greval/parc700/test.not-ne.stag" 
                                  "./data/test.not-ne.stag.sys.out")))
(defun run-system-parc140 ()
  (setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
  (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print-with-weights)
  (setq +parc700+ t)
  (setq +n-best-retained+ 1)
  (TIME (LR1-PARSE-ANALYSIS-TREES 
	 "./prob/greval/parc700/heldout/heldout.ne.stag"
	 "./data/heldout.ne.stag.sys.out")))

(defun run-system-gdc ()
 (setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
 (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print-with-weights)
  (setq +parc700+ t)
  (setq +n-best-retained+ 1)
  (TIME (LR1-PARSE-ANALYSIS-TREES "./extra/bec/gram-dev-corpus/all-corpus.goldtag" 
                                  "./data/all-corpus.goldtag.sys.out")))
(defun run-ewg-parc700 ()
  (setq +analysis-tree-type+ 'extracted-grs)
  (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print)
  (setq +parc700+ t)
  (TIME (LR1-PARSE-ANALYSIS-TREES "./prob/greval/parc700/test.not-ne.stag" 
                                  "./data/test.not-ne.stag.ewg.out")))

(defun run-ewg-parc700-upperbounds ()
  (setq +analysis-tree-type+ 'extracted-grs)
  (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print)
  (setq +parc700+ t)
  (setq +file-out+ "data/test.not-ne.stag.ewg-")
  (setq +threshold-list+ '(0.0d0 1.0d0))
  (TIME (LR1-PARSE-ANALYSIS-TREES "./prob/greval/parc700/test.not-ne.stag" 
                                  "./data/test.not-ne.stag.ewg.out")))

(defun run-ewg-parc140-upperbounds ()
  (setq +analysis-tree-type+ 'extracted-grs)
  (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print)
  (setq +parc700+ t)
  (setq +file-out+ "data/heldout.ne.stag.ewg-")
  (setq +threshold-list+ '(0.0d0 1.0d0))
  (TIME (LR1-PARSE-ANALYSIS-TREES 
	 "./prob/greval/parc700/heldout/heldout.ne.stag" 
	 "./data/heldout.ne.stag.ewg.out")))

(defun run-ewg-gdc-upperbounds ()
  (setq +analysis-tree-type+ 'extracted-grs)
  (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print)
  (setq +parc700+ t)
  (setq +file-out+ "data/all-corpus.goldtag.ewg-")
  (setq +threshold-list+ '(0.0d0 1.0d0))
  (TIME (LR1-PARSE-ANALYSIS-TREES "./extra/bec/gram-dev-corpus/all-corpus.goldtag" 
                                  "./data/all-corpus.goldtag.ewg.out")))

(defun get-r-rules ()
  (let ((rules nil))
    (dotimes (n (length *productions*))
      (if (elt *rare-productions* n)
          (push (cfrule-name (elt *productions* n)) rules)))
    (pprint rules)
    ))

(defun alp (x y)
  (if (> x y)
      (add-log-probs x y)
    (add-log-probs y x)))

(defun add-log-probs (xd yd)
  (+ xd (log (+ 1.0D0 (expt 10.0D0 (- yd xd))) 10.0D0)))

(defun slp (x y)
  (subtract-log-probs x y))

(defun subtract-log-probs (xd yd)
  (+ xd (log (- 1.0D0 (expt 10.0D0 (- yd xd))) 10.0D0)))

;;;-------------------------------------------------------------------
;;; PRINT FUNCTIONS
;;; extract grs - nbest done through systems current print functions
;;;             - weighted grs printed here
;;;             - backoff requires the system print functions modified
;;;               here as want to print multiple files (each +threshold-list+)
;;;-------------------------------------------------------------------

(defun spf (o)
  (set-print-fn o))
(defun set-print-fn (outformat)
  (cond
    ;; alias:
    ((eq outformat 'a)
     (progn
       (setq +analysis-tree-type+ 'get-cat-labelling-from-parse-tree)
       (setq +analysis-tree-print-fn+ 'nil)
       ))
    ;; grs:
    ((eq outformat 'g)
     (progn 
       (setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
       (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print)))
    ;; weighted grs:
    ((eq outformat 'gw)
     (progn 
       (setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
       (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print-with-weights)
       ))
    ;; ewg weighted grs:
    ((eq outformat 'gio)
     (progn 
       (setq +analysis-tree-type+ 'extracted-grs)
       (setq +analysis-tree-print-fn+ 'print-extracted-weighted-grs)
       ))
    ;; rmrs:
    ((eq outformat 'r)
     (progn
       (setq +analysis-tree-type+ 'nil)
       ;;(setq +analysis-tree-print-fn+ 'mrs::rasp-tree-mrs-print)
       ))
    ;; sparkle
    ((eq outformat 's)
     (progn
       (setq +analysis-tree-type+ 'sparkle)
       (setq +analysis-tree-print-fn+ 'nil)
       ))
    ;; tree:
    ((eq outformat 't)
     (progn
       (setq +analysis-tree-type+ 'nil)
       (setq +analysis-tree-print-fn+ 'nil)
       ))
    ;; tree and grs:
    ((eq outformat 'tg)
     (progn 
       (setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
       (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-trees-and-grs-print)))
    ;; trees and gio
    ;; deleted
    ;; upenn
    ((eq outformat 'u)
     (progn
       (setq +analysis-tree-type+ 'phrasal-parse-structure-upenn)
       (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-upenn-print)
       ))
    ;; susanne
   ((eq outformat 'z)
    (progn
      (setq +analysis-tree-type+ 'susanne)
      (setq +analysis-tree-print-fn+ 'nil)
      ))
   (t (format t "incorrect option"))))

;;; following options used by bec to enable faster experimentation
;;; not for use in rasp releases!!
;;;------------------------------------------
;; doestn't print weights next to the weighted grs
(defvar +parc700+)
(setq +parc700+ nil)
;; applies a threshold to the weighted grs when printing
(defvar +gr-threshold+)
(setq +gr-threshold+ 0.0d0) ;; threshold used if system applied

;; enables me to print to multiple files - one each for
;; each threshold in threshold-list:
(defvar +file-out+)
(setq +file-out+ "data/temp")
(defvar +threshold-list+)
(setq +threshold-list+ NIL) ;; thresholds used if ewg applied
;;(setq +threshold-list+ '(0.0d0 0.1d0 0.2d0 0.3d0 0.4d0 0.5d0 0.6d0 0.7d0 0.8d0 0.9d0 1.0d0))
;;;------------------------------------------

(defun extracted-grs (tree nvt)      
  (if (lr1-fragmentary-parse-p tree)
      (progn
	(unless +xml-output+
	  ;;use system's print function (then reset in function below)
	  (setq +analysis-tree-print-fn+ 
		'lr1-parse-analysis-grs-print-sys-backoff))
	(lr1-parse-analysis-grs tree nvt))
      (progn
	(unless +xml-output+
	  (setq +analysis-tree-print-fn+ 
		'print-extracted-weighted-grs))
	(car tree)))
  )

(defun print-extracted-weighted-grs (tree out-str-ignored)
  (print-extracted-weighted-grs-thresh tree out-str-ignored)
  ;;also do for all the different thresholds listed here!
  (when +threshold-list+
    (dolist (threshold +threshold-list+)
      (with-output-file-or-stream (out-str 
                                   (get-name-fileout threshold) 
                                   :direction :output 
                                   :if-exists :append 
                                   :if-does-not-exist :create)
        (setq +gr-threshold+ threshold)
        (format out-str "~A()" *current-sentence*)
        (terpri out-str)
        (print-extracted-weighted-grs-thresh tree out-str)
        (terpri out-str)
        (setq +gr-threshold+ 0.0d0))))
    (setq *current-sentence* nil) ;; free var
  )

(defun print-extracted-weighted-grs-thresh (tree out-str)
  ;;(format t "~%printing weighted grs ~A~%" +gr-threshold+)
  ;;(pprint tree)
  (format out-str "~&")
  (when +parseval-output-p+ (format out-str "{~%"))
  (let ((grw-set (cadr tree))
        (*read-default-float-format* 'single-float)) ; get E instead of S!
    (dolist (grw grw-set)
      (when (>= (cadr grw) +gr-threshold+)
	(progn
	  (when (not +parc700+)
	      (if (eql (cadr grw) 1.0s0)
		  (format out-str " 1.0 ~16,1T ")
		(format out-str " ~,6G ~16,1T " (coerce (cadr grw) 'single-float))))
	  (lr1-parse-analysis-gr-print (car grw) out-str)
	  (terpri out-str)
	  ))
      ))
  (when +parseval-output-p+ (format out-str "}~%")))

(defun get-name-fileout (threshold)
  (concatenate 'string 
    (string-downcase 
     (if (null +file-out+) 
         "data/temp" +file-out+)) 
    (format nil "~A" threshold) ".out"))

(defun lr1-parse-analysis-grs-print-sys-backoff (grs-sets out-str-ignored)
  ;;(pprint grs-sets)
  (lr1-parse-analysis-grs-print-with-weights grs-sets out-str-ignored)
  (when +threshold-list+
    (dolist (threshold +threshold-list+)
      (with-output-file-or-stream (out-str 
                                   (get-name-fileout threshold) 
                                   :direction :output 
                                   :if-exists :append
                                   :if-does-not-exist :create)
        (setq +gr-threshold+ threshold)
        ;;(format t "printing weighted grs - from sys ~A~%" +gr-threshold+)
        (format out-str "~A()" *current-sentence*)
        (terpri out-str)
        (loop for grs in (car grs-sets)
            do
              (dolist (gr grs)
		(when (not +parc700+)
		  (format out-str " 1.0 ~16,1T "))
                (lr1-parse-analysis-gr-print gr out-str) (terpri out-str)))
        (terpri out-str)))
    (setq +gr-threshold+ 0.0d0)))

;;;-------------------------------------------------------------------
;;; TOP LEVEL CALL TO EXTRACT GRS - NBEST & WEIGHTED 
;;; returns required analysis structure
;;;-------------------------------------------------------------------

(defun extract-grs ()
;;  (format t "IOA Extraction of Weighted GRS...")
  (let ((*weighted-grs-table (make-hash-table :test #'equal))
	(*total-prob 0.0))
;;;  (when +get-tags+
;;;    (setq *tags-and-scores 
;;;      (nreverse (let ((tags))
;;;		  (dolist (wordtag (cdr *current-sentence*))
;;;		    (if (consp wordtag)
;;;			(dolist (tag (cdr wordtag))
;;;			  (push (list (car tag) nil) tags))
;;;		      (push (list wordtag nil) tags)))
;;;		  tags))))
    (mapcan
     #'(lambda (vt-and-tree)
	 ;;Calculating inside then outside probs
	 (lr1-parse-extract-weighted-outside
	  (lr1-parse-extract-weighted-inside 
	   (cdr vt-and-tree) (car vt-and-tree) nil)
	  ;;(format t "done inside...~%")
	  )
	 )
     *accepted-vertex-analyses*)
    (let ((wgrs NIL))
      (maphash
       #'(lambda (gr w)
	   (let ((gr-prob 
		  (coerce (/ w (if +count-parses+ *total-prob
				 (expt 10.0d0 *total-prob))) 
			  'single-float)))
	     (push (list gr gr-prob) wgrs))
	   )
       *weighted-grs-table)
      ;;(setq *weighted-grs-table nil) ;; free space
      ;; return weighted grs:
      (list (make-analysis 0 
			   *total-prob 
			   (list nil (list *total-prob wgrs)))))))

;;;-------------------------------------------------------------------
;;; EXTRACT N-BEST & WEIGHTED GR FUNCTION - weighed t if from weighted
;;;-------------------------------------------------------------------

(defun lr1-parse-extract-daughter (d vt d-cat daughters-alts daughters-alts-scores)
  ;;(format t "daugther....~%")
   (let ((unpacked nil) (unpacked-scores nil))
      (dolist (alt daughters-alts)
	(let ((alt-score (pop daughters-alts-scores))
	      (alt-heads
		      (mapcar #'car (cdr alt)))) 
	  (multiple-value-bind (unps unp-scores)
	      (lr1-parse-extract-weighted-inside d vt d-cat)
	    ;; need to make sure that same head doesn't come up both sides!
	    (dolist (u unps)
	      (let ((unp-score (pop unp-scores)))
		;; right now needs the word numbering to be switched to true!
		;; otherwise SAME word but different position will clash!
		(when (or (null alt-heads)
			  (not (member (car u) alt-heads :test #'equal)))
		  (push (append alt (ncons u)) unpacked)
		  (push (mult-probs 
			 unp-score alt-score) unpacked-scores)
		  ))))))
      (values unpacked unpacked-scores)))

;;;-------------------------------------------------------------------
;;; EXTRACT WEIGHTED FUNCTIONS - inside - outside+collect grs/breadthfirst
;;;-------------------------------------------------------------------

(defun lr1-parse-extract-weighted-inside (tree vt rule-cat)
  (g-perform-stack-check)
  ;;(when (consp (node-ntrans (car tree)))
  ;;  (let ((cached (assoc rule-cat (node-ntrans (car tree)) :test #'eq)))
  ;;    (when cached
;;	(return-from lr1-parse-extract-weighted-inside
;;	  (values (cadr cached) (cddr cached))))))
  ;;(format t "starting...~%")
  
  ;; UPDATED 2/2/08 bec:
  ;; can use previously unpacked parse forest by emptying ntrans if not
  ;; set by the ewg alg!
  (when (consp (node-ntrans (car tree)))
    (if (and (consp (car (node-ntrans (car tree)))) (eq 'EWG-ALG (cadar (node-ntrans (car tree)))))
	(let ((cached (assoc rule-cat (node-ntrans (car tree)) :test #'eq)))
	  (when cached
	    (return-from lr1-parse-extract-weighted-inside
	      (values (caddr cached) (cdddr cached)))))
	(setf (node-ntrans (car tree)) nil)))
  (let
      ((tail (node-rest (car tree)))
       (unpacked nil) (unpacked-scores nil))
      (loop
	(when (or (atom tail) (atom (car tail))) 
	  (return))
         (let ((packed (cdar tail)))
           (multiple-value-bind (unps unp-scores)
	       (lr1-parse-extract-weighted-inside (cddr packed) vt rule-cat)
	     (dolist (u unps)
	       ;; first tree in set is the enclosing one, 
	       ;; but must include host
	       ;; tree so nodes packed into 
	       ;; packed nodes will be found later
	       (push (list* u) unpacked))
	     (dolist (s unp-scores) 
	       (push s unpacked-scores))
	     ))
	 (setq tail (cdr tail)))
      (cond
       ((and (cdr tree) (atom (cadr tree)))
	  ;; don't need to prune as getting ALL (weighted)
	  (let ((e-score (node-score (car tree))))
	    (setq unpacked 
              (cons (list (list (cadr tree)) 
			  (list (init-value e-score)))
                    unpacked))
	    (setq unpacked-scores (cons (init-value e-score) 
                                        unpacked-scores))
            ))
	 (t
	   (let* ((rname
		   (and (consp tail) (car tail)))
		  (d-cats		; check for elide case
		   (and rname (cdr (get (intern rname) 'lr1-psrule))))
		  (r-score (lr1-parse-unpack-score tree rname))
		  (daughters-alts-scores (list (init-value r-score)))
		  (n (+ 1 (length (cdr tree))))
		  (n-and-categories 
		    (cons (list 0 (car tree))
			  (mapcar
			   #'(lambda (daughter) 
			       (cons (decf n) daughter))
			   (cdr tree))))
		   (mother-forms (filter-semantic-forms
				   (semantic-forms-present (cdr tail)
							   (car tail))
				   n-and-categories vt))
		  (daughters-alts (list (list mother-forms))))
	     (dolist (d (cdr tree))
	       (multiple-value-setq (daughters-alts daughters-alts-scores)
		  (lr1-parse-extract-daughter
		   d vt (and d-cats (car (pop d-cats))) daughters-alts
		   daughters-alts-scores)))
	     ;;(pprint daughters-alts)
	     
	     ;; daughters-alts now holds list of alternative
	     ;; (MF . daughters)
	     ;; each daughter is structure:(<heads>,<scores>,<possible-sub-analyses>)
	     ;; note that <scores> in daughters-alts is actually (e-score r-score p-count)
	     ;; later we calculate f-score and append the total probability of the
	     ;; sub-analyses to this list of <scores> 
	     ;;
	     ;; daughters-alts-scores still contains the list of scores for
	     ;; each alternative in daughters-alts.
	     ;; We now fill in the MF with the head(s) of the daughters,
	     ;; creating a new format: 
	     ;; (mother-heads, mother-GRs, mother-e-scores)
	     (setq daughters-alts
	       (mapcan
		#'(lambda (dalts) 
		    (let* ((motherf (copy-reentrant-tree 
                                     (car dalts) nil))
			   (current-grs nil)
			   (e-score (pop daughters-alts-scores))
			   (n (length dalts))			    
			   (n-and-daughters 
			    (mapcar
			     #'(lambda (dt)
				 (cons (decf n) (car dt)))
			     (cdr dalts)))
			   (heads nil))
		      (setq n-and-daughters
			(nconc n-and-daughters
	       	       (kleene-daughter-substitutions 
			n-and-daughters mother-forms)))
		      (mapcan
		       #'(lambda (mother-form)
			   (let ((forms (ncons mother-form)))
			     (dolist (n-and-daughter n-and-daughters)
			       (setq forms
				 (mapcan
				  #'(lambda (form)
				      (if (member-any-level-form 
					   (car n-and-daughter) form)
					  (maplist
					   #'(lambda (tail)
					       (let ((form
						      (if (cdr tail)
							  (copy-reentrant-tree form nil)
							form)))
						 (if (symbolp (car n-and-daughter))
						     ;; it's a kleene n+ marker
						     (nsplice-into-form
						      (car tail)
						      (car n-and-daughter) form
						      nil)
						   (nsubstitute-into-form
						    (car tail) 
						    (car n-and-daughter) form))))
					   (cdr n-and-daughter))
					(list form)))
				  forms)))
			     (dolist (f forms)
			       (if (and (consp f) (atom (car f)) (not (eq (car f) '|lambda|)))
				   (push (simplify-lambda-formula f) current-grs)
				   (push f heads)))))
		       motherf)
                      (list (list heads current-grs 
                                  (list e-score r-score) (cdr dalts)))))
		daughters-alts))
	     ;;(format t "~%here2 ~%")
	     ;; now daughters-alts holds alternative : 
	     ;; (mother-heads, mother-grs, mother-e-scores, daughters)
	     ;; store this list but also pass up only 
	     ;; alt-mother-heads and alt-mother-e-scores
	     (let ((alt-mother-heads nil)
		   (alt-mother-e-scores nil))
	       (dolist (dalt daughters-alts)
		 (let* ((heads (car dalt))
                        (e-score (caaddr dalt))
			(alt-mother 
			 (member heads alt-mother-heads :test #'equal))
			(alt-mother-pos 
			 (if alt-mother
			     (- (length alt-mother-heads) 
				(length alt-mother)) nil)))
		   (if alt-mother-pos
		       (setf (elt alt-mother-e-scores alt-mother-pos) 
                         (add-probs e-score 
                                    (elt alt-mother-e-scores alt-mother-pos)))
                     (progn
                       (push heads alt-mother-heads)
		       (push e-score alt-mother-e-scores)))))
	       (dolist (unp unpacked)
		 (let* ((heads (car unp))
                        (e-score (pop unpacked-scores))
			(alt-mother 
			 (member heads alt-mother-heads :test #'equal))
			(alt-mother-pos 
			 (if alt-mother
			     (- (length alt-mother-heads) (length alt-mother)) nil)))
		   (if alt-mother-pos
		       (setf (elt alt-mother-e-scores alt-mother-pos) 
                         (add-probs e-score 
                                    (elt alt-mother-e-scores alt-mother-pos)))
		     (progn
                       (push heads alt-mother-heads)
		       (push e-score alt-mother-e-scores)))))
	       
	       ;;(format t "~%here3 ~%")
	       ;;(format t "alt-mother ~A ~%" alt-mother-heads) 
               ;;(when (> (length alt-mother-heads) 1)
               ;;  (pprint alt-mother-heads)
               ;;  (pprint tail)
               ;;  )
	       ;; pass up alt-mother-heads and scores:
	       ;; for each alt-mother-head we create structure (alt-head-structure)
	       ;; (alt-head(s), alt-e-score, all-possible-analyses)
	       ;; all-possible-analyses is a list of structures of the form:
	       ;; (<heads>,<scores:e-score, r-score>,<GRs>,<daughter-structure>) 
	       ;;
	       ;; as possible-analyses contains all sub-analyses, each
	       ;; of these is only a sub-analyses of the particular alt-head
	       ;; if the alt-head(s) are the same!
	       ;;
	       ;; each of the <daughter-structure>s are the same format as
	       ;; the alt-head-structure described here.
	       ;;
	       ;; The entire structure is built up so that the parse forest
	       ;; only need be walked once, then the end structure with
	       ;; same heiarchy as parse forest is traversed as it contains
	       ;; all the info I need - inside/reduce probs and heads/GRs 
	       ;; of each node.
	       ;; Therefore, calculate outside probs and get GRS later...
	       
	       ;; also need to make sure that the list of sub-analyses 
	       ;; is not duplicated in unpacked - 
	       ;; so get unique set of sub-analyses first
	       (let ((unpacked-daughters nil))
		 (dolist (unpd unpacked)
		   (dolist (alt-unpd (cddr unpd))
		     (pushnew alt-unpd unpacked-daughters)))
                 (dolist (dalt daughters-alts)
                   (push dalt unpacked-daughters))
		 (setq unpacked (mapcar
				 #'(lambda (alt-mother alt-score)
				       (nconc
					(list alt-mother (ncons alt-score))
					unpacked-daughters))
				 alt-mother-heads
		 		 alt-mother-e-scores)))
	       (setq unpacked-scores alt-mother-e-scores)
;;;               (when (> (length alt-mother-heads) 1)
;;;                 ;;(format t "~%multiple heads: ~%")
;;;                 ;;(pprint unpacked)
;;;                 )
               ))))
      (setf (node-ntrans (car tree))
	(cons (list* rule-cat 'EWG-ALG unpacked unpacked-scores) ;;(list* rule-cat unpacked unpacked-scores) 
	      (if (consp (node-ntrans (car tree))) 
		  (node-ntrans (car tree)) nil)))
      ;;(format t "returning....~%")
      (values unpacked unpacked-scores)))


;; traverse unpacked structure and get grs!
(defun lr1-parse-extract-weighted-outside (unps)
  ;;(format t "doing outside:~%")
  (dolist (u unps)
    ;;(format t "~%A U NODE ~A~%" (length unps))
    ;;(format t "~%before~%")
    ;;(pprint u)
    ;;(lr1-parse-extract-weighted-outside1 u (car u) 0.0)
    (lr1-parse-extract-weighted-outside1-breadthfirst u)
    ;;(format t "~%after~%")
    ;;(pprint u)
    ;;(lr1-parse-extract-weighted-outside-getgrs1 u (car u))
    ;;(format t "~%done getting grs~%")
    (let ((u-score (caadr u)))
      (if (eql *total-prob 0.0)
	  (setf *total-prob u-score)
	(setf *total-prob (add-probs *total-prob u-score)
              )))))

(defun add-probs (&rest r)
  (if +count-parses+ (apply #'+ r) (apply #'alp r)))

(defun mult-probs (&rest r)
  (if +count-parses+ (apply #'* r) (apply #'+ r)))

(defun div-probs (x y)
  (if +count-parses+ (/ x y) (- x y)))

(defun init-value (value)
  (if +count-parses+ 1 value))

(defun lr1-parse-extract-weighted-outside1-breadthfirst (node-top)
  (let ((queue (list (list node-top (list (init-value 0.0d0)
                                          )))))
    (loop
      (if (null queue) (return))
      (let ((node-set (if (atom queue) queue (car queue))))
        (setq queue (if (atom queue) NIL (cdr queue)))
        ;; do for each alternative analysis (has GRs)
        ;; for node-set (given semantic head):
        (dolist (alt (cddar node-set)) 
          (let* ((fs-parent (cadr node-set))
                 (alt-head (car alt))
                 ;; (e-score r-score)
                 (alt-e-score (car (caddr alt)))
                 (alt-reduce (init-value (cadr (caddr alt))))
                 (alt-prob (reduce #'add-probs 
                                   (mapcar #'(lambda (f-score)
                                               (mult-probs f-score alt-e-score))
                                      fs-parent)))
                 (daughters (cadddr alt))
                 (mult-daughters-e-score
                  (reduce #'mult-probs 
                          daughters :key #'caadr)))
            
            ;; check ok to follow down path - same head as one in node-set
            (when (equal (caar node-set) alt-head)
              (if (> (length (caddr alt)) 2)
                  (setf (elt (caddr alt) 2) 
                    (alp (elt (caddr alt) 2) alt-prob))
                (nconc (caddr alt) (list alt-prob))
                )
              ;; get GRs if any
              (when (cadr alt)
                (dolist (gr (cadr alt))
                  (incf (gethash gr *weighted-grs-table 0.0d0)
                        (if +count-parses+ alt-prob (expt 10.0D0 alt-prob))))
                )
              ;; get total e then subtract daughter e 
              ;; to get sum of other siblings e
              ;; (format t "length daughters ~A~%" (length daughters))
              
              ;; do for each of the daughters in particular analysis
              ;; daughters are same format as node-set
              (dolist (d daughters)
                ;;(pprint d)
                ;;(terpri t)
                (let* ((d-score (caadr d))
                       (f-daughter 
                        (reduce #'add-probs 
                                (mapcar #'(lambda (f-score)
                                            (mult-probs 
                                             f-score alt-reduce
                                             (div-probs 
                                              mult-daughters-e-score 
                                              d-score)))
                                        fs-parent))))
                  ;;(format t "~%--------~%daughter: ~%")
                  ;;(pprint d)

                  ;; add ioa log sum to daughters list of scores!
                  (if (> (length (cadr d)) 1)
                      (setf (elt (cadr d) 1) 
                        (add-probs (elt (cadr d) 1)
                             (mult-probs d-score f-daughter)))
                    (nconc (cadr d) (list (mult-probs d-score f-daughter)))
                    )
                  ;;(terpri t)
                  ;;(pprint d)
                  
                  (let ((mem (member d queue :key #'car)))
                    (if mem
                        ;;(progn
                          ;;(format t "member: ~%")
                          (push f-daughter (cadar mem))
                          ;;)
                      (setq queue (nconc queue (list (list d (list f-daughter))))) 
                      ))
                  ;; if word node then store IOA score in *tags-and-scores
;;;                  (when +get-tags+
;;;                    (when (null (cddr d))
;;;                      (let ((mem (member (intern (caar d)) *tags-and-scores :key #'car)))
;;;                        (if mem
;;;                            (if (cadar mem)
;;;                                (setf (cadar mem) 
;;;                                  (add-probs 
;;;                                   (cadar mem) 
;;;                                   (mult-probs d-score f-daughter)))
;;;                              (setf (cadar mem) 
;;;                                (mult-probs d-score f-daughter)))
;;;                          (format t "CAN:T FIND: ~A ~A ~A~%" (caar d) 
;;;                                  (type-of (caar d)) (type-of (caar *tags-and-scores)))
;;;                          ))))
                        )))))))))


