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

;;; Extract complementation grammatical relations from parse trees
;;; in runprob job. This will call the equivalent of
;;;
;;;   (lr1-parse-analysis-rule-tree (car *current-parse-trees))
;;;
;;; to compute result. Use comp-stats.lsp - which returns number of parse
;;; trees, then a list of:
;;;
;;;   ((<pre-verb> <rule-name> <word>_<tag> {* | <compl-head>} ...) . <freq>)
;;;   where <pre-verb> is either {* | <subj-lex>} or PASSIVE {* | <dir-obj-lex>}
;;;
;;; and then create
;;;
;;; Calls stats-from-parses so needs sub/comp-stats.lsp to be loaded

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
(setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print)
;; (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-grs-print-with-weights)


;;; GR print functions

(defun lr1-parse-analysis-grs-print (grs-sets out-str)
   (loop for grs in grs-sets
      do
      (format out-str "~&")
      (dolist (gr grs)
         (lr1-parse-analysis-gr-print gr out-str) (terpri out-str))))


(defun lr1-parse-analysis-gr-print (gr out-str)
   (declare (special +parseval-output-p+ +parc700+))
   (write
    (if +parseval-output-p+
	(cons (car gr)
            (mapcar
               #'(lambda (item)
                   (if (or (consp item) (eq item '_))
            	      item
            	      (let* ((str (string item))
                             (tag-pos (position #\_ str :from-end t))
                             (etag-pos
                                (and tag-pos (position #\< str :start tag-pos))))
                         (if tag-pos
                            (make-symbol
                               (if etag-pos
                                  (concatenate 'string (subseq str 0 tag-pos)
                                     (subseq str etag-pos))
                                  (subseq str 0 tag-pos)))
                            item))))
               (cdr gr)))
         gr)
      :stream out-str :pretty nil 
      :escape (and (null +parc700+) (null +parseval-output-p+))))


(defun lr1-parse-analysis-tree-and-grs-print (grs-sets out-str)
   (declare (special *raw-tree*))
   (terpri out-str)
   (write (lr1-analysis-rule-tree *raw-tree*) :stream out-str :pretty t :escape t)
   (loop for grs in grs-sets
      do
      (terpri out-str)
      (dolist (gr grs)
         (lr1-parse-analysis-gr-print gr out-str) (terpri out-str))))


(defvar *weights-and-grs* nil)

(defvar +parc700+)
(setq +parc700+ nil) ;; for printing out parc700 (no weights)

(defvar +gr-threshold+) ;; threshold over system weighted grs
(setq +gr-threshold+ 0.0d0)

(defun lr1-parse-analysis-grs-print-with-weights (grs-sets out-str)
   (declare (special +parseval-output-p+ *first-tree-p* *last-tree-p* *tree-weight*))
   (when *first-tree-p* (setq *weights-and-grs* nil))
   (dolist (grs grs-sets)
      (push (cons (- *tree-weight* (log (length grs-sets) 10)) grs)
         *weights-and-grs*))
   (when *last-tree-p*
      (let ((table nil)
            (total 0))
         (dolist (weight-and-grs *weights-and-grs*) ; in reverse rank order
            ;; compute in double precision otherwise underflow
            (let ((weight (expt 10.0D0 (car weight-and-grs))))
               (incf total weight)
               (dolist (gr (cdr weight-and-grs))
                  (let ((entry (assoc gr table :test #'lr1-parse-grs-equal-p)))
                     (unless entry (push (setq entry (cons gr 0)) table))
                     (incf (cdr entry) weight)))))
         (format out-str "~&")
         ;; gramreleval expects braces enclosing set of GRs if probs are output
         (when +parseval-output-p+ (format out-str "{~%"))
         (dolist (entry table)
            (let ((gr (car entry)) (w (cdr entry))
                  (*read-default-float-format* 'single-float))
                (when (not +parc700+)
                  (if (eql w total)
                     (format out-str " 1.0 ~16,1T ")
                     (format out-str " ~,6G ~16,1T " 
                        (coerce (/ w total) 'single-float)))) ; get E rather than D
                (lr1-parse-analysis-gr-print gr out-str) (terpri out-str)))
         (when +parseval-output-p+ (format out-str "}~%"))
         (setq *weights-and-grs* nil))))


(defun lr1-parse-grs-equal-p (a b)
   (cond
      ((null a) (null b))
      ((null b) (null a))
      ((or (not (symbolp (car a))) (not (symbolp (car b)))) nil)
      ((string= (string (car a)) (string (car b)))
         (lr1-parse-grs-equal-p (cdr a) (cdr b)))))


;;; GR extraction

(defparameter +gr-semantics-p+ t)

(defun lr1-parse-analysis-grs (tree nvt)
   (mapcar
      (lambda (alt) (mapcar #'simplify-lambda-formula alt))
      (extract-semantics-from-parse-tree tree nvt)))


;;; End of file
