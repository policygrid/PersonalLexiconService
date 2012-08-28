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

;;; Recovery of partial parses.
;;;
;;; Author: John Carroll
;;;         University of Sussex
;;;
;;; When the parser fails to produce a single complete analysis, there are three
;;; main cases:
;;;
;;; (T/frag ... fragments ...)
;;; extragrammaticality, perhaps due to a tagging mistake; the parser could get
;;; to the end of the utterance but it was unable to build any single complete
;;; analysis -- the parser returns a sequence of fragmentary analyses (any of
;;; which might be just an individual lexical item)
;;; 
;;; (T/fragx ... fragments ... words ...)
;;; extragrammaticality such that the grammar allowed no way of shifting one or
;;; more words onto the stack, so there were lexical items still left in the
;;; input buffer -- the parser returns a sequence of fragmentary analyses
;;; followed by the remaining words
;;; 
;;; (X ... words ...)
;;; the parser timed out -- no parse tree returned

(in-package #+(or x3j13 cltl2 ansi-cl) common-lisp-user #-(or x3j13 cltl2 ansi-cl) 'user)


(defun lr1-parse-collect-partial-analyses (vs word-rest-cats)
   ;; !!! returns first-ranked path only, ignoring +n-best-retained+ setting
   (let ((table (make-hash-table :test #'eq))
         (max nil)
         (max-score 0.0)
         (max-weight most-negative-single-float)
         (+n-best-retained+ 1))
      ;; find max scoring partial analyses
      (dolist (v vs)
         (let ((new (lr1-parse-collect-partial-analyses1 v table)))
            (when (and (consp new) (> (car new) max-weight))
               (setq max-weight (car new) max-score (cadr new) max (cddr new)))))
      ;; unpack any remaining, non-shifted lexical items one by one
      (dolist (alt-sets word-rest-cats)
         (let ((unpacked nil) (unpacked-scores nil))
            (dolist (n-and-analyses alt-sets)
               (dolist (a (cdr n-and-analyses))
                  (multiple-value-bind (unps unp-scores)
                        (lr1-parse-unpack (cdr a) (car a) nil)
                     (setq unpacked (append unps unpacked))
                     (setq unpacked-scores (append unp-scores unpacked-scores)))))
            (multiple-value-setq (unpacked unpacked-scores)
               (lr1-parse-unpack-prune unpacked unpacked-scores))
            (push unpacked max)
            (incf max-score (car unpacked-scores))))
      ;; construct daughters from max-ranked nodes sets
      (let ((rev-daughters
               (mapcan
                  #'(lambda (trees-sets)
                      (dolist (trees-set trees-sets
                                 ;; all alternatives for a daughter have failed
                                 (return-from lr1-parse-collect-partial-analyses nil))
                         (let ((analysis
                                  (cdr
                                     (lr1-pf-verify-tree
                                        ;; NB first tree in set is the enclosing one
                                        (lr1-parse-first-node-in-set trees-set)
                                        nil trees-set))))
                            (when analysis
                               (return
                                  (if (eq (cdr (car analysis)) '|elide|)
                                     (copy-list (cdr analysis))
                                     (list analysis)))))))
                  max))
            (tcats
               (top-declaration-categories
                  (normalise-top-definition (car *top)))))
         (when rev-daughters
            (list
               (make-analysis 0 max-score
                  (cons nil ; vt - lose nvt bindings of fitted daughters
                     `((,(car (convert-category-to-parser (caar tcats) (car tcats) nil)) 
                        ,(if (lr1-cats-punct-or-empty word-rest-cats) "T/frag" "T/fragx"))
                       ,@rev-daughters))))))))


(defparameter +punct-tag-list+ '(|,| |;| |:| |!| |?| |-| |--| |(| |)| |.| |...| |/| |"|))

(defun lr1-cats-punct-or-empty (word-cats)
   ;; no remaining words (was lookaheads), or all of them punctuation and
   ;; therefore not interesting for analysis result
   (or (null word-cats)
      (every
         #'(lambda (alt-sets)
            (every
               #'(lambda (n-and-analyses)
                  (every
                     #'(lambda (a) 
                        (let ((w (cadr (cdr a))))
                           (every
                              #'(lambda (p) (member p +punct-tag-list+ :test #'eq))
                              (word-input-item-pos-tags w))))
                     (cdr n-and-analyses)))
               alt-sets))
         word-cats)))


(defun lr1-parse-first-node-in-set (node-set)
   ;; like car only knows about 'fake' appends marked with :embedded
   (if (and (consp node-set) (eq (car node-set) :embedded))
      (lr1-parse-first-node-in-set (cadr node-set))
      (car node-set)))

(defun lr1-parse-collect-partial-analyses1 (v table)
   ;; return (max-weight max-analysis-score . tree-set-alts-list) for best scoring
   ;; path of analyses from this vertex back to the root - or t if no possible path
   ;; (due to unpacking failure)
   (cond
      ((null (vertex-parents+analyses v)) ; the root
         (list* 0 0.0 nil))
      ((gethash v table))
      (t
         (let ((max-new nil)
               (max-new-weight most-negative-single-float)
               (max-new-score 0.0))
            (dolist (p+as (vertex-parents+analyses v))
               (let ((p-entry
                        ;; get best path from parent back to root
                        (lr1-parse-collect-partial-analyses1 (car p+as) table)))
                  (when (consp p-entry) ; is there a legitimate path from parent?
                     (let* ((weight ; of non-packed version of first analysis
                              (lr1-parse-partial-analysis-weight (car (cdr p+as))))
                            (new-weight (+ (car p-entry) weight))
                            (unpacked nil) (unpacked-scores nil))
                        (when (> new-weight max-new-weight)
                           ;; get best analysis arc back to parent - there may be packed
                           ;; nodes in it so unpack to n-best set of nodes
                           (dolist (a (cdr p+as))
                              (multiple-value-bind (unps unp-scores)
                                  (lr1-parse-unpack (cdr a) (car a) nil)
                                 (setq unpacked (append unps unpacked))
                                 (setq unpacked-scores (append unp-scores unpacked-scores))))
                           (multiple-value-setq (unpacked unpacked-scores)
                              (lr1-parse-unpack-prune unpacked unpacked-scores))
                           (when unpacked
                              (setq max-new-weight new-weight
                                    max-new (cons unpacked (cddr p-entry))
                                    max-new-score
                                    (+ (car unpacked-scores) (cadr p-entry)))))))))
            (setf (gethash v table)
               (if max-new (list* max-new-weight max-new-score max-new) t))))))


(defun lr1-parse-partial-analysis-weight (vt-and-tree)
   ;; number of tree nodes, not counting any nodes introduced by unary rules,
   ;; but single lexical nodes are penalised with a negative weight
   (labels ((tree-nodes (tree)
              (if (and (cdr tree) (atom (cadr tree)))
                 1
                 (let ((n (length (cddr tree))))
                    (dolist (d (cdr tree) n)
                       (incf n (tree-nodes d)))))))
      (let ((nnodes (tree-nodes (cdr vt-and-tree))))
         ;;(print (list* (find-if #'stringp
         ;;                (and (consp (node-rest (cadr vt-and-tree))) (node-rest (cadr vt-and-tree))))
         ;;             (node-score (cadr vt-and-tree))
         ;;             nnodes))
         (if (eql nnodes 1) -5 nnodes))))


;;; End of file
