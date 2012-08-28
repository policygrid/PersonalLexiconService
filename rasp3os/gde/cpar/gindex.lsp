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

;;; ANLT CHART PARSER - GRAMMAR RULE INDEX FUNCTIONS

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Rules are expected to be in object grammar formalism

(defun g-indexrules (rules)
   (setq g-tracerules nil)
   (setq g-gramtree nil)
   (setq g-gramtree
      (g-makeruletree (mapcan #'g-ruleindex rules)
         (make-array (1+ *current-category-index) :initial-element nil))))



;;; Given a PS rule, return a list consisting of (<rule index>
;;; <rule>) for parser rule tree. Index is category of first
;;; non-trace daughter. Return nil if no non-trace daughters -
;;; if this is the case then add rule to trace rule vector.

(defun g-ruleindex (rule)
   (g-nontracedaughter (cdr rule) rule))


(defun g-nontracedaughter (daughters rule)
   (cond
      ((null daughters)
         (setq g-tracerules
            (nconc g-tracerules
               (list
                  (cons (car rule)
                     (mapcar
                        #'(lambda (d)
                           (cons d
                              (cons 'e (semantics-for-null))))
                        (cdr rule))))))
         nil)
      ((eq (cdar daughters) '|null|)
         (g-nontracedaughter (cdr daughters) rule))
      (t
         (list (cons (caar daughters) rule)))))


;;; Make a indexed tree from cat-rule-alist, each element of which is a
;;; pair of the form (index-category . rule). Tree contains rules indexed
;;; on the index-categories.

(defun g-makeruletree (cat-rule-alist tree)
   (dolist (cat-rule cat-rule-alist)
      (let ((cat (car cat-rule)))
         (setf (svref tree (svref cat 0))
            (cons
               (or (car (svref tree (svref cat 0)))
                  (g-reentrant-p cat nil))
               (g-addtoruletree cat 1 (cdr cat-rule)
                  (cdr (svref tree (svref cat 0))))))))
   tree)


(defun g-addtoruletree (cat offset data tree)
   (cond
      ((>= offset (length cat))
         (cons data tree))
      ((null tree)
         (g-addruleentry cat offset data))
      (t
         (let ((cval (svref cat offset))
               (tval (car tree)))
            (if
               (or (eq cval tval)
                  (and (g-categoryp cval) (g-categoryp tval)
                     (g-identical-p cval tval)))
               (setf (cadr tree)
                  (g-addtoruletree cat (1+ offset) data (cadr tree)))
               (setf (cddr tree)
                  (g-addtoruletree cat offset data (cddr tree))))
            tree))))


(defun g-addruleentry (cat offset data)
   (cond
      ((>= offset (length cat))
         (list data))
      (t
         (list (svref cat offset)
            (g-addruleentry cat (1+ offset) data)))))


;;; Search relevant tree for rules indexed on cat, i.e. whose indexes
;;; match category argument. Features are guaranteed to be the same and
;;; in same order. Don't need to look up variable bindings in index
;;; category in a tree which contains no re-entrant index categories

#-gde-debug
(eval-when (compile)
   (declaim (ftype (function (t fixnum t t t t) t) g-retrieve-term))
   (declaim (ftype (function (t fixnum t t t t) t) g-retrieve-graph)))


(defun g-retrieve-rules (cat rvt nvt tree-vec)
   (let ((tree-pair
            (vector-svref tree-vec (vector-svref cat 0))))
      (when tree-pair
         (let ((g-search-bindings (cons-car tree-pair)))
            (cond
               ((g-fixnum-eql (simple-vector-length cat) 1)
                  (cons
                     (cons (cons rvt nvt) (cons-cdr tree-pair))
                     nil))
               (*term-unification
                  (g-retrieve-term cat 1 rvt nvt
                     (cons-cdr tree-pair) nil))
               (t
                  (g-retrieve-graph cat 1 rvt nvt
                     (cons-cdr tree-pair) nil)))))))


(defun g-retrieve-term (cat offset rvt nvt tree res)
   #-gde-debug (declare (fixnum offset))
   (loop
      (if tree
         (multiple-value-bind (succeed new-rvt new-nvt)
            (g-unify-values
               (cons-car tree) (vector-svref cat offset) rvt nvt)
            (when succeed
               (setq res
                  (let ((new-offset (fixnum-add-1 offset)))
                     #-gde-debug (declare (fixnum new-offset))
                     (if (g-fixnum-eql new-offset (simple-vector-length cat))
                        (cons
                           (cons (cons new-rvt new-nvt)
                              (cons-car (cons-cdr tree)))
                           res)
                        (g-retrieve-term cat new-offset
                           new-rvt new-nvt (cons-car (cons-cdr tree)) res)))))
            (setq tree (cons-cdr (cons-cdr tree))))
         (return res))))


(defun g-retrieve-graph (cat offset rvt nvt tree res)
   #-gde-debug (declare (fixnum offset))
   (loop
      (if tree
         (multiple-value-bind (succeed new-rvt new-nvt)
            (g-unify-values-graph
               (cons-car tree) (vector-svref cat offset) rvt nvt)
            (when succeed
               (setq res
                  (let ((new-offset (fixnum-add-1 offset)))
                     #-gde-debug (declare (fixnum new-offset))
                     (if (g-fixnum-eql new-offset (simple-vector-length cat))
                        (cons
                           (cons (cons new-rvt new-nvt)
                              (cons-car (cons-cdr tree)))
                           res)
                        (g-retrieve-graph cat new-offset
                           new-rvt new-nvt (cons-car (cons-cdr tree)) res)))))
            (setq tree (cons-cdr (cons-cdr tree))))
         (return res))))


;;; End of file
