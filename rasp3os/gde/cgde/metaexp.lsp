#|----------------------------------------------------------------------------|
 | Copyright 1992, 2002, 2006, 2011 John Carroll, Ted Briscoe, Rebecca Watson |
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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - METARULE EXPANSION
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code for for matching ID rules to
;;; rule patterns, and expanding ID rules with metarules.
;;;
;;; Entry points:
;;;
;;;  * (defun Metarule-expand (idrules metarule-names) ...
;;;  * (defun Linear-metarule-expand (idrules metarule-names) ...
;;;  * (defun Match-rule-pattern (idrule-bindings ...
;;;  * (defun Filter-linear-bindings (match-bindings idrule
;;; pattern-rule-binding-nos pattern-rule-bindings linear) ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; ID rules for ordinary metarule expansion are assumed not to
;;; have been previously propagated and defaulted. Linear ID
;;; rules on the other hand should have been. ID rules for both
;;; functions are assumed to be not linearised or to be
;;; linearised respectively.

(defun metarule-expand (idrules metarule-names)
   (when idrules
      (let
         ((full-idrules
             (catrule-expand
                (proprule-and-defrule-expand idrules))))
         (append full-idrules
            (mapcan
               #'(lambda (metarule-name)
                    (cond
                       ((not
                           (meta-rule-linear
                              (get metarule-name 'metarule)))
                          (metarule-expand
                             (mapcan
                                #'(lambda (idrule)
                                     (apply-metarule idrule
                                        (normalise-metarule-definition
                                           metarule-name)
                                        metarule-name))
                                full-idrules)
                             (cdr
                                (member metarule-name metarule-names
                                   :test #'eq))))))
               metarule-names)))))


(defun linear-metarule-expand (full-idrules metarule-names)
   (when full-idrules
      (append full-idrules
         (mapcan
            #'(lambda (metarule-name)
                 (cond
                    ((meta-rule-linear
                        (get metarule-name 'metarule))
                       (linear-metarule-expand
                          (sort-features-in-idrules
                             (catrule-expand
                                (proprule-and-defrule-expand
                                   (mapcan
                                      #'(lambda (idrule)
                                           (apply-metarule
                                              idrule
                                              (normalise-metarule-definition
                                                 metarule-name)
                                              metarule-name))
                                      full-idrules))))
                          (cdr
                             (member metarule-name metarule-names
                                :test #'eq))))))
            metarule-names))))


;;; Apply a single metarule to an idrule.

(defun apply-metarule
   (idrule metarule metarule-name)
   (when
      (or (id-rule-lexical idrule)
         (not (meta-rule-lexical metarule)))
      (let
         ((match-bindings
             (remove-multiple-expansions
                (filter-linear-bindings
                   (match-rule-pattern
                      (id-rule-binding-list idrule)
                      (id-rule-binding-nos idrule)
                      (meta-rule-cat-bindings metarule)
                      (meta-rule-lhs-binding-nos metarule))
                   idrule (meta-rule-lhs-binding-nos metarule)
                   (meta-rule-cat-bindings metarule)
                   (meta-rule-linear metarule))
                (id-rule-binding-list idrule)
                (id-rule-name idrule) metarule-name))
            (index 0))
         (cond
            ((and match-bindings
                (cdr match-bindings))
               (gde-warn "multiple match between "
                  (idrule-name-string (id-rule-name idrule))
                  " and " metarule-name)))
         (split-optional-category-idrules
            (mapcar
               #'(lambda (m-b)
                  (setf index (1+ index))
                  (instantiate-idrule
                     (let
                        ((structure-56
                              (copy-top-rule-name
                                 (id-rule-name idrule))))
                        (setf
                           (top-rule-name-meta-names
                              structure-56)
                           (append
                              (top-rule-name-meta-names
                                 structure-56)
                              (ncons
                                 (make-sub-rule-name :base
                                    metarule-name :index
                                    (when (cdr match-bindings)
                                       index)
                                    :split nil))))
                        structure-56)
                     idrule metarule m-b))
               match-bindings)))))


;;; The matching functions return a list of sets of
;;; Match-binding records, one for each possible match between
;;; the pattern rule and the idrule.

(defun match-rule-pattern
   (idrule-bindings idrule-binding-nos
      pattern-bindings pattern-binding-nos)
   (if
      (match-category
         (car pattern-binding-nos)
         pattern-bindings
         (car idrule-binding-nos)
         idrule-bindings)
      (let
         ((w-var-binding-no
             (look-for-w-var-binding-no pattern-bindings
                pattern-binding-nos)))
         (mapcar
            #'(lambda (rhs-match-bindings)
                 (cons
                    (make-match-binding :pattern-binding-no
                       (car pattern-binding-nos)
                       :matched-binding-nos
                       (ncons (car idrule-binding-nos)))
                    rhs-match-bindings))
            (match-rhs-rule-pattern
               (remove-list-1 w-var-binding-no
                  (cdr pattern-binding-nos))
               pattern-bindings
               (cdr idrule-binding-nos)
               idrule-bindings w-var-binding-no)))))


(defun look-for-w-var-binding-no
   (cat-bindings binding-nos)
   (dolist (binding cat-bindings)
      (let
         ((var-57
             (if
                (and
                   (eq (category-binding-repetition binding)
                      '*w*)
                   (member (category-binding-number binding)
                      binding-nos))
                (category-binding-number binding))))
         (if var-57 (return var-57)))))


(defun match-rhs-rule-pattern
   (rhs-pattern-binding-nos pattern-bindings
      rhs-idrule-binding-nos idrule-bindings
      w-var-binding-no)
   (cond
      (rhs-pattern-binding-nos
         (collect-possible-bindings
            (car rhs-pattern-binding-nos)
            (mapcan
               #'(lambda (idrule-binding-no)
                    (cond
                       ((match-category
                           (car rhs-pattern-binding-nos)
                           pattern-bindings idrule-binding-no
                           idrule-bindings)
                          (list
                             (ncons
                                (make-match-binding
                                   :pattern-binding-no
                                   (car
                                      rhs-pattern-binding-nos)
                                   :matched-binding-nos
                                   (ncons
                                      idrule-binding-no)))))))
               rhs-idrule-binding-nos)
            (cdr rhs-pattern-binding-nos)
            pattern-bindings rhs-idrule-binding-nos
            idrule-bindings w-var-binding-no))
      ((and (null w-var-binding-no)
          (null rhs-idrule-binding-nos))
         (ncons nil))
      (w-var-binding-no
         (ncons
            (ncons
               (make-match-binding :pattern-binding-no
                  w-var-binding-no :matched-binding-nos
                  rhs-idrule-binding-nos))))))


;;; Match-bindings record which idrule categories match the
;;; pattern category under consideration (indexed by
;;; pattern-binding-no). It is a list of sets of match-binding
;;; records.
;;;
;;; Collect-binding-sets takes the list of match-binding sets
;;; and returns a list, each element of which is a cons of a
;;; single match-binding set and a list of ID rule categories
;;; yet to be matched. Thus fewer or more than one ID rule
;;; categories may get consumed by the current pattern category,
;;; depending on its repetition spec.

(defun collect-possible-bindings
   (pattern-binding-no match-bindings
      rhs-pattern-binding-nos pattern-bindings
      rhs-idrule-binding-nos idrule-bindings
      w-var-binding-no)
   (cond
      (match-bindings
         (mapcan
            #'(lambda (single-set-and-remaining-nos)
                 (mapcar
                    #'(lambda (remainding-bindings)
                         (append
                            (car single-set-and-remaining-nos)
                            remainding-bindings))
                    (match-rhs-rule-pattern
                       rhs-pattern-binding-nos pattern-bindings
                       (cdr single-set-and-remaining-nos)
                       idrule-bindings w-var-binding-no)))
            (collect-binding-sets pattern-binding-no
               match-bindings pattern-bindings
               rhs-idrule-binding-nos)))))


(defun collect-binding-sets
   (pattern-binding-no match-bindings-sets
      pattern-bindings rhs-idrule-binding-nos)
   (let
      ((repetition
          (category-binding-repetition
             (f-find (the fixnum pattern-binding-no)
                pattern-bindings :key
                #'category-binding-number :test #'eql))))
      (cond
         ((or (eq repetition '*once*)
             (eq repetition '*opt*))
            (nconc
               (if (eq repetition '*opt*)
                  (ncons (cons nil rhs-idrule-binding-nos)))
               (mapcar
                  #'(lambda (single-set)
                       (cons single-set
                          (remove-list-1
                             (car
                                (match-binding-matched-binding-nos
                                   (car single-set)))
                             rhs-idrule-binding-nos)))
                  match-bindings-sets)))
         (t
            (mapcan
               #'(lambda (combination)
                    (cond
                       ((or (eq repetition '*rep0*)
                           (car combination))
                          (list
                             (cons
                                (cons
                                   (make-match-binding
                                      :pattern-binding-no
                                      pattern-binding-no
                                      :matched-binding-nos
                                      (mapcan
                                         #'(lambda (single-set)
                                            (copy-list
                                               (match-binding-matched-binding-nos
                                                 (car single-set))))
                                         (car combination)))
                                   (combine-match-binding-sets
                                      (mapcan
                                         #'(lambda (single-set)
                                              (copy-list
                                                 (cdr single-set)))
                                         (car combination))))
                                (mapcan
                                   #'(lambda (single-set)
                                      (copy-list
                                         (match-binding-matched-binding-nos
                                           (car single-set))))
                                   (cdr combination)))))))
               (collect-all-binding-no-combinations
                  match-bindings-sets))))))


(defun combine-match-binding-sets
   (bindings)
   (let
      ((combined nil))
      (dolist (binding bindings)
         (let
            ((combined-binding
                (f-find
                   (the fixnum
                      (match-binding-pattern-binding-no binding))
                   combined :key
                   #'match-binding-pattern-binding-no :test
                   #'eql)))
            (setf combined
               (cons
                  (make-match-binding :pattern-binding-no
                     (match-binding-pattern-binding-no binding)
                     :matched-binding-nos
                     (if combined-binding
                        (combine-binding-nos
                           (match-binding-matched-binding-nos
                              combined-binding)
                           (match-binding-matched-binding-nos
                              binding))
                        (match-binding-matched-binding-nos
                           binding)))
                  (remove-list-1 combined-binding
                     combined)))))
      combined))


(defun combine-binding-nos (set1 set2)
   (cond
      ((null set1) set2)
      ((member (car set1) set2)
         (combine-binding-nos (cdr set1) set2))
      (t
         (cons (car set1)
            (combine-binding-nos (cdr set1)
               set2)))))


(defun collect-all-binding-no-combinations
   (nos)
   (cond
      ((null nos) (ncons (cons nil nil)))
      (t
         (mapcan
            #'(lambda (res)
                 (list
                    (cons (cons (car nos) (car res))
                       (cdr res))
                    (cons (car res)
                       (cons (car nos) (cdr res)))))
            (collect-all-binding-no-combinations
               (cdr nos))))))


;;; If the pattern rule was linear, just retain the bindings
;;; from linear matches between it and the ID rule. Scan down the
;;; pattern rule binding nos, checking that the corresponding matched
;;; ID rule categories are in the same order as in the pattern rule.
;;; Ignore W or U variables.

(defun filter-linear-bindings (match-bindings idrule
      pattern-rule-binding-nos pattern-rule-bindings linear)
   (cond
      (linear
         (mapcan
            #'(lambda (binding)
                 (when
                    (linear-match-bindings binding
                       pattern-rule-binding-nos
                       (id-rule-binding-nos idrule)
                       pattern-rule-bindings)
                    (ncons binding)))
            match-bindings))
      (t match-bindings)))


(defun linear-match-bindings (binding pattern-rule-binding-nos
      idrule-binding-nos pattern-rule-bindings)
   (cond
      ((null pattern-rule-binding-nos) t)
      ((eq
          (category-binding-repetition
             (f-find (car pattern-rule-binding-nos)
                pattern-rule-bindings :key
                #'category-binding-number :test 'eql))
          '*w*)
         (linear-match-bindings
            binding (cdr pattern-rule-binding-nos)
            idrule-binding-nos pattern-rule-bindings))
      (t
         (let
            ((m-binding
                  (f-find (car pattern-rule-binding-nos) binding :key
                     #'match-binding-pattern-binding-no :test
                     #'eql)))
            (cond
               ((null m-binding) ; optional category in pattern rule
                  (linear-match-bindings
                     binding (cdr pattern-rule-binding-nos)
                     idrule-binding-nos pattern-rule-bindings))
               (t (let
                     ((matched-binding-nos
                           (match-binding-matched-binding-nos m-binding)))
                     (if (member (car matched-binding-nos) idrule-binding-nos)
                        (linear-match-bindings
                           binding (cdr pattern-rule-binding-nos)
                           (cdr
                              (member (car matched-binding-nos)
                                 idrule-binding-nos))
                           pattern-rule-bindings)))))))))


;;; Filter out duplicate expansions if flag set. Look down sets
;;; of match bindings, comparing categories in ID rules
;;; corresponding to matched ID rule binding numbers. Print a
;;; warning if duplicates whatever value of flag.

(defun remove-multiple-expansions
   (match-bindings binding-list idrule-name
      metarule-name)
   (cond
      ((or (null match-bindings)
          (null (cdr match-bindings)))
         match-bindings)
      ((and
          (dolist
             (match-bind (cdr match-bindings))
             (if
                (when
                   (is-duplicate-expansion
                      (car match-bindings) match-bind
                      binding-list)
                   (gde-warn
                      "multiple identical match between "
                      (idrule-name-string idrule-name) " and "
                      metarule-name)
                   t)
                (return match-bind)))
          (not *multiple-expansions))
         (remove-multiple-expansions
            (cdr match-bindings) binding-list
            idrule-name metarule-name))
      (t
         (cons (car match-bindings)
            (remove-multiple-expansions
               (cdr match-bindings) binding-list
               idrule-name metarule-name)))))


(defun is-duplicate-expansion (match-bind1 match-bind2 binding-list)
   (dolist (binding-rec1 match-bind1 t)
      (let ((binding-rec2
               (f-find
                  (the fixnum
                     (match-binding-pattern-binding-no binding-rec1))
                  match-bind2
                  :key #'match-binding-pattern-binding-no :test #'eql)))
         (unless
            (and binding-rec2
               (let
                  ((binding-nos2
                        (match-binding-matched-binding-nos binding-rec2)))
                  (and
                     (= (list-length
                           (match-binding-matched-binding-nos binding-rec1))
                        (list-length binding-nos2))
                     (dolist
                        (binding-no1
                           (match-binding-matched-binding-nos binding-rec1)
                           t)
                        (let ((binding-no2 (pop binding-nos2)))
                           (unless
                              (or (= binding-no1 binding-no2)
                                 (match-category
                                    binding-no1 binding-list binding-no2
                                    binding-list))
                              (return nil)))))))
            (return nil)))))


(progn
   (defvar rhs-idrule-match-bindings nil)
   (defvar grammar-errors))


;;; Return a new ID rule resulting from its expansion by the
;;; given metarule. For each RHS metarule top level category,
;;; find the corresponding LHS metarule category (if there is
;;; one), and thence through the match-bindings, the
;;; corresponding input ID rule category. Merge this category
;;; with the RHS metarule category (the metarule one overwriting
;;; if feature values different). If there was no corresponding
;;; LHS metarule category, just add RHS metarule category.
;;;
;;; For each RHS metarule category, add a match-binding
;;; structure to rhs-idrule-match-bindings recording the RHS
;;; metarule / input ID rule category binding number
;;; correspondances for use by semantic ID rule metarule
;;; combination code.
;;;
;;; N.B. Repetition type of input ID rule categories are carried
;;; through unchanged - RHS metarule category repetitions
;;; ignored.

(defun instantiate-idrule
   (expanded-idrule-name idrule metarule
      match-bindings)
   (let
      ((variable-substitutions nil)
         (highest-binding-no
            (id-rule-highest-binding-no idrule))
         (rhs-idrule-match-bindings nil)
         (grammar-errors nil))
      (multiple-value-bind
         (idrule-binding-nos new-idrule-bindings)
         (instantiate-idrule1
            (mapcar
               #'(lambda (binding)
                    (let
                       ((structure-58
                           (copy-category-binding binding)))
                       (setf
                          (category-binding-category
                             structure-58)
                          (copy-list
                             (category-binding-category
                                structure-58)))
                       structure-58))
               (id-rule-binding-list idrule))
            (meta-rule-lhs-rhs-corresponds metarule)
            (meta-rule-lhs-binding-nos metarule)
            (meta-rule-rhs-binding-nos metarule)
            (meta-rule-cat-bindings metarule)
            match-bindings)
         (report-metarule-errors
            expanded-idrule-name)
         (make-id-rule :name expanded-idrule-name
            :binding-nos idrule-binding-nos
            :highest-binding-no highest-binding-no
            :binding-list
            (substitute-feature-bindings
               variable-substitutions new-idrule-bindings)
            :lexical (id-rule-lexical idrule) :linear
            (id-rule-linear idrule) :rules-applied nil
            :semantic-forms
            (combine-idrule-metarule-semantics
               (id-rule-semantic-forms idrule)
               (meta-rule-semantic-forms metarule)
               (nconc rhs-idrule-match-bindings
                  match-bindings)
               (meta-rule-rhs-binding-nos metarule)
               idrule-binding-nos)
            :file nil :comment nil))))


(defun report-metarule-errors (resulting-name)
   ;; only expect incompatible-values error but don't bother
   ;; reporting it
;;;    (dolist
;;;       (error
;;;          (nreverse (remove-duplicates grammar-errors :test #'equal)))
;;;       (let
;;;          ((type (car error)))
;;;          (cond
;;;             ((eq type 'incompatible-values)
;;;                (gde-warn "metarule changed value of "
;;;                   (cddr error) " to produce "
;;;                   (idrule-name-string resulting-name))))))
   (declare (ignore resulting-name))
   nil)


(defun instantiate-idrule1
   (idrule-bindings lhs-rhs-corresponds
      lhs-metarule-binding-nos
      rhs-metarule-binding-nos metarule-bindings
      match-bindings)
   (declare (ignore lhs-metarule-binding-nos))
   (let
      ((idrule-binding-nos nil)
         (overwrite-p
            (if *addition-checking 'metarule-checking
               t)))
      (dolist
         (rhs-metarule-binding-no
            rhs-metarule-binding-nos)
         (let
            ((correspond
                (rassoc rhs-metarule-binding-no
                   lhs-rhs-corresponds))
               (rhs-metarule-binding
                  (f-find
                     (the fixnum rhs-metarule-binding-no)
                     metarule-bindings :key
                     #'category-binding-number :test #'eql)))
            (cond
               (correspond
                  (dolist
                     (idrule-binding-no
                        (match-binding-matched-binding-nos
                           (f-find
                              (the fixnum
                                 (car correspond))
                              match-bindings :key
                              #'match-binding-pattern-binding-no
                              :test #'eql)))
                     (let
                        ((idrule-binding
                            (f-find
                               (the fixnum idrule-binding-no)
                               idrule-bindings :key
                               #'category-binding-number :test
                               #'eql)))
                        (multiple-value-bind (cat new-bindings)
                           (merge-category
                              (category-binding-category
                                 idrule-binding)
                              (remove-list-1 idrule-binding
                                 idrule-bindings)
                              (category-binding-category
                                 rhs-metarule-binding)
                              metarule-bindings overwrite-p)
                           (push
                              (make-match-binding
                                 :pattern-binding-no
                                 rhs-metarule-binding-no
                                 :matched-binding-nos
                                 (ncons idrule-binding-no))
                              rhs-idrule-match-bindings)
                           (push idrule-binding-no
                              idrule-binding-nos)
                           (setf idrule-bindings
                              (cons
                                 (make-category-binding :number
                                    idrule-binding-no :category
                                    cat :repetition
                                    (category-binding-repetition
                                       idrule-binding))
                                 new-bindings))))))
               (t
                  (multiple-value-bind (cat new-bindings)
                     (merge-category nil idrule-bindings
                        (category-binding-category
                           rhs-metarule-binding)
                        metarule-bindings overwrite-p)
                     (let
                        ((idrule-binding-no
                            highest-binding-no))
                        (setf highest-binding-no
                           (1+ highest-binding-no))
                        (push
                           (make-match-binding
                              :pattern-binding-no
                              rhs-metarule-binding-no
                              :matched-binding-nos
                              (ncons idrule-binding-no))
                           rhs-idrule-match-bindings)
                        (push idrule-binding-no
                           idrule-binding-nos)
                        (setf idrule-bindings
                           (cons
                              (make-category-binding :number
                                 idrule-binding-no :category
                                 cat :repetition
                                 (category-binding-repetition
                                    rhs-metarule-binding))
                              new-bindings))))))))
      (values (nreverse idrule-binding-nos) idrule-bindings)))


;;; End of file

