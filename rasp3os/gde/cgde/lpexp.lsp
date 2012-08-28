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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - LP EXPANSION
;;;
;;; Author: John Carroll
;;;
;;; This file contains the top level code for linearising an
;;; idrule.
;;;
;;; Entry points:
;;;
;;;  * (defun Lprule-expand (idrule) ...
;;;  * (defun Match-category

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Return a list of the orderings of a rule which don't violate
;;; the lprules. A list of complete normalised idrule records is
;;; returned

(defun lprule-expand (idrule)
   (let
      ((idrule-lhs
            (car (id-rule-binding-nos idrule)))
         (idrule-rhs
            (cdr (id-rule-binding-nos idrule)))
         (binding-list
            (id-rule-binding-list idrule))
         (name (id-rule-name idrule)))
      (unless *reduced-lp-rules
         (setf *reduced-lp-rules
            (reduce-lprules *lp-rules)))
      (let
         ((rhs-orderings
               (remove-multiple-linearisations
                  (allowed-category-orderings idrule-rhs
                     binding-list *reduced-lp-rules)
                  binding-list name))
            (index 0))
         (unless rhs-orderings
            (gde-warn "no valid linearisations for "
               (idrule-name-string name)))
         (mapcar
            #'(lambda (linearised-rhs)
               (progn
                  (setf index (1+ index))
                  (let
                     ((structure-53 (copy-id-rule idrule)))
                     (setf (id-rule-name structure-53)
                        (cond
                           ((cdr rhs-orderings)
                              (let
                                 ((structure-54
                                       (copy-top-rule-name
                                          name)))
                                 (setf
                                    (top-rule-name-base
                                       structure-54)
                                    (let
                                       ((structure-55
                                             (copy-sub-rule-name
                                                (top-rule-name-base
                                                   name))))
                                       (setf
                                          (sub-rule-name-index
                                             structure-55)
                                          index)
                                       structure-55))
                                 structure-54))
                           (t name)))
                     (setf (id-rule-binding-nos structure-53)
                        (cons idrule-lhs linearised-rhs))
                     (setf (id-rule-linear structure-53) t)
                     structure-53)))
            rhs-orderings))))


;;; If relevant flag is set, remove binding number ordering
;;; candidates which have matching categories in the same order.
;;; A matching category is one which has identical feature value
;;; pairs occuring in the same order, or feature variable pairs
;;; occuring in the same order.

(defun remove-multiple-linearisations (binding-no-orderings binding-list
      idrule-name)
   (let ((lins
            (remove-duplicates binding-no-orderings :test
               #'(lambda (o1 o2)
                  (is-duplicate-linearisation o1 o2 binding-list)))))
      (unless (eql (length lins) (length binding-no-orderings))
         (gde-warn
            "multiple identical possible linearisations of "
            (idrule-name-string idrule-name)))
      (if *multiple-linearisations binding-no-orderings lins)))


(defun is-duplicate-linearisation (ordering1 ordering2 binding-list)
   (dolist (binding-no1 ordering1 t)
      (let ((binding-no2 (pop ordering2)))
         (unless
            (or (= binding-no1 binding-no2)
               (match-category
                  binding-no1 binding-list binding-no2 binding-list))
            (return nil)))))


;;; Reduce a set of normal lprules of the form (a b c d) to a
;;; set of reduced lprules e.g. (a b) (a c) (a d) (b c) (b d) (c
;;; d) where a,b,c and d have all been normalised.

(defun reduce-lprules (lprules)
   (mapcan
      #'(lambda (name)
           (reduce-lprule
              (lp-rule-lp-terms
                 (normalise-lprule-definition name))))
      lprules))


(defun reduce-lprule (lprule)
   (cond
      (lprule
         (nconc
            (mapcar
               #'(lambda (tail-element)
                    (list (car lprule) tail-element))
               (cdr lprule))
            (reduce-lprule (cdr lprule))))))


;;; Produce possible orderings of the binding numbers of an
;;; idrules RHS consistent with the LP rules.

(defun allowed-category-orderings
   (rhs-binding-nos binding-list
      reduced-lprules)
   (cond
      ((and rhs-binding-nos
          (cdr rhs-binding-nos))
         (mapcan
            #'(lambda (binding-no)
                 (let
                    ((rest-binding-nos
                        (remove-list-1 binding-no
                           rhs-binding-nos)))
                    (and
                       (dolist
                          (reduced-lprule reduced-lprules t)
                          (if
                             (lp-ordering-violated binding-no
                                rest-binding-nos binding-list
                                (car reduced-lprule)
                                (cadr reduced-lprule))
                             (return nil)))
                       (mapcar
                          #'(lambda (ordering)
                               (cons binding-no ordering))
                          (allowed-category-orderings
                             rest-binding-nos binding-list
                             reduced-lprules)))))
            rhs-binding-nos))
      (t (ncons rhs-binding-nos))))


;;; Check a potential first ID rule category against those
;;; already there just considering a single reduced lprule.
;;; First-item and second-item are the two halves of the reduced
;;; lprule. Return true if this ordering is not allowed.

(defun lp-ordering-violated
   (first-binding-no rest-binding-nos
      binding-list first-item second-item)
   (and
      (match-category
         (category-binding-number
            (car second-item))
         second-item first-binding-no binding-list)
      (dolist (binding-no rest-binding-nos)
         (if
            (match-category
               (category-binding-number
                  (car first-item))
               first-item binding-no binding-list)
            (return binding-no)))))


;;; Check if cat2 is an extension of cat1 & return non-NIL if
;;; so. The match fails if a feature specified as present in the
;;; first category has only a variable value in the second.
;;;
;;; Optimise this function since it is in an innermost
;;; loop, getting called of the order of 10000 times during
;;; grammar compilation.
;;;
;;; Find-feature-value searches a feature bundle for a given
;;; feature name. If the feature name is found then its value is
;;; returned, otherwise *ABSENT*. (This is to avoid features
;;; with value NIL messing things up).

(defmacro find-feature-value (name category)
   (let ((feat (gensym)) (cat (gensym)))
      `(prog ((,feat ,name) (,cat ,category))
       lp
          (cond
             ((atom ,cat) (return '*absent*))
             ((eq (fv-pair-feature (car (the cons ,cat))) ,name)
                (return
                   (fv-pair-value (car (the cons ,cat))))))
          (setf ,cat (cdr (the cons ,cat)))
          (go lp))))


(progn
   (defvar category-match-bindings nil)
   (defvar match-category-variable-table nil))


(defun match-category
   (cat1-no cat1-bindings cat2-no
      cat2-bindings)
   (let
      ((match-category-variable-table nil))
      (match-category1 cat1-no cat1-bindings
         cat2-no cat2-bindings)))


(defun match-category1
   (cat1-no cat1-bindings cat2-no
      cat2-bindings)
   (let
      ((cat2-category
          (category-binding-category
             (f-find (the fixnum cat2-no) cat2-bindings
                :key #'category-binding-number :test
                #'eql))))
      (dolist
         (feature-spec
            (category-binding-category
               (f-find (the fixnum cat1-no) cat1-bindings
                  :key #'category-binding-number :test
                  #'eql))
            t)
         (unless
            (match-category-values
               (fv-pair-value feature-spec)
               (find-feature-value
                  (fv-pair-feature feature-spec)
                  cat2-category)
               cat1-bindings cat2-bindings)
            (return nil)))))


(defun match-category-values
   (cat1-feature-value cat2-feature-value
      cat1-bindings cat2-bindings)
   (cond
      ((consp cat1-feature-value)
         (dolist (val cat1-feature-value)
            (if
               (match-category-values val
                  cat2-feature-value cat1-bindings
                  cat2-bindings)
               (return val))))
      ((and (numberp cat1-feature-value)
          (numberp cat2-feature-value))
         (match-category1 cat1-feature-value
            cat1-bindings cat2-feature-value
            cat2-bindings))
      ((eq cat1-feature-value
          cat2-feature-value))
      ((eq cat1-feature-value '*novalue*)
         (or (varp cat2-feature-value)
            (eq cat2-feature-value (unnamed-variable))))
      ((eq cat1-feature-value '*present*)
         (not
            (or (eq cat2-feature-value '*absent*)
               (varp cat2-feature-value)
               (eq cat2-feature-value (unnamed-variable)))))
      ((varp cat1-feature-value)
         (match-variable-value cat1-feature-value
            cat2-feature-value))))


(defun match-variable-value
   (cat1-feature-value cat2-feature-value)
   (cond
      ((eq cat2-feature-value '*absent*) nil)
      ((member cat2-feature-value '(*present* *novalue*) :test #'eq)
         t)
      (t
         (let
            ((bnd
                (assoc cat1-feature-value
                   match-category-variable-table :test #'eq)))
            (if bnd
               (eq (cdr bnd) cat2-feature-value)
               (push
                  (cons cat1-feature-value
                     cat2-feature-value)
                  match-category-variable-table))))))


;;; End of file

