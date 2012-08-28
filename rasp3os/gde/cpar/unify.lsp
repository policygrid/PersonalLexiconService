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

;;; ANLT CHART PARSER - UNIFICATION, COPYING ETC FUNCTIONS
;;;
;;; Unification depends on the value of the global
;;; variable *term-unification. If non-NIL, the match performed
;;; is term unification, otherwise is graph unification - the
;;; second argument may be an extension of the first, features
;;; without values being marked by optional variable values. 
;;;
;;; Entry points:
;;;
;;;  * (defun g-dereference (v vt)
;;;  * (defun g-unify (cat1 cat2 vt1 vt2) ...
;;;  * (defun g-unify-values (rv nv vt1 vt2) ...
;;;  * (defun g-unify-values-graph (rv nv vt1 vt2) ...
;;;  * (defun g-subsume (cat1 cat2 vt1) ...
;;;  * (defun g-equal-p (cat1 cat2 vt1) ...
;;;  * (defun g-identical-p (cat1 cat2) ...
;;;  * (defun g-reentrant-p (cat) ...
;;;  * (defun g-copy-category (cat vt1 embedded-p local-vt local-vars) ...
;;;
;;; Categories consist of an integer index followed by feature values.
;;; Categories with same index have same features in same order.
;;;
;;; Variables in completed consituents must work for both g-varp and
;;; g-node-varp.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(eval-when (compile load eval)
(defmacro g-add-variable-binding (a b lst)
   `(cons (cons ,a ,b) ,lst))
)


(defun g-dereference (v vt)
   #-gde-debug (declare (optimize (speed 3) (safety 0) (space 0)))
   (loop
      (let ((ans (g-fast-assoc-eq v vt)))
         (unless ans (return v))
         (setq ans (cons-cdr ans))
         (when (or (g-categoryp ans) (not (g-varp ans)))
            (return ans))
         ;; ans must be a variable, so go round again
         (setq v ans))))


;;; Return t and new updated versions of variable tables if the unification
;;; succeeds, otherwise return nil

(defun g-unify (cat1 cat2 vt1 vt2)
   (when (g-fixnum-eql (vector-svref cat1 0) (vector-svref cat2 0))
      (if *term-unification
         (g-unify-term cat1 cat2 vt1 vt2)
         (g-unify-graph cat1 cat2 vt1 vt2))))


(defun g-unify-term (cat1 cat2 vt1 vt2)
   ;; cat1 and cat2 categories are guaranteed to be the same length
   (fixnum-dotimes-from-1
      (offset (simple-vector-length cat1) (values t vt1 vt2))
      (multiple-value-bind (success new-vt1 new-vt2)
         (g-unify-values
            (vector-svref cat1 offset) (vector-svref cat2 offset) vt1 vt2)
         (unless success (return nil))
         (setq vt1 new-vt1 vt2 new-vt2))))


;;; The graph unification version of the unifier. Categories contain
;;; values for all declared features, with special 'optional' variable
;;; values in cases where feature was not specified in grammar / lexical
;;; entry.

(defun g-unify-graph (cat1 cat2 vt1 vt2)
   (fixnum-dotimes-from-1
      (offset (simple-vector-length cat1) (values t vt1 vt2))
      (multiple-value-bind (success new-vt1 new-vt2)
         (g-unify-values-graph
            (vector-svref cat1 offset) (vector-svref cat2 offset) vt1 vt2)
         (unless success (return nil))
         (setq vt1 new-vt1 vt2 new-vt2))))


(defun g-unify-values-graph (val1 val2 vt1 vt2)
   ;; bind optional variables to ordinary variables and proper values, but
   ;; don't bind anything to an 'optional' variable else it will disappear
   (cond
      ((or (g-categoryp val1) (g-categoryp val2))
         (g-unify-values val1 val2 vt1 vt2))
      ((and (g-varp val2) (g-optionalp val2))
         (if (and (g-varp val1) (g-optionalp val1))
            (values t vt1 vt2)
            (values t vt1
               (g-add-variable-binding val2 (g-dereference val1 vt1) vt2))))
      ((and (g-varp val1) (g-optionalp val1))
         (values t
            (g-add-variable-binding val1 (g-dereference val2 vt2) vt1)
            vt2))
      (t (g-unify-values val1 val2 vt1 vt2))))


;;; Attempt to unify 2 feature values.
;;;
;;; De-reference v1 and v2 to give val1 and val2. If
;;; 
;;; 1. val1 = val2 then succeed
;;; 
;;; 2. val1 is a category
;;;    a. val2 is a category then call g-unify (val1 val2 vt1 vt2)
;;;    b. val2 is a variable then add binding (val2 = val1) to vt2
;;;          and also to vt1 if val2 is not a node variable
;;;    c. else fail
;;;    
;;; 3. val2 is a category
;;;    a. val1 is a variable then add binding (val1 = val2) to vt1
;;;          and also to vt2 if val1 is a node variable
;;;    b. else fail
;;;    
;;; 4. val1 is a variable
;;;    a. val2 is a variable then add binding (val1 = val2) to vt1
;;;          and (val2 = val1) to vt2 if val1 is a node variable
;;;          (but if (val1 = val2) present in vt2 or (val2 = val1) in vt1
;;;          avoid creating circular binding lists)
;;;    b. otherwise (val2 atomic) then add binding (val1 = val2) to vt1
;;;          and also to vt2 if val1 is a node variable
;;;    
;;; 5. val2 is a variable (val1 atomic) then add binding (val2 = val1) to vt2
;;;          and also to vt1 if val2 is not a node variable
;;; 
;;; 6. else fail

(defun g-unify-values (v1 v2 vt1 vt2)
   #-gde-debug (declare (optimize (speed 3) (space 0) (safety 0)))
   (when
      (or (eq v1 (g-unnamed-variable)) (eq v2 (g-unnamed-variable)))
      (return-from g-unify-values (values t vt1 vt2)))
   (let
      ((val1
            (if (and g-search-bindings (not (g-categoryp v1)) (g-varp v1))
               (g-dereference v1 vt1) v1))
         (val2
            (if (and (not (g-categoryp v2)) (g-varp v2))
               (g-dereference v2 vt2) v2)))
      (cond
         ((eq val1 val2) (values t vt1 vt2))
         ((g-categoryp val1)
            (cond
               ((g-categoryp val2)
                  (g-unify val1 val2 vt1 vt2))
               ((g-varp val2)
                  (values t
                     (if (g-node-varp val2) vt1
                        (g-add-variable-binding val2 val1 vt1))
                     (g-add-variable-binding val2 val1 vt2)))))
         ((g-categoryp val2)
            (when (g-varp val1)
               (values t (g-add-variable-binding val1 val2 vt1)
                  (if (g-node-varp val1)
                     (g-add-variable-binding val1 val2 vt2) vt2))))
         ((g-varp val1)
            (if (g-varp val2)
               (if (g-node-varp val1)
                  (values t 
                     (if (eq (g-dereference val2 vt1) val1) vt1
                        (g-add-variable-binding val1 val2 vt1))
                     (if (eq (g-dereference val1 vt2) val2) vt2
                        (g-add-variable-binding val2 val1 vt2)))
                  (values t (g-add-variable-binding val1 val2 vt1) vt2))
               ;; val2 is an atomic value
               (values t (g-add-variable-binding val1 val2 vt1)
                  (if (g-node-varp val1)
                     (g-add-variable-binding val1 val2 vt2) vt2))))
         ((g-varp val2)
            ;; val1 is an atomic value
            (values t
               (if (g-node-varp val2) vt1
                  (g-add-variable-binding val2 val1 vt1))
               (g-add-variable-binding val2 val1 vt2))))))


;;; Return true if first category subsumes (or is equally general as)
;;; second. NB This code is incorrect if the first category contains
;;; any reentrancy -- so take notice of the second return value from
;;; g-copy-category and do not attempt to call this function categories
;;; for which it was true.

(defun g-subsume (cat1 cat2 vt1)
   (when
      (and (g-fixnum-eql (vector-svref cat1 0) (vector-svref cat2 0))
         *term-unification)
      (fixnum-dotimes-from-1 (offset (simple-vector-length cat1) t)
         (unless
            (g-subsume-values
               (vector-svref cat1 offset) (vector-svref cat2 offset) vt1)
            (return nil)))))


(defun g-subsume-values (v1 v2 vt1)
   #-gde-debug (declare (optimize (speed 3) (safety 0) (space 0)))
   (cond
      ((g-categoryp v1)
         (when (g-categoryp v2) (g-subsume v1 v2 vt1)))
      ((g-varp v1)
         (let ((v1-val (g-dereference v1 vt1)))
            (or (g-varp v1-val) (eq v1-val v2))))
      ;; Cannot use eq test on 2 variables since possible from vt1 that
      ;; same variable has different bindings, so test must come here
      ((eq v1 v2) t)))


;;; Return true if categories are equal. NB Same restrictions as g-subsume.

(defun g-equal-p (cat1 cat2 vt1)
   (when
      (and (g-fixnum-eql (vector-svref cat1 0) (vector-svref cat2 0))
         *term-unification)
      (fixnum-dotimes-from-1 (offset (simple-vector-length cat1) t)
         (unless
            (g-equal-values
               (vector-svref cat1 offset) (vector-svref cat2 offset) vt1)
            (return nil)))))


(defun g-equal-values (v1 v2 vt1)
   #-gde-debug (declare (optimize (speed 3) (safety 0) (space 0)))
   (cond
      ((g-categoryp v1)
         (when (g-categoryp v2) (g-equal-p v1 v2 vt1)))
      ((g-varp v1)
         (let ((v1-val (g-dereference v1 vt1)))
            (or (eq v1-val v2) (and (g-varp v1-val) (g-varp v2)))))
      ;; Cannot use eq test on 2 variables since possible from vt1 that
      ;; same variable has different bindings, so test must come here
      ((eq v1 v2) t)))



;;; Return true if two categories are identical.

(defun g-identical-p (cat1 cat2)
   (when (g-fixnum-eql (vector-svref cat1 0) (vector-svref cat2 0))
      (fixnum-dotimes-from-1 (offset (simple-vector-length cat1) t)
         (unless
            (g-identical-values (vector-svref cat1 offset)
               (vector-svref cat2 offset))
            (return nil)))))


(defun g-identical-values (v1 v2)
   #-gde-debug (declare (optimize (speed 3) (safety 0) (space 0)))
   (cond
      ((eq v1 v2))
      ((and (g-categoryp v1) (g-categoryp v2))
         (g-identical-p v1 v2))))



;;; Check if a category is re-entrant, i.e. the same unbound variable
;;; occurs in it more than once

(defun g-reentrant-p (cat variables)
   (fixnum-dotimes-from-1
      (offset (simple-vector-length cat) (values nil variables))
      (let ((value (vector-svref cat offset)))
         (cond
            ((eq value (g-unnamed-variable)))
            ((g-categoryp value)
               (multiple-value-bind (re-entrant-p new-variables)
                  (g-reentrant-p value variables)
                  (if re-entrant-p
                     (return (values t nil))
                     (setq variables new-variables))))
            ((g-varp value)
               (if (member value variables :test #'eq)
                  (return (values t nil))
                  (push value variables)))))))


;;; Substitute values for, or rename variables if they have no value,
;;; in category. Second value returned is true if resulting category
;;; is re-entrant, i.e. contains 2 or more occurrences of the same
;;; (unbound) variable. This can happen if the original category is
;;; re-entrant, or a single variable is the value of 2 originally 
;;; distinct variables.

(defun g-copy-category (cat vt re-entrant-p local-vt local-vars)
   (let*
      ((len (simple-vector-length cat))
         (res (make-simple-vector len)))
      (setf (vector-svref res 0) (vector-svref cat 0))
      (fixnum-dotimes-from-1
         (offset len (values res re-entrant-p local-vt local-vars))
         (setf (vector-svref res offset) 
            (let ((v (vector-svref cat offset)))
               (cond
                  ((eq v (g-unnamed-variable))
                     (g-genvar))
                  ((g-categoryp v)
                     (multiple-value-bind
                        (new-cat new-re-entrant-p new-local-vt
                           new-local-vars)
                        (g-copy-category v vt re-entrant-p local-vt local-vars)
                        (setq local-vt new-local-vt
                           re-entrant-p new-re-entrant-p
                           local-vars new-local-vars)
                        new-cat))
                  ((not (g-varp v)) v)
                  ((g-optionalp v) (g-genoptvar))
                  (t
                     (let ((val (g-fast-assoc-eq v local-vt)))
                        (cond
                           (val
                              (setq re-entrant-p t)
                              (cons-cdr val))
                           (t (setq val (g-dereference v vt))
                              (cond
                                 ((eq val v)
                                    ;; no binding for v in vt - rename
                                    ;; unless v is an node var (inside
                                    ;; an node category bound to an rule
                                    ;; variable)
                                    (if (g-node-varp v) v 
                                       (let ((new-var (g-genvar)))
                                          (setq local-vt
                                             (g-add-variable-binding
                                                v new-var local-vt))
                                          new-var)))
                                 ((g-categoryp val)
                                    (multiple-value-bind
                                       (new-val new-re-entrant-p
                                          new-local-vt new-local-vars)
                                       (g-copy-category val vt re-entrant-p
                                          local-vt local-vars)
                                       (setq local-vt new-local-vt
                                          re-entrant-p new-re-entrant-p
                                          local-vars new-local-vars)
                                       new-val))
                                 ((and (not re-entrant-p) (g-varp val))
                                    ;; v is bound to another variable
                                    (if (member val local-vars :test #'eq)
                                       (setq re-entrant-p t)
                                       (push val local-vars))
                                    val)
                                 (t val))))))))))))


;;; End of file
