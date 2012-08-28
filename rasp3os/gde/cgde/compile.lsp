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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - GRAMMAR COMPILATION
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code to add default features to
;;; categories, and to propagate features in ID rules, under
;;; control of the feature propagation statements. Also, code
;;; control the compilation of ID rules, and to dump out for
;;; inspection/porting.
;;;
;;; Entry points:
;;;
;;;  * (defun share-first-daughters-in-idrules (idrules)
;;;  * (defun Fill-unrestricted-idrule (idrule) ...
;;;  * (defun Fill-unrestricted-category (bindings) ...
;;;  * (defun Catrule-expand (idrules) ...
;;;  * (defun Proprule-and-defrule-expand (idrules)
;;;  * (defun Compile-world () ...
;;;  * (defun Compile-world-stats1 () ...
;;;  * (defun Compile-world-stats2 () ...
;;;  * (defun Compile-idrule (idrule-name) ...
;;;  * (defun Linearise-idrule (idrule-name) ...
;;;  * (defun Fully-instantiate-idrule (idrule-name) ...
;;;  * (defun Fully-instantiate-category (cat-bindings lexical) ...
;;;  * (defun Dump-grammar-to-file (readable) ...
;;;  * (defun Dump-word-definitions-to-file (readable) ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; For parser efficiency ensure that variable values for each
;;; feature in first non-NULL daughter are same for each ID rule.

(defun share-first-daughters-in-idrules (idrules)
   (mapc
      #'(lambda (idrule)
         (reset-variable-value-pairs)
         (let
            ((id-rule-binding-list (id-rule-binding-list idrule))
               (share-bindings nil)
               (embedded-nos nil))
            (mapc
               #'(lambda (cat)
                  (multiple-value-setq (share-bindings embedded-nos)
                     (reallocate-variables-in-binding
                        cat share-bindings embedded-nos)))
               (share-first-reordered-binding-list idrule id-rule-binding-list))
            (mark-shared-embedded-variables
               share-bindings embedded-nos id-rule-binding-list)
            (substitute-single-use-variables idrule
               (do
                  ((tail share-bindings (cddr tail)) (res nil))
                  ((null tail) res)
                  (unless (eq (cdr (cadr tail)) t)
                     (push (fv-pair-value (car (cadr tail))) res))))))
      idrules))


(defun share-first-reordered-binding-list (idrule binding-list)
   (mapc
      #'(lambda (d-no)
         (let
            ((d-cat
                  (f-find (the fixnum d-no) binding-list
                     :key #'category-binding-number :test #'eql)))
            (unless
               (f-find (null-feature-name)
                  (category-binding-category d-cat)
                  :key #'fv-pair-feature :test #'eq)
               (let*
                  ((cat-values
                        (remove-duplicates
                           (mapcan
                              #'(lambda (fvpair)
                                 (when (numberp (fv-pair-value fvpair))
                                    (list
                                       (f-find
                                          (the fixnum (fv-pair-value fvpair))
                                          binding-list
                                          :key #'category-binding-number
                                          :test #'eql))))
                              (category-binding-category d-cat))
                           :test #'eq))
                     (first-cats (cons d-cat cat-values)))
                  (return-from share-first-reordered-binding-list
                     (append first-cats
                        (set-difference binding-list first-cats :test #'eq)))))))
      (cdr (id-rule-binding-nos idrule)))
   binding-list)
             

(defun reallocate-variables-in-binding (cat share-bindings embedded-nos)
   (setf (category-binding-category cat)
      (mapcar
         #'(lambda (fvpair)
            (let ((value (fv-pair-value fvpair)))
               (cond
                  ((varp value)
                     (let ((rename (getf share-bindings value)))
                        (if rename
                           (progn
                              (setf (cdr rename) t)
                              (if (eq (fv-pair-feature (car rename))
                                    (fv-pair-feature fvpair))
                                 (car rename)
                                 (make-fv-pair (fv-pair-feature fvpair)
                                    (fv-pair-value (car rename)))))
                           (let
                              ((new-fvpair
                                    (feature-variable-value-pair
                                       (fv-pair-feature fvpair))))
                              (setf (getf share-bindings (fv-pair-value fvpair))
                                 (cons new-fvpair (category-binding-number cat)))
                              new-fvpair))))
                  ((numberp value)
                     (push value embedded-nos) fvpair)
                  (t fvpair))))
         (category-binding-category cat)))
   (values share-bindings embedded-nos))


(defun mark-shared-embedded-variables (share-bindings embedded-nos
      id-rule-binding-list)
   (do ((tail embedded-nos (cdr tail)))
      ((null (cdr tail)))
      (when (member (car tail) (cdr tail))
         ;; this category and all ones inside it are shared between 2 or more
         ;; top-level categories
         (mark-shared-embedded-variables1
            share-bindings (car tail) id-rule-binding-list))))


(defun mark-shared-embedded-variables1 (share-bindings n id-rule-binding-list)
   (do
      ((tail share-bindings (cddr tail)))
      ((null tail))
      (when (eql (cdadr tail) n) (setf (cdadr tail) t)))
   (mapc
      #'(lambda (fvpair)
         (when (numberp (fv-pair-value fvpair))
            (mark-shared-embedded-variables1
               share-bindings (fv-pair-value fvpair) id-rule-binding-list)))
      (category-binding-category
         (f-find (the fixnum n) id-rule-binding-list
            :key #'category-binding-number :test #'eql))))


(defun substitute-single-use-variables (idrule variables)
   (mapc
      #'(lambda (cat)
         (setf (category-binding-category cat)
            (mapcar
               #'(lambda (fvpair)
                  (if (member (fv-pair-value fvpair) variables :test #'eq)
                     (feature-proper-value-pair
                        (fv-pair-feature fvpair) (unnamed-variable))
                     fvpair))
               (category-binding-category cat))))
      (id-rule-binding-list idrule)))


;;; For unrestricted unfication. Bundles filled out with every
;;; possible feature - new ones given an optional variable
;;; value.

(defun fill-unrestricted-idrule (idrule)
   (let
      ((structure-9 (copy-id-rule idrule)))
      (setf (id-rule-binding-list structure-9)
         (fill-unrestricted-category
            (id-rule-binding-list structure-9)))
      structure-9))


(defun fill-unrestricted-category (bindings)
   (mapcar
      #'(lambda (binding)
           (let
              ((structure-10
                  (copy-category-binding binding)))
              (setf
                 (category-binding-category structure-10)
                 (let
                    ((bundle
                        (category-binding-category
                           structure-10)))
                    (mapcar
                       #'(lambda (feature)
                            (cond
                               ((and bundle
                                   (eq feature
                                      (fv-pair-feature
                                         (car bundle))))
                                  (pop bundle))
                               (t
                                  (make-fv-pair feature
                                     (generate-optvariable)))))
                       *features)))
              structure-10))
      bindings))


;;; Apply category declarations. Lexical flag used to indicate
;;; whether just LCATEGORY declarations should be applied - but
;;; now CATEGORY ones are as well.
;;;
;;; Category bindings are assumed to already be normalised. If
;;; it is a top level one (its index is in binding-nos) then it
;;; is straight-forward. If not, find all feature paths from
;;; other categories which end up at this category, and for each
;;; distinct path do the filling on this category. Naive
;;; approach would duplicate work. for each category value
;;; feature once only.
;;;
;;; Don't bother trying a category declaration if it is not
;;; specified as adding any features. This means that a category
;;; declaration will not show up in list of those applied even
;;; if it is strictly applicable - sacrifice for grammar
;;; compilation efficiency.

(defvar category-rules-applied nil)


(defun catrule-expand (idrules)
   (cond
      (idrules
         (cons
            (let
               ((idrule (car idrules)))
               (let
                  ((category-rules-applied nil))
                  (let
                     ((structure-11 (copy-id-rule idrule)))
                     (setf (id-rule-binding-list structure-11)
                        (catrule-expand-bindings
                           (id-rule-binding-nos idrule)
                           (id-rule-binding-list idrule) nil))
                     (setf (id-rule-rules-applied structure-11)
                        (nconc category-rules-applied
                           (id-rule-rules-applied idrule)))
                     structure-11)))
            (catrule-expand (cdr idrules))))))


(defun catrule-expand-bindings
   (binding-nos binding-list lexical)
   (mapcar
      #'(lambda (binding)
           (let
              ((binding-no
                  (category-binding-number binding)))
              (cond
                 ((member binding-no binding-nos)
                    (catrule-expand-binding binding
                       binding-list nil lexical))
                 (t
                    (mapl
                       #'(lambda (cat-feature-tail)
                            (unless
                               (member
                                  (car cat-feature-tail)
                                  (cdr cat-feature-tail)
                                  :test #'equal)
                               (setf binding
                                  (catrule-expand-binding
                                     binding binding-list
                                     (car
                                        cat-feature-tail)
                                     lexical))))
                       (mapcan
                          #'(lambda (b)
                               (cond
                                  ((not
                                      (=
(category-binding-number
                                            b)
                                         binding-no))
                                     (feature-paths-to-target b
                                        binding-list
                                        binding-no))))
                          binding-list))
                    binding))))
      binding-list))


(defun feature-paths-to-target
   (binding binding-list target-no)
   (cond
      ((= (category-binding-number binding)
          target-no)
         (ncons nil))
      (t
         (mapcan
            #'(lambda (fvpair)
                 (cond
                    ((numberp (fv-pair-value fvpair))
                       (mapcar
                          #'(lambda (rest-path)
                               (cons (fv-pair-feature fvpair)
                                  rest-path))
                          (feature-paths-to-target
                             (f-find
                                (the fixnum
                                   (fv-pair-value fvpair))
                                binding-list :key
                                #'category-binding-number :test
                                #'eql)
                             binding-list target-no)))))
            (category-binding-category binding)))))


(defun catrule-expand-binding
   (binding cat-bindings feature-path lexical)
   (declare (ignore lexical))
   (let
      ((category
          (category-binding-category binding))
         (old-category
            (category-binding-category binding)))
      (dolist (cat-name *categories)
         (let
            ((cat-dec
                (normalise-category-definition cat-name)))
            (when
               (and
                  (category-declaration-features cat-dec)
                  (equal feature-path
                     (category-declaration-feature-path
                        cat-dec))
;;;                   (or (null lexical)
;;;                      (eq lexical
;;;                         (category-declaration-lexical
;;;                            cat-dec)))
                  )
               (setf category
                  (apply-category-to-bundle category cat-name
                     cat-dec binding cat-bindings)))))
      (if (eq category old-category) binding
         (let
            ((structure-12
                (copy-category-binding binding)))
            (setf
               (category-binding-category structure-12)
               category)
            structure-12))))


(defun apply-category-to-bundle
   (category cat-name cat-dec binding
      cat-bindings)
   (when
      (match-category
         (category-binding-number
            (car
               (category-declaration-cat-bindings
                  cat-dec)))
         (category-declaration-cat-bindings cat-dec)
         (category-binding-number binding)
         cat-bindings)
      (unless
         (member cat-name category-rules-applied :test #'eq)
         (push cat-name category-rules-applied))
      (let
         ((old-category category))
         (dolist
            (feature
               (category-declaration-features cat-dec))
            (cond
               ((not
                   (f-find feature old-category :key
                      #'fv-pair-feature :test #'eq))
                  (setf category
                     (cons
                        (feature-variable-value-pair feature)
                        category)))))))
   category)


;;; Sort features in a category into a canonical order. The
;;; sorting is allowed to be destructive since the original
;;; category before addition of features by category rules was
;;; copied. Sort key (feature name) and predicate built in to
;;; the sorting function. Best case performance is when features
;;; are in reverse order.

(defun sort-features-in-idrules (idrules)
   (cond
      (idrules
         (cons
            (let
               ((idrule (car idrules)))
               (let
                  ((structure-13 (copy-id-rule idrule)))
                  (setf (id-rule-binding-list structure-13)
                     (sort-features-in-category
                        (id-rule-binding-list idrule)))
                  structure-13))
            (sort-features-in-idrules
               (cdr idrules))))))


(defun sort-features-in-category
   (binding-list)
   (cond
      (binding-list
         (cons
            (let
               ((binding (car binding-list)))
               (let
                  ((structure-14
                      (copy-category-binding binding)))
                  (setf
                     (category-binding-category structure-14)
                     (sort-features-in-bundle
                        (category-binding-category binding)
                        nil))
                  structure-14))
            (sort-features-in-category
               (cdr binding-list))))))


(defun sort-features-in-bundle (in out)
   (cond
      (in
         (let
            ((tail (cdr in))
               (f1-position
                  (get (fv-pair-feature (car in))
                     'feature-order)))
            (cond
               ((and out
                     (> f1-position
                        (get (fv-pair-feature (car out))
                           'feature-order)))
                  (sort-features-in-bundle1 f1-position in
                     out (cdr out))
                  (sort-features-in-bundle tail out))
               (t
                  (sort-features-in-bundle tail
                     (rplacd in out))))))
      (t out)))


(defun sort-features-in-bundle1
   (f1-position el prev-lst lst)
   (cond
      ((and lst
          (> f1-position
             (get (fv-pair-feature (car lst))
                'feature-order)))
         (sort-features-in-bundle1 f1-position el
            (cdr prev-lst) (cdr lst)))
      (t (rplacd prev-lst (rplacd el lst)))))


;;; Propagation, defaulting, alias or metarule expansion may
;;; have resulted in several features (perhaps in different ID
;;; rule categories) being bound to the same variable value.
;;; This variable value must be substituted for the new value in
;;; all the categories. The subs argument is an a-list of pairs
;;; old value / feature-value pair containing new value. When
;;; substituting back, insert whole feature-value pair if
;;; features are the same, otherwise extract value from pair
;;; (saves some consing).

(defun substitute-feature-bindings (subs binding-list)
   (cond
      ((null subs) binding-list)
      (t
         (dolist (binding binding-list)
            (mapl
               #'(lambda (tail)
                  (let
                     ((new-pair
                           (substitute-feature-bindings-deref
                              (fv-pair-value (car tail))
                              subs))
                        (fvpair (car tail)))
                     (if
                        (and new-pair
                           (numberp
                              (fv-pair-value
                                 (cdr new-pair))))
                        (check-bindings-for-circularities
                           (fv-pair-value
                              (cdr new-pair))
                           binding-list
                           (category-binding-number binding)))
                     (cond
                        ((and new-pair
                              (eq (fv-pair-feature fvpair)
                                 (fv-pair-feature
                                    (cdr new-pair))))
                           (setf (car tail)
                              (cdr new-pair)))
                        (new-pair
                           (setf (car tail)
                              (feature-proper-value-pair
                                 (fv-pair-feature fvpair)
                                 (fv-pair-value
                                    (cdr new-pair))))))))
               (category-binding-category binding)))
         binding-list)))


(defun substitute-feature-bindings-deref (val subs)
   (let ((res-pair (assoc val subs)))
      (cond
         ((null res-pair) nil)
         ((varp (fv-pair-value (cdr res-pair)))
          (or
             (substitute-feature-bindings-deref
                (fv-pair-value (cdr res-pair))
                ;; !!! avoid any circularities
                (remove res-pair subs :test #'eq))
             res-pair))
         (t res-pair))))


(defun check-bindings-for-circularities
   (binding-no binding-list target)
   (if (= binding-no target)
      (gde-ferror
         "circularity detected in a category")
      (dolist
         (fvpair
            (category-binding-category
               (f-find (the fixnum binding-no)
                  binding-list :key #'category-binding-number
                  :test #'eql)))
         (if (numberp (fv-pair-value fvpair))
            (check-bindings-for-circularities
               (fv-pair-value fvpair) binding-list
               target)))))


;;; Apply propagation and default rules, then fill out
;;; categories with category rules to a set of (already
;;; normalised) idrules. Copy top level list structure of each
;;; feature bundle in ID rules so that new features may be added
;;; destructively to the end of them - makes subsequent matching
;;; faster since major features left at beginning of categories.

(defun proprule-and-defrule-expand
   (idrules)
   (cond
      (idrules
         (cons
            (cond
               (*prop-before-default
                  (defrule-expand
                     (proprule-expand
                        (copy-idrule-feature-bundles
                           (car idrules))
                        *prop-rules)
                     *default-rules))
               (t
                  (proprule-expand
                     (defrule-expand
                        (copy-idrule-feature-bundles
                           (car idrules))
                        *default-rules)
                     *prop-rules)))
            (proprule-and-defrule-expand
               (cdr idrules))))))


(defun copy-idrule-feature-bundles (idrule)
   (let
      ((structure-15 (copy-id-rule idrule)))
      (setf (id-rule-binding-list structure-15)
         (mapcar
            #'(lambda (binding)
                 (let
                    ((structure-16
                        (copy-category-binding binding)))
                    (setf
                       (category-binding-category structure-16)
                       (copy-list
                          (category-binding-category
                             structure-16)))
                    structure-16))
            (id-rule-binding-list structure-15)))
      structure-15))


(defun proprule-expand
   (idrule proprule-names)
   (dolist (proprule-name proprule-names)
      (setf idrule
         (apply-proprule idrule
            (normalise-proprule-definition
               proprule-name)
            proprule-name)))
   idrule)


(defun apply-proprule (idrule proprule proprule-name)
   (when
      (and
         (or (id-rule-lexical idrule)
            (not (prop-rule-lexical proprule)))
         (or
            (id-rule-linear idrule)
            (not (prop-rule-linear proprule))))
      (dolist
         (match-bindings
            (filter-linear-bindings
               (match-rule-pattern
                  (id-rule-binding-list idrule)
                  (id-rule-binding-nos idrule)
                  (prop-rule-cat-bindings proprule)
                  (prop-rule-binding-nos proprule))
               idrule (prop-rule-binding-nos proprule)
               (prop-rule-cat-bindings proprule)
               (id-rule-linear idrule)))
         (dolist (equality (prop-rule-ident-specs proprule))
            (setf idrule
               (propagate-idrule idrule equality
                  match-bindings proprule-name)))))
   idrule)


;;; For each ident-spec, call Propagate-idrule-binding to find
;;; the category binding it is referring to (possibly having to
;;; add a category-value feature / value pair and / or traverse
;;; one category value if a cat-feature is specified), and merge
;;; into this binding a feature-bundle consisting of the
;;; features in the ident-spec, each with a variable value.

(defun propagate-idrule
   (idrule ident-specs match-bindings
      proprule-name)
   (let
      ((binding-list
          (id-rule-binding-list idrule))
         (highest-binding-no
            (id-rule-highest-binding-no idrule))
         (variable-substitutions nil)
         (fvpairs-or-var
            (propagate-feature-bindings ident-specs)))
      (dolist (ident-spec ident-specs)
         (let
            ((cat-index
                (prop-ident-spec-category-index
                   ident-spec)))
            (dolist
               (binding-no
                  (match-binding-matched-binding-nos
                     (f-find
                        (the fixnum
                           (category-index-binding-no
                              cat-index))
                        match-bindings :key
                        #'match-binding-pattern-binding-no
                        :test #'eql)))
               (multiple-value-bind
                  (binding new-binding-list)
                  (propagate-idrule-binding binding-no
                     (category-index-cat-feature cat-index)
                     binding-list)
                  (setf binding-list
                     (propagate-idrule-category binding
                        new-binding-list
                        (prop-ident-spec-feature-names
                           ident-spec)
                        fvpairs-or-var))))))
      (let
         ((structure-17 (copy-id-rule idrule)))
         (setf (id-rule-binding-list structure-17)
            (substitute-feature-bindings
               variable-substitutions binding-list))
         (setf
            (id-rule-highest-binding-no structure-17)
            highest-binding-no)
         (setf (id-rule-rules-applied structure-17)
            (cons proprule-name
               (id-rule-rules-applied structure-17)))
         structure-17)))


(defun propagate-feature-bindings
   (ident-specs)
   (if
      (member-if
         #'(lambda (ident-spec)
              (null
                 (prop-ident-spec-feature-names
                    ident-spec)))
         ident-specs)
      (generate-variable)
      (dolist (ident-spec ident-specs)
         (let
            ((var-18
                (mapcar
                   #'(lambda (feature)
                        (feature-variable-value-pair feature))
                   (prop-ident-spec-feature-names
                      ident-spec))))
            (if var-18 (return var-18))))))


(defun propagate-idrule-category
   (binding binding-list feature-names
      fvpairs-or-var)
   (cond
      (feature-names
         (multiple-value-bind
            (cat propagated-binding-list)
            (merge-category
               (category-binding-category binding)
               binding-list
               (propagate-idrule-fvpairs-bundle
                  feature-names fvpairs-or-var)
               nil
               (if *addition-checking 'default-checking
                  nil))
            (setf
               (car
                  (binding-list-tail
                     (category-binding-number binding)
                     propagated-binding-list))
               (let
                  ((structure-19
                      (copy-category-binding binding)))
                  (setf
                     (category-binding-category structure-19)
                     cat)
                  structure-19))
            propagated-binding-list))
      (t
         (merge-category-variable fvpairs-or-var
            (make-fv-pair 'x
               (category-binding-number binding))
            binding-list nil))))


(defun binding-list-tail
   (binding-no binding-list)
   (cond
      ((null binding-list) nil)
      ((= binding-no
          (category-binding-number
             (car binding-list)))
         binding-list)
      (t
         (binding-list-tail binding-no
            (cdr binding-list)))))


(defun propagate-idrule-binding
   (binding-no cat-feature binding-list)
   (let
      ((binding
          (f-find (the fixnum binding-no)
             binding-list :key #'category-binding-number
             :test #'eql)))
      (if cat-feature
         (multiple-value-bind (cat new-binding-list)
            (merge-category
               (category-binding-category binding)
               binding-list
               (ncons
                  (make-fv-pair cat-feature
                     1))
               (ncons
                  (make-category-binding :number 1 :category
                     nil :repetition '*once*))
               'default-checking)
            (setf
               (car
                  (binding-list-tail
                     (category-binding-number binding)
                     new-binding-list))
               (make-category-binding :number binding-no
                  :category cat :repetition
                  (category-binding-repetition binding)))
            (propagate-idrule-binding
               (propagated-binding-number-for
                  (fv-pair-value
                     (f-find cat-feature cat :key
                        #'fv-pair-feature :test #'eq)))
               nil new-binding-list))
         (values binding binding-list))))


(defun propagated-binding-number-for (value)
   (if (numberp value) value
      (fv-pair-value
         (cdr (assoc value variable-substitutions)))))


(defun propagate-idrule-fvpairs-bundle
   (features fvpairs-or-var)
   (mapcar
      #'(lambda (feature)
           (if (atom fvpairs-or-var)
              (make-fv-pair feature
                 fvpairs-or-var)
              (prog1
                 (if
                    (eq feature
                       (fv-pair-feature
                          (car fvpairs-or-var)))
                    (car fvpairs-or-var)
                    (make-fv-pair feature
                       (fv-pair-value
                          (car fvpairs-or-var))))
                 (setf fvpairs-or-var
                    (cdr fvpairs-or-var)))))
      features))


(defun top-subst-1 (new old lst)
   (cond
      ((null lst) nil)
      ((eq (car lst) old)
         (cons new (cdr lst)))
      (t
         (cons (car lst)
            (top-subst-1 new old (cdr lst))))))


;;; Applying the default rules is much the same as the feature
;;; propagation rules. The variable substitution of proper
;;; values for old variable values takes place over all ID rule
;;; categories after each application.
;;;
;;; N.B. Nothing happens if default rule is assigning values to
;;; features in an embedded category and the feature the
;;; category is the value of is not even present in the ID rule.

(defun defrule-expand
   (idrule defrule-names)
   (dolist (defrule-name defrule-names)
      (setf idrule
         (apply-defrule idrule
            (normalise-defrule-definition defrule-name)
            defrule-name)))
   idrule)


(defun apply-defrule
   (idrule defrule defrule-name)
   (when
      (and
         (or (id-rule-lexical idrule)
            (not (default-rule-lexical defrule)))
         (or
            (id-rule-linear idrule)
            (not (default-rule-linear defrule))))
      (dolist
         (match-bindings
            (filter-linear-bindings
               (match-rule-pattern
                  (id-rule-binding-list idrule)
                  (id-rule-binding-nos idrule)
                  (default-rule-cat-bindings defrule)
                  (default-rule-binding-nos defrule))
               idrule (default-rule-binding-nos defrule)
               (default-rule-cat-bindings defrule)
               (id-rule-linear idrule)))
         (setf idrule
            (default-idrule idrule defrule
               match-bindings defrule-name))))
   idrule)


(defun default-idrule
   (idrule defrule match-bindings
      defrule-name)
   (let
      ((highest-binding-no
          (id-rule-highest-binding-no idrule)))
      (let
         ((structure-20 (copy-id-rule idrule)))
         (setf (id-rule-binding-list structure-20)
            (default-onto-category
               (default-category-binding-nos
                  match-bindings
                  (default-rule-category-index defrule)
                  (id-rule-binding-list idrule))
               (default-rule-feature-names defrule)
               (default-rule-value defrule)
               (default-rule-cat-bindings defrule)
               (id-rule-binding-list idrule)))
         (setf
            (id-rule-highest-binding-no structure-20)
            highest-binding-no)
         (setf (id-rule-rules-applied structure-20)
            (cons defrule-name
               (id-rule-rules-applied structure-20)))
         structure-20)))


(defun default-category-binding-nos
   (match-bindings cat-index
      idrule-binding-list)
   (mapcan
      #'(lambda (trans-binding-no)
           (cond
              ((category-index-cat-feature cat-index)
                 (let
                    ((category-fvpair
                        (f-find
                           (category-index-cat-feature
                              cat-index)
                           (category-binding-category
                              (f-find
                                 (the fixnum trans-binding-no)
                                 idrule-binding-list :key
                                 #'category-binding-number
                                 :test #'eql))
                           :key #'fv-pair-feature :test #'eq)))
                    (if category-fvpair
                       (ncons
                          (fv-pair-value category-fvpair)))))
              (t (ncons trans-binding-no))))
      (match-binding-matched-binding-nos
         (f-find
            (the fixnum
               (category-index-binding-no cat-index))
            match-bindings :key
            #'match-binding-pattern-binding-no :test
            #'eql))))


(defun default-onto-category
   (binding-nos feature-names value
      defrule-binding-list idrule-binding-list)
   (let
      ((variable-substitutions nil))
      (dolist (binding-no binding-nos)
         (let
            ((binding
                (f-find (the fixnum binding-no)
                   idrule-binding-list :key
                   #'category-binding-number :test #'eql)))
            (multiple-value-bind
               (new-cat new-binding-list)
               (merge-category
                  (category-binding-category binding)
                  idrule-binding-list
                  (mapcar
                     #'(lambda (feature)
                          (cond
                             ((eq value '\@)
                                (feature-variable-value-pair
                                   feature))
                             (t
                                (feature-proper-value-pair
                                   feature value))))
                     feature-names)
                  defrule-binding-list
                  (if *addition-checking 'default-checking
                     nil))
               (setf
                  (car
                     (binding-list-tail
                        (category-binding-number binding)
                        new-binding-list))
                  (make-category-binding :number binding-no
                     :category new-cat :repetition
                     (category-binding-repetition binding)))
               (setf idrule-binding-list
                  new-binding-list))))
      (substitute-feature-bindings
         variable-substitutions
         idrule-binding-list)))


;;; Compile all the idrules in the system into context free
;;; rules. Print out some statistics on the way to keep the user
;;; happy and be generally friendly. Does not compile words in
;;; GDE lexicon

(defun compile-world nil
   (compile-world-stats1)
   (dolist (name *id-rules)
      (compile-idrule name))
   (compile-world-stats2))


(defun compile-world-stats1 nil
   (format t
      "~A ID rules, ~A metarules, ~A propagation rules, ~A default rules, ~A LP rules~%"
      (list-length *id-rules)
      (list-length *meta-rules)
      (list-length *prop-rules)
      (list-length *default-rules)
      (list-length *lp-rules)))


(defun compile-world-stats2 nil
   (let
      ((number-expanded-rules 0)
         (number-compiled-rules 0))
      (dolist (name *id-rules)
         (progn
            (setf number-compiled-rules
               (+ number-compiled-rules
                  (list-length
                     (get name 'compiled-idrules))))
            (setf number-expanded-rules
               (+ number-expanded-rules
                  (list-length
                     (get name 'expanded-idrules))))))
      (format t
         "~A expanded ID rules, ~A phrase structure rules~%"
         number-expanded-rules number-compiled-rules)
      number-compiled-rules))


(defun compile-idrule (idrule-name)
   (or (get idrule-name 'compiled-idrules)
      (progn
         (reset-variable-value-pairs)
         (setf (get idrule-name 'compiled-idrules)
            (check-idrules-semantic-forms
               (share-first-daughters-in-idrules
                  (linear-metarule-expand
                     (cond
                        ((id-rule-linear
                              (get idrule-name 'idrule))
                           (sort-features-in-idrules
                              (catrule-expand
                                 (proprule-and-defrule-expand
                                    (normalise-idrule-definition
                                       idrule-name)))))
                        (t
                           (let
                              ((exist-linear-rules
                                    (exist-linear-prop-defrules)))
                              (mapcan
                                 #'(lambda (full-idrule)
                                    (if exist-linear-rules
                                       (sort-features-in-idrules
                                          (catrule-expand
                                             (proprule-and-defrule-expand
                                                (lprule-expand
                                                   full-idrule))))
                                       (lprule-expand
                                          full-idrule)))
                                 (or
                                    (get idrule-name
                                       'expanded-idrules)
                                    (setf
                                       (get idrule-name
                                          'expanded-idrules)
                                       (sort-features-in-idrules
                                          (metarule-expand
                                             (normalise-idrule-definition
                                                idrule-name)
                                             *meta-rules))))))))
                     *meta-rules)))))))


(defun exist-linear-prop-defrules nil
   (or
      (dolist (rule *prop-rules)
         (if (prop-rule-linear (get rule 'proprule))
            (return rule)))
      (dolist (rule *default-rules)
         (if
            (default-rule-linear (get rule 'defrule))
            (return rule)))))


(defun linearise-idrule (idrule-name)
   (reset-variable-value-pairs)
   (let
      ((exist-linear-rules
            (exist-linear-prop-defrules)))
      (check-idrules-semantic-forms
         (share-first-daughters-in-idrules
            (mapcan
               #'(lambda (idrule)
                  (cond
                     ((id-rule-linear idrule) (ncons idrule))
                     (t
                        (if exist-linear-rules
                           (sort-features-in-idrules
                              (catrule-expand
                                 (proprule-and-defrule-expand
                                    (lprule-expand idrule))))
                           (lprule-expand idrule)))))
               (sort-features-in-idrules
                  (catrule-expand
                     (proprule-and-defrule-expand
                        (normalise-idrule-definition
                           idrule-name)))))))))


(defun fully-instantiate-idrule (idrule-name)
   (reset-variable-value-pairs)
   (check-idrules-semantic-forms
      (share-first-daughters-in-idrules
         (sort-features-in-idrules
            (catrule-expand
               (proprule-and-defrule-expand
                  (normalise-idrule-definition
                     idrule-name)))))))


(defun fully-instantiate-category (cat-bindings lexical)
   (sort-features-in-category
      (let
         ((category-rules-applied nil))
         (catrule-expand-bindings
            (ncons
               (category-binding-number
                  (car cat-bindings)))
            cat-bindings lexical))))


(defun check-idrules-semantic-forms (idrules)
   (dolist (idrule idrules)
      (dolist
         (form (id-rule-semantic-forms idrule))
         (type-check-semantic-form form
            (id-rule-name idrule)
            (car (id-rule-binding-nos idrule))
            (id-rule-binding-list idrule))))
   idrules)


;;; Print current grammar, or all words that the GDE knows about
;;; (i.e. all words in private lexicon and all cached morphology
;;; system words). Alternative output in conventional form, or
;;; just pretty print categories in internal format. If the latter,
;;; then grammar is probably going to be used by a standalone
;;; parser, so give it the ordering of features and the correspondance
;;; between category indexes and feature lists.
;;;
;;; When reloading into GDE, take care about escape character before
;;; e.g. numeric feature values, and about variables - these begin
;;; with @ and need to be made into grammar-variable structures.

(defun dump-grammar-to-file (dump-readable)
   (if *id-rules
      (let
         ((input (prompt-if-necessary "File name? ")))
         (when input
            (let
               ((file (canonise-grammar-file-name input)))
               (backup-grammar-file file)
               (compile-world)
               (with-open-stream
                  (*standard-output*
                     (open file :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create))
                  (if dump-readable
                     (mapc #'print-compiled-idrule *id-rules)
                     (let
                        ((defns
                              (mapcan #'dumped-compiled-idrules
                                 *id-rules))
                           (*print-array* t) (*print-escape* t)
                           (*print-length* nil) (*print-level* nil))
                        (print *features) (terpri)
                        (print *index-category-table) (terpri)
                        (mapc #'print defns)
                        (terpri))))
               (format t "Grammar dumped~%"))))
      (gde-cerror "no ID rules in grammar")))


(defun print-compiled-idrule (idrule)
   (dolist
      (compiled-rule (get idrule 'compiled-idrules))
      (print-idrule-definition compiled-rule nil)
      (terpri))
   (terpri) (terpri))


(defun dumped-compiled-idrules (idrule)
   (mapcar
      #'(lambda (compiled-rule)
         (convert-idrule-to-parser
            (if *term-unification compiled-rule
               (fill-unrestricted-idrule compiled-rule))))
      (get idrule 'compiled-idrules)))


(defun dump-word-definitions-to-file (dump-readable)
   (if (or *words *cached-words *disk-resident-words)
      (let
         ((input (prompt-if-necessary "File name? ")))
         (when input
            (let
               ((words
                     (sort
                        (copy-list (append *words *cached-words *disk-resident-words))
                        #'(lambda (x y)
                           (string-lessp (string x) (string y)))))
                  (file
                     (canonise-grammar-file-name input)))
               (backup-grammar-file file)
               (with-open-stream
                  (*standard-output*
                     (open file :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create))
                  (if dump-readable
                     (mapc #'print-compiled-word-definition words)
                     (let
                        ((defns
                           (mapcan #'dumped-compiled-word-definitions
                              words))
                           (*print-array* t) (*print-escape* t)
                           (*print-length* nil) (*print-level* nil))
                        (print *features) (terpri)
                        (print *index-category-table) (terpri)
                        (mapc #'print defns)
                        (terpri))))
               (format t "Word definitions dumped~%"))))
      (gde-cerror "no word definitions known")))


(defun print-compiled-word-definition (word)
   (print-word-definition word
      (get-word-definition word 'normalised 'full)
      nil)
   (terpri))


(defun dumped-compiled-word-definitions (word)
   (mapcar
      #'(lambda (sense)
         (let
            ((bindings
                  (if *term-unification
                     (word-sense-cat-bindings sense)
                     (fill-unrestricted-category
                        (word-sense-cat-bindings sense)))))
            (list
               (convert-category-to-parser
                  (car bindings) (cdr bindings) t)
               word)))
      (word-definition-senses
         (get-word-definition word 'normalised 'full))))


;;; End of file

