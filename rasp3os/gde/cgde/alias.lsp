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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - ALIAS EXPANSION
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code for expanding out the aliases in
;;; categories inside rules and declarations.
;;;
;;; Entry points:
;;;
;;;  * (defun Normalise-alias-definition (alias-name) ...
;;;  * (defun Normalise-category-definition (category-name) ...
;;;  * (defun Normalise-extension-definition (extension-name) ...
;;;  * (defun Normalise-top-definition (top-name) ...
;;;  * (defun Normalise-idrule-definition (idrule-name) ...
;;;  * (defun Split-optional-category-idrules (idrules) ...
;;;  * (defun Normalise-metarule-definition (metarule-name) ...
;;;  * (defun Normalise-defrule-definition (defrule-name) ...
;;;  * (defun Normalise-proprule-definition (proprule-name) ...
;;;  * (defun Normalise-lprule-definition (lprule-name) ...
;;;  * (defun Normalise-word-definition (word) ...
;;;  * (defun Normalise-category-specification (cat-bindings) ...
;;;  * (defun Merge-category (cat1 cat1-bindings cat2 cat2-bindings
;;;      overwrite-p) ...
;;;
;;; All of the definition normalising functions behave in the
;;; same manner. They are passed the name of the definition
;;; whose normalisation is required. Each function first looks
;;; to see if the normalised definition is already stored on the
;;; property list of the named definition and if it is found
;;; then the normalised definition is returned. If no data is
;;; found then the un-normalised definition (as supplied by the
;;; user) is retrieved from the property list of the named
;;; definition and an attempt made to normalise it. If
;;; normalisation fails (as a result of undefined aliases, etc)
;;; the function throws out with an ERROR, otherwise the
;;; normalised definition is stored on the property list for
;;; possible future use and returned as the value of the
;;; function

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defvar grammar-errors)


(defun semantic-form-pattern-p (x)
   (and (consp x)
      (integerp (car x))
      (consp (cdr x)) (category-binding-p (cadr x))))


;;; Simply normalise the category stored as the alias
;;; definition.

(defun normalise-alias-definition
   (alias-name)
   (normalise-alias-definition1 alias-name
      nil))


(defun normalise-alias-definition1
   (alias-name alias-chain)
   (let
      ((grammar-errors nil)
         (normalised-alias
            (get alias-name 'normalised-alias)))
      (cond
         (normalised-alias normalised-alias)
         (t
            (setf normalised-alias
               (let
                  ((structure-129
                      (copy-alias-declaration
                         (get alias-name 'alias))))
                  (setf
                     (alias-declaration-cat-bindings
                        structure-129)
                     (normalise-category-bindings1
                        (alias-declaration-cat-bindings
                           structure-129)
                        (cons alias-name alias-chain)))
                  structure-129))
            (report-normalisation-errors "alias"
               alias-name)
            (setf (get alias-name 'normalised-alias)
               normalised-alias)
            normalised-alias))))


(defun normalise-extension-definition
   (name)
   (let
      ((grammar-errors nil)
         (normalised-definition
            (get name 'normalised-extension)))
      (cond
         (normalised-definition
            normalised-definition)
         (t
            (setf normalised-definition
               (let
                  ((structure-130
                      (copy-extension-declaration
                         (get name 'extension))))
                  (setf
                     (extension-declaration-features
                        structure-130)
                     (normalise-set-spec
                        (extension-declaration-features
                           structure-130)))
                  structure-130))
            (report-normalisation-errors
               "extension declaration" name)
            (setf (get name 'normalised-extension)
               normalised-definition)))))


(defun normalise-top-definition (name)
   (let
      ((grammar-errors nil)
         (normalised-definition
            (get name 'normalised-top)))
      (cond
         (normalised-definition
            normalised-definition)
         (t
            (setf normalised-definition
               (let
                  ((decl
                      (copy-top-declaration (get name 'top))))
                  (setf
                     (top-declaration-categories decl)
                     (mapcar #'normalise-category-bindings
                        (top-declaration-categories decl)))
                  decl))
            (report-normalisation-errors "top declaration" name)
            (setf (get name 'normalised-top)
               normalised-definition)))))


;;; Category declaration semantic types may contain categories
;;; which are taken to mean the semantic type of that category -
;;; if not known then return *.

(defun normalise-category-definition (name)
   (normalise-category-definition1 name nil))


(defun normalise-category-definition1
   (name category-chain)
   (let
      ((grammar-errors nil)
         (normalised-definition
            (get name 'normalised-category)))
      (cond
         (normalised-definition
            normalised-definition)
         (t
            (setf normalised-definition
               (let
                  ((structure-131
                      (copy-category-declaration
                         (get name 'category))))
                  (setf
                     (category-declaration-cat-bindings
                        structure-131)
                     (normalise-category-bindings
                        (category-declaration-cat-bindings
                           structure-131)))
                  (setf
                     (category-declaration-features
                        structure-131)
                     (normalise-set-spec
                        (category-declaration-features
                           structure-131)))
                  (setf
                     (category-declaration-semantic-types
                        structure-131)
                     (mapcan
                        #'(lambda (type)
                             (normalise-semantic-type type
                                (category-declaration-lexical
                                   (get name 'category))
                                category-chain))
                        (category-declaration-semantic-types
                           structure-131)))
                  structure-131))
            (report-normalisation-errors
               (if
                  (category-declaration-lexical
                     normalised-definition)
                  "lcategory declaration"
                  "category declaration")
               name)
            (setf (get name 'normalised-category)
               normalised-definition)))))


;;; Normalise a semantic type into e, t and * - normalise a
;;; semantic formula by exapnding out semantic operators,
;;; translating their category indices in the process.

(defun normalise-semantic-type
   (type lexical category-chain)
   (cond
      ((basic-type-p type)
         (cond
            ((and (get (basic-type-name type) 'alias)
                (normalise-semantic-category-type
                   (normalise-category-bindings
                      (ncons
                         (make-category-binding :number 0
                            :category
                            (make-alias-instantiation :name
                               (basic-type-name type) :bundle
                               nil)
                            :repetition '*once*)))
                   lexical category-chain)))
            (t (ncons type))))
      ((complex-type-p type)
         (mapcan
            #'(lambda (arg)
                 (mapcar
                    #'(lambda (res)
                         (make-complex-type :arg arg :res res))
                    (normalise-semantic-type
                       (complex-type-res type) lexical
                       category-chain)))
            (normalise-semantic-type
               (complex-type-arg type) lexical
               category-chain)))
      (t
         (normalise-semantic-category-type type
            lexical category-chain))))


(defun normalise-semantic-category-type
   (type lexical category-chain)
   (let
      ((bindings
          (normalise-category-bindings type)))
      (let
         ((matching-decl
             (dolist (category *categories)
                (let
                   ((var-132
                       (and
                          (not
                             (member category category-chain :test #'eq))
                          (let
                             ((decl
(normalise-category-definition1
                                    category
                                    (cons category
                                       category-chain))))
                             (and
                                (or
(category-declaration-lexical
                                      decl)
                                   (eq
(category-declaration-lexical
                                         decl)
                                      lexical))
                                (null
(category-declaration-feature-path
                                      decl))
                                (match-category
                                   (category-binding-number
                                      (car
(category-declaration-cat-bindings
                                            decl)))
(category-declaration-cat-bindings
                                      decl)
                                   (category-binding-number
                                      (car bindings))
                                   bindings)
                                decl)))))
                   (if var-132 (return var-132))))))
         (mapcan
            #'(lambda (type)
                 (copy-list
                    (normalise-semantic-type type lexical
                       category-chain)))
            (if matching-decl
               (category-declaration-semantic-types
                  matching-decl)
               (ncons (make-basic-type :name '*)))))))


;;; Normalise a list of semantic forms (from an ID rule or metarule)

(defun normalise-semantic-forms (forms nos-for-bound nos-for-free)
   (mapcar
      #'(lambda (form)
         (if (and (consp form) (semantic-form-pattern-p (car form)))
            (normalise-semantic-forms1 form nos-for-bound nos-for-free
               nil)
            (normalise-semantic-form form nos-for-bound nos-for-free)))
      forms))


(defun normalise-semantic-forms1 (forms nos-for-bound nos-for-free
      pattern-nos)
   (cond
      ((null forms) nil)
      ((semantic-form-pattern-p (car forms))
         (when (member (caar forms) pattern-nos)
            (push (ncons 'duplicate-pattern-nos) grammar-errors))
         (cons
            (cons (caar forms)
               (if (integerp (cdar forms)) (cdar forms)
                  (normalise-category-bindings (cdar forms))))
            (normalise-semantic-forms1 (cdr forms) nos-for-bound
               nos-for-free (cons (caar forms) pattern-nos))))
      (t
         (mapcar
            #'(lambda (f)
               (normalise-semantic-form f nos-for-bound nos-for-free))
            forms))))


(defun normalise-semantic-form (form nos-for-bound nos-for-free)
   (if
      (and (symbolp form) (get form *metarule-operator-property)
         (compiled-function-p (get form *metarule-operator-property)))
      (get form *metarule-operator-property)
      (normalise-semantic-form1 form nos-for-bound nos-for-free)))


(defun normalise-semantic-form1 (form nos-for-bound nos-for-free)
   (cond
      ((and (symbolp form)
          (get form *semantic-operator-property))
         (translate-positions-to-bindings
            (get form *semantic-operator-property) nil
            nos-for-bound nos-for-free nil))
      ((atom form) form)
      (t
         (mapcar
            #'(lambda (item)
                 (normalise-semantic-form1 item nos-for-bound nos-for-free))
            form))))


;;; Normalise an ID rule

(defun normalise-idrule-definition
   (idrule-name)
   (let
      ((grammar-errors nil)
         (normalised-bindings nil)
         (normalised-semantics nil)
         (normalised-idrules
            (get idrule-name 'normalised-idrules))
         (idrule-definition nil))
      (cond
         (normalised-idrules normalised-idrules)
         (t
            (setf idrule-definition
               (get idrule-name 'idrule))
            (setf normalised-bindings
               (normalise-category-bindings
                  (id-rule-binding-list idrule-definition)))
            (setq normalised-semantics
               (normalise-semantic-forms
                  (id-rule-semantic-forms idrule-definition)
                  (id-rule-binding-nos idrule-definition)
                  (id-rule-binding-nos idrule-definition)))
            (report-normalisation-errors
               (if (id-rule-linear idrule-definition)
                  "PS rule" "ID rule")
               idrule-name)
            (split-optional-category-idrules
               (ncons
                  (let
                     ((structure-133
                         (copy-id-rule idrule-definition)))
                     (setf
                        (id-rule-highest-binding-no
                           structure-133)
                        (highest-number-in-bindings
                           normalised-bindings))
                     (setf (id-rule-binding-list structure-133)
                        normalised-bindings)
                     (setf (id-rule-lexical structure-133)
                        (lexical-head-in-bindings
                           normalised-bindings))
                     (setf
                        (id-rule-semantic-forms structure-133)
                        normalised-semantics)
                     structure-133)))))))


(defun highest-number-in-bindings (bindings)
   (1+
      (apply #'max
         (mapcar
            #'(lambda (binding)
               (category-binding-number binding))
            bindings))))


;;; A lexical head is [H +, BAR 0] and specified for either TAKES
;;; or SUBCAT.

(defun lexical-head-in-bindings (cat-bindings)
   (member-if
      #'(lambda (cat-binding)
           (let
              ((head-p nil) (bar-zero-p nil) (subcat-p nil))
              (mapc
                 #'(lambda (fvpair)
                      (let
                         ((feature (fv-pair-feature fvpair)))
                         (cond
                            ((eq feature (head-feature-name))
                               (setq head-p
                                  (eq (fv-pair-value fvpair) '+)))
                            ((eq feature (bar-feature-name))
                               (setq bar-zero-p
                                  (eq (fv-pair-value fvpair) '\0)))
                            ((or (eq feature (subcat-feature-name))
                                (eq feature (takes-feature-name)))
                               (setq subcat-p t)))))
                 (category-binding-category cat-binding))
              (and head-p bar-zero-p subcat-p)))
      cat-bindings))


;;; Split ID rules with ( ) and ( )* categories into two. This
;;; must happen recursively if there is more than one category
;;; of these types in the rule.

(defun split-optional-category-idrules
   (idrules)
   (mapcan
      #'(lambda (idrule)
           (or
              (split-optional-category-idrules
                 (dolist
                    (binding-no (id-rule-binding-nos idrule))
                    (let
                       ((var-134
                           (let
                              ((binding
                                  (f-find
                                     (the fixnum binding-no)
                                     (id-rule-binding-list
                                        idrule)
                                     :key
                                     #'category-binding-number
                                     :test #'eql)))
                              (cond
                                 ((eq
(category-binding-repetition
                                        binding)
                                     '*opt*)
(split-optional-category-idrules1
                                       idrule binding-no
                                       binding '*once*))
                                 ((eq
(category-binding-repetition
                                        binding)
                                     '*rep0*)
(split-optional-category-idrules1
                                       idrule binding-no
                                       binding '*rep1*))))))
                       (if var-134 (return var-134)))))
              (ncons idrule)))
      idrules))


(defun split-optional-category-idrules1
   (idrule binding-no binding new-rep)
   (list
      (let
         ((structure-137 (copy-id-rule idrule)))
         (setf (id-rule-name structure-137)
            (split-idrule-new-name
               (id-rule-name idrule) nil))
         (setf (id-rule-binding-nos structure-137)
            (remove-list-1 binding-no
               (id-rule-binding-nos structure-137)))
         (setf (id-rule-binding-list structure-137)
            (remove-list-1
               (f-find (the fixnum binding-no)
                  (id-rule-binding-list structure-137) :key
                  #'category-binding-number :test #'eql)
               (id-rule-binding-list structure-137)))
         (setf
            (id-rule-semantic-forms structure-137)
            (mapcan
               #'(lambda (form)
                    (unless
                       (binding-no-in-form form binding-no)
                       (ncons form)))
               (id-rule-semantic-forms structure-137)))
         structure-137)
      (let
         ((structure-135 (copy-id-rule idrule)))
         (setf (id-rule-name structure-135)
            (split-idrule-new-name
               (id-rule-name idrule) t))
         (setf (id-rule-binding-list structure-135)
            (mapcar
               #'(lambda (bind)
                    (cond
                       ((eq bind binding)
                          (let
                             ((structure-136
                                 (copy-category-binding bind)))
                             (setf
                                (category-binding-repetition
                                   structure-136)
                                new-rep)
                             structure-136))
                       (t bind)))
               (id-rule-binding-list structure-135)))
         structure-135)))


(defun binding-no-in-form (form n)
   (cond
      ((atom form) (equal form n))
      (t
         (or (binding-no-in-form (car form) n)
            (binding-no-in-form (cdr form) n)))))


(defun split-idrule-new-name
   (rule-name bigger)
   (cond
      ((top-rule-name-meta-names rule-name)
         (let
            ((structure-140
                (copy-top-rule-name rule-name)))
            (setf
               (top-rule-name-meta-names structure-140)
               (mapcar
                  #'(lambda (meta-name)
                       (cond
                          ((eq meta-name
                              (car
                                 (last
                                    (top-rule-name-meta-names
                                       rule-name))))
                             (let
                                ((structure-141
                                    (copy-sub-rule-name
                                       meta-name)))
                                (setf
                                   (sub-rule-name-split
                                      structure-141)
                                   (split-idrule-new-name1
                                      (sub-rule-name-split
                                         meta-name)
                                      bigger))
                                structure-141))
                          (t meta-name)))
                  (top-rule-name-meta-names rule-name)))
            structure-140))
      (t
         (let
            ((structure-138
                (copy-top-rule-name rule-name)))
            (setf (top-rule-name-base structure-138)
               (let
                  ((structure-139
                      (copy-sub-rule-name
                         (top-rule-name-base rule-name))))
                  (setf (sub-rule-name-split structure-139)
                     (split-idrule-new-name1
                        (sub-rule-name-split
                           (top-rule-name-base rule-name))
                        bigger))
                  structure-139))
            structure-138))))


(defun split-idrule-new-name1
   (old-split bigger)
   (let
      ((new-split
          (cond
             (bigger "+")
             (t "-"))))
      (cond
         (old-split
            (concat-string old-split new-split))
         (t new-split))))


;;; Normalise a metarule.

(defun normalise-metarule-definition
   (metarule-name)
   (let
      ((grammar-errors nil)
         (normalised-category-bindings nil)
         (normalised-semantics nil)
         (normalised-metarule
            (get metarule-name 'normalised-metarule))
         (metarule-definition nil))
      (cond
         (normalised-metarule normalised-metarule)
         (t
            (setf metarule-definition
               (get metarule-name 'metarule))
            (setf normalised-category-bindings
               (normalise-category-bindings
                  (meta-rule-cat-bindings
                     metarule-definition)))
            (setq normalised-semantics
               (normalise-semantic-forms
                  (meta-rule-semantic-forms metarule-definition)
                  (meta-rule-lhs-binding-nos metarule-definition)
                  (meta-rule-rhs-binding-nos metarule-definition)))
            (report-normalisation-errors "metarule"
               metarule-name)
            (setf
               (get metarule-name 'normalised-metarule)
               (let
                  ((structure-142
                      (copy-meta-rule metarule-definition)))
                  (setf
                     (meta-rule-cat-bindings structure-142)
                     normalised-category-bindings)
                  (setf
                     (meta-rule-semantic-forms structure-142)
                     normalised-semantics)
                  structure-142))))))


;;; Normalise a default rule definition.

(defun normalise-defrule-definition
   (defrule-name)
   (let
      ((grammar-errors nil)
         (normalised-bindings nil)
         (normalised-names nil)
         (normalised-defrule
            (get defrule-name 'normalised-defrule))
         (defrule-definition nil))
      (cond
         (normalised-defrule normalised-defrule)
         (t
            (setf defrule-definition
               (get defrule-name 'defrule))
            (setf normalised-bindings
               (normalise-category-bindings
                  (default-rule-cat-bindings
                     defrule-definition)))
            (setf normalised-names
               (normalise-set-spec
                  (default-rule-feature-names
                     defrule-definition)))
            (report-normalisation-errors "default rule"
               defrule-name)
            (setf
               (get defrule-name 'normalised-defrule)
               (let
                  ((structure-143
                      (copy-default-rule defrule-definition)))
                  (setf
                     (default-rule-cat-bindings structure-143)
                     normalised-bindings)
                  (setf
                     (default-rule-feature-names structure-143)
                     normalised-names)
                  structure-143))))))


;;; Normalise a feature propagation rule by normalising the
;;; category binding list of the match portion of the rule.

(defun normalise-proprule-definition (proprule-name)
   (let
      ((grammar-errors nil)
         (normalised-proprule
            (get proprule-name 'normalised-proprule)))
      (cond
         (normalised-proprule normalised-proprule)
         (t
            (setf normalised-proprule
               (let
                  ((structure-144
                      (copy-prop-rule
                         (get proprule-name 'proprule))))
                  (setf
                     (prop-rule-cat-bindings structure-144)
                     (normalise-category-bindings
                        (prop-rule-cat-bindings
                           structure-144)))
                  (setf (prop-rule-ident-specs structure-144)
                     (mapcar
                        #'(lambda (equality)
                           (mapcar
                              #'(lambda (spec)
                                 (let
                                    ((structure-145
                                          (copy-prop-ident-spec spec)))
                                    (setf
                                       (prop-ident-spec-feature-names
                                          structure-145)
                                       (normalise-set-spec
                                          (prop-ident-spec-feature-names
                                             structure-145)))
                                    structure-145))
                              equality))
                        (prop-rule-ident-specs structure-144)))
                  structure-144))
            (check-proprule-ident-specs
               normalised-proprule)
            (report-normalisation-errors
               "propagation rule" proprule-name)
            (setf
               (get proprule-name 'normalised-proprule)
               normalised-proprule)))))


(defun check-proprule-ident-specs (normalised-proprule)
   (unless
      (or
         (dolist (equality (prop-rule-ident-specs normalised-proprule) t)
            (dolist (spec equality t)
               (unless
                  (= (list-length
                        (prop-ident-spec-feature-names spec))
                     (list-length
                        (prop-ident-spec-feature-names (car equality))))
                  (return nil))))
         (dolist (equality (prop-rule-ident-specs normalised-proprule) t)
            (dolist (spec equality t)
               (unless
                  (or (null (prop-ident-spec-feature-names spec))
                     (dolist (feature (prop-ident-spec-feature-names spec) t)
                        (unless (is-category-valued feature)
                           (return nil))))
                  (return nil)))))
      (push (ncons 'unequal-ident-specs) grammar-errors)))


;;; Normalise an LP rule definition.

(defun normalise-lprule-definition
   (lprule-name)
   (let
      ((grammar-errors nil)
         (normalised-lprule
            (get lprule-name 'normalised-lprule)))
      (cond
         (normalised-lprule normalised-lprule)
         (t
            (setf normalised-lprule
               (let
                  ((structure-146
                      (copy-lp-rule
                         (get lprule-name 'lprule))))
                  (setf (lp-rule-lp-terms structure-146)
                     (mapcar
                        #'(lambda (lpterm)
                             (normalise-category-bindings
                                lpterm))
                        (lp-rule-lp-terms structure-146)))
                  structure-146))
            (report-normalisation-errors "LP rule"
               lprule-name)
            (setf (get lprule-name 'normalised-lprule)
               normalised-lprule) normalised-lprule))))


;;; Normalise the definition of a word in the lexicon by
;;; normalising all the categories, each representing a word
;;; sense. Words from morphology system are stored normalised,
;;; so just return that stored definition.

(defun normalise-word-definition (word)
   (let
      ((grammar-errors nil)
         (normalised-word
            (if (word-definition-file (get word 'word))
               (get word 'normalised-word)
               (get word 'word))))
      (cond
         (normalised-word normalised-word)
         (t
            (setf normalised-word
               (let
                  ((structure-147
                      (copy-word-definition (get word 'word))))
                  (setf
                     (word-definition-senses structure-147)
                     (mapcar
                        #'(lambda (sense)
                             (let
                                ((bindings
(normalise-category-bindings
                                       (word-sense-cat-bindings
                                          sense))))
                                (let
                                   ((structure-148
                                       (copy-word-sense
                                          sense)))
                                   (setf
                                      (word-sense-cat-bindings
                                         structure-148) bindings)
                                   (setf
(word-sense-semantic-forms
                                         structure-148)
                                      (normalise-semantic-forms
(word-sense-semantic-forms
                                            structure-148)
                                         nil nil))
                                   structure-148)))
                        (word-definition-senses
                           structure-147)))
                  structure-147))
            (report-normalisation-errors "word" word)
            (setf (get word 'normalised-word)
               normalised-word)))))


;;; --- Normalise category --- Normalise a set of category
;;; bindings specifying a category. This is an entry point;
;;; Normalise-category-binding is not.

(defun normalise-category-specification
   (category)
   (let
      ((grammar-errors nil)
         (normalised-category nil))
      (setf normalised-category
         (normalise-category-bindings category))
      (report-normalisation-errors "category"
         "input")
      normalised-category))


;;; Normalise a set name by expanding it out into its feature
;;; names. If the set name is a list, then the feature names are
;;; there already, so only have to check that they have been
;;; defined as feature names.

(defun normalise-set-spec (feature-names)
   (unless (listp feature-names)
      (let
         ((set-name feature-names))
         (setq feature-names
            (cond
               ((member set-name *sets :test #'eq)
                  (set-declaration-features
                     (get set-name 'set)))
               (t
                  (push (cons 'undefined-set set-name)
                     grammar-errors)
                  nil)))))
   (dolist (feature-name feature-names)
      (unless (is-feature feature-name)
         (push
            (cons 'undefined-feature feature-name)
            grammar-errors)))
   feature-names)


;;; Normalise a set of category bindings by normalising the
;;; category inside each, always passing round the rest of the
;;; bindings in case a category value alias has to modify them.

(defun normalise-category-bindings
   (bindings)
   (normalise-category-bindings1 bindings
      nil))


(defun normalise-category-bindings1
   (bindings alias-chain)
   (let
      ((highest-binding-no
          (highest-number-in-bindings bindings))
         (variable-substitutions nil))
      (let
         ((normalised-bindings
             (normalise-category-bindings2
                (mapcar
                   #'(lambda (binding)
                        (let
                           ((structure-149
                               (copy-category-binding
                                  binding)))
                           (setf
                              (category-binding-category
                                 structure-149)
                              (copy-category-binding-category
                                 (category-binding-category
                                    structure-149)))
                           structure-149))
                   bindings)
                alias-chain)))
         (substitute-feature-bindings
            variable-substitutions
            normalised-bindings))))



(defun copy-category-binding-category (x)
   (cond
      ((alias-instantiation-p x)
         (make-alias-instantiation :name
            (alias-instantiation-name x) :bundle
            (copy-category-binding-category
               (alias-instantiation-bundle x))))
      (t
         (copy-list x))))


(defun normalise-category-bindings2
   (binding-list alias-chain)
   (let*
      ((first-binding-no
          (category-binding-number
             (car binding-list)))
         (binding nil))
      (loop
         (cond
            ((and binding
                (=
                   (category-binding-number
                      (car binding-list))
                   first-binding-no))
               (return nil)))
         (setf binding (car binding-list))
         (setf binding-list (cdr binding-list))
         (multiple-value-bind (new-cat new-bindings)
            (normalise-category
               (category-binding-category binding)
               binding-list alias-chain)
            (setf binding-list
               (nconc new-bindings
                  (ncons
                     (make-category-binding :number
                        (category-binding-number binding)
                        :category new-cat :repetition
                        (category-binding-repetition
                           binding)))))))
      binding-list))


;;; Normalise a list of Fv-pair and Alias-instantiation records.
;;; Alias-chain is a list of the aliases currently being
;;; normalised, to check for circular alias dependencies.

(defun normalise-category
   (category bindings alias-chain)
   (if (alias-instantiation-p category)
      (normalise-alias-instantiation category nil
         bindings alias-chain)
      (normalise-feature-bundle category bindings
         alias-chain)))


(defun normalise-feature-bundle
   (feature-bundle bindings alias-chain)
   (let
      ((alias
          (dolist
             (feature-specification feature-bundle)
             (if
                (alias-instantiation-p
                   feature-specification)
                (return feature-specification)))))
      (cond
         (alias
            (multiple-value-bind
               (new-bundle new-bindings)
               (normalise-alias-instantiation alias
                  nil
                  bindings alias-chain)
               (normalise-feature-bundle
                  (nconc new-bundle
                     (remove-list-1 alias feature-bundle))
                  new-bindings alias-chain)))
         (t
            (dolist (fvpair feature-bundle)
               (check-feature-specification-value
                  (fv-pair-feature fvpair)
                  (fv-pair-value fvpair)))
            (check-for-duplicate-features
               feature-bundle)
            (values feature-bundle bindings)))))


(defun check-feature-specification-value
   (feature value)
   (cond
      ((not (is-feature feature))
         (push (cons 'undefined-feature feature)
            grammar-errors))
      ((consp value)
         (dolist (val value)
            (check-feature-specification-value feature
               val)))
      ((member value
          '(*present* *novalue* *absent*) :test #'eq))
      ((varp value))
      ((is-category-valued feature)
         (unless (numberp value)
            (push
               (cons 'unknown-category-feature feature)
               grammar-errors)))
      ((not
          (member value
             (feature-declaration-values
                (get feature 'feature)) :test #'eq))
         (push
            (cons 'undefined-value
               (cons feature value))
            grammar-errors))))


(defun check-for-duplicate-features
   (bundle)
   (mapl
      #'(lambda (tail)
           (if
              (f-find (fv-pair-feature (car tail))
                 (cdr tail) :key #'fv-pair-feature
                 :test #'eq)
              (push
                 (cons 'duplicate-feature
                    (fv-pair-feature (car tail)))
                 grammar-errors)))
      bundle))


(defun is-feature (name)
   (and (symbolp name) (get name 'feature)))


;;; Take the definition of an alias instantiation and add it to
;;; the category argument. Rename any variables in the
;;; definition of the alias.

(defun normalise-alias-instantiation
   (alias category bindings alias-chain)
   (let
      ((alias-bindings
          (get-alias-definition
             (alias-instantiation-name alias)
             alias-chain)))
      (multiple-value-bind
         (new-bundle new-bindings)
         (merge-category category bindings
            (category-binding-category
               (car alias-bindings))
            (cdr alias-bindings) 'alias-checking)
         (multiple-value-bind
            (rest-bundle rest-bindings)
            (normalise-feature-bundle
               (alias-instantiation-bundle alias) bindings
               alias-chain)
            (merge-category new-bundle new-bindings
               rest-bundle rest-bindings
               'alias-checking)))))


(defun get-alias-definition
   (alias alias-chain)
   (cond
      ((member alias alias-chain :test #'eq)
         (gde-ferror "definitions of aliases "
            (nreverse alias-chain) " are circular"))
      ((and (symbolp alias) (get alias 'alias))
         (let
            ((subs nil) (bindings nil))
            (setf bindings
               (mapcar
                  #'(lambda (binding)
                       (let
                          ((structure-150
                              (copy-category-binding binding)))
                          (setf
                             (category-binding-category
                                structure-150)
                             (mapcar
                                #'(lambda (fvpair)
                                     (progn
                                        (if
                                           (and
                                              (varp
                                                 (fv-pair-value
                                                    fvpair))
                                              (not
                                                 (assoc
                                                    (fv-pair-value
                                                       fvpair)
                                                    subs :test #'eq)))
                                           (push
                                              (cons
                                                 (fv-pair-value
                                                    fvpair)
                                                 (make-fv-pair
                                                    (fv-pair-feature
                                                       fvpair)
                                                    (generate-variable)))
                                              subs))
                                        fvpair))
                                (category-binding-category
                                   binding)))
                          structure-150))
                  (alias-declaration-cat-bindings
                     (normalise-alias-definition1 alias
                        alias-chain))))
            (substitute-feature-bindings subs
               bindings)))
      (t
         (push (cons 'undefined-alias alias)
            grammar-errors)
         (ncons
            (make-category-binding :number 0 :category
               nil :repetition '*once*)))))


;;; Called after attempting to normalise a declaration to throw
;;; out with error if any undefined aliases, features, or sets,
;;; etc. were found in the course of normalisation.

(defun report-normalisation-errors
   (definition-type name)
   (dolist
      (error
         (nreverse (remove-duplicates grammar-errors :test #'equal)))
      (let
         ((type (car error)))
         (cond
            ((eq type 'undefined-feature)
               (gde-cerror "unknown feature name "
                  (cdr error)))
            ((eq type 'duplicate-feature)
               (gde-cerror "feature " (cdr error)
" occurs more than once in a single category"))
            ((eq type 'undefined-alias)
               (gde-cerror "unknown alias "
                  (cdr error)))
            ((eq type 'undefined-value)
               (gde-cerror (cddr error)
                  " is not a legal value for feature "
                  (cadr error)))
            ((eq type 'undefined-set)
               (gde-cerror "unknown set "
                  (cdr error)))
            ((eq type 'incompatible-values)
               (gde-cerror "two different values "
                  (abbrev-print-value
                     (car (cadr error)))
                  " and "
                  (abbrev-print-value
                     (cdr (cadr error)))
                  " found for feature " (cddr error)))
            ((eq type 'unknown-category-feature)
               (gde-cerror "feature " (cdr error)
" was not known to be category-valued when construct "
                  "was declared"))
            ((eq type 'unequal-ident-specs)
               (gde-cerror
                  "unequal numbers of features on RHS "
                  "of propagation rule "))
            ((eq type 'duplicate-pattern-nos)
               (gde-cerror
                  "more than one semantic condition on "
                  "a single daughter ")))))
   (and grammar-errors
      (gde-ferror definition-type " " name
         " is faulty")))


(defun abbrev-print-value (value)
   (cond
      ((numberp value) "<category>")
      (t value)))


;;; Merge cat2 argument into category represented by cat1.
;;; Overwrite-p controls what happens when both categories have
;;; the same feature but with different non-variable values.
;;; Cat2 value may override cat1 value (overwrite-p = t, for
;;; metarule expansion), cat1 value may remain (overwrite-p =
;;; nil, for default and propagation rule instantation), or an
;;; error may be signalled (overwrite-p = error, for alias
;;; expansion).
;;;
;;; Expects special variable variable-substitutions to be bound
;;; before call, and adds to variable if variable values found.
;;; Highest-binding-no special should also be bound.

(defvar new-merge-cat-bindings nil)


(defun merge-category (cat1 cat1-bindings cat2 cat2-bindings
      overwrite-p)
   (dolist (fvpair2 cat2)
      (let
         ((cat1-tail
             (merge-category-tail
                (fv-pair-feature fvpair2) cat1))
            (fvpair1 nil))
         (if cat1-tail
            (setf fvpair1 (car cat1-tail)))
         (if (numberp (fv-pair-value fvpair2))
            (let
               ((new-merge-cat-bindings nil))
               (setf fvpair2
                  (rename-fvpair-value fvpair2
                     cat2-bindings))
               (setf cat1-bindings
                  (nconc new-merge-cat-bindings
                     cat1-bindings))))
         (cond
            ((null fvpair1)
               (setf cat1 (nconc cat1 (ncons fvpair2))))
            ((and (numberp (fv-pair-value fvpair2))
                (numberp (fv-pair-value fvpair1)))
               (cond
                  ((eq overwrite-p 'default-checking))
                  ((eq overwrite-p 'metarule-checking)
                     (setf (car cat1-tail) fvpair2))
                  (t
                     (setf cat1-bindings
                        (merge-category-values
                           (fv-pair-value fvpair1)
                           (fv-pair-value fvpair2)
                           cat1-bindings overwrite-p)))))
            ((eq (fv-pair-value fvpair2)
                (fv-pair-value fvpair1)))
            ((and
                  (or (varp (fv-pair-value fvpair1))
                     (varp (fv-pair-value fvpair2)))
                (member overwrite-p '(alias-checking t metarule-checking)
                   :test #'eq))
               (when
                  (and
                     (member overwrite-p '(alias-checking metarule-checking)
                        :test #'eq)
                     (varp (fv-pair-value fvpair2)))
                  (merge-category-incompatible-values fvpair1 fvpair2))
               (setf (car cat1-tail) fvpair2))
            ((varp (fv-pair-value fvpair1))
               (setf cat1-bindings
                  (merge-category-variable
                     (fv-pair-value fvpair1) fvpair2
                     cat1-bindings overwrite-p))
               (unless (varp (fv-pair-value fvpair2))
                  (setf (car cat1-tail) fvpair2)))
            ((varp (fv-pair-value fvpair2))
               (setf cat1-bindings
                  (merge-category-variable
                     (fv-pair-value fvpair2) fvpair1
                     cat1-bindings overwrite-p))
               (unless (eq overwrite-p 'default-checking)
                  (setf (car cat1-tail) fvpair2)))
            ((member overwrite-p '(alias-checking metarule-checking)
                  :test #'eq)
               (merge-category-incompatible-values fvpair1 fvpair2)
               (setf (car cat1-tail) fvpair2))
            ((eq overwrite-p t)
               (setf (car cat1-tail) fvpair2)))))
   (values cat1 cat1-bindings))


(defun merge-category-incompatible-values (fvpair1 fvpair2)
   (push
      (cons 'incompatible-values
         (cons
            (cons (fv-pair-value fvpair2)
               (fv-pair-value fvpair1))
            (fv-pair-feature fvpair1)))
      grammar-errors))


(defun merge-category-tail (feature bundle)
   (cond
      ((null bundle) nil)
      ((eq (fv-pair-feature (car bundle))
          feature)
         bundle)
      (t
         (merge-category-tail feature
            (cdr bundle)))))


(defun merge-category-variable
   (var1 fvpair2 cat-bindings overwrite-p)
   (let
      ((val2 (fv-pair-value fvpair2))
         (bnd (assoc var1 variable-substitutions :test #'eq)))
      (cond
         ((null bnd)
            (push (cons var1 fvpair2)
               variable-substitutions))
         ((varp val2)
            (push (cons val2 (cdr bnd))
               variable-substitutions))
         ((and (numberp val2)
             (numberp (fv-pair-value (cdr bnd))))
            (setf cat-bindings
               (mapcar
                  #'(lambda (binding)
                       (let
                          ((structure-151
                              (copy-category-binding binding)))
                          (setf
                             (category-binding-category
                                structure-151)
                             (substitute-number-in-bundle
                                (fv-pair-value (cdr bnd))
                                val2
                                (category-binding-category
                                   structure-151)))
                          structure-151))
                  (merge-category-values
                     (fv-pair-value (cdr bnd)) val2
                     cat-bindings overwrite-p))))
         ((varp (fv-pair-value (cdr bnd)))
            (push (cons var1 fvpair2)
               variable-substitutions)
            (push
               (cons (fv-pair-value (cdr bnd))
                  fvpair2)
               variable-substitutions))))
   cat-bindings)


(defun substitute-number-in-bundle
   (var value bundle)
   (mapcar
      #'(lambda (fvpair)
           (cond
              ((equal (fv-pair-value fvpair) value)
                 (make-fv-pair
                    (fv-pair-feature fvpair) var))
              (t fvpair)))
      bundle))


(defun merge-category-values
   (cat1-no cat2-no cat-bindings overwrite-p)
   (let
      ((cat1-binding
          (f-find (the fixnum cat1-no) cat-bindings
             :key #'category-binding-number :test
             #'eql))
         (cat2-binding
            (f-find (the fixnum cat2-no) cat-bindings
               :key #'category-binding-number :test
               #'eql)))
      (multiple-value-bind (new-cat new-bindings)
         (merge-category
            (category-binding-category cat1-binding)
            cat-bindings
            (category-binding-category cat2-binding)
            cat-bindings overwrite-p)
         (mapcar
            #'(lambda (binding)
                 (if
                    (equal (category-binding-number binding)
                       cat1-no)
                    (make-category-binding :number cat1-no
                       :category new-cat :repetition
                       (category-binding-repetition
                          cat1-binding))
                    binding))
            new-bindings))))


(defun rename-fvpair-value
   (fvpair bindings)
   (let
      ((binding-no highest-binding-no)
         (b
            (f-find (the fixnum (fv-pair-value fvpair))
               bindings :key #'category-binding-number
               :test #'eql)))
      (setf highest-binding-no (1+ highest-binding-no))
      (push
         (make-category-binding :number binding-no
            :category
            (rename-category-value
               (category-binding-category b) bindings)
            :repetition
            (category-binding-repetition b))
         new-merge-cat-bindings)
      (make-fv-pair
         (fv-pair-feature fvpair)
         binding-no)))


(defun rename-category-value (cat bindings)
   (cond
      ((alias-instantiation-p cat)
         (let
            ((structure-152
                (copy-alias-instantiation cat)))
            (setf
               (alias-instantiation-bundle structure-152)
               (rename-category-value
                  (alias-instantiation-bundle structure-152)
                  bindings))
            structure-152))
      (t
         (mapcar
            #'(lambda (fvpair)
                 (cond
                    ((alias-instantiation-p fvpair)
                       (rename-category-value fvpair bindings))
                    ((numberp (fv-pair-value fvpair))
                       (rename-fvpair-value fvpair bindings))
                    (t fvpair)))
            cat))))


;;; End of file

