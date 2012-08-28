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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - SYNTAX CHECKERS
;;;
;;; Author: John Carroll
;;;
;;; This file contains hard-wired parsers for reading the
;;; various GPSG constructs.
;;;
;;; Entry points:
;;;
;;;  * (defun Varp (x) ...
;;;  * (defun Generate-variable () ...
;;;  * (defun Generate-named-variable (name) ...
;;;  * (defun Optvarp (x) ...
;;;  * (defun Generate-optvariable () ...
;;;  * (defun Feature-variable-value-pair (feature-name) ...
;;;  * (defun Parse-feature-declaration (input-list) ...
;;;  * (defun Parse-set-declaration (input-list) ...
;;;  * (defun Parse-alias-declaration (input-list) ...
;;;  * (defun Parse-category-declaration (input-list) ...
;;;  * (defun Parse-extension-declaration (input-list) ...
;;;  * (defun Parse-top-declaration (input-list) ...
;;;  * (defun Parse-idrule-declaration (input-list) ...
;;;  * (defun Parse-metarule-declaration (input-list) ...
;;;  * (defun Parse-defrule-declaration (input-list) ...
;;;  * (defun Parse-proprule-declaration (input-list) ...
;;;  * (defun Parse-lprule-declaration (input-list) ...
;;;  * (defun Parse-word-definition (input-list) ...
;;;  * (defun Is-category-valued (feature-name) ...
;;;  * (defun Idrule-name-string (name) ...
;;;
;;; All the globally accessible functions behave in roughly the
;;; same way. If they encounter a syntax error then they throw
;;; out with an ERROR, otherwise they return a list whose head
;;; is the name of the declaration just parsed. The
;;; interpretation of the tail of the list depends on the type
;;; of construct.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(progn
   (defvar *input-text nil)
   (defvar *input-comments nil)
   (defvar *current-item nil)
   (defvar new-syntax-cat-bindings nil))


(defun generate-variable nil
   (make-grammar-variable nil nil
      (setq *current-variable-name* (1+ *current-variable-name*))))


(defun generate-named-variable (name)
   (or (get name 'grammar-variable)
      (setf (get name 'grammar-variable)
         (make-grammar-variable nil nil (string name)))))


(defun optvarp (x)
   (and (varp x)
      (grammar-variable-optional-p x)))


(defun generate-optvariable nil
   (make-grammar-variable nil t
      (setq *current-variable-name* (1+ *current-variable-name*))))


;;; Each feature holds a list of feature-value pairs that can be
;;; used when that feature is given a variable value during
;;; feature propagation, defaulting or filling using category
;;; declarations. Variables must be unique within a single ID
;;; rule or word definition, but may be shared between them
;;; (indeed this is the idea, conserving storage and gensyms).
;;; List is kept on property variable-list, with the pointer to
;;; the next pair that may be used stored on property
;;; variable-pointer. Pointer is reset beteween processing of
;;; each ID rule and word.

(defun feature-variable-value-pair
   (feature-name)
   (let
      ((fvpairs
          (get feature-name 'variable-pointer)))
      (cond
         ((null (cdr fvpairs))
            (setf (cdr fvpairs)
               (ncons
                  (make-fv-pair feature-name
                     (generate-variable))))))
      (setf (get feature-name 'variable-pointer)
         (cdr
            (get feature-name 'variable-pointer)))
      (car fvpairs)))


(defun reset-variable-value-pairs nil
   (dolist (feature *features)
      (progn
         (unless (get feature 'variable-list)
            (setf (get feature 'variable-list)
               (ncons
                  (make-fv-pair feature
                     (generate-variable)))))
         (setf (get feature 'variable-pointer)
            (get feature 'variable-list)))))


;;; Cache feature-value pair structures on property list of
;;; feature for all instances where value is not a variable, a a
;;; category binding numeric index, or a pair representing a set
;;; of alternative values in a pattern. Save storage.

(defun feature-proper-value-pair
   (feature-name value)
   (let
      ((fvpair
          (f-find value
             (get feature-name 'proper-value-list) :key
             #'fv-pair-value :test #'eq)))
      (cond
         ((null fvpair)
            (setf fvpair
               (make-fv-pair feature-name
                  value))
            (unless
               (or (numberp value) (consp value)
                  (varp value))
               (push fvpair
                  (get feature-name 'proper-value-list)))))
      fvpair))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;;    <feature-declaration> ::= <feature-name>
;;;      \{ <feature-value>+ \}
;;;    <feature-name>    ::= <atomic-symbol>
;;;    <feature-value>    ::= <atomic-symbol>
;;;
;;; If the parse is successful it returns a list of the form:
;;;
;;;    ( <feature-name> <feature-value-1> ...
;;;   <feature-value-n> )
;;;
;;; otherwise it throws out with an ERROR.

(defun parse-feature-declaration
   (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (feature-name nil) (feature-values nil))
      (setf *current-item (pop *input-text))
      (setf feature-name (parse-feature-name))
      (cond
         ((eq *current-item (cat-feature-value))
            (setf feature-values (cat-feature-value)))
         (t (parse-literal '\{ "{ expected")
            (when (eq *current-item '\})
               (gde-ferror
                  "a feature must have at least one value"))
            (let* ((current-value nil))
               (loop
                  (cond
                     ((eq *current-item '\}) (return nil)))
                  (setf current-value
                     (cond
                        ((eq *current-item '\@)
                           (parse-variable-value nil))
                        (t (parse-name "feature value"))))
                  (when (eq current-value '\~)
                     (gde-ferror
                        "a feature may not have the value ~"))
                  (when (member current-value feature-values)
                     (gde-ferror "duplicate feature values "
                        current-value
                        " in feature declaration"))
                  (push current-value feature-values)
                  (allow-for-literal '\,))
               (setf feature-values
                  (nreverse feature-values)))))
      (list feature-name
         (make-feature-declaration :values
            feature-values :file
            (cond
               ((get feature-name 'feature)
                  (feature-declaration-file
                     (get feature-name 'feature)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;;    <set-declaration> ::= <set-name> \=
;;;         \{ <feature-name>+ \}
;;;    <set-name>        ::= <atomic-symbol>
;;;    <feature-name>    ::= <atomic-symbol>
;;;
;;; If the parse is successful it returns a list of the form:
;;;
;;;    ( <set-name> <feature-name-1> ... <featur@\@< )
;;;
;;; otherwise it throws out with an ERROR.

(defun parse-set-declaration (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (set-name nil) (feature-names nil))
      (setf *current-item (pop *input-text))
      (setf set-name (parse-name "set name"))
      (parse-literal '= "= expected")
      (setf feature-names (parse-set-body))
      (list set-name
         (make-set-declaration :features
            feature-names :file
            (cond
               ((get set-name 'set)
                  (set-declaration-file (get set-name 'set)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


(defun parse-set-body nil
   (parse-literal '\{ "{ expected")
   (let*
      ((current-name nil) (feature-names nil))
      (loop
         (cond
            ((eq *current-item '\}) (return nil)))
         (setf current-name (parse-feature-name))
         (when (member current-name feature-names)
            (gde-ferror "duplicate feature names "
               current-name " in set"))
         (push current-name feature-names)
         (allow-for-literal '\,))
      (nreverse feature-names)))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;;    <alias-declaration> ::= <alias-name> \= <category>
;;;
;;; If the parse is successful it returns a list of the form:
;;;
;;;    ( <alias-name> <category-record> )
;;;
;;; otherwise it throws out with an ERROR.

(defun parse-alias-declaration (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (alias-name nil) (highest-binding-no 0))
      (setf *current-item (pop *input-text))
      (setf alias-name (parse-name "alias name"))
      (parse-literal '= "= expected")
      (list alias-name
         (make-alias-declaration :cat-bindings
            ;; allow lists of alternative feature values in category
            (parse-category-binding t nil) :file
            (cond
               ((get alias-name 'alias)
                  (alias-declaration-file
                     (get alias-name 'alias)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


;;; Parse a category declaration

(defun parse-category-declaration (lexical input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (declaration-name nil) (feature-path nil)
         (cat-bindings nil) (features nil)
         (semantic-type nil) (highest-binding-no 0))
      (setf *current-item (pop *input-text))
      (setf declaration-name
         (parse-name "category declaration name"))
      (parse-literal '\: ": expected")
      (cond
         ((eq *current-item '\() (consume-item)
            (setf feature-path (parse-feature-path)))
         ((is-category-valued *current-item)
            (setf feature-path (ncons *current-item))
            (consume-item)))
      (setf cat-bindings
         (parse-category-binding t nil))
      (parse-literal '= "=> expected")
      (parse-literal '> "=> expected")
      (setf features
         (cond
            ((eq *current-item '\{) (parse-set-body))
            (t *current-item)))
      (consume-item)
      (setf semantic-type
         (cond
            ((eq *current-item '\.) nil)
            ((eq *current-item '\:) (consume-item)
               (parse-semantic-types))
            (t (parse-literal '\. ": or . expected"))))
      (list declaration-name
         (make-category-declaration :feature-path
            feature-path :cat-bindings cat-bindings
            :features features :lexical lexical
            :semantic-types semantic-type :file
            (cond
               ((get declaration-name 'category)
                  (category-declaration-file
                     (get declaration-name 'category)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


(defun parse-feature-path nil
   (let* ((res nil))
      (loop
         (setf res (cons (parse-feature-name) res))
         (cond
            ((eq *current-item '\)) (return nil)))
         (allow-for-literal '\,))
      (consume-item)
      (if res (nreverse res)
         (gde-ferror
"'()' feature path must contain at least one feature"))))


;;; Parse an extension declaration

(defun parse-extension-declaration
   (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (declaration-name 'extension)
         (features nil))
      (consume-item)
      (setf features
         (cond
            ((eq *current-item '\{) (parse-set-body))
            (t *current-item)))
      (list declaration-name
         (make-extension-declaration :features
            features :file
            (cond
               ((get declaration-name 'extension)
                  (extension-declaration-file
                     (get declaration-name 'extension)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


(defun parse-top-declaration (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (declaration-name 'top)
         (highest-binding-no 0) (categories nil))
      (consume-item)
      (loop
         (when (eq *current-item '\.)
            (return))
         (push (parse-category-binding t nil)
            categories)
         (allow-for-literal '\,))
      (list declaration-name
         (make-top-declaration :categories
            (nreverse categories) :file
            (cond
               ((get declaration-name 'top)
                  (top-declaration-file
                     (get declaration-name 'top)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;; To parse a metarule, first parse the lhs as a normal idrule
;;; to give a list of lhs binding numbers and a list of category
;;; bindings. For the rhs, parse it a category at a time trying
;;; to relate each category to a category on the lhs. If we fail
;;; to relate the category then create a New-category action for
;;; it in an action binding with a new binding number, otherwise
;;; calculate the differences between the rhs category and the
;;; lhs category it is related to, and create an action binding
;;; whose binding number is the same as the binding number o8
;;; the lhs category.
;;;
;;; Addition binding records for category values must end up
;;; before those of the feature specification which introduced
;;; them.
;;;
;;; Allow features with unspecified values or ~ on RHS -
;;; otherwise if feature on LHS could be interpreted as feature
;;; having to be deleted.

(defvar rule-lexical-p nil)


(defun parse-metarule-declaration (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (metarule-name nil) (lhs-idrule nil)
         (lhs-binding-nos nil) (rhs-binding-nos nil)
         (cat-bindings nil) (lhs-rhs-bindings nil)
         (highest-binding-no 0) (rule-lexical-p nil)
         (rhs-categories nil) (lhs-w-binding-no nil)
         (semantic-forms nil))
      (setf *current-item (pop *input-text))
      (setf metarule-name
         (non-ambiguous-rule-name
            (parse-name "metarule name")
            "metarule name"))
      (parse-literal '\: ": expected")
      (setf lhs-idrule
         (parse-idrule-body nil t t nil))
      (setf lhs-binding-nos
         (id-rule-binding-nos lhs-idrule))
      (setf cat-bindings
         (id-rule-binding-list lhs-idrule))
      (setf lhs-w-binding-no
         (parsed-metarule-w-binding cat-bindings))
      (consume-item)
      (parse-literal '= "==> expected")
      (parse-literal '= "==> expected")
      (parse-literal '> "==> expected")
      (let*
         ((arrow-skipped nil) (new-categories nil))
         (loop
            (setf new-categories
               (parse-metarule-category lhs-w-binding-no))
            (setf lhs-rhs-bindings
               (nconc
                  (compute-lhs-rhs-corresponds new-categories
                     cat-bindings lhs-binding-nos metarule-name
                     arrow-skipped)
                  lhs-rhs-bindings))
            (setf rhs-categories
               (nconc rhs-categories new-categories))
            (setf rhs-binding-nos
               (cons
                  (category-binding-number
                     (car new-categories))
                  rhs-binding-nos))
            (cond
               ((or (eq *current-item '\.)
                   (eq *current-item '\:))
                  (return nil)))
            (cond
               (arrow-skipped
                  (unless (id-rule-linear lhs-idrule)
                     (parse-literal '\, ", expected")))
               (t (parse-literal '-- "--> expected")
                  (parse-literal '> "--> expected")
                  (setf arrow-skipped t)))
            (cond
               ((or (eq *current-item '\.)
                   (eq *current-item '\:))
                  (return nil)))))
      (setf rhs-binding-nos
         (nreverse rhs-binding-nos))
      (if (eq *current-item '\:)
         (setf semantic-forms
            (parse-semantic-forms lhs-binding-nos
               rhs-binding-nos
               (list lhs-w-binding-no
                  (parsed-metarule-w-binding
                     rhs-categories))
               t)))
      (list metarule-name
         (make-meta-rule :lhs-binding-nos
            lhs-binding-nos :rhs-binding-nos
            rhs-binding-nos :cat-bindings
            (nconc cat-bindings rhs-categories)
            :lhs-rhs-corresponds lhs-rhs-bindings
            :lexical rule-lexical-p :linear
            (id-rule-linear lhs-idrule) :semantic-forms
            semantic-forms :file
            (cond
               ((get metarule-name 'metarule)
                  (meta-rule-file
                     (get metarule-name 'metarule)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


(defun parsed-metarule-w-binding
   (cat-bindings)
   (dolist (binding cat-bindings)
      (let
         ((var-9
             (and
                (eq (category-binding-repetition binding)
                   '*w*)
                (category-binding-number binding))))
         (if var-9 (return var-9)))))


(defun compute-lhs-rhs-corresponds
   (rhs-bindings lhs-bindings lhs-binding-nos
      metarule-name idrule-rhs-p)
   (let
      ((related-lhs-binding
          (relate-to-lhs (car rhs-bindings)
             lhs-bindings lhs-binding-nos metarule-name
             idrule-rhs-p)))
      (if related-lhs-binding
         (ncons
            (cons
               (category-binding-number
                  related-lhs-binding)
               (category-binding-number
                  (car rhs-bindings)))))))


(defun parse-metarule-category
   (w-category-present)
   (let
      ((bindings (parse-rhs-term nil t)))
      (let
         ((repetition
             (category-binding-repetition
                (car bindings))))
         (when
            (and (not w-category-present)
               (or (eq repetition '*w*)
                  (eq repetition '*u*)))
            (gde-ferror
"U or W category on RHS but not on LHS of metarule")))
      bindings))


;;; Check if a metarule rhs category is related to any of the
;;; categories on the metarules lhs. All comparisons are done on
;;; the un-normalised categories as entered by the user. There
;;; may be only one W-type variable, so dealing with this is
;;; easy. Otherwise, first try to relate category to
;;; corresponding side of lhs pattern, then to other side. Throw
;;; out with an error if more than one match.

(defun relate-to-lhs
   (rhs-binding lhs-bindings lhs-binding-nos
      metarule-name idrule-rhs-p)
   (let
      ((rhs-category
          (category-binding-category rhs-binding)))
      (let
         ((related-cat-bindings
             (cond
                ((eq
                    (category-binding-repetition rhs-binding)
                    '*w*)
                   (ncons
                      (f-find '*w* lhs-bindings :key
                         #'category-binding-repetition :test
                         #'eq)))
                (t
                   (relate-to-lhs1 rhs-category lhs-bindings
                      (cond
                         (idrule-rhs-p
                            (cdr lhs-binding-nos))
                         (t
                            (ncons
                               (car
                                  lhs-binding-nos)))))))))
         (cond
            ((null related-cat-bindings) nil)
            ((null (cdr related-cat-bindings))
               (car related-cat-bindings))
            (t
               (gde-ferror
"ambiguous correspondence between LHS & RHS categories in "
                  metarule-name))))))


(defun relate-to-lhs1
   (rhs-category lhs-bindings binding-nos)
   (cond
      ((alias-instantiation-p rhs-category)
         (relate-alias-names rhs-category
            lhs-bindings binding-nos))
      (t
         (relate-feature-bundle rhs-category
            lhs-bindings binding-nos))))


;;; Try and relate an alias instantiation from the rhs of a
;;; metarule to a category binding from the lhs of a metarule.
;;; To be related the lhs category must be an alias
;;; instantiation with the same alias name as the rhs
;;; instantiation. Return a list of related bindings.

(defun relate-alias-names
   (rhs-alias lhs-bindings binding-nos)
   (let
      ((rhs-alias-name
          (alias-instantiation-name rhs-alias)))
      (mapcan
         #'(lambda (binding)
              (when
                 (and
                    (member (category-binding-number binding)
                       binding-nos :test #'equal)
                    (let
                       ((category
                           (category-binding-category
                              binding)))
                       (and (alias-instantiation-p category)
                          (eq
                             (alias-instantiation-name
                                category)
                             rhs-alias-name))))
                 (ncons binding)))
         lhs-bindings)))


;;; Try and relate a feature bundle from the rhs of a metarule
;;; to a category binding from the lhs of a metarule. To be
;;; related the lhs category must be a feature bundle (not an
;;; alias instantiation) - also make sure U and W categories are
;;; excluded.

(defun relate-feature-bundle
   (rhs-bundle lhs-bindings binding-nos)
   (declare (ignore rhs-bundle))
   (mapcan
      #'(lambda (lhs-binding)
           (when
              (and
                 (member
                    (category-binding-number lhs-binding)
                    binding-nos :test #'equal)
                 (not
                    (eq
                       (category-binding-repetition
                          lhs-binding)
                       '*w*))
                 (not
                    (alias-instantiation-p
                       (category-binding-category
                          lhs-binding))))
              (ncons lhs-binding)))
      lhs-bindings))


;;; Parse a feature default rule definition.

(defun parse-defrule-declaration
   (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (rule-name nil) (lhs-idrule nil)
         (binding-nos nil) (cat-bindings nil)
         (ident-spec nil) (value nil)
         (highest-binding-no 0) (rule-lexical-p nil))
      (setf *current-item (pop *input-text))
      (setf rule-name
         (parse-name "default rule name"))
      (parse-literal '\: ": expected")
      (setf lhs-idrule
         (parse-idrule-body nil t t nil))
      (setf binding-nos
         (id-rule-binding-nos lhs-idrule))
      (setf cat-bindings
         (id-rule-binding-list lhs-idrule))
      (consume-item)
      (setf ident-spec
         (parse-proprule-ident-spec binding-nos))
      (parse-literal '= "= expected")
      (setf value
         (cond
            ((eq *current-item '\@) (consume-item) '\@)
            ((or
                (null
                   (prop-ident-spec-feature-names ident-spec))
                (and
                   (consp
                      (prop-ident-spec-feature-names
                         ident-spec))
                   (is-category-valued
                      (car
                         (prop-ident-spec-feature-names
                            ident-spec)))))
               (let
                  ((value-bindings
                      (parse-category-binding nil nil)))
                  (setf cat-bindings
                     (nconc cat-bindings value-bindings))
                  (category-binding-number
                     (car value-bindings))))
            (t (parse-name "feature value"))))
      (list rule-name
         (make-default-rule :binding-nos binding-nos
            :cat-bindings cat-bindings :category-index
            (prop-ident-spec-category-index ident-spec)
            :feature-names
            (if
               (atom
                  (prop-ident-spec-feature-names ident-spec))
               (parse-set-membership)
               (prop-ident-spec-feature-names ident-spec))
            :value value :lexical rule-lexical-p :linear
            (id-rule-linear lhs-idrule) :file
            (cond
               ((get rule-name 'defrule)
                  (default-rule-file
                     (get rule-name 'defrule)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


;;; Parse a feature propagation rule definition.

(defun parse-proprule-declaration
   (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (proprule-name nil) (lhs-idrule nil)
         (binding-nos nil) (ident-specs nil)
         (highest-binding-no 0) (rule-lexical-p nil)
         (set nil))
      (setf *current-item (pop *input-text))
      (setf proprule-name
         (parse-name "propagation rule name"))
      (parse-literal '\: ": expected")
      (setf lhs-idrule
         (parse-idrule-body nil t t nil))
      (setf binding-nos
         (id-rule-binding-nos lhs-idrule))
      (consume-item)
      (loop
         (push (list (parse-proprule-ident-spec binding-nos)) ident-specs)
         (loop
            (parse-literal '= "= or , expected")
            (push (parse-proprule-ident-spec binding-nos) (car ident-specs))
            (when (member *current-item '(\, \.)) (return nil)))
         (cond
            ((eq *current-item '\.) (return nil))
            ((eq *current-item '\,)
               (if (eq (look-ahead 1) (f-variable-name)) (return nil)
                  (consume-item)))))
      (setf set (parse-set-membership))
      (list proprule-name
         (make-prop-rule :binding-nos binding-nos
            :cat-bindings
            (id-rule-binding-list lhs-idrule)
            :ident-specs
            (mapcar
               #'(lambda (ident)
                  (mapcar
                     #'(lambda (i-s)
                        (if (atom (prop-ident-spec-feature-names i-s))
                           (make-prop-ident-spec :category-index
                              (prop-ident-spec-category-index i-s)
                              :feature-names set)
                           i-s))
                     (nreverse ident)))
               (nreverse ident-specs))
            :lexical rule-lexical-p :linear
            (id-rule-linear lhs-idrule) :file
            (cond
               ((get proprule-name 'proprule)
                  (prop-rule-file
                     (get proprule-name 'proprule)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


(defun parse-proprule-ident-spec
   (binding-nos)
   (cond
      ((symbol-to-number *current-item)
         (make-prop-ident-spec :category-index
            (parse-proprule-ident-spec1
               (parse-name "numeric category index")
               binding-nos)
            :feature-names nil))
      (t
         (let
            ((feature-name
                (parse-name "feature or set name")))
            (parse-literal '\( "( expected")
            (prog1
               (make-prop-ident-spec :category-index
                  (parse-proprule-ident-spec1
                     (parse-name "numeric category index")
                     binding-nos)
                  :feature-names
                  (cond
                     ((eq feature-name (f-variable-name))
                        feature-name)
                     (t (ncons feature-name))))
               (parse-literal '\) ") expected"))))))


(defun parse-proprule-ident-spec1
   (index binding-nos)
   (let
      ((binding-no
          (translate-parsed-category-index
             (symbol-to-number index) binding-nos))
         (cat-feature nil))
      (when (eq *current-item '\[) (consume-item)
         (setf cat-feature (parse-feature-name))
         (unless (is-category-valued cat-feature)
            (gde-ferror "feature " cat-feature
               " is not category-valued"))
         (parse-literal '\] "] expected"))
      (make-category-index :binding-no binding-no
         :cat-feature cat-feature)))


(defun translate-parsed-category-index
   (index binding-nos)
   (cond
      ((not (numberp index))
         (gde-ferror "number expected, but "
            *current-item " found instead"))
      ((nth index binding-nos))
      (t
         (gde-ferror "category index " index
            " is larger than the number of daughters"))))


(defun parse-set-membership nil
   (cond
      ((eq *current-item '\.) nil)
      (t (parse-literal '\, ", expected")
         (parse-literal (f-variable-name)
            "F variable name expected")
         (parse-literal (element-variable-name)
            "invalid set membership specification")
         (cond
            ((eq *current-item '\{)
               (prog1 (parse-set-body)
                                ;;; and read final "."
                   (consume-item)))
            (t (parse-name "set name"))))))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;;    <idrule-declaration> ::= <idrule-name> \: <idrule-body>
;;;    <idrule-name>   ::= <atomic-symbol>
;;;
;;; If the parse is successful it returns a list of the form:
;;;
;;;    ( <composite-rule-name> <idrule-record> )
;;;
;;; otherwise it throws out with an ERROR.

(defun parse-idrule-declaration
   (input-list linear-rule)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (parsed-name nil)
         (rule-type
            (if linear-rule "PS rule name"
               "ID rule name"))
         (highest-binding-no 0) (body nil)
         (semantic-forms nil))
      (setf *current-item (pop *input-text))
      (setf parsed-name
         (non-ambiguous-rule-name
            (parse-name rule-type) rule-type))
      (parse-literal '\: ": expected")
      (setf body
         (parse-idrule-body parsed-name nil nil
            linear-rule))
      (if (eq *current-item '\:)
         (setf semantic-forms
            (parse-semantic-forms
               (id-rule-binding-nos body)
               (id-rule-binding-nos body) nil t)))
      (list parsed-name
         (let
            ((structure-10 (copy-id-rule body)))
            (setf (id-rule-semantic-forms structure-10)
               semantic-forms)
            (setf (id-rule-file structure-10)
               (cond
                  ((get parsed-name 'idrule)
                     (id-rule-file (get parsed-name 'idrule)))
                  (t (get-new-construct-file))))
            (setf (id-rule-comment structure-10)
               *input-comments)
            structure-10))))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;;    <idrule-body>   ::= <lhs-term> \-\-\> <rhs-term>+
;;;    <lhs-term>    ::= <category>
;;;
;;; if it succeeds then it returns an idrule record, otherwise
;;; it throws out with an ERROR.

(defun parse-idrule-body
   (idrule-name unspec-value-allowed
      w-variable-allowed linear-rule)
   (let
      ((binding-nos nil) (bindings nil))
      (push 0 binding-nos)
      (setf bindings
         (parse-category-binding
            unspec-value-allowed nil))
      (parse-literal '-- "--> expected")
      (parse-literal '> "--> expected")
      (let* ((next-bindings nil))
         (loop
            (cond
               ((or (eq *current-item '\.)
                   (eq *current-item '\:))
                  (return nil)))
            (setf next-bindings
               (parse-rhs-term unspec-value-allowed
                  w-variable-allowed))
            (push
               (category-binding-number
                  (car next-bindings))
               binding-nos)
            (setf bindings
               (nconc bindings next-bindings))
            (cond
               ((or (eq *current-item '\.)
                   (eq *current-item '\:))
                  (return nil)))
            (cond
               ((and linear-rule (eq *current-item '\,))
                  (gde-ferror
                     ", not expected in linear rule"))
               ((eq *current-item '\,) (consume-item))
               (t (setf linear-rule t)))))
      (make-id-rule :name
         (make-top-rule-name :base
            (make-sub-rule-name :base idrule-name
               :index nil :split nil)
            :meta-names nil)
         :binding-nos (reverse binding-nos)
         :highest-binding-no highest-binding-no
         :binding-list bindings :lexical nil :linear
         linear-rule :rules-applied nil
         :semantic-forms nil :file nil :comment
         nil)))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;;    <rhs-term> ::= \( <category> \) | \( <category> \)\+ |
;;;       \( <category> \)\*
;;;
;;; if the parse is successful it returns a binding list
;;; otherwise it throws out with an ERROR.

(defun parse-rhs-term
   (unspec-value-allowed w-variable-allowed)
   (let
      ((parsed-bindings nil))
      (cond
         ((eq *current-item '\() (consume-item)
            (setf parsed-bindings
               (parse-category-binding
                  unspec-value-allowed w-variable-allowed))
            (parse-literal '\) ") expected")
            (cond
               ((eq *current-item '+) (consume-item)
                  (setf
                     (category-binding-repetition
                        (car parsed-bindings))
                     '*rep1*))
               ((eq *current-item '*) (consume-item)
                  (setf
                     (category-binding-repetition
                        (car parsed-bindings))
                     '*rep0*))
               (t
                  (setf
                     (category-binding-repetition
                        (car parsed-bindings))
                     '*opt*))))
         (t
            (setf parsed-bindings
               (parse-category-binding
                  unspec-value-allowed w-variable-allowed))))
      parsed-bindings))


;;; This function parses a top level category in a
;;; definition and returns a binding list containing the top
;;; level categories as its head, and additional bindings for
;;; any internal categories encountered within the top level
;;; category. The fluid "highest-binding" is updated to reflect
;;; the number of bindings used.

(defun parse-category-binding
   (unspec-value-allowed w-variable-allowed)
   (let
      ((new-syntax-cat-bindings nil)
         (this-binding highest-binding-no))
      (setf highest-binding-no (1+ highest-binding-no))
      (let
         ((top-binding
             (cond
                ((or (eq *current-item (w-category-name))
                    (eq *current-item (u-category-name)))
                   (unless w-variable-allowed
                      (gde-ferror
"W and U variables only allowed in rule patterns"))
                   (setf rule-lexical-p
                      (eq *current-item (w-category-name)))
                   (consume-item)
                   (make-category-binding :number this-binding
                      :category nil :repetition '*w*))
                (t
                   (make-category-binding :number this-binding
                      :category
                      (parse-category t unspec-value-allowed)
                      :repetition '*once*)))))
         (cons top-binding
            new-syntax-cat-bindings))))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;;    <category> ::= <alias-instantiation> | <feature-bundle>
;;;
;;; If the parse is successful it returns either an
;;; alias-instantiation record or a list of alias-instantiation
;;; and fv-pair records otherwise it throws out with an ERROR.
;;;
;;; If the paramater "top-level is T then this indicates that we
;;; are parsing a category at the top level of an idrule and
;;; will require any internal categories to be made into a
;;; category binding and placed on the the list "binding-list"
;;; which is a fluid maintained by "Parse-category-binding".

(defun parse-category
   (top-level unspec-value-allowed)
   (cond
      ((eq *current-item '\[)
         (parse-feature-bundle top-level
            unspec-value-allowed))
      (t
         (parse-alias-instantiation top-level
            unspec-value-allowed))))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;;    <feature-bundle> ::= \[ < <feature-specification> \, >*
;;;        ( <feature-specification> ) \]

(defun parse-feature-bundle
   (top-level unspec-value-allowed)
   (let
      ((parsed-bundle nil))
      (consume-item)
      (unless (eq *current-item '\])
         (loop
            (push
               (parse-feature-specification top-level
                  unspec-value-allowed)
               parsed-bundle)
            (cond
               ((eq *current-item '\]) (return nil)))
            (parse-literal '\, "] or , expected")))
      (consume-item)
      (nreverse parsed-bundle)))


;;; If function succeeds it returns either an
;;; alias-instantiation record or an fv-pair record, otherwise
;;; it throws out with an ERROR. If the unspec-value-allowed
;;; flag is non-NIL, feature-specifications of the form
;;; <feature-name> or ~<feature-name> are allowed.
;;; <feature-name> on its own is ambiguous with an alias name,
;;; so only allow it if the name is both the name of a feature
;;; and not one of an alias.

(defun parse-feature-specification
   (top-level unspec-value-allowed)
   (let
      ((name *current-item))
      (consume-item)
      (cond
         ((eq name '\~)
            (cond
               (unspec-value-allowed
                  (feature-proper-value-pair
                     (parse-name "feature name, found after ~")
                     '*absent*))
               (t
                  (gde-ferror
                     "~ only allowed in pattern categories"))))
         ((member *current-item '(\, \]))
            (cond
               ((and unspec-value-allowed
                   (get name 'feature)
                   (null (get name 'alias)))
                  (feature-proper-value-pair name
                     '*present*))
               (t
                  (make-alias-instantiation :name name
                     :bundle nil))))
         ((is-category-valued name)
            (make-fv-pair name
               (parse-feature-value name top-level
                  unspec-value-allowed)))
         ((eq *current-item '\[)
            (make-alias-instantiation :name name
               :bundle
               (parse-feature-bundle top-level
                  unspec-value-allowed)))
         (t
            (feature-proper-value-pair name
               (parse-feature-value name top-level
                  unspec-value-allowed))))))


;;; This function is a hardwired parser for the following BNF
;;; definitions:
;;;
;;;    <alias-instantiation> ::= <alias-name>
;;;      ( <feature-bundle> )
;;;
;;; If it succeeds it returns either alias-instantiation record,
;;; otherwise it throws out with an ERROR.

(defun parse-alias-instantiation
   (top-level unspec-value-allowed)
   (let
      ((alias-name (parse-name "alias name")))
      (cond
         ((eq *current-item '\[)
            (make-alias-instantiation :name alias-name
               :bundle
               (parse-feature-bundle top-level
                  unspec-value-allowed)))
         (t
            (make-alias-instantiation :name alias-name
               :bundle nil)))))


;;; A feature name is just an atomic symbol.

(defun parse-feature-name nil
   (parse-name "feature name"))


;;; This function is where clever things are done with category
;;; values. If the feature name whose value is being parsed
;;; takes a category value then we check to see if we are
;;; parsing at the top level of an idrule, if yes then
;;; the category value is formed into a category binding the
;;; binding number returned as the value of the function,
;;; otherwise a normal parse is made and the category record
;;; returned as the value.

(defun parse-feature-value (feature-name top-level unspec-value-allowed)
   (cond
      ((eq *current-item '\()
         (unless unspec-value-allowed
            (gde-ferror "'(...)' only allowed in pattern categories"))
         (let ((res nil))
            (consume-item)
            (loop
               (setf res
                  (cons
                     (parse-feature-value feature-name
                        top-level unspec-value-allowed)
                     res))
               (when (eq *current-item '\))
                  (consume-item)
                  (if res (return (nreverse res))
                     (gde-ferror
                        "'()' must contain at least one feature value")))
               (allow-for-literal '\,))))
      ((is-category-valued feature-name)
         (cond
            ((eq *current-item '\@)
               (parse-variable-value
                  unspec-value-allowed))
            (top-level
               (let
                  ((this-binding highest-binding-no))
                  (setf highest-binding-no (1+ highest-binding-no))
                  (push
                     (make-category-binding :number
                        this-binding :category
                        (parse-category t unspec-value-allowed)
                        :repetition '*once*)
                     new-syntax-cat-bindings)
                  this-binding))
            (t
               (parse-category nil
                  unspec-value-allowed))))
      ((eq *current-item '\[)
         (gde-ferror "feature " feature-name
            " cannot take a category value"))
      ((eq *current-item '\~)
         (consume-item)
         (if unspec-value-allowed '*absent*
            (gde-ferror
               "~ only allowed in pattern categories")))
      ((eq *current-item '\@)
         (parse-variable-value
            unspec-value-allowed))
      (t (parse-name "feature value"))))


(defun parse-variable-value
   (unspec-value-allowed) (consume-item)
   (if
      (and unspec-value-allowed
         (member *current-item '(\, \] \))))
      '*novalue*
      (generate-named-variable
         (parse-name
            "variable name, found after @"))))


;;; This function is a hard wired parser for the following BNF
;;; definitions:
;;;
;;;    <lprule-declaration> ::= <lprule-name> \: <category>
;;;     <lp-term>+
;;;    <lprule-name>   ::= <atomic-symbol>
;;;    <lp-term>    ::= \< <category>
;;;
;;; if the parse succeeds it returns a list of the form:
;;;
;;;    (<lprule-name> <lpterm-record-1> ... <lpterm-record-n>)
;;;
;;; otherwise it throws out with an ERROR.

(defun parse-lprule-declaration
   (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (lprule-name nil) (lpterms nil)
         (highest-binding-no 0))
      (setf *current-item (pop *input-text))
      (setf lprule-name
         (parse-name "LP rule name"))
      (parse-literal '\: ": expected")
      (push (parse-category-binding t nil)
         lpterms)
      (parse-literal '< "< expected")
      (loop
         (setf highest-binding-no 0)
         (push (parse-category-binding t nil)
            lpterms)
         (cond
            ((eq *current-item '\.) (return nil)))
         (parse-literal '< "< expected"))
      (list lprule-name
         (make-lp-rule :lp-terms (nreverse lpterms)
            :file
            (cond
               ((get lprule-name 'lprule)
                  (lp-rule-file (get lprule-name 'lprule)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


;;; Parse the definition of a word to go into the lexicon. The
;;; definition is a list of categories:
;;;
;;;   <word-definition> ::= [\(] <word> [\)] \: <category>+
;;;
;;; If the parse is successful it returns a list of the form:
;;;
;;;   ( <word> <word-record> )
;;;
;;; otherwise it throws out with an ERROR. Call function to
;;; parse several semantic forms, but just take the first.

(defun parse-word-definition (input-list)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (word nil) (senses nil))
      (setf *current-item (pop *input-text))
      (if (and (eq *current-item '\() (eq (look-ahead 1) '\:))
         (progn (consume-item) (setq word '\())
         (setq word
            (canonise-word (parse-name "name for a word" t))))
      (allow-for-literal '\))
      (parse-literal '\: ": expected")
      (let* ((cat-bindings nil))
         (loop
            (setf cat-bindings
               (let
                  ((highest-binding-no 0))
                  (parse-category-binding nil nil)))
            (setf senses
               (push
                  (make-word-sense :cat-bindings cat-bindings
                     :structure nil :semantic-forms
                     (if (eq *current-item '\:)
                        (parse-semantic-forms nil nil nil nil)))
                  senses))
            (cond
               ((eq *current-item '\.) (return nil)))
            (allow-for-literal '\,)))
      (list word
         (make-word-definition :senses
            (nreverse senses) :file
            (or
               (and (get word 'word)
                  (word-definition-file (get word 'word)))
               (get-new-construct-file))
            :comment *input-comments))))


;;; Parse a sequence of semantic forms. Each one may be an atom,
;;; in which case the item after will be ':', '.' or ',',
;;; otherwise a list. Always consume the terminating character.
;;; Allow special characters in formula - they will however be
;;; split from surrounding ones. Any numbers are taken to refer
;;; to rule daughters / mother, so try and translate them into
;;; category binding indices (wrt binding-nos). If there are no
;;; binding-nos then we are dealing with a word so flag any
;;; number as an error.

(defun parse-semantic-forms (bound-nos free-nos illegal-nos
      pattern-allowed-p)
   (consume-item)
   (cond
      ((member *current-item '(\, \.)) nil)
      (t (allow-for-literal '\:)
         (let
            ((form (parse-semantic-form pattern-allowed-p nil)))
            (cons
               (if (consp form)
                  (mapcar
                     #'(lambda (f)
                        (if (semantic-form-pattern-p f)
                           (translate-positions-to-bindings f nil
                              nil (append bound-nos free-nos) illegal-nos)
                           (translate-positions-to-bindings f nil
                              bound-nos free-nos illegal-nos)))
                     form)
                  (translate-positions-to-bindings form nil
                     bound-nos free-nos illegal-nos))
               (parse-semantic-forms bound-nos free-nos
                  illegal-nos pattern-allowed-p))))))


(defun parse-semantic-form (pattern-allowed-p pattern-found-p)
   ;; If pattern-allowed-p, then allowed to parse input of the form
   ;; 'n = <category-pattern>, ... <semantic-form>' as well as
   ;; just '<semantic-form>'
   (if pattern-allowed-p
      (let ((index (symbol-to-number *current-item)))
         (if (and (numberp index) (eq (look-ahead 1) '=))
            (let ((highest-binding-no 0))
               (consume-item) (consume-item)
               (cons
                  (cons index (parse-category-binding t nil))
                  (progn
                     (allow-for-literal '\,) (parse-semantic-form t t))))
            (if pattern-found-p (list (parse-semantic-form1))
               (parse-semantic-form1))))
      (parse-semantic-form1)))


(defun parse-semantic-form1 nil
   (cond
      ((eq *current-item '\() (consume-item)
         (let ((res nil))
            (loop
               (cond
                  ((eq *current-item '\)) (return nil)))
               (setf res (cons (parse-semantic-form1) res))
               (consume-item))
            (nreverse res)))
      (t
         (let ((index (symbol-to-number *current-item)))
            (if (numberp index) index *current-item)))))


;;; Parse a semantic type, of the form <e , <e ,t>> etc. Comma
;;; is optional.

(defun parse-semantic-types nil
   (cond
      ((member *current-item '(\, \.)) nil)
      (t (allow-for-literal '\:)
         (let
            ((form (parse-semantic-type)))
            (cons form (parse-semantic-types))))))


(defun parse-semantic-type nil
   (cond
      ((eq *current-item '<) (consume-item)
         (let
            ((arg (parse-semantic-type)))
            (allow-for-literal '\,)
            (prog1
               (make-complex-type :arg arg :res
                  (parse-semantic-type))
               (parse-literal '> "> expected"))))
      ((or (eq *current-item '\[)
          (eq (look-ahead 1) '\[))
         (parse-category-binding nil nil))
      (t
         (prog1
            (make-basic-type :name *current-item)
            (consume-item)))))


;;; *** Utility functions for use by all the grammar
;;; construct parsers.

;;; Check that the current item is a token that could sensibly
;;; function as a feature value, set name etc. If so, return it.
;;; It certainly is not sensible if it is a construct delimiter.

(defun parse-name (error-message &optional allow-category-symb-p)
   (cond
      ((or (null *current-item)
          (and (not allow-category-symb-p)
             (gde-category-symb-p *current-item)))
         (gde-ferror *current-item
            " is not allowed as a " error-message))
      (t
         (let
            ((last-item *current-item))
            (consume-item)
            last-item))))


;;; ID, PS and metarules are potentially ambiguous with
;;; linearised and multiple match versions if the names end with
;;; /+, /-, or /n, where n is an integer.

(defun non-ambiguous-rule-name (name rule-type)
   (let
      ((chars
          (coerce (princ-to-string name) 'list)))
      (if (cdr chars)
         (let
            ((last-two
                (nthcdr (- (length chars) 2) chars)))
            (if
               (member (concatl-string last-two)
                  '("/+" "/-" "/1" "/2" "/3" "/4" "/5" "/6"
                      "/7" "/8" "/9")
                  :test #'equal)
               (gde-ferror name
                  " is a potentially ambiguous "
                  rule-type))))
      name))


;;; Check if the current item is the specified literal and if it
;;; is then skip over it in the input stream.

(defun parse-literal
   (literal error-message)
   (cond
      ((equal literal *current-item)
         (consume-item))
      (t
         (gde-ferror error-message ", but "
            *current-item " found instead"))))


;;; If the specified literal is the current item then skip over
;;; it.

(defun allow-for-literal (literal)
   (when (eq literal *current-item)
      (consume-item)))


;;; Look ahead n items in the I/P stream. Ignore comments. If we
;;; run out of text then get another line.

(defun look-ahead (n)
   (let* ((text *input-text))
      (loop
         (unless text
            (setf *input-text
               (append *input-text
                  (setf text (get-more-text)))))
         (setf n
            (cond
               ((gde-comment-p (car text)) n)
               (t (1- n))))
         (cond
            ((zerop n) (return nil)))
         (setf text (cdr text)))
      (car text)))


;;; Move along 1 item in the input text. If at end of text then
;;; get another line.

(defun consume-item nil
   (unless *input-text
      (setf *input-text (get-more-text)))
   (setf *current-item
      (cond
         ((gde-comment-p (car *input-text))
            (setf *input-comments
               (add-input-comment (pop *input-text)
                  *input-comments))
            (consume-item))
         (t (pop *input-text)))))


(defun get-more-text nil
   (if *file-read
      (unless (peek-char nil *standard-input* nil nil)
         ;; changed from (listen *standard-input*) due to bug in KCL
         (gde-ferror "unexpected end of file"))
      (princ "> "))
   (or (get-reply) (get-more-text)))


(defun add-input-comment
   (new-comment old-comment)
   (cond
      (old-comment
         (make-gde-comment
            (gde-comment-depth old-comment)
            (nconc (gde-comment-text old-comment)
               (gde-comment-text new-comment))))
      (t new-comment)))


;;; Check if a feature is allowed to take a category as its
;;; value.

(defun is-category-valued (feature-name)
   (and (get feature-name 'feature)
      (eq
         (feature-declaration-values
            (get feature-name 'feature))
         (cat-feature-value))))


;;; Return a string that will print out an idrule name in a nice
;;; form. The name may be a root idrule name, or the name of a
;;; metaexpanded rule.

(defun idrule-name-string (rule-name)
   (cond
      ((top-rule-name-p rule-name)
         (let
            ((name (top-rule-name-base rule-name))
               (meta-names
                  (top-rule-name-meta-names rule-name)))
            (concatl-string
               (nconc (ncons (sub-rule-name-base name))
                  (when (sub-rule-name-split name)
                     (list "/" (sub-rule-name-split name)))
                  (when meta-names
                     (cons "("
                        (nconc
                           (metarule-name-list
                              (car meta-names))
                           (mapcan
                              #'(lambda (meta-name)
                                   (cons ","
                                      (metarule-name-list
                                         meta-name)))
                              (cdr meta-names))
                           (ncons ")"))))
                  (when (sub-rule-name-index name)
                     (list "/" (sub-rule-name-index name)))))))
      (t rule-name)))


(defun metarule-name-list (meta-name)
   (nconc
      (ncons (sub-rule-name-base meta-name))
      (when (sub-rule-name-index meta-name)
         (list "/" (sub-rule-name-index meta-name)))
      (when (sub-rule-name-split meta-name)
         (list "/"
            (sub-rule-name-split meta-name)))))


;;; End of file

