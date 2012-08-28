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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - PRETTY PRINTERS
;;;
;;; Author: John Carroll
;;;
;;; This file contains the pretty printers for all types of
;;; grammar constructs.
;;;
;;; Entry points:
;;;
;;;  * (defun print-feature-definition
;;;       (feature-name feature-definition identify) ...
;;;  * (defun print-set-definition
;;;       (set-name set-definition identify) ...
;;;  * (defun print-alias-definition
;;;       (alias-name alias-definition identify) ...
;;;  * (defun print-category-definition
;;;       (category-name category-definition identify) ...
;;;  * (defun print-extension-definition
;;;       (extension-name extension-definition identify) ...
;;;  * (defun print-top-definition
;;;       (top-name top-definition identify) ...
;;;  * (defun print-metarule-definition
;;;       (metarule-name metarule-definition
;;;          identify) ...
;;;  * (defun print-defrule-definition
;;;       (defrule-name defrule-definition identify) ...
;;;  * (defun print-proprule-definition
;;;       (proprule-name proprule-definition
;;;          identify) ...
;;;  * (defun print-idrule-definition
;;;       (idrule-definition linearised identify) ...
;;;  * (defun print-lprule-definition
;;;       (lprule-name lprule-definition identify) ...
;;;  * (defun print-word-definition
;;;       (word word-definition identify) ...
;;;  * (defun chars-in-category (category binding-list) ...
;;;  * (defun chars-in-realiased-category
;;;       (binding-list split &optional short-p) ...
;;;  * (defun print-category-binding (category binding-list ...
;;;
;;; All the pretty printers work in the same way. The print
;;; buffer holds the list of characters waiting to be printed.
;;; When the higher level processing of the constructs comes to
;;; a point where it would be acceptable to go onto a new line,
;;; it calls gde-pp-put-marker to record the fact. When sub-
;;; functions are called to print structures with internal
;;; structure (e.g. "pp-print-feature-bundle", the value of the
;;; variable pplevel is incremented and this results in any
;;; break records being of a higher level. When
;;; the time comes to flush the buffer, it is flushed up until
;;; the lowest valued break record (i.e. the highest
;;; level break point). All the globally accessibly functions
;;; take the parameter "identify" which is true if the function was
;;; called to write a definition out as a result of the "Write"
;;; command. If we are doing a file print then the construct
;;; type is printed out before the definition itself so that
;;; when the file is read back into the system we can tell the
;;; type of each definition in the file (e.g. SET, ALIAS,
;;; METARULE etc.).

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(progn
   (defvar lexical-rule nil)
   (defvar ppbuf
      (make-array 512
         :element-type #+(or cltl2 x3j13 ansi-cl) 'character #-(or cltl2 x3j13 ansi-cl) 'string-char
         :fill-pointer 0))
   (defvar ppfree 0)
   (defvar pplevel 0)
   (defvar pplowest 0)
   (defvar ppindex 0))


(defun print-feature-definition
   (feature-name feature-definition identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (values
            (feature-declaration-values
               feature-definition)))
      (gde-pp-start)
      (when identify (gde-pp-put-string "FEATURE "))
      (gde-pp-put-text feature-name)
      (gde-pp-comment
         (feature-declaration-comment
            feature-definition))
      (cond
         ((atom values) (gde-pp-put-string " ")
            (gde-pp-put-text (cat-feature-value)))
         (t (gde-pp-put-string "{")
            (loop
               (cond
                  ((null values) (return nil)))
               (gde-pp-put-text (pop values))
               (when values (gde-pp-put-string ", ")
                  (gde-pp-put-marker pplevel)))
            (gde-pp-put-string "}")))
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


(defun print-set-definition
   (set-name set-definition identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (features
            (set-declaration-features set-definition)))
      (gde-pp-start)
      (when identify (gde-pp-put-string "SET "))
      (gde-pp-put-text set-name)
      (gde-pp-put-string " = ")
      (gde-pp-comment
         (set-declaration-comment set-definition))
      (pp-print-set-body features)
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


(defun pp-print-set-body (features)
   (gde-pp-put-string "{")
   (loop
      (cond
         ((null features) (return nil)))
      (gde-pp-put-text (pop features))
      (when features (gde-pp-put-string ", ")
         (gde-pp-put-marker pplevel)))
   (gde-pp-put-string "}"))


(defun print-alias-definition
   (alias-name alias-definition identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0))
      (gde-pp-start)
      (when identify (gde-pp-put-string "ALIAS "))
      (gde-pp-put-text alias-name)
      (gde-pp-put-string " = ")
      (gde-pp-comment
         (alias-declaration-comment
            alias-definition))
      (pp-print-category
         (category-binding-category
            (car
               (alias-declaration-cat-bindings
                  alias-definition)))
         (alias-declaration-cat-bindings
            alias-definition))
      (gde-pp-put-string ".")
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


(defun print-category-definition
   (category-name category-definition
      identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (lexical
            (category-declaration-lexical
               category-definition))
         (feature-set
            (category-declaration-features
               category-definition)))
      (gde-pp-start)
      (when identify
         (when lexical (gde-pp-put-string "L"))
         (gde-pp-put-string "CATEGORY "))
      (gde-pp-put-text category-name)
      (gde-pp-put-string " : ")
      (gde-pp-comment
         (category-declaration-comment
            category-definition))
      (pp-print-feature-path
         (category-declaration-feature-path
            category-definition))
      (pp-print-category
         (category-binding-category
            (car
               (category-declaration-cat-bindings
                  category-definition)))
         (cdr
            (category-declaration-cat-bindings
               category-definition)))
      (gde-pp-put-marker pplevel)
      (gde-pp-put-string " => ")
      (let
         ((pplevel (1+ pplevel)))
         (gde-pp-put-marker pplevel)
         (pp-print-set-value feature-set)
         (pp-print-semantic-types
            (category-declaration-semantic-types
               category-definition)))
      (gde-pp-put-string ".")
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


(defun pp-print-feature-path (path)
   (when path (gde-pp-put-string "(")
      (gde-pp-put-text (car path))
      (dolist (name (cdr path))
         (gde-pp-put-string ", ")
         (gde-pp-put-text name))
      (gde-pp-put-string ") ")))


(defun pp-print-set-value (feature-set)
   (if
      (or (null feature-set) (consp feature-set))
      (pp-print-set-body feature-set)
      (gde-pp-put-text feature-set)))


(defun print-extension-definition
   (extension-name extension-definition
      identify)
   (declare (ignore extension-name))
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (feature-set
            (extension-declaration-features
               extension-definition)))
      (gde-pp-start)
      (if identify (gde-pp-put-string "EXTENSION ") (gde-pp-put-string "   "))
      (gde-pp-comment
         (extension-declaration-comment
            extension-definition))
      (pp-print-set-value feature-set)
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


(defun print-top-definition (top-name top-definition identify)
   (declare (ignore top-name))
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (categories
            (top-declaration-categories top-definition)))
      (gde-pp-start)
      (when identify (gde-pp-put-string "TOP "))
      (gde-pp-comment
         (top-declaration-comment top-definition))
      (loop
         (unless categories (return))
         (let ((bindings (pop categories)))
            (pp-print-category
               (category-binding-category (car bindings))
               bindings)
            (when categories
               (gde-pp-put-string ", ")
               (gde-pp-put-marker pplevel))))
      (gde-pp-put-string ".")
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


;;; Print out a metarule. Reduces to printing 2 ID rules

(defun print-metarule-definition
   (metarule-name metarule-definition
      identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (lexical-rule
            (meta-rule-lexical metarule-definition))
         (lhs-nos
            (meta-rule-lhs-binding-nos metarule-definition))
         (rhs-nos
            (meta-rule-rhs-binding-nos metarule-definition)))
      (gde-pp-start)
      (when identify (gde-pp-put-string "METARULE "))
      (gde-pp-put-text metarule-name)
      (gde-pp-put-string " : ")
      (gde-pp-comment
         (meta-rule-comment metarule-definition))
      (pp-print-idrule-body
         (meta-rule-lhs-binding-nos
            metarule-definition)
         (meta-rule-cat-bindings
            metarule-definition)
         (meta-rule-linear metarule-definition))
      (gde-pp-put-string ". ")
      (gde-pp-put-marker pplevel)
      (gde-pp-put-string "==> ")
      (let
         ((pplevel (1+ pplevel)))
         (gde-pp-put-marker pplevel)
         (pp-print-idrule-body
            (meta-rule-rhs-binding-nos
               metarule-definition)
            (meta-rule-cat-bindings
               metarule-definition)
            (meta-rule-linear metarule-definition)))
      (pp-print-semantic-forms
         (mapcar
            #'(lambda (form)
               (if (consp form)
                  (mapcar
                     #'(lambda (f)
                        (if (semantic-form-pattern-p f)
                           (translate-bindings-to-positions f nil
                              nil (append lhs-nos rhs-nos))
                           (translate-bindings-to-positions f nil
                              lhs-nos rhs-nos)))
                     form)
                  (translate-bindings-to-positions form nil
                     lhs-nos rhs-nos)))
            (meta-rule-semantic-forms metarule-definition)))
      (gde-pp-put-string ".")
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


;;; Print out a feature default rule definition.

(defun print-defrule-definition
   (defrule-name defrule-definition identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (lexical-rule
            (default-rule-lexical defrule-definition)))
      (gde-pp-start)
      (when identify (gde-pp-put-string "DEFRULE "))
      (gde-pp-put-text defrule-name)
      (gde-pp-put-string " : ")
      (gde-pp-comment
         (default-rule-comment defrule-definition))
      (pp-print-idrule-body
         (default-rule-binding-nos
            defrule-definition)
         (default-rule-cat-bindings
            defrule-definition)
         (default-rule-linear defrule-definition))
      (gde-pp-put-string ". ")
      (gde-pp-put-marker pplevel)
      (let ((pplevel (1+ pplevel)))
         (let
            ((f-variable-set
                (pp-print-rule-identity
                   (default-rule-category-index
                      defrule-definition)
                   (default-rule-feature-names
                      defrule-definition)
                   t
                   (default-rule-binding-nos
                      defrule-definition)
                   nil)))
            (let
               ((feature-set-value
                   (default-rule-value defrule-definition)))
               (if (numberp feature-set-value)
                  (pp-print-category
                     (category-binding-category
                        (f-find (the fixnum feature-set-value)
                           (default-rule-cat-bindings
                              defrule-definition)
                           :key #'category-binding-number :test
                           #'eql))
                     (default-rule-cat-bindings
                        defrule-definition))
                  (gde-pp-put-text feature-set-value)))
            (when f-variable-set
               (pp-print-set-member-spec f-variable-set)))
         (gde-pp-put-string ".")
         (gde-pp-put-marker 0)
         (gde-pp-flush))))


(defun pp-print-rule-identity
   (cat-index feature-names more binding-nos
      f-variable-set)
   (pp-print-feature-identity binding-nos
      cat-index
      (cond
         ((null feature-names) nil)
         ((or (atom feature-names)
             (cdr feature-names))
            (setf f-variable-set feature-names)
            (f-variable-name))
         (t (car feature-names))))
   (when more (gde-pp-put-string " = ")
      (gde-pp-put-marker (1+ pplevel)))
   f-variable-set)


;;; Print out a feature propagation rule definition.

(defun print-proprule-definition
   (proprule-name proprule-definition
      identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (lexical-rule
            (prop-rule-lexical proprule-definition)))
      (gde-pp-start)
      (when identify (gde-pp-put-string "PROPRULE "))
      (gde-pp-put-text proprule-name)
      (gde-pp-put-string " : ")
      (gde-pp-comment
         (prop-rule-comment proprule-definition))
      (pp-print-idrule-body
         (prop-rule-binding-nos proprule-definition)
         (prop-rule-cat-bindings
            proprule-definition)
         (prop-rule-linear proprule-definition))
      (gde-pp-put-string ". ")
      (gde-pp-put-marker pplevel)
      (let ((pplevel (1+ pplevel)))
         (pp-print-proprule-identities
            (prop-rule-binding-nos proprule-definition)
            (prop-rule-ident-specs
               proprule-definition)))
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


(defun pp-print-proprule-identities (binding-nos ident-specs)
   (let ((f-variable-set nil))
      (mapl
         #'(lambda (i-tail)
            (mapl
               #'(lambda (e-tail)
                  (let ((spec (car e-tail)))
                     (setf f-variable-set
                        (pp-print-rule-identity
                           (prop-ident-spec-category-index spec)
                           (prop-ident-spec-feature-names spec)
                           (cdr e-tail) binding-nos f-variable-set))))
               (car i-tail))
            (when (cdr i-tail) (gde-pp-put-string ", ")))
         ident-specs)
      (when f-variable-set
         (pp-print-set-member-spec f-variable-set))
      (gde-pp-put-string ".")))


(defun pp-print-set-member-spec (feature-set)
   (gde-pp-put-string ", ")
   (gde-pp-put-marker pplevel)
   (let ((pplevel (1+ pplevel)))
      (gde-pp-put-text (f-variable-name))
      (gde-pp-put-string " ")
      (gde-pp-put-marker pplevel)
      (gde-pp-put-text (element-variable-name))
      (gde-pp-put-string " ")
      (let ((pplevel (1+ pplevel)))
         (gde-pp-put-marker pplevel)
         (pp-print-set-value feature-set))))


;;; Print out the F(n) or simple value term in the second part
;;; of a feature propagation rule definition. The binding no
;;; must be converted into an index over the categories actually
;;; printed out on the lhs, rather than the internal category
;;; binding number.

(defun pp-print-feature-identity
   (binding-nos cat-index term-name)
   (when term-name (gde-pp-put-text term-name)
      (gde-pp-put-string "("))
   (gde-pp-put-text
      (translate-index-for-printing
         (category-index-binding-no cat-index)
         binding-nos))
   (when
      (category-index-cat-feature cat-index)
      (gde-pp-put-string "[")
      (gde-pp-put-text
         (category-index-cat-feature cat-index))
      (gde-pp-put-string "]"))
   (if term-name (gde-pp-put-string ")")))


(defun translate-index-for-printing
   (n binding-nos)
   (- (list-length binding-nos)
      (list-length (member n binding-nos))))


;;; Make a list of match bindings which relate each category
;;; number in the list to itself. This is passed to the function
;;; "Instantiate-idrule" so that it can generate the rhs id rule
;;; of a metarule from the lhs category bindings and the action
;;; list.

(defun dummy-match-bindings (binding-nos)
   (mapcar
      #'(lambda (number)
           (make-match-binding :pattern-binding-no
              number :matched-binding-nos (list number)))
      binding-nos))


;;; Pretty print an idrule definition.

(defun print-idrule-definition
   (idrule-definition identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (idrule-name
            (idrule-name-string
               (id-rule-name idrule-definition)))
         (binding-nos
            (id-rule-binding-nos idrule-definition)))
      (gde-pp-start)
      (when identify
         (if (id-rule-linear idrule-definition)
            (gde-pp-put-string "PSRULE ")
            (gde-pp-put-string "IDRULE ")))
      (gde-pp-put-text idrule-name)
      (gde-pp-put-string " : ")
      (gde-pp-comment
         (id-rule-comment idrule-definition))
      (pp-print-idrule-body binding-nos
         (id-rule-binding-list idrule-definition)
         (id-rule-linear idrule-definition))
      (pp-print-semantic-forms
         (mapcar
            #'(lambda (form)
                 (translate-bindings-to-positions form nil
                    binding-nos binding-nos))
            (id-rule-semantic-forms idrule-definition)))
      (gde-pp-put-string ".")
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


;;; Pretty print an idrule body. This function takes 3
;;; parameters, a list of binding numbers, a list of category
;;; bindings, and a flag to say whether the idrule is
;;; linearised. The category bindings will have been taken
;;; directly from the rule if we are printing an idrule or the
;;; lhs of a metarule. In the case of the rhs of a metarule, the
;;; bindings will have been calculated from the lhs bindings and
;;; the rhs actions.

(defun pp-print-idrule-body
   (binding-nos binding-list linear)
   (pp-print-id-term (car binding-nos)
      binding-list)
   (gde-pp-put-string " --> ")
   (gde-pp-put-marker pplevel)
   (let* ((nos (cdr binding-nos)))
      (loop
         (cond
            ((null nos) (return nil)))
         (pp-print-id-term (pop nos) binding-list)
         (when nos
            (cond
               (linear (gde-pp-put-string " "))
               (t (gde-pp-put-string ", ")))
            (gde-pp-put-marker pplevel)))))



;;; Pretty print an rhs term for either an ID rule or a Meta
;;; rule. If a *W* is found then it must be a metarule, so check
;;; the fluid lexical-metarule to see which spanning variable to
;;; print.

(defun pp-print-id-term
   (binding-no binding-list)
   (let
      ((binding
          (f-find (the fixnum binding-no)
             binding-list :key #'category-binding-number
             :test #'eql)))
      (cond
         (binding
            (let
               ((term-type
                   (category-binding-repetition binding)))
               (unless (member term-type '(*once* *w*))
                  (gde-pp-put-string "( "))
               (unless (eq term-type '*w*)
                  (pp-print-category
                     (category-binding-category binding)
                     binding-list))
               (cond
                  ((eq term-type '*w*)
                     (gde-pp-put-text
                        (cond
                           (lexical-rule (w-category-name))
                           (t (u-category-name)))))
                  ((eq term-type '*opt*)
                     (gde-pp-put-string " )"))
                  ((eq term-type '*rep1*)
                     (gde-pp-put-string " )+"))
                  ((eq term-type '*rep0*)
                     (gde-pp-put-string " )*")))))
         (t (gde-pp-put-string "[]")))))


(defun pp-print-category (category binding-list)
   (cond
      (category
         (cond
            ((numberp category)
               (pp-print-category
                  (category-binding-category
                     (f-find (the fixnum category) binding-list
                        :key #'category-binding-number :test
                        #'eql))
                  binding-list))
            ((alias-instantiation-p category)
               (pp-print-alias-instantiation category
                  binding-list))
            (t
               (pp-print-feature-bundle category
                  binding-list))))
      (t (gde-pp-put-string "[]"))))


(defun pp-print-feature-bundle (bundle binding-list)
   (gde-pp-put-string "[")
   (let* ((feature-spec nil) (pplevel (1+ pplevel)))
      (loop
         (cond
            ((null bundle) (return nil)))
         (setf feature-spec (pop bundle))
         (pp-print-feature-specification
            feature-spec binding-list)
         (when bundle (gde-pp-put-string ", ")
            (gde-pp-put-marker pplevel))))
   (gde-pp-put-string "]"))


(defun pp-print-feature-specification
   (feature-specification binding-list)
   (cond
      ((alias-instantiation-p
          feature-specification)
         (pp-print-alias-instantiation
            feature-specification binding-list))
      (t
         (let
            ((feature
                (fv-pair-feature feature-specification))
               (value
                  (fv-pair-value feature-specification)))
            (cond
               ((eq value '*present*)
                  (gde-pp-put-text feature))
               ((eq value '*absent*) (gde-pp-put-string "~")
                  (gde-pp-put-text feature))
               (t (gde-pp-put-text feature)
                  (gde-pp-put-string " ")
                  (let ((pplevel (1+ pplevel)))
                     (gde-pp-put-marker pplevel)
                     (pp-print-feature-value value
                        binding-list))))))))


(defun pp-print-feature-value
   (value binding-list)
   (cond
      ((consp value)
         (let ((pplevel (1+ pplevel)))
            (gde-pp-put-string "(")
            (pp-print-feature-value (car value)
               binding-list)
            (dolist (val (cdr value))
               (gde-pp-put-string ", ")
               (gde-pp-put-marker pplevel)
               (pp-print-feature-value val binding-list))
            (gde-pp-put-string ")")))
      ((numberp value)
         (pp-print-category value binding-list))
      ((eq value '*absent*)
         (gde-pp-put-string "~"))
      ((eq value '*novalue*)
         (gde-pp-put-string "@"))
      ((varp value)
         (gde-pp-put-string "@")
         (gde-pp-put-text (grammar-variable-name value)))
      (t (gde-pp-put-text value))))


(defun pp-print-alias-instantiation
   (category binding-list)
   (gde-pp-put-text
      (alias-instantiation-name category))
   (let
      ((bundle
          (alias-instantiation-bundle category)))
      (when bundle
         (pp-print-feature-bundle bundle
            binding-list))))


;;; Print the definition of an LP rule.

(defun print-lprule-definition
   (lprule-name lprule-definition identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (lpterms
            (lp-rule-lp-terms lprule-definition)))
      (gde-pp-start)
      (when identify (gde-pp-put-string "LPRULE "))
      (gde-pp-put-text lprule-name)
      (gde-pp-put-string " : ")
      (gde-pp-comment
         (lp-rule-comment lprule-definition))
      (pp-print-category
         (category-binding-category (caar lpterms))
         (cdar lpterms))
      (loop
         (setf lpterms (cdr lpterms))
         (cond
            ((null lpterms) (return nil)))
         (when lpterms (gde-pp-put-string " < ")
            (gde-pp-put-marker pplevel))
         (pp-print-category
            (category-binding-category (caar lpterms))
            (cdar lpterms)))
      (gde-pp-put-string ".")
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


;;; Print the definition of a word.

(defun print-word-definition
   (word definition identify)
   (let
      ((ppfree (gde-linelength))
         (pplevel 1) (pplowest 1) (ppindex 0)
         (senses
            (word-definition-senses definition))
         (paren-p
            (null (word-definition-file definition))))
      (gde-pp-start)
      (when identify (gde-pp-put-string "WORD "))
      (cond
         (paren-p (gde-pp-put-string "(")
            (gde-pp-put-text word) (gde-pp-put-string ") : "))
         (t (gde-pp-put-text word) (gde-pp-put-string " : ")))
      (gde-pp-comment
         (word-definition-comment definition))
      (loop
         (cond
            ((null senses) (return nil)))
         (pp-print-category
            (category-binding-category
               (car
                  (word-sense-cat-bindings
                     (car senses))))
            (word-sense-cat-bindings
               (car senses)))
         (pp-print-semantic-forms
            (word-sense-semantic-forms (car senses)))
         (setf senses (cdr senses))
         (when senses (gde-pp-put-string ", ")
            (gde-pp-put-marker pplevel)))
      (gde-pp-put-string ".")
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


;;; Print a set of semantic formula.

(defun pp-print-semantic-forms (forms)
   (dolist (form forms)
      (gde-pp-put-string " : ")
      (gde-pp-put-marker pplevel)
      (let ((pplevel (1+ pplevel)))
         (cond
            ((and (consp form) (semantic-form-pattern-p (car form)))
               (pp-print-semantic-pattern (car form))
               (mapc
                  #'(lambda (sub)
                     (gde-pp-put-string ", ")
                     (gde-pp-put-marker pplevel)
                     (if (semantic-form-pattern-p sub)
                        (pp-print-semantic-pattern sub)
                        (pp-print-semantic-form1 sub)))
                  (cdr form)))
            (t (pp-print-semantic-form1 form))))))


(defun pp-print-semantic-pattern (pattern)
   (let ((pplevel (1+ pplevel)))
      (gde-pp-put-text (car pattern))
      (gde-pp-put-string " = ")
      (let ((pplevel (1+ pplevel)))
         (pp-print-category
            (category-binding-category (cadr pattern))
            (cddr pattern)))))
  

(defun pp-print-semantic-form1 (form)
   (cond
      ((consp form)
         (let ((pplevel (1+ pplevel)))
            (gde-pp-put-string "(")
            (loop
               (cond
                  ((null form) (return nil)))
               (pp-print-semantic-form1 (pop form))
               (when form (gde-pp-put-string " ")
                  (gde-pp-put-marker pplevel)))
            (gde-pp-put-string ")")))
      (t (gde-pp-put-text form))))


;;; Print a semantic type.

(defun pp-print-semantic-types (types)
   (dolist (type types)
      (gde-pp-put-string " : ")
      (gde-pp-put-marker pplevel)
      (pp-print-semantic-type type)))


(defun pp-print-semantic-type (type)
   (cond
      ((basic-type-p type)
         (gde-pp-put-text (basic-type-name type)))
      ((complex-type-p type)
         (gde-pp-put-string "<")
         (let
            ((pplevel (1+ pplevel)))
            (pp-print-semantic-type
               (complex-type-arg type))
            (gde-pp-put-string ", ")
            (gde-pp-put-marker pplevel)
            (pp-print-semantic-type
               (complex-type-res type)))
         (gde-pp-put-string ">"))
      (t
         (pp-print-category
            (category-binding-category (car type))
            (cdr type)))))


;;; Print out a comment inside a rule if there is one, indented
;;; to the current column position, or 3 if we are already past
;;; the maximum linelength. Force rejustification of the comment
;;; to extend to the end of the line.

(defun gde-pp-comment (rule-comment)
   (cond
      (rule-comment
         (let*
            ((lmar (fill-pointer ppbuf))
               (length (+ ppfree lmar))
               (*linelength* ppfree)
               (comment-depth
                  (gde-comment-depth rule-comment)))
            (when (> lmar ppfree)
               (gde-pp-flush-line lmar)
               (setf (fill-pointer ppbuf) 0)
               (setq lmar 3)
               (setf *linelength* (- length 3)))
            (gde-pp-comment-chars comment-depth)
            (dolist
               (str
                  (gde-comment-text
                     (fill-gde-comment rule-comment)))
               (cond
                  ((equal str *eol-string)
                     (gde-pp-flush-line (fill-pointer ppbuf))
                     (setf (fill-pointer ppbuf) 0)
                     (dotimes (i lmar)
                        (gde-pp-put-string " "))
                     (gde-pp-comment-chars comment-depth))
                  (t
                     (gde-pp-put-string " ")
                     (gde-pp-put-string str))))
            (setq ppindex (fill-pointer ppbuf))
            (setq ppfree (- length ppindex))
            (setq pplowest pplevel)
            (gde-pp-flush)))
      (t
         (gde-pp-put-marker pplevel))))


(defun gde-pp-comment-chars (depth)
   (gde-pp-put-string
      (make-string depth :initial-element *grammar-comment-char)))


;;; Put a symbol, string or integer representing text into the pretty
;;; print buffer. Some chars in it may need escaping. If the buffer
;;; fills up then flush it.

(defun gde-pp-put-text (text)
   (let*
      ((text-string (atom-to-string text))
         (len (length (the string text-string)))
         (may-escape-p (> len 1)))
      (dotimes (ind len)
         (let ((char (char (the string text-string) ind)))
            (when
               (or (char= char *grammar-comment-char)
                  (and may-escape-p (gde-endword-p char nil)))
               ;; always escape comment char, but only escape others if
               ;; token is more than 1 char long
               (when (<= ppfree 0) (gde-pp-flush))
               (vector-push #\\ ppbuf)
               (decf ppfree))
            (when (<= ppfree 0) (gde-pp-flush))
            (vector-push char ppbuf)
            (decf ppfree)))))


;;; Put a string into the pretty print buffer. If the buffer fills up
;;; then flush it.

(defun gde-pp-put-string (text-string)
   (dotimes (ind (length text-string))
      (when (<= ppfree 0) (gde-pp-flush))
      (vector-push (char text-string ind) ppbuf)
      (decf ppfree)))


;;; If print level is less than it has been before than mark
;;; current point in print buffer for a possible line break.

(defun gde-pp-put-marker (level)
   (when (<= level pplowest)
      (setq pplowest level)
      (setq ppindex (fill-pointer ppbuf))))


;;; Flush the pretty print buffer.

(defun gde-pp-flush nil
   (gde-pp-flush-line ppindex)
   (let
      ((lmar (* pplowest 3))
       (end (fill-pointer ppbuf)))
      (if (>= lmar ppindex)
         (let ((temp (subseq ppbuf ppindex end)))
            (dotimes (i (- end ppindex))
               (setf (char ppbuf (+ i lmar))
                  (char temp i))))
         (dotimes (i (- end ppindex))
            (setf (char ppbuf (+ i lmar))
               (char ppbuf (+ i ppindex)))))
      (dotimes (i lmar)
         (setf (char ppbuf i) #\space))
      (setf (fill-pointer ppbuf) (+ lmar (- end ppindex)))
      (setq ppfree (- ppfree lmar))
      (setq ppindex
         (1+
            (or (position #\space ppbuf :start lmar :from-end t)
               (1- lmar))))
      (setq pplowest pplevel)))


(defun gde-pp-flush-line (limit)
   (if output-to-buffer
      (setq definition-output-buffer
         (nconc definition-output-buffer
            (coerce (subseq ppbuf 0 limit) 'list)))
      (write-line ppbuf *standard-output* :end limit))
   (setq ppfree (+ ppfree limit)))


;;; Setup before printing

(defun gde-pp-start nil
   (setf (fill-pointer ppbuf) 0))


;;; Return a list of characters that will print out as the
;;; category given as argument.

(defun chars-in-category
   (category binding-list)
   (let
      ((ppfree 512)
         (pplevel 0) (pplowest 0) (ppindex 0)
         (output-to-buffer t)
         (definition-output-buffer nil))
      (gde-pp-start)
      (pp-print-category category binding-list)
      (gde-pp-put-marker 0)
      (gde-pp-flush)
      definition-output-buffer))


;;; Return a list of the characters in a category. If the second
;;; arg is true, then split the characters in half at a blank
;;; into two lists, so the category may be printed out on two
;;; lines. If third, optional, argument is true, then just return
;;; top level alias for category or 'X' if none.

(defun chars-in-realiased-category (binding-list split &optional short-p)
   (let
      ((realiased-bindings
          (realias-category-specification binding-list)))
      (let*
         ((cat
               (category-binding-category
                  (car realiased-bindings)))
          (chars
             (cond
                ((not short-p)
                   (chars-in-category cat
                      realiased-bindings))
                ((alias-instantiation-p cat)
                   (coerce
                      (atom-to-string
                         (alias-instantiation-name cat))
                      'list))
                (t (list #\X)))))
         (cond
            (split
               (let
                  ((category-tail
                      (member #\space
                         (nthcdr
                            (max
                               (floor (list-length chars) 2)
                               10)
                            chars))))
                  (cons (ldiff chars category-tail)
                     category-tail)))
            (t chars)))))


;;; Print out a just a single category

(defun print-category-binding
   (category binding-list start-posn)
   (let
      ((ppfree (gde-linelength))
         (pplevel (ceiling start-posn 3))
         (pplowest 100) (ppindex 0))
      (gde-pp-start)
      (dotimes (i start-posn)
         (gde-pp-put-string " "))
      (pp-print-category category binding-list)
      (gde-pp-put-marker 0)
      (gde-pp-flush)))


(defun semantic-type-string (type)
   (let
      ((ppfree 512)
         (pplevel 0) (pplowest 0) (ppindex 0)
         (output-to-buffer t)
         (definition-output-buffer nil))
      (gde-pp-start)
      (pp-print-semantic-type type)
      (gde-pp-put-marker 0)
      (gde-pp-flush)
      (concatl-string definition-output-buffer)))


;;; End of file


