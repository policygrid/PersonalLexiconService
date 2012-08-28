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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - FINDING DEFINITIONS
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code to find grammar definitions
;;; whose names match a given pattern. Includes searching for a
;;; given feature, set, or alias name in all definitions in the
;;; grammar. Used by feature, set, alias deletion consistency
;;; checking, 'forget' command consistency checking, and by '='
;;; patterns in 'view', 'names' and 'delete' commands.
;;;
;;; Entry points:
;;;
;;;  * (defun Get-items (item-list search-pattern) ...
;;;  * (defun Definitions-containing-feature (feature-name ...
;;;  * (defun Definitions-containing-set (set-name) ...
;;;  * (defun Definitions-containing-alias (alias-name) ...

;;; Search for a set of items in the given list. The wildcards
;;; '*' and '?' are supported, respectively matching zero or
;;; more or precisely one character in the subject string being
;;; matched. The character '=' preceeding the pattern means look
;;; for items containing the atom following (which may not be
;;; wildcarded). The 'altered' option is really another kind of
;;; pattern and is processed here as well.
;;;
;;; The input search pattern is of the form produced by
;;; Get-reply; the function returns either:
;;;
;;; No parentheses in pattern:    (item-1 ... item-n)
;;;
;;; Otherwise:   (compound-idrule-name-1 ...)
;;;
;;; Ignore any further pattern specifications after the ')'
;;; which finishes this pattern, since items returned will be
;;; complex ID rule names, not simple symbols.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defun get-items (item-list search-pattern)
   (cond
      ((null search-pattern) item-list)
      (t
         (let
            ((pattern-tail (member '& search-pattern :test #'eq)))
            (get-items
               (cond
                  ((eq pattern-tail search-pattern)
                     item-list)
                  (t
                     (get-single-pattern-items item-list
                        (ldiff search-pattern pattern-tail))))
               (and pattern-tail
                  (cdr pattern-tail)))))))


(defun get-single-pattern-items
   (item-list search-pattern)
   (cond
      ((equal search-pattern '(*)) item-list)
      ((uncased-eq-symbols
          (car search-pattern) 'applied)
         (if (cdr search-pattern)
            (get-applied-idrules item-list
               (cdr search-pattern))
            (gde-ferror
"'applied' option must be followed by a rule pattern")))
      ((uncased-eq-symbols
          (car search-pattern) 'altered)
         (mapcan
            #'(lambda (item)
                 (when
                    (and (symbolp item) (get item 'altered))
                    (ncons item)))
            item-list))
      ((and (null (cdr search-pattern))
          (not (eq (car search-pattern) '?)))
         (when
            (member (car search-pattern)
               item-list)
            (ncons (car search-pattern))))
      ((eq (car search-pattern) '=)
         (get-items-containing-names item-list
            (get-single-pattern-items *features
               (cdr search-pattern))
            (get-single-pattern-items *sets
               (cdr search-pattern))
            (get-single-pattern-items *aliases
               (cdr search-pattern))))
      ((member '\( search-pattern :test #'eq)
         (get-expanded-idrules item-list
            search-pattern))
      (t
         (get-multiple-items item-list
            (mapcan
               #'(lambda (x)
                    (coerce (princ-to-string x) 'list))
               search-pattern)))))


(defun get-multiple-items
   (item-list x-pattern)
   (mapcan
      #'(lambda (item)
           (when
              (match-item-to-pattern item x-pattern)
              (ncons item)))
      item-list))


;;; Returns true if an identifier matches a pattern consisting
;;; of a list of characters, optionally containing the wildcard
;;; characters "*" and "?".

(defun match-item-to-pattern
   (item x-pattern)
   (cond
      ((and (char= (car x-pattern) #\*)
          (null (cdr x-pattern)))
         item)
      (t
         (match-item-to-pattern1
            (when item
               (coerce (princ-to-string item) 'list))
            x-pattern))))


(defun match-item-to-pattern1
   (x-item x-pattern)
   (cond
      ((null x-item)
         (or (null x-pattern)
            (and (char= (car x-pattern) #\*)
               (null (cdr x-pattern)))))
      ((null x-pattern) nil)
      ((or
          (char= (car x-item)
             (car x-pattern))
          (char= (car x-pattern) #\?))
         (match-item-to-pattern1 (cdr x-item)
            (cdr x-pattern)))
      ((and (char= (car x-pattern) #\*)
          (cdr x-pattern))
         (or
            (match-item-to-pattern1 x-item
               (cdr x-pattern))
            (match-item-to-pattern1 (cdr x-item)
               x-pattern)))
      ((char= (car x-pattern) #\*)
         (match-item-to-pattern1 (cdr x-item)
            x-pattern))
      (t nil)))


;;; Return the names of those ID rules whose names are in
;;; item-list that have been acted upon (propagated, defaulted,
;;; or category-filled) by a rule matching the
;;; applied-rule-pattern argument. If a name is a Top-rule-name
;;; record, then the base rule will have already been compiled.
;;; If name is a symbol, the rule might still have some cached
;;; compilation results - if so, use them.

(defun get-applied-idrules
   (item-list applied-rule-pattern)
   (mapcan
      #'(lambda (name)
           (when
              (cond
                 ((top-rule-name-p name)
                    (get-items
                       (id-rule-rules-applied
                          (f-find name
                             (compile-idrule
                                (sub-rule-name-base
                                   (top-rule-name-base name)))
                             :key #'id-rule-name :test #'eq))
                       applied-rule-pattern))
                 ((and (symbolp name)
                     (id-rule-p (get name 'idrule)))
                    (member-if
                       #'(lambda (compiled-idrule)
                            (and
                               (null
                                  (top-rule-name-meta-names
                                     (id-rule-name
                                        compiled-idrule)))
                               (get-items
                                  (id-rule-rules-applied
                                     compiled-idrule)
                                  applied-rule-pattern)))
                       (compile-idrule name))))
              (ncons name)))
      item-list))


;;; Return all items which contain name as either a feature
;;; name, a set name, or an alias. Operation is not really
;;; meaningful on items which are themselves names of features
;;; or sets, but ignore this - the names may also represent
;;; other types in construct as well as the feature, set etc. Do
;;; not check if e.g. name is a feature before looking for it as
;;; a feature, since it may not have been declared yet.

(defun get-items-containing-names
   (item-list features sets aliases)
   (let
      ((containers
          (nconc
             (mapcan
                #'(lambda (name)
                     (definitions-containing-feature name t))
                features)
             (mapcan
                #'(lambda (name)
                     (definitions-containing-set name))
                sets)
             (mapcan
                #'(lambda (name)
                     (definitions-containing-alias name))
                aliases))))
      (mapcan
         #'(lambda (container)
              (let
                 ((var-21
                     (cond
                        ((member container item-list)
                           container)
                        ((and (top-rule-name-p container)
                            (null
                               (top-rule-name-meta-names
                                  container))
                            (member
                               (sub-rule-name-base
                                  (top-rule-name-base
                                     container))
                               item-list))
                           (sub-rule-name-base
                              (top-rule-name-base
                                 container))))))
                 (if var-21 (ncons var-21))))
         containers)))


;;; Search for ID rules matching a pattern which includes a
;;; metarule specification. The metarule expanded names returned
;;; must include those resulting from linear metarules, so have
;;; to compile the base ID rule to get all the names. Names
;;; returned will be composite, and may include several
;;; linearisations - some of these names will need to be
;;; filtered if just metarule expanded names are required.
;;;
;;; Each item inside the parentheses in the pattern is taken as
;;; one metarule name - metarule name patterns other than * and
;;; ? on their own are not possible since a pattern would
;;; consist of more than one atom.

(defun get-expanded-idrules
   (item-list search-pattern)
   (let
      ((meta-tail (member '\( search-pattern :test #'eq)))
      (get-expanded-idrules1 item-list
         (ldiff search-pattern meta-tail)
         (mapcan
            #'(lambda (name)
                 (unless (member name '(\( \) \,) :test #'eq)
                    (ncons name)))
            meta-tail))))


(defun get-expanded-idrules1
   (item-list idrule-pattern metarule-pattern)
   (mapcan
      #'(lambda (name)
           (cond
              ((top-rule-name-p name)
                 (if
                    (match-metarule-pattern
                       (top-rule-name-meta-names name)
                       metarule-pattern)
                    (ncons name)))
              ((and (symbolp name)
                  (id-rule-p (get name 'idrule)))
                 (mapcan
                    #'(lambda (rule)
                         (cond
                            ((and
                                (top-rule-name-meta-names
                                   (id-rule-name rule))
                                (match-metarule-pattern
                                   (top-rule-name-meta-names
                                      (id-rule-name rule))
                                   metarule-pattern))
                               (list (id-rule-name rule)))))
                    (compile-idrule name)))))
      (get-items item-list idrule-pattern)))


(defun match-metarule-pattern
   (meta-names meta-patterns)
   (cond
      ((null meta-names)
         (cond
            ((null meta-patterns) t)
            ((equal meta-patterns '(*)) t)))
      ((null meta-patterns) nil)
      ((or
          (eq
             (sub-rule-name-base (car meta-names))
             (car meta-patterns))
          (eq (car meta-patterns) '?))
         (match-metarule-pattern
            (cdr meta-names)
            (cdr meta-patterns)))
      ((and (equal (car meta-patterns) '*)
          (cdr meta-patterns))
         (or
            (match-metarule-pattern meta-names
               (cdr meta-patterns))
            (match-metarule-pattern
               (cdr meta-names) meta-patterns)))
      ((equal (car meta-patterns) '*)
         (match-metarule-pattern
            (cdr meta-names) meta-patterns))
      (t nil)))


;;; Return a list of names of all definitions which contain the
;;; given feature in their bodies. If compiled-idrules-p is true
;;; then also look in the results of any ID rule compilation
;;; that may be around. Won't find feature in a category
;;; declaration semantic type if type of category containing
;;; feature in the type is not known.

(defun definitions-containing-feature (feature-name compiled-idrules-p)
   (nconc
      (mapcan
         #'(lambda (name)
              (when
                 (member feature-name
                    (set-declaration-features (get name 'set))
                    :test #'eq)
                 (ncons name)))
         *sets)
      (mapcan
         #'(lambda (name)
              (when
                 (let
                    ((normalised-category
                        (normalise-category-definition name)))
                    (or
                       (member feature-name
                          (category-declaration-feature-path
                             normalised-category) :test #'eq)
                       (member-feature-bindings feature-name
                          (category-declaration-cat-bindings
                             normalised-category))
                       (member feature-name
                          (category-declaration-features
                             normalised-category) :test #'eq)
                       (member-if
                          #'(lambda (type)
                               (member-feature-semantic-type
                                  feature-name type))
                          (category-declaration-semantic-types
                             normalised-category))))
                 (ncons name)))
         *categories)
      (mapcan
         #'(lambda (name)
              (when
                 (member feature-name
                    (extension-declaration-features
                       (normalise-extension-definition name))
                    :test #'eq)
                 (ncons name)))
         *extensions)
      (mapcan
         #'(lambda (name)
              (when
                 (some
                    #'(lambda (category)
                       (member-feature-bindings feature-name category))
                    (top-declaration-categories
                       (normalise-top-definition name)))
                 (ncons name)))
         *top)
      (mapcan
         #'(lambda (name)
              (when
                 (member-feature-bindings feature-name
                    (alias-declaration-cat-bindings
                       (normalise-alias-definition name)))
                 (ncons name)))
         *aliases)
      (mapcan
         #'(lambda (name)
              (mapcan
                 #'(lambda (idrule)
                    (when
                       (or
                          (member-feature-bindings feature-name
                             (id-rule-binding-list idrule))
                          (some
                             #'(lambda (form)
                                (and (consp form)
                                   (semantic-form-pattern-p (car form))
                                   (member-feature-bindings
                                      feature-name (cdar form))))
                             (id-rule-semantic-forms idrule)))
                       (list
                          (if compiled-idrules-p
                             (id-rule-name idrule) name))))
                 (if
                    (and compiled-idrules-p
                       (get name 'expanded-idrules))
                    (get name 'expanded-idrules)
                    (normalise-idrule-definition name))))
         *id-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (or
                    (member-feature-bindings feature-name
                       (meta-rule-cat-bindings
                          (normalise-metarule-definition name)))
                    (some
                       #'(lambda (form)
                          (and (consp form)
                             (semantic-form-pattern-p (car form))
                             (member-feature-bindings
                                feature-name (cdar form))))
                       (meta-rule-semantic-forms
                          (normalise-metarule-definition name))))
                 (list name)))
         *meta-rules)
      (definitions-containing-feature1 feature-name)))


(defun definitions-containing-feature1 (feature-name)
   (nconc
      (mapcan
         #'(lambda (name)
              (when
                 (let
                    ((normalised-defrule
                        (normalise-defrule-definition name)))
                    (or
                       (member-feature-bindings feature-name
                          (default-rule-cat-bindings
                             normalised-defrule))
                       (member feature-name
                          (default-rule-feature-names
                             normalised-defrule) :test #'eq)
                       (eq feature-name
                          (category-index-cat-feature
                             (default-rule-category-index
                                normalised-defrule)))))
                 (ncons name)))
         *default-rules)
      (mapcan
         #'(lambda (name)
            (when
               (let
                  ((normalised-proprule
                        (normalise-proprule-definition name)))
                  (or
                     (member-feature-bindings feature-name
                        (prop-rule-cat-bindings normalised-proprule))
                     (member-if
                        #'(lambda (equality)
                           (member-if
                              #'(lambda (spec)
                                 (or
                                    (member feature-name
                                       (prop-ident-spec-feature-names spec)
                                       :test #'eq)
                                    (eq feature-name
                                       (category-index-cat-feature
                                          (prop-ident-spec-category-index spec)))))
                              equality))
                        (prop-rule-ident-specs normalised-proprule))))
               (ncons name)))
         *prop-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (member-if
                    #'(lambda (lpterm)
                         (member-feature-bindings feature-name
                            lpterm))
                    (lp-rule-lp-terms
                       (normalise-lprule-definition name)))
                 (ncons name)))
         *lp-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (member-if
                    #'(lambda (sense)
                         (member-feature-bindings feature-name
                            (word-sense-cat-bindings sense)))
                    (word-definition-senses
                       (normalise-word-definition name)))
                 (ncons name)))
         (append *words *cached-words))
      (mapcan
         #'(lambda (name)
              (when
                 (let
                    ((rule (get name 'ecr)))
                    (or
                       (and (consp (ec-rule-features rule))
                          (member feature-name
                             (ec-rule-features rule) :test #'eq))
                       (member-feature-pattern feature-name
                          (ec-rule-precond rule))
                       (member-feature-pattern feature-name
                          (ec-rule-action rule))))
                 (ncons name)))
         *ec-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (or
                    (member-feature-pattern feature-name
                       (multiply-rule-precond (get name 'mr)))
                    (member-feature-pattern feature-name
                       (multiply-rule-skeletons
                          (get name 'mr))))
                 (ncons name)))
         *multiply-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (or
                    (member-feature-pattern feature-name
                       (cc-rule-precond (get name 'cc)))
                    (member-feature-pattern feature-name
                       (cc-rule-postcond (get name 'cc))))
                 (ncons name)))
         *cc-rules)))


(defun member-feature-bindings
   (feature-name cat-bindings)
   (member-if
      #'(lambda (cat-binding)
           (f-find feature-name
              (category-binding-category cat-binding)
              :key #'fv-pair-feature :test #'eq))
      cat-bindings))


(defun member-feature-pattern
   (feature-name pattern)
   (member-if
      #'(lambda (x)
           (cond
              ((atom x) nil)
              ((atom (car x))
                 (eq (car x) feature-name))
              (t
                 (member-feature-pattern feature-name x))))
      pattern))


(defun member-feature-semantic-type
   (feature-name type)
   (cond
      ((basic-type-p type) nil)
      ((complex-type-p type)
         (or
            (member-feature-semantic-type feature-name
               (complex-type-arg type))
            (member-feature-semantic-type feature-name
               (complex-type-res type))))
      (t
         (member-feature-bindings feature-name
            type))))


;;; All definitions containing references to given set name.

(defun definitions-containing-set (set-name)
   (nconc
      (mapcan
         #'(lambda (name)
              (when
                 (eq set-name
                    (category-declaration-features
                       (get name 'category)))
                 (ncons name)))
         *categories)
      (mapcan
         #'(lambda (name)
              (when
                 (eq set-name
                    (extension-declaration-features
                       (get name 'extension)))
                 (ncons name)))
         *extensions)
      (mapcan
         #'(lambda (name)
              (when
                 (eq set-name
                    (default-rule-feature-names
                       (get name 'defrule)))
                 (ncons name)))
         *default-rules)
      (mapcan
         #'(lambda (name)
            (when
               (member-if
                  #'(lambda (equality)
                     (member-if
                        #'(lambda (spec)
                           (eq set-name
                              (prop-ident-spec-feature-names spec)))
                        equality))
                  (prop-rule-ident-specs (get name 'proprule)))
               (ncons name)))
         *prop-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (eq set-name
                    (ec-rule-features (get name 'ecr)))
                 (ncons name)))
         *ec-rules)))


;;; All definitions containing references to an alias name.
;;; Cached words cannot contain an alias, so don't bother
;;; looking.

(defun definitions-containing-alias (alias-name)
   (nconc
      (mapcan
         #'(lambda (name)
              (when
                 (or
                    (member-alias-bindings alias-name
                       (category-declaration-cat-bindings
                          (get name 'category)))
                    (member-if
                       #'(lambda (type)
                            (member-alias-semantic-type
                               alias-name type))
                       (category-declaration-semantic-types
                          (get name 'category))))
                 (ncons name)))
         *categories)
      (mapcan
         #'(lambda (name)
              (when
                 (member-alias-bindings alias-name
                    (alias-declaration-cat-bindings
                       (get name 'alias)))
                 (ncons name)))
         *aliases)
      (mapcan
         #'(lambda (name)
              (when
                 (some
                    #'(lambda (category)
                       (member-alias-bindings alias-name category))
                    (top-declaration-categories (get name 'top)))
                 (ncons name)))
         *top)
      (mapcan
         #'(lambda (name)
              (when
                 (or
                    (member-alias-bindings alias-name
                       (id-rule-binding-list (get name 'idrule)))
                    (some
                       #'(lambda (form)
                            (and (consp form)
                               (semantic-form-pattern-p (car form))
                               (member-alias-bindings
                                  alias-name (cdar form))))
                       (id-rule-semantic-forms (get name 'idrule))))
                 (ncons name)))
         *id-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (or
                    (member-alias-bindings alias-name
                       (meta-rule-cat-bindings
                          (get name 'metarule)))
                    (some
                       #'(lambda (form)
                            (and (consp form)
                               (semantic-form-pattern-p (car form))
                               (member-alias-bindings
                                  alias-name (cdar form))))
                       (meta-rule-semantic-forms (get name 'metarule))))
                 (ncons name)))
         *meta-rules)
      (definitions-containing-alias1 alias-name)))


(defun definitions-containing-alias1
   (alias-name)
   (nconc
      (mapcan
         #'(lambda (name)
              (when
                 (member-alias-bindings alias-name
                    (default-rule-cat-bindings
                       (get name 'defrule)))
                 (ncons name)))
         *default-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (member-alias-bindings alias-name
                    (prop-rule-cat-bindings
                       (get name 'proprule)))
                 (ncons name)))
         *prop-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (member-if
                    #'(lambda (lpterm)
                         (member-alias-bindings alias-name
                            lpterm))
                    (lp-rule-lp-terms (get name 'lprule)))
                 (ncons name)))
         *lp-rules)
      (mapcan
         #'(lambda (name)
              (when
                 (member-if
                    #'(lambda (sense)
                         (member-alias-bindings alias-name
                            (word-sense-cat-bindings sense)))
                    (word-definition-senses (get name 'word)))
                 (ncons name)))
         *words)))


(defun member-alias-bindings
   (alias-name cat-bindings)
   (member-if
      #'(lambda (cat-binding)
           (member-alias-category alias-name
              (category-binding-category cat-binding)))
      cat-bindings))


(defun member-alias-category
   (alias-name category)
   (and (alias-instantiation-p category)
      (or
         (eq alias-name
            (alias-instantiation-name category))
         (member-if
            #'(lambda (i)
                 (member-alias-category alias-name i))
            (alias-instantiation-bundle category)))))


(defun member-alias-semantic-type
   (alias-name type)
   (cond
      ((basic-type-p type)
         (eq alias-name (basic-type-name type)))
      ((complex-type-p type)
         (or
            (member-alias-semantic-type alias-name
               (complex-type-arg type))
            (member-alias-semantic-type alias-name
               (complex-type-res type))))
      (t
         (member-alias-bindings alias-name type))))


;;; End of file

