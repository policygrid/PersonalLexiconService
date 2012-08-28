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

;;; GRAMMAR DEVELOPMENT ENVIROMENT - GRAMMAR ITEM VIEWING
;;;
;;; Author: John Carroll
;;;
;;; Top level command handlers for handling all the possible
;;; view commands.
;;;
;;; Entry points:
;;;
;;;  * (defun View-all-constructs (pattern-head) ...
;;;  * (defun View-feature () ...
;;;  * (defun View-set () ...
;;;  * (defun View-alias (format) ...
;;;  * (defun View-category (format) ...
;;;  * (defun View-extension (format) ...
;;;  * (defun View-top (format) ...
;;;  * (defun View-idrule (linear-rule format mode) ...
;;;  * (defun View-metarule (format) ...
;;;  * (defun View-defrule (form ...
;;;  * (defun View-proprule (format) ...
;;;  * (defun View-lprule (format) ...
;;;  * (defun View-word (format mode) ...
;;;  * (defun View-word1 (word format mode) ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Print out all constructs whose names match a given pattern.
;;; ID rule names could come as symbols, or as compound names if
;;; a pattern element specifies a metarule expanded ID rule.

(defun view-all-constructs nil
   (let
      ((pattern
          (prompt-if-necessary "Pattern? ")))
      (if pattern
         (unless
            (nconc
               (view-all-constructs1
                  (get-items *features pattern)
                  (get-items *sets pattern)
                  (get-items *aliases pattern)
                  (get-items *categories pattern)
                  (get-items *extensions pattern)
                  (get-items *top pattern)
                  (get-items *id-rules pattern))
               (view-all-constructs2
                  (get-items *meta-rules pattern)
                  (get-items *prop-rules pattern)
                  (get-items *default-rules pattern)
                  (get-items *lp-rules pattern)
                  (get-items *ec-rules pattern)
                  (get-items *multiply-rules pattern)
                  (get-items *cc-rules pattern)
                  (get-items (append *words *cached-words)
                     pattern)))
            (gde-cerror "no constructs found")))))


(defun view-all-constructs1
   (features sets aliases categories
      extensions top idrules)
   (nconc
      (when features (format t "~%Features: ~%")
         (dolist (name features)
            (print-feature-definition name
               (get name 'feature) nil))
         (ncons t))
      (when sets (format t "~%Sets: ~%")
         (dolist (name sets)
            (print-set-definition name (get name 'set)
               nil))
         (ncons t))
      (when aliases (format t "~%Aliases: ~%")
         (dolist (name aliases)
            (print-alias-definition name
               (get name 'alias) nil))
         (ncons t))
      (when categories
         (format t
            "~%Category and Lcategory declarations: ~%")
         (dolist (name categories)
            (print-category-definition name
               (get name 'category) nil))
         (ncons t))
      (when extensions
         (format t "~%Extensions: ~%")
         (dolist (name extensions)
            (print-extension-definition name
               (get name 'extension) nil))
         (ncons t))
      (when top
         (format t "~%Top: ~%")
         (dolist (name top)
            (print-top-definition name
               (get name 'top) nil))
         (ncons t))
      (when idrules
         (format t "~%ID and PS rules: ~%")
         (dolist (name idrules)
            (print-idrule-definition
               (cond
                  ((symbolp name) (get name 'idrule))
                  (t
                     (f-find name
                        (get
                           (sub-rule-name-base
                              (top-rule-name-base name))
                           'compiled-idrules)
                        :key #'id-rule-name :test #'eq)))
               nil))
         (ncons t))))


(defun view-all-constructs2
   (metarules proprules defrules lprules
      ecrules multrules ccrules words)
   #-gde-morph (declare (ignore ecrules multrules ccrules))
   (nconc
      (when metarules
         (format t "~%Metarules: ~%")
         (dolist (name metarules)
            (print-metarule-definition name
               (get name 'metarule) nil))
         (ncons t))
      (when proprules
         (format t "~%Propagation rules: ~%")
         (dolist (name proprules)
            (print-proprule-definition name
               (get name 'proprule) nil))
         (ncons t))
      (when defrules
         (format t "~%Default rules: ~%")
         (dolist (name defrules)
            (print-defrule-definition name
               (get name 'defrule) nil))
         (ncons t))
      (when lprules (format t "~%LP rules: ~%")
         (dolist (name lprules)
            (print-lprule-definition name
               (get name 'lprule) nil))
         (ncons t))
      #+gde-morph
      (when ecrules (format t "~%Entry CRs: ~%")
         (dolist (name ecrules)
            (print-ecr-definition name (get name 'ecr)
               nil))
         (ncons t))
      #+gde-morph
      (when multrules
         (format t "~%Multiplication rules: ~%")
         (dolist (name multrules)
            (print-mr-definition name (get name 'mr)
               nil))
         (ncons t))
      #+gde-morph
      (when ccrules
         (format t "~%Consistency checks: ~%")
         (dolist (name ccrules)
            (print-cc-definition name (get name 'cc)
               nil))
         (ncons t))
      (when words (format t "~%Words: ~%")
         (dolist (name words)
            (print-word-definition name
               (get name 'word) nil))
         (ncons t))))


;;; The command handler called by top loop for displaying a
;;; feature declaration (the "View feature" command).

(defun view-feature nil
   (let
      ((pattern
          (prompt-if-necessary "Feature? ")))
      (when pattern
         (let
            ((feature-names
                (get-items *features pattern)))
            (cond
               (feature-names
                  (dolist (name feature-names)
                     (progn
                        (terpri)
                        (print-feature-definition name
                           (get name 'feature) nil))))
               (t (gde-cerror "feature not found")))))))


;;; The command handler called by top loop for displaying a set
;;; declaration (the "View set" command).

(defun view-set nil
   (let
      ((pattern (prompt-if-necessary "Set? ")))
      (when pattern
         (let
            ((set-names (get-items *sets pattern)))
            (cond
               (set-names
                  (dolist (name set-names)
                     (progn
                        (terpri)
                        (print-set-definition name
                           (get name 'set) nil))))
               (t (gde-cerror "set not found")))))))


;;; The command handler called by top loop for displaying an
;;; alias declaration (the "View alias" command).

(defun view-alias (format)
   (let
      ((pattern (prompt-if-necessary "Alias? ")))
      (when pattern
         (let
            ((alias-names
                (get-items *aliases pattern)))
            (cond
               (alias-names
                  (dolist (name alias-names)
                     (progn
                        (terpri)
                        (cond
                           ((eq format 'aliased)
                              (print-alias-definition name
                                 (get name 'alias) nil))
                           (t
                              (print-alias-definition name
                                 (normalise-alias-definition
                                    name)
                                 nil))))))
               (t (gde-cerror "alias not found")))))))


;;; The command handler called by top loop for displaying a
;;; category declaration (the "View category command).

(defun view-category (lexical format)
   (let
      ((pattern
          (prompt-if-necessary
             "Category declaration name? ")))
      (when pattern
         (let
            ((category-names
                (get-items *categories pattern)))
            (cond
               (category-names
                  (dolist (name category-names)
                     (cond
                        ((eq
                            (category-declaration-lexical
                               (get name 'category))
                            lexical)
                           (terpri)
                           (cond
                              ((eq format 'aliased)
                                 (print-category-definition
                                    name (get name 'category)
                                    nil))
                              (t
                                 (print-category-definition name
(normalise-category-definition
                                       name)
                                    nil)))))))
               (t
                  (gde-cerror
                     "category declaration not found")))))))


;;; The command handler called by top loop for displaying an
;;; extension declaration (the "View extension command).

(defun view-extension (format)
   (cond
      (*extensions
         (dolist (name *extensions)
            (progn
               (terpri)
               (cond
                  ((eq format 'aliased)
                     (print-extension-definition name
                        (get name 'extension) nil))
                  (t
                     (print-extension-definition name
                        (normalise-extension-definition name)
                        nil))))))
      (t
         (gde-cerror
            "extension declaration not found"))))


(defun view-top (format)
   (cond
      (*top
         (dolist (name *top)
            (progn
               (terpri)
               (cond
                  ((eq format 'aliased)
                     (print-top-definition name
                        (get name 'top) nil))
                  (t
                     (print-top-definition name
                        (normalise-top-definition name)
                        nil))))))
      (t
         (gde-cerror "top declaration not found"))))


;;; The command handler called by top loop for displaying an
;;; idrule declaration or metarule expansion of an idrule (the
;;; "View idrule" command).

(defun view-idrule
   (linear-rule format mode)
   (let
      ((pattern
          (prompt-if-necessary "Rule name? ")))
      (when pattern
         (let
            ((idrule-names
                (get-items
                   (mapcan
                      #'(lambda (rule)
                           (when
                              (eq
                                 (id-rule-linear
                                    (get rule 'idrule))
                                 linear-rule)
                              (ncons rule)))
                      *id-rules)
                   pattern)))
            (cond
               (idrule-names
                  (dolist (name idrule-names)
                     (cond
                        ((symbolp name)
                           (view-simple-idrule name format
                              mode))
                        (t
                           (view-expanded-idrule name format
                              mode)))))
               (t (gde-cerror "rule not found")))))))


(defun view-idrules (idrules format)
   (dolist (idrule idrules)
      (progn
         (terpri)
         (print-idrule-definition
            (cond
               ((eq format 'aliased)
                  (realias-idrule-definition idrule))
               (t idrule))
            nil))))


;;; Print out an ID rule (or set of normalisations of an ID rule
;;; that has not been metarule expanded - the name in the
;;; original command did not contain parentheses, so no
;;; compilation needed to be performed to get the metarule
;;; expansions. Name is a symbol. The expanded and unexpanded
;;; cases are treated separately to avoid calling for
;;; compilation in the unexpanded case.

(defun view-simple-idrule
   (name format mode)
   (cond
      ((eq mode 'basic)
         (cond
            ((eq format 'aliased) (terpri)
               (print-idrule-definition (get name 'idrule)
                  nil))
            (t
               (dolist
                  (idrule (normalise-idrule-definition name))
                  (progn
                     (terpri)
                     (print-idrule-definition idrule nil))))))
      ((eq mode 'full)
         (view-full-simple-idrule name format))
      ((eq mode 'linearised)
         (view-linearised-simple-idrule name
            format))))


(defun view-full-simple-idrule
   (name format)
   (view-idrules
      (cond
         ((get name 'expanded-idrules)
            (mapcan
               #'(lambda (rule)
                    (unless
                       (top-rule-name-meta-names
                          (id-rule-name rule))
                       (ncons rule)))
               (get name 'expanded-idrules)))
         (t (fully-instantiate-idrule name)))
      format))


(defun view-linearised-simple-idrule
   (name format)
   (view-idrules
      (cond
         ((get name 'compiled-idrules)
            (mapcan
               #'(lambda (rule)
                    (unless
                       (top-rule-name-meta-names
                          (id-rule-name rule))
                       (ncons rule)))
               (get name 'compiled-idrules)))
         (t (linearise-idrule name)))
      format))


;;; Print out a set of metarule expanded ID rules. Each rule
;;; will have been compiled already (so it will have a
;;; compiled-idrules property) - this was done in order to find
;;; all the metarules that were applicable to it.

(defun view-expanded-idrule
   (name format mode)
   (cond
      ((id-rule-linear
          (get
             (sub-rule-name-base
                (top-rule-name-base name))
             'idrule))
         (view-linearised-expanded-idrules name
            format))
      ((member mode '(basic full) :test #'eq)
         (view-full-expanded-idrule name format))
      ((eq mode 'linearised)
         (view-linearised-expanded-idrules name
            format))))


(defun view-full-expanded-idrule
   (name format)
   (if
      (or
         (null
            (sub-rule-name-index
               (top-rule-name-base name)))
         (=
            (sub-rule-name-index
               (top-rule-name-base name))
            1))
      (view-idrules
         (mapcan
            #'(lambda (idrule)
                 (when
                    (eq
                       (top-rule-name-meta-names
                          (id-rule-name idrule))
                       (top-rule-name-meta-names name))
                    (ncons idrule)))
            (get
               (sub-rule-name-base
                  (top-rule-name-base name))
               'expanded-idrules))
         format)))


(defun view-linearised-expanded-idrules
   (name format)
   (view-idrules
      (mapcan
         #'(lambda (idrule)
              (when (eq name (id-rule-name idrule))
                 (ncons idrule)))
         (get
            (sub-rule-name-base
               (top-rule-name-base name))
            'compiled-idrules))
      format))


;;; The command handler called by top loop for displaying a
;;; metarule declaration (the "View metarule" command).

(defun view-metarule (format)
   (let
      ((pattern
          (prompt-if-necessary "Metarule? ")))
      (when pattern
         (let
            ((metarule-names
                (get-items *meta-rules pattern)))
            (cond
               (metarule-names
                  (dolist (name metarule-names)
                     (progn
                        (terpri)
                        (cond
                           ((eq format 'aliased)
                              (print-metarule-definition name
                                 (get name 'metarule) nil))
                           (t
                              (print-metarule-definition name
                                 (normalise-metarule-definition
                                    name)
                                 nil))))))
               (t (gde-cerror "metarule not found")))))))


;;; The command handler called by top loop for displaying a
;;; default rule declaration (the "View defrule command).

(defun view-defrule (format)
   (let
      ((pattern
          (prompt-if-necessary "Default rule? ")))
      (when pattern
         (let
            ((defrule-names
                (get-items *default-rules pattern)))
            (cond
               (defrule-names
                  (dolist (name defrule-names)
                     (progn
                        (terpri)
                        (cond
                           ((eq format 'aliased)
                              (print-defrule-definition name
                                 (get name 'defrule) nil))
                           (t
                              (print-defrule-definition name
                                 (normalise-defrule-definition
                                    name) nil))))))
               (t
                  (gde-cerror "default rule not found")))))))


;;; The command handler called by top loop for displaying a
;;; feature propagation rule.

(defun view-proprule (format)
   (let
      ((pattern
          (prompt-if-necessary
             "Propagation rule? ")))
      (when pattern
         (let
            ((proprule-names
                (get-items *prop-rules pattern)))
            (cond
               (proprule-names
                  (dolist (name proprule-names)
                     (progn
                        (terpri)
                        (cond
                           ((eq format 'aliased)
                              (print-proprule-definition name
                                 (get name 'proprule) nil))
                           (t
                              (print-proprule-definition name
                                 (normalise-proprule-definition
                                    name)
                                 nil))))))
               (t
                  (gde-cerror
                     "propagation rule not found")))))))


;;; The command handler called by top loop for displaying an
;;; lprule declaration (the "View lprule" command).

(defun view-lprule (format)
   (let
      ((pattern
          (prompt-if-necessary "LP rule? ")))
      (when pattern
         (let
            ((lprule-names
                (get-items *lp-rules pattern)))
            (cond
               (lprule-names
                  (dolist (name lprule-names)
                     (progn
                        (terpri)
                        (cond
                           ((eq format 'aliased)
                              (print-lprule-definition name
                                 (get name 'lprule) nil))
                           (t
                              (print-lprule-definition name
                                 (normalise-lprule-definition
                                    name)
                                 nil))))))
               (t (gde-cerror "LP rule not found")))))))


;;; The command handler called by top level for displaying the
;;; definitions of a word in either the GDE dictionary or the
;;; morphology system dictionary. Check in the GDE dictionary by
;;; calling Get-items to get a list of words matching pattern -
;;; if this fails, then try first element of pattern on morph
;;; dictionary. A word definition returned by
;;; Get-word-definitions is a list of categories, each a list of
;;; category binding records.

(defun view-word (format mode)
   (let
      ((pattern (prompt-if-necessary "Word? ")))
      (when pattern
         (setf pattern (canonise-word pattern))
         (dolist
            (word
               (or
                  (get-items (append *words *cached-words *disk-resident-words)
                     pattern)
                  (ncons (car pattern))))
            (view-word1 word format mode)))))


(defun view-word1 (word format mode)
   (let
      ((definition
          (get-word-definition word format mode)))
      (cond
         (definition (terpri)
            (print-word-definition word definition
               nil))
         (t
            (gde-cerror "word " word " not found")))))


;;; End of file

