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

;;; GRAMMAR DEVELOPMENT ENVIROMENT - GRAMMAR ITEM NAMES
;;;
;;; Author: John Carroll
;;;
;;; Top level command handlers for handling all the possible
;;; names commands.
;;;
;;; Entry points:
;;;
;;;  * (defun Names-all-constructs (pattern-head) ...
;;;  * (defun Names-feature () ...
;;;  * (defun Names-set () ...
;;;  * (defun Names-alias () ...
;;;  * (defun Names-category (lexical) ...
;;;  * (defun Names-idrule (linear-rule format mode) ...
;;;  * (defun Names-metarule () ...
;;;  * (defun Names-defrule () ...
;;;  * (defun Names-proprule () ...
;;;  * (defun Names-lprule () ...
;;;  * (defun Names-word () ...
;;;
;;; These functions behave very similarly to the View commands,
;;; calling Get-items for the list of names to be displayed.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Print names of all constructs whose names match a given
;;; pattern. ID rule names could come as symbols, or as compound
;;; names if a pattern element specifies a metarule expanded ID
;;; rule.

(defun names-all-constructs nil
   (let
      ((pattern
          (prompt-if-necessary "Pattern? ")))
      (if pattern
         (unless
            (nconc
               (names-all-constructs1
                  (get-items *features pattern)
                  (get-items *sets pattern)
                  (get-items *aliases pattern)
                  (get-items *categories pattern)
                  (get-items *extensions pattern)
                  (get-items *id-rules pattern))
               (names-all-constructs2
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


(defun names-all-constructs1
   (features sets aliases categories
      extensions idrules)
   (nconc
      (when features (terpri)
         (write-construct-names "Features: "
            features)
         (ncons t))
      (when sets (terpri)
         (write-construct-names "Sets: " sets)
         (ncons t))
      (when aliases (terpri)
         (write-construct-names "Aliases: " aliases)
         (ncons t))
      (when categories (terpri)
         (write-construct-names
            "Category and Lcategory declarations: "
            categories)
         (ncons t))
      (when extensions (terpri)
         (write-construct-names "Extensions: "
            extensions)
         (ncons t))
      (when idrules (terpri)
         (write-construct-names "ID and PS rules: "
            (mapcar
               #'(lambda (name)
                    (cond
                       ((symbolp name) name)
                       (t (idrule-name-string name))))
               idrules))
         (ncons t))))


(defun names-all-constructs2
   (metarules proprules defrules lprules
      ecrules multrules ccrules words)
   (nconc
      (when metarules (terpri)
         (write-construct-names "Metarules: "
            metarules)
         (ncons t))
      (when proprules (terpri)
         (write-construct-names
            "Propagation rules: " proprules)
         (ncons t))
      (when defrules (terpri)
         (write-construct-names "Default rules: "
            defrules)
         (ncons t))
      (when lprules (terpri)
         (write-construct-names "LP rules: "
            lprules)
         (ncons t))
      (when ecrules (terpri)
         (write-construct-names "Entry CRs: "
            ecrules)
         (ncons t))
      (when multrules (terpri)
         (write-construct-names
            "Multiplication rules: " multrules)
         (ncons t))
      (when ccrules (terpri)
         (write-construct-names
            "Consistency checks: " ccrules)
         (ncons t))
      (when words (terpri)
         (write-construct-names "Words: "
            (mapcar
               #'(lambda (word)
                    (cond
                       ((word-definition-file (get word 'word))
                          word)
                       (t (concat-string "(" word ")"))))
               words))
         (ncons t))))


;;; Print names of definitions belonging to specific construct
;;; types.

(defun names-feature nil
   (let
      ((pattern
          (prompt-if-necessary "Feature? ")))
      (when pattern
         (let
            ((feature-names
                (get-items *features pattern)))
            (cond
               (feature-names
                  (write-construct-names "" feature-names))
               (t (gde-cerror "feature not found")))))))


(defun names-set nil
   (let
      ((pattern (prompt-if-necessary "Set? ")))
      (when pattern
         (let
            ((set-names (get-items *sets pattern)))
            (cond
               (set-names
                  (write-construct-names "" set-names))
               (t (gde-cerror "set not found")))))))


(defun names-alias nil
   (let
      ((pattern (prompt-if-necessary "Alias? ")))
      (when pattern
         (let
            ((alias-names
                (get-items *aliases pattern)))
            (cond
               (alias-names
                  (write-construct-names "" alias-names))
               (t (gde-cerror "alias not found")))))))


(defun names-category (lexical)
   (let
      ((pattern
          (prompt-if-necessary
             "Category declaration name? ")))
      (when pattern
         (let
            ((category-names
                (get-items *categories pattern)))
            (let
               ((filtered-category-names
                   (mapcan
                      #'(lambda (name)
                           (when
                              (eq
                                 (category-declaration-lexical
                                    (get name 'category)) lexical)
                              (ncons name)))
                      category-names)))
               (cond
                  (filtered-category-names
                     (write-construct-names ""
                        filtered-category-names))
                  (t
                     (gde-cerror
"category declaration not found"))))))))


(defun names-idrule
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
                  (write-construct-names ""
                     (mapcan
                        #'(lambda (name)
                             (cond
                                ((symbolp name)
                                   (names-simple-idrule name
                                      format mode))
                                (t
                                   (names-expanded-idrule name
                                      mode))))
                        idrule-names)))
               (t (gde-cerror "rule not found")))))))


(defun names-simple-idrule
   (name format mode)
   (cond
      ((eq mode 'linearised)
         (names-linearised-simple-idrule name))
      ((eq format 'aliased) (ncons name))
      ((eq format 'normalised)
         (mapcar
            #'(lambda (normalised-idrule)
                 (idrule-name-string
                    (id-rule-name normalised-idrule)))
            (normalise-idrule-definition name)))))


(defun names-linearised-simple-idrule
   (name)
   (cond
      ((get name 'compiled-idrules)
         (mapcan
            #'(lambda (rule)
                 (cond
                    ((null
                        (top-rule-name-meta-names
                           (id-rule-name rule)))
                       (list
                          (idrule-name-string
                             (id-rule-name rule))))))
            (get name 'compiled-idrules)))
      (t
         (mapcan
            #'(lambda (idrule)
                 (cond
                    ((id-rule-linear idrule)
                       (ncons
                          (idrule-name-string
                             (id-rule-name idrule))))
                    (t
                       (mapcar
                          #'(lambda (rule)
                               (idrule-name-string
                                  (id-rule-name rule)))
                          (lprule-expand idrule)))))
            (fully-instantiate-idrule name)))))


(defun names-expanded-idrule (name mode)
   (cond
      ((or (eq mode 'linearised)
          (id-rule-linear
             (get
                (sub-rule-name-base
                   (top-rule-name-base name))
                'idrule)))
         (ncons (idrule-name-string name)))
      (t
         (if
            (or
               (null
                  (sub-rule-name-index
                     (top-rule-name-base name)))
               (=
                  (sub-rule-name-index
                     (top-rule-name-base name))
                  1))
            (mapcan
               #'(lambda (idrule)
                    (cond
                       ((eq
                           (top-rule-name-meta-names
                              (id-rule-name idrule))
                           (top-rule-name-meta-names name))
                          (list
                             (idrule-name-string
                                (id-rule-name idrule))))))
               (get
                  (sub-rule-name-base
                     (top-rule-name-base name))
                  'expanded-idrules))))))


(defun names-metarule nil
   (let
      ((pattern
          (prompt-if-necessary "Metarule? ")))
      (when pattern
         (let
            ((metarule-names
                (get-items *meta-rules pattern)))
            (cond
               (metarule-names
                  (write-construct-names "" metarule-names))
               (t (gde-cerror "metarule not found")))))))


(defun names-defrule nil
   (let
      ((pattern
          (prompt-if-necessary "Default rule? ")))
      (when pattern
         (let
            ((defrule-names
                (get-items *default-rules pattern)))
            (cond
               (defrule-names
                  (write-construct-names "" defrule-names))
               (t
                  (gde-cerror "default rule not found")))))))


(defun names-proprule nil
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
                  (write-construct-names "" proprule-names))
               (t
                  (gde-cerror
                     "propagation rule not found")))))))


(defun names-lprule nil
   (let
      ((pattern
          (prompt-if-necessary "LP rule? ")))
      (when pattern
         (let
            ((lprule-names
                (get-items *lp-rules pattern)))
            (cond
               (lprule-names
                  (write-construct-names "" lprule-names))
               (t (gde-cerror "LP rule not found")))))))


(defun names-word nil
   (let
      ((pattern (prompt-if-necessary "Word? ")))
      (when pattern
         (setf pattern (canonise-word pattern))
         (let
            ((words
                (get-items (append *words *cached-words *disk-resident-words)
                   pattern)))
            (cond
               (words
                  (write-construct-names ""
                     (mapcar
                        #'(lambda (word)
                             (cond
                                ((word-definition-file
                                    (get word 'word))
                                   word)
                                (t
                                   (concat-string "(" word
                                      ")"))))
                        words)))
               (t (gde-cerror "word not found")))))))


;;; End of file

