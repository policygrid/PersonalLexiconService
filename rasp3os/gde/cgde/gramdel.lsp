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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - GRAMMAR DELETION
;;;
;;; Author: John Carroll
;;;
;;; Top level command handlers for deleting GPSG constructs (the
;;; "Delete" command), for finding definition names containing
;;; given feature, set or alias names, and for completely
;;; clearing the grammar.
;;;
;;; Entry points:
;;;
;;;  * (defun Delete-all-constructs () ...
;;;  * (defun Delete-feature () ...
;;;  * (defun Delete-set () ...
;;;  * (defun Delete-alias () ...
;;;  * (defun Delete-category () ...
;;;  * (defun Delete-extension () ...
;;;  * (defun Delete-top () ...
;;;  * (defun Delete-idrule () ...
;;;  * (defun Delete-metarule () ...
;;;  * (defun Delete-defrule () ...
;;;  * (defun Delete-proprule () ...
;;;  * (defun Delete-lprule () ...
;;;  * (defun Delete-word () ...
;;;  * (defun Clear-whole-grammar () ...
;;;  * (defun Clear-cached-grammar (all) ...
;;;  * (defun Forget-file () ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defun delete-all-constructs nil
   (let
      ((pattern
          (prompt-if-necessary "Pattern? ")))
      (unless
         (nconc
            (delete-all-constructs1
               (get-items *features pattern)
               (get-items *sets pattern)
               (get-items *aliases pattern)
               (get-items *categories pattern)
               (get-items *extensions pattern)
               (get-items *top pattern)
               (get-items *id-rules pattern))
            (delete-all-constructs2
               (get-items *meta-rules pattern)
               (get-items *prop-rules pattern)
               (get-items *default-rules pattern)
               (get-items *lp-rules pattern)
               (get-items *ec-rules pattern)
               (get-items *multiply-rules pattern)
               (get-items *cc-rules pattern)
               (get-items (append *words *cached-words)
                  pattern)))
         (gde-cerror "no constructs found"))))


(defun delete-all-constructs1
   (features sets aliases categories
      extensions tops idrules)
   (nconc
      (when features
         (delete-feature-list features) (ncons t))
      (when sets (delete-set-list sets)
         (ncons t))
      (when aliases (delete-alias-list aliases)
         (ncons t))
      (when categories
         (delete-category-list categories)
         (ncons t))
      (when extensions
         (delete-extension-list extensions)
         (ncons t))
      (when tops
         (delete-top-list tops)
         (ncons t))
      (when idrules (delete-idrule-list idrules)
         (ncons t))))


(defun delete-all-constructs2
   (metarules proprules defrules lprules
      ecrules multrules ccrules words)
   #-gde-morph (declare (ignore ecrules multrules ccrules))
   (nconc
      (when metarules
         (delete-metarule-list metarules) (ncons t))
      (when proprules
         (delete-proprule-list proprules) (ncons t))
      (when defrules
         (delete-defrule-list defrules) (ncons t))
      (when lprules (delete-lprule-list lprules)
         (ncons t))
      #+gde-morph
      (when ecrules (delete-ecr-list ecrules)
         (ncons t))
      #+gde-morph
      (when multrules (delete-mr-list multrules)
         (ncons t))
      #+gde-morph
      (when ccrules (delete-cc-list ccrules)
         (ncons t))
      (when words (delete-word-list words)
         (ncons t))))


;;; The command handler called by top loop for deleting a
;;; feature declaration (the "Delete feature" command). Make
;;; sure that the feature is not used in any constructs
;;; elsewhere in the grammar, otherwise we have consistency
;;; problems.

(defun delete-feature nil
   (let
      ((pattern
          (prompt-if-necessary "Feature? ")))
      (when pattern
         (let
            ((feature-names
                (get-items *features pattern)))
            (cond
               (feature-names
                  (delete-feature-list feature-names))
               (t (gde-cerror "feature not found")))))))


(defun delete-feature-list (feature-names)
   (dolist (feature feature-names)
      (cond
         ((is-valid-feature-deletion feature)
            (print-feature-definition feature
               (get feature 'feature) nil)
            (when
               (yes-for-question
                  "Delete feature definition")
               (delete-feature-invalidations feature))
            (terpri)))))


(defun delete-feature-invalidations
   (feature)
   (setf *features
      (remove-list-1 feature *features))
   (remprop feature 'altered)
   (remprop feature 'feature)
   (remprop feature 'feature-order)
   (remprop feature 'variable-list)
   (remprop feature 'variable-pointer)
   (remprop feature 'proper-value-list)
   (setf d-features nil)
   (setf d-variables nil))


(defun is-valid-feature-deletion
   (feature-name)
   (let
      ((definition-names
          (definitions-containing-feature
             feature-name nil)))
      (cond
         (definition-names
            (write-construct-names
               (concat-string "Feature " feature-name
                  " still exists in: ")
               definition-names)
            (gde-cerror "feature may not be removed")
            (terpri) nil)
         (t t))))


;;; The command handler called by top loop for deleting a set
;;; declaration (the "Delete set" command). Make sure that the
;;; set is not used in any constructs elsewhere in the grammar,
;;; otherwise we have consistency problems.

(defun delete-set nil
   (let
      ((pattern (prompt-if-necessary "Set? ")))
      (when pattern
         (let
            ((set-names (get-items *sets pattern)))
            (cond
               (set-names (delete-set-list set-names))
               (t (gde-cerror "set not found")))))))


(defun delete-set-list (set-names)
   (dolist (set set-names)
      (cond
         ((is-valid-set-deletion set)
            (print-set-definition set (get set 'set)
               nil)
            (when
               (yes-for-question "Delete set definition")
               (delete-set-invalidations set))
            (terpri)))))


(defun delete-set-invalidations (set)
   (setf *sets (remove-list-1 set *sets))
   (remprop set 'altered) (remprop set 'set)
   (dolist (word *cached-words)
      (delete-word-invalidations word))
   (input-word-invalidations *words 'compiled)
   (setf d-whead nil) (setf d-wdaughter nil))


(defun is-valid-set-deletion (set-name)
   (let
      ((definition-names
          (definitions-containing-set set-name)))
      (cond
         (definition-names
            (write-construct-names
               (concat-string "Set " set-name
                  " still exists in: ")
               definition-names)
            (gde-cerror "set may not be removed")
            (terpri) nil)
         (t t))))


;;; The command handler called by top loop for deleting an alias
;;; declaration (the "Delete alias" command). Make sure that the
;;; alias is not used in any constructs elsewhere in the
;;; grammar, otherwise we have consistency problems.

(defun delete-alias nil
   (let
      ((pattern (prompt-if-necessary "Alias? ")))
      (when pattern
         (let
            ((alias-names
                (get-items *aliases pattern)))
            (cond
               (alias-names
                  (delete-alias-list alias-names))
               (t (gde-cerror "alias not found")))))))


(defun delete-alias-list (alias-names)
   (dolist (alias alias-names)
      (cond
         ((is-valid-alias-deletion alias)
            (print-alias-definition alias
               (get alias 'alias) nil)
            (when
               (yes-for-question
                  "Delete alias definition")
               (delete-alias-invalidations alias))
            (terpri)))))


(defun delete-alias-invalidations (alias)
   (setf *aliases
      (remove-list-1 alias *aliases))
   (remprop alias 'altered)
   (setf *sorted-aliases
      (remove-list-1 alias *sorted-aliases))
   (remprop alias 'alias)
   (remprop alias 'normalised-alias)
   (setf d-aliases nil))


(defun is-valid-alias-deletion (alias-name)
   (let
      ((definition-names
          (definitions-containing-alias alias-name)))
      (cond
         (definition-names
            (write-construct-names
               (concat-string "Alias " alias-name
                  " still exists in: ")
               definition-names)
            (gde-cerror "alias may not be removed")
            (terpri) nil)
         (t t))))


;;; The command handler called by the delete category
;;; declaration command

(defun delete-category nil
   (let
      ((pattern
          (prompt-if-necessary
             "Category declaration? ")))
      (when pattern
         (let
            ((category-names
                (get-items *categories pattern)))
            (cond
               (category-names
                  (delete-category-list category-names))
               (t
                  (gde-cerror
                     "category declaration not found")))))))


(defun delete-category-list
   (category-names)
   (dolist (category category-names)
      (progn
         (print-category-definition category
            (get category 'category) nil)
         (when
            (yes-for-question
               "Delete category declaration")
            (delete-category-invalidations category))
         (terpri))))


(defun delete-category-invalidations
   (category)
   (setf *categories
      (remove-list-1 category *categories))
   (remprop category 'altered)
   (remprop category 'category)
   (remprop category 'normalised-category)
   (input-idrule-invalidations
      (catrule-invalidated-idrules nil category)
      'expanded)
   (input-word-invalidations
      (append *words *cached-words) 'normalised)
   (dolist (rule *categories)
      (remprop rule 'normalised-category)))


;;; The command handler called by the delete extension
;;; declaration command

(defun delete-extension nil
   (cond
      (*extensions
         (delete-extension-list *extensions))
      (t
         (gde-cerror
            "extension declaration not found"))))


(defun delete-extension-list (extensions)
   (dolist (extension extensions)
      (progn
         (print-extension-definition extension
            (get extension 'extension) nil)
         (when
            (yes-for-question
               "Delete extension declaration")
            (delete-extension-invalidations extension))
         (terpri))))


(defun delete-extension-invalidations
   (extension)
   (setf *extensions
      (remove-list-1 extension *extensions))
   (remprop extension 'altered)
   (remprop extension 'extension)
   (remprop extension 'normalised-extension))


;;; The command handler called by the delete top
;;; declaration command

(defun delete-top nil
   (cond
      (*top
         (delete-top-list *top))
      (t
         (gde-cerror "top declaration not found"))))


(defun delete-top-list (tops)
   (dolist (top tops)
      (progn
         (print-top-definition top
            (get top 'top) nil)
         (when
            (yes-for-question "Delete top declaration")
            (delete-top-invalidations top))
         (terpri))))


(defun delete-top-invalidations (top)
   (setf *top
      (remove-list-1 top *top))
   (remprop top 'altered)
   (remprop top 'top)
   (remprop top 'normalised-top))


;;; The command handler called by top loop for deleting an
;;; idrule declaration (the "Delete idrule" command).

(defun delete-idrule (linear-rule)
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
                  (delete-idrule-list idrule-names))
               (t (gde-cerror "rule not found")))))))


(defun delete-idrule-list (idrule-names)
   (dolist (idrule idrule-names)
      (progn
         (print-idrule-definition
            (get idrule 'idrule) nil)
         (when
            (yes-for-question
               (if (id-rule-linear (get idrule 'idrule))
                  "Delete PS rule definition"
                  "Delete ID rule definition"))
            (delete-idrule-invalidations idrule))
         (terpri))))


(defun delete-idrule-invalidations (idrule)
   (setf *id-rules
      (remove-list-1 idrule *id-rules))
   (remprop idrule 'altered)
   (remprop idrule 'idrule)
   (input-idrule-invalidations (ncons idrule)
      'normalised))


;;; The command handler called by top loop for deleting a
;;; metarule declaration (the "Delete metarule" command).

(defun delete-metarule nil
   (let
      ((pattern
          (prompt-if-necessary "Metarule? ")))
      (when pattern
         (let
            ((metarule-names
                (get-items *meta-rules pattern)))
            (cond
               (metarule-names
                  (delete-metarule-list metarule-names))
               (t (gde-cerror "metarule not found")))))))


(defun delete-metarule-list
   (metarule-names)
   (dolist (metarule metarule-names)
      (progn
         (print-metarule-definition metarule
            (get metarule 'metarule) nil)
         (when
            (yes-for-question
               "Delete metarule definition")
            (delete-metarule-invalidations metarule))
         (terpri))))


(defun delete-metarule-invalidations
   (metarule)
   (let
      ((linear
          (meta-rule-linear
             (get metarule 'metarule))))
      (setf *meta-rules
         (remove-list-1 metarule *meta-rules))
      (remprop metarule 'altered)
      (remprop metarule 'metarule)
      (remprop metarule 'normalised-metarule)
      (input-idrule-invalidations
         (metarule-invalidated-idrules nil metarule)
         (if linear 'compiled 'expanded))))


;;; The command handler called by the top loop for deleting a
;;; default rule declaration (the "Delete defrule command).

(defun delete-defrule nil
   (let
      ((pattern
          (prompt-if-necessary "Default rule? ")))
      (when pattern
         (let
            ((defrule-names
                (get-items *default-rules pattern)))
            (cond
               (defrule-names
                  (delete-defrule-list defrule-names))
               (t
                  (gde-cerror "default rule not found")))))))


(defun delete-defrule-list (defrule-names)
   (dolist (defrule defrule-names)
      (progn
         (print-defrule-definition defrule
            (get defrule 'defrule) nil)
         (when
            (yes-for-question
               "Delete default rule definition")
            (delete-defrule-invalidations defrule))
         (terpri))))


(defun delete-defrule-invalidations
   (defrule)
   (setf *default-rules
      (remove-list-1 defrule *default-rules))
   (remprop defrule 'altered)
   (remprop defrule 'defrule)
   (remprop defrule 'normalised-defrule)
   (input-idrule-invalidations
      (defrule-invalidated-idrules nil defrule)
      'expanded))


;;; The command handler called by the top loop for deleting a
;;; feature propagation rule declaration (the "Delete proprule"
;;; command).

(defun delete-proprule nil
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
                  (delete-proprule-list proprule-names))
               (t
                  (gde-cerror
                     "propagation rule not found")))))))


(defun delete-proprule-list
   (proprule-names)
   (dolist (proprule proprule-names)
      (progn
         (print-proprule-definition proprule
            (get proprule 'proprule) nil)
         (when
            (yes-for-question
               "Delete propagation rule definition")
            (delete-proprule-invalidations proprule))
         (terpri))))


(defun delete-proprule-invalidations
   (proprule)
   (setf *prop-rules
      (remove-list-1 proprule *prop-rules))
   (remprop proprule 'altered)
   (remprop proprule 'proprule)
   (remprop proprule 'normalised-proprule)
   (input-idrule-invalidations
      (proprule-invalidated-idrules nil proprule)
      'expanded))


;;; The command handler called by top loop for deleting an
;;; lprule declaration (the "Delete lprule" command).

(defun delete-lprule nil
   (let
      ((pattern
          (prompt-if-necessary "LP rule? ")))
      (when pattern
         (let
            ((lprule-names
                (get-items *lp-rules pattern)))
            (cond
               (lprule-names
                  (delete-lprule-list lprule-names))
               (t (gde-cerror "LP rule not")))))))


(defun delete-lprule-list (lprule-names)
   (dolist (lprule lprule-names)
      (progn
         (print-lprule-definition lprule
            (get lprule 'lprule) nil)
         (when
            (yes-for-question
               "Delete lprule definition")
            (delete-lprule-invalidations lprule))
         (terpri))))


(defun delete-lprule-invalidations (lprule)
   (setf *lp-rules
      (remove-list-1 lprule *lp-rules))
   (remprop lprule 'altered)
   (remprop lprule 'lprule)
   (remprop lprule 'normalised-lprule)
   (setf *reduced-lp-rules nil)
   (input-idrule-invalidations *id-rules
      'compiled))


;;; The command handler called by top level for deleting a word
;;; from the lexicon (the "Delete word" command).

(defun delete-word nil
   (let
      ((pattern (prompt-if-necessary "Word? ")))
      (when pattern
         (let
            ((words
                (get-items (append *words *cached-words)
                   (canonise-word pattern))))
            (cond
               (words (delete-word-list words))
               (t (gde-cerror "word not found")))))))


(defun delete-word-list (words)
   (dolist (word words)
      (progn
         (print-word-definition word
            (get word 'word) nil)
         (when
            (yes-for-question "Delete word definition")
            (delete-word-invalidations word))
         (terpri))))


(defun delete-word-invalidations (word)
   (setf *words (remove-list-1 word *words))
   (setf *cached-words
      (remove-list-1 word *cached-words))
   (setf *disk-resident-words
      (remove-list-1 word *disk-resident-words))
   (remprop word 'altered)
   (remprop word 'word)
   (input-word-invalidations (ncons word)
      'normalised))


;;; Clear grammar, asking for confirmation first.

(defun clear-whole-grammar nil
   (when
      (yes-for-question
         "Do you really want to clear the grammar")
      (d-markunload 'di) (d-markunload 'gr)
      (dolist (feature *features)
         (dolist
            (prop
               '(feature feature-order variable-list
                   variable-pointer proper-value-list
                   altered))
            (remprop feature prop)))
      (setf *features nil) (setf d-features nil)
      (setf d-variables nil)
      (dolist (set *sets)
         (progn
            (remprop set 'set)
            (remprop set 'altered)))
      (setf *sets nil)
      (dolist (alias *aliases)
         (dolist
            (prop '(alias normalised-alias altered))
            (remprop alias prop)))
      (setf *aliases nil)
      (setf *sorted-aliases nil)
      (setf d-aliases nil)
      (dolist (category *categories)
         (dolist
            (prop
               '(category normalised-category altered))
            (remprop category prop)))
      (setf *categories nil)
      (dolist (extension *extensions)
         (dolist
            (prop
               '(extension normalised-extension altered))
            (remprop extension prop)))
      (setf *extensions nil)
      (dolist (top *top)
         (dolist
            (prop
               '(top normalised-top altered))
            (remprop top prop)))
      (setf *top nil)
      (dolist (idrule *id-rules)
         (dolist
            (prop
               '(idrule normalised-idrules
                   expanded-idrules compiled-idrules altered))
            (remprop idrule prop)))
      (setf *id-rules nil)
      (dolist (lprule *lp-rules)
         (progn
            (remprop lprule 'lprule)
            (remprop lprule 'altered)))
      (setf *lp-rules nil)
      (setf *reduced-lp-rules nil)
      (dolist (metarule *meta-rules)
         (dolist
            (prop
               '(metarule normalised-metarule altered))
            (remprop metarule prop)))
      (setf *meta-rules nil)
      (clear-whole-grammar1)))


(defun clear-whole-grammar1 nil
   (dolist (defrule *default-rules)
      (dolist
         (prop
            '(defrule normalised-defrule altered))
         (remprop defrule prop)))
   (setf *default-rules nil)
   (dolist (proprule *prop-rules)
      (dolist
         (prop
            '(proprule normalised-proprule altered))
         (remprop proprule prop)))
   (setf *prop-rules nil)
   (dolist (ecr *ec-rules)
      (progn
         (remprop ecr 'ecr)
         (remprop ecr 'altered)))
   (setf *ec-rules nil)
   (dolist (mr *multiply-rules)
      (progn
         (remprop mr 'mr)
         (remprop mr 'altered)))
   (setf *multiply-rules nil)
   (dolist (cc *cc-rules)
      (progn
         (remprop cc 'cc)
         (remprop cc 'altered)))
   (setf *cc-rules nil)
   (dolist
      (word (append *words *cached-words *disk-resident-words))
      (dolist
         (prop
            '(word normalised-word compiled-word
                altered))
         (remprop word prop)))
   (setf *words nil) (setf *cached-words nil)
   (setf *current-variable-name* 0)
   (setf *category-index-dnet nil)
   (setf *index-category-table nil)
   (setf *current-category-index -1)
   (setf *current-parse-trees nil)
   (g-init-parse)
   (when (fboundp 'clear-lr1-parser) (funcall 'clear-lr1-parser))
   (setf *generator-rules nil)
   (setf *generator-words nil)
   (setf *generator-nodes nil)
   (clear-grammar-files)
   (format t "Grammar cleared~%"))


;;; When 'all' is true, clear all the cached data in the system
;;; - called when an alias is redefined, or useful when
;;; debugging the GDE. Otherwise, leave compiled versions of
;;; words, and parser and generator rule and word trees - saves
;;; space when debugging word definitions when the grammar is
;;; fixed.

(defun clear-cached-grammar (all)
   (dolist (feature *features)
      (progn
         (remprop feature 'variable-list)
         (remprop feature 'variable-pointer)))
   (setf d-features nil)
   (setf d-variables nil)
   (dolist (alias *aliases)
      (remprop alias 'normalised-alias))
   (setf *sorted-aliases nil)
   (setf d-aliases nil)
   (dolist (category *categories)
      (remprop category 'normalised-category))
   (dolist (extension *extensions)
      (remprop extension 'normalised-extension))
   (dolist (top *top)
      (remprop top 'normalised-top))
   (dolist (idrule *id-rules)
      (dolist
         (prop
            '(normalised-idrules expanded-idrules
                compiled-idrules))
         (remprop idrule prop)))
   (setf *reduced-lp-rules nil)
   (dolist (metarule *meta-rules)
      (remprop metarule 'normalised-metarule))
   (dolist (defrule *default-rules)
      (remprop defrule 'normalised-defrule))
   (dolist (proprule *prop-rules)
      (remprop proprule 'normalised-proprule))
   (dolist (word (append *words *disk-resident-words))
      (progn
         (remprop word 'normalised-word)
         (when all (remprop word 'compiled-word))))
   (when all
      (dolist (word *cached-words)
         (progn
            (remprop word 'word)
            (remprop word 'compiled-word)))
      (setf *cached-words nil)
      (setf *current-variable-name* 0)
      (setf *category-index-dnet nil)
      (setf *index-category-table nil)
      (setf *current-category-index -1)
      (setf *current-parse-trees nil)
      (g-init-parse)
      (setf *generator-rules nil)
      (setf *generator-words nil)
      (setf *generator-nodes nil))
   (format t "~A~%"
      (cond
         (all "All cached data cleared")
         (t "Cached data excised"))))


;;; Delete all data in a given file. Do not allow if any
;;; features, sets or aliases in the file are used anywhere
;;; (even if only in the same file).

(defun forget-file nil
   (cond
      (*grammar-files
         (let
            ((pattern
                (prompt-if-necessary "File name? ")))
            (when pattern
               (unless
                  (mapcan
                     #'(lambda (file)
                          (cond
                             ((pathname-equal file
                                 (canonise-grammar-file-name
                                    pattern))
                                (list
                                   (progn
                                      (forget-file1 file)
                                      t)))))
                     *grammar-files)
                  (gde-cerror
                     "file has not been read in")))))
      (t
         (gde-cerror
            "no files have been read in"))))


(defun forget-file1 (file)
   (cond
      ((and
          (dolist (feature *features t)
             (cond
                ((equal
                    (feature-declaration-file
                       (get feature 'feature))
                    file)
                   (unless (is-valid-feature-deletion feature)
                      (return nil)))))
          (dolist (set *sets t)
             (cond
                ((equal
                    (set-declaration-file (get set 'set)) file)
                   (unless (is-valid-set-deletion set)
                      (return nil)))))
          (dolist (alias *aliases t)
             (cond
                ((equal
                    (alias-declaration-file (get alias 'alias))
                    file)
                   (unless (is-valid-alias-deletion alias)
                      (return nil))))))
         (dolist (feature *features)
            (cond
               ((equal
                   (feature-declaration-file
                      (get feature 'feature))
                   file)
                  (delete-feature-invalidations feature))))
         (dolist (set *sets)
            (cond
               ((equal
                   (set-declaration-file (get set 'set)) file)
                  (delete-set-invalidations set))))
         (dolist (alias *aliases)
            (cond
               ((equal
                   (alias-declaration-file (get alias 'alias))
                   file)
                  (delete-alias-invalidations alias))))
         (dolist (category *categories)
            (cond
               ((equal
                   (category-declaration-file
                      (get category 'category))
                   file)
                  (delete-category-invalidations category))))
         (dolist (extension *extensions)
            (cond
               ((equal
                   (extension-declaration-file
                      (get extension 'extension))
                   file)
                  (delete-extension-invalidations
                     extension))))
         (dolist (top *top)
            (cond
               ((equal
                   (top-declaration-file (get top 'top))
                   file)
                  (delete-top-invalidations top))))
         (dolist (idrule *id-rules)
            (cond
               ((equal (id-rule-file (get idrule 'idrule))
                   file)
                  (delete-idrule-invalidations idrule))))
         (forget-file2 file))
      (t
         (gde-cerror "file may not be forgotten"))))


(defun forget-file2 (file)
   (dolist (metarule *meta-rules)
      (cond
         ((equal
             (meta-rule-file (get metarule 'metarule))
             file)
            (delete-metarule-invalidations metarule))))
   (dolist (defrule *default-rules)
      (cond
         ((equal
             (default-rule-file (get defrule 'defrule))
             file)
            (delete-defrule-invalidations defrule))))
   (dolist (proprule *prop-rules)
      (cond
         ((equal
             (prop-rule-file (get proprule 'proprule))
             file)
            (delete-proprule-invalidations proprule))))
   (dolist (lprule *lp-rules)
      (cond
         ((equal (lp-rule-file (get lprule 'lprule))
             file)
            (delete-lprule-invalidations lprule))))
   (dolist (word (append *words *disk-resident-words))
      (cond
         ((equal
             (word-definition-file (get word 'word))
             file)
            (delete-word-invalidations word))))
   #+gde-morph
   (dolist (ecr *ec-rules)
      (cond
         ((equal (ec-rule-file (get ecr 'ecr)) file)
            (delete-ecr-invalidations ecr))))
   #+gde-morph
   (dolist (mr *multiply-rules)
      (cond
         ((equal (multiply-rule-file (get mr 'mr))
             file)
            (delete-mr-invalidations mr))))
   #+gde-morph
   (dolist (cc *cc-rules)
      (cond
         ((equal (cc-rule-file (get cc 'cc)) file)
            (delete-cc-invalidations cc))))
   (forget-grammar-file file)
   (format t "File forgotten~%"))


;;; End of file

