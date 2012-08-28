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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - GRAMMAR INPUT
;;;
;;; Author: John Carroll
;;;
;;; Top level command handlers for inputting GPSG constructs
;;; (the "Input" command).
;;;
;;; Entry points:
;;;
;;;  * (defun Insert-declaration-at-end (old-names name) ...
;;;  * (defun Input-feature () ...
;;;  * (defun Input-set () ...
;;;  * (defun Input-alias () ...
;;;  * (defun Input-category () ...
;;;  * (defun Input-extension () ...
;;;  * (defun Input-top () ...
;;;  * (defun Input-idrule () ...
;;;  * (defun Input-psrule () ...
;;;  * (defun Input-metarule () ...
;;;  * (defun Input-defrule () ...
;;;  * (defun Input-proprule () ...
;;;  * (defun Input-lprule () ...
;;;  * (defun Input-word () ...
;;;  * (defun Check-redefinition (item ...
;;;
;;; The functions which handle the insertion of the various GPSG
;;; construct definitions all make sure that they invalidate the
;;; relevant cached data if a redefinition takes place (e.g.
;;; setting to NIL the "compiled-idrules", "expanded-idrules"
;;; and "normalised-idrules" properties of a redefined ID rule).

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defun insert-declaration-at-end
   (old-names name)
   (unless *file-read
      (setf (get name 'altered) t))
   (cond
      ((member name old-names :test #'eq) old-names)
      (t (nconc old-names (ncons name)))))


(defun output-defining-message (type name)
   (when *defining-messages
      (format t "~%Defining ~A: ~A~%" type name)))


;;; The command handler called by top loop for inputting a
;;; feature declaration (the "Input feature" command).

(defun input-feature nil
   (let
      ((input
          (prompt-if-necessary
             "Feature declaration? ")))
      (when input
         (insert-feature-declaration input))))


(defun insert-feature-declaration
   (input-list)
   (let
      ((parsed-feature
          (parse-feature-declaration input-list)))
      (when
         (check-redefinition 'feature *features
            "feature" (car parsed-feature))
         (output-defining-message "feature"
            (car parsed-feature))
         (unless
            (member (car parsed-feature) *features :test #'eq)
            (setf
               (get (car parsed-feature)
                  'feature-order)
               (1+
                  (if *features
                     (get (car (last *features)) 'feature-order)
                     0))))
         (setf
            (get (car parsed-feature) 'feature)
            (cadr parsed-feature))
         (setf *features
            (insert-declaration-at-end *features
               (car parsed-feature)))
         (setf d-features nil)
         (setf d-variables nil))))


;;; The command handler called by top loop for inputting a set
;;; declaration (the "Input set" command). Altering a set may
;;; potentially change the normalised form of category, default
;;; and propagation rules, and so maybe the expanded forms of ID
;;; rules and compiled words that they were applied to.

(defun input-set nil
   (let
      ((input
          (prompt-if-necessary "Set declaration? ")))
      (when input
         (insert-set-declaration input))))


(defun insert-set-declaration (input-list)
   (let
      ((parsed-set
          (parse-set-declaration input-list)))
      (when
         (check-redefinition 'set *sets "set"
            (car parsed-set))
         (output-defining-message "set"
            (car parsed-set))
         (setf (get (car parsed-set) 'set)
            (cadr parsed-set))
         (setf *sets
            (insert-declaration-at-end *sets
               (car parsed-set)))
         (dolist (name *default-rules)
            (remprop name 'normalised-defrule))
         (dolist (name *prop-rules)
            (remprop name 'normalised-proprule))
         (dolist (name *categories)
            (remprop name 'normalised-category))
         (input-idrule-invalidations *id-rules
            'expanded)
         (dolist (word *cached-words)
            (delete-word-invalidations word))
         (input-word-invalidations *words 'compiled)
         (setf d-whead nil)
         (setf d-wdaughter nil))))


;;; The command handler called by top loop for inputting an
;;; alias declaration (the "Input alias" command). Note that the
;;; inputting of an alias may invalidate the current
;;; normalisations of some lprules, idrules and metarules.

;;; If an alias has been changed, call Clear-cached-grammar to
;;; throw away all normalisations of aliases, lprules, idrules,
;;; metarules and propagation rules. One day it might be nice to
;;; keep a dependency structure recording which aliases are used
;;; in which rules so the invalidation can be more intelligent
;;; but for now just invalidate every normalisation in the
;;; system.

(defun input-alias nil
   (let
      ((input
          (prompt-if-necessary
             "Alias declaration? ")))
      (when input
         (insert-alias-declaration input))))


(defun insert-alias-declaration
   (input-list)
   (let
      ((parsed-alias
          (parse-alias-declaration input-list)))
      (when
         (check-redefinition 'alias *aliases "alias"
            (car parsed-alias))
         (output-defining-message "alias"
            (car parsed-alias))
         (setf (get (car parsed-alias) 'alias)
            (cadr parsed-alias))
         (when
            (member (car parsed-alias) *aliases :test #'eq)
            (clear-cached-grammar t))
         (setf *aliases
            (insert-declaration-at-end *aliases
               (car parsed-alias)))
         (setf *sorted-aliases nil)
         (setf d-aliases nil))))


;;; The command handler called by top loop for inputting a set
;;; declaration (the "Input category command). Altering a
;;; category may potentially change the compiled form of words
;;; and the expanded forms of ID rules also.

(defun input-category (lexical)
   (let
      ((input
          (prompt-if-necessary
             "Category declaration? ")))
      (when input
         (insert-category-declaration lexical
            input))))


(defun insert-category-declaration (lexical input-list)
   (let
      ((parsed-category
          (parse-category-declaration lexical input-list)))
      (when
         (check-redefinition 'category *categories
            "category" (car parsed-category))
         (output-defining-message "category"
            (car parsed-category))
         (setf
            (get (car parsed-category) 'category)
            (cadr parsed-category))
         (remprop (car parsed-category)
            'normalised-category)
         (setf *categories
            (insert-declaration-at-end *categories
               (car parsed-category)))
         (input-idrule-invalidations
            (catrule-invalidated-idrules
               (cadr parsed-category)
               (car parsed-category))
            'expanded)
         (input-word-invalidations
            (append *words *cached-words) 'normalised))))


;;; The command handler called by top loop for inputting an
;;; extension declaration (the "Input extension command). Check
;;; that all features currently declared are either part of
;;; category declarations, or are extensions. This is only
;;; relevant for full category matching - just omit extension
;;; declaration for matching by extension.

(defun input-extension nil
   (let
      ((input
          (prompt-if-necessary
             "Extension declaration? ")))
      (when input
         (insert-extension-declaration input))))


(defun insert-extension-declaration
   (input-list)
   (let
      ((parsed-extension
          (parse-extension-declaration input-list)))
      (when
         (check-redefinition 'extension *extensions
            "extension set"
            (car parsed-extension))
         (output-defining-message "extension"
            (car parsed-extension))
         (setf
            (get (car parsed-extension)
               'extension)
            (cadr parsed-extension))
         (setf *extensions
            (insert-declaration-at-end *extensions
               (car parsed-extension)))
         (remprop (car parsed-extension)
            'normalised-extension)
         (check-extension-features
            (car parsed-extension)))))


(defun check-extension-features
   (extension-name)
   (let
      ((category-features
          (mapcan
             #'(lambda (cat-dec)
                  (copy-list
                     (category-declaration-features
                        (normalise-category-definition
                           cat-dec))))
             *categories))
         (extension-features
            (extension-declaration-features
               (normalise-extension-definition
                  extension-name))))
      (let
         ((extra-features
             (mapcan
                #'(lambda (feature)
                     (unless
                        (or (member feature category-features :test #'eq)
                           (member feature extension-features :test #'eq))
                        (ncons feature)))
                *features)))
         (when (and extra-features *warning-messages)
            (terpri)
            (gde-warn
"not all non-extension features are category features")
            (write-construct-names "Features are: "
               extra-features)))))


;;; The command handler called by top loop for inputting an
;;; top declaration (the "Input top" command).

(defun input-top nil
   (let
      ((input
          (prompt-if-necessary "Top declaration? ")))
      (when input
         (insert-top-declaration input))))


(defun insert-top-declaration (input-list)
   (let
      ((parsed-top
          (parse-top-declaration input-list)))
      (when
         (check-redefinition 'top *top "top"
            (car parsed-top))
         (output-defining-message "top"
            (car parsed-top))
         (setf
            (get (car parsed-top) 'top)
            (cadr parsed-top))
         (setf *top
            (insert-declaration-at-end *top
               (car parsed-top)))
         (remprop (car parsed-top)
            'normalised-top))))


;;; The command handler called by top loop for inputting an ID
;;; or PS rule declaration (the "Input idrule" / psrule
;;; command).

(defun input-idrule nil
   (let
      ((input
          (prompt-if-necessary
             "ID rule declaration? ")))
      (when input
         (insert-idrule-declaration input nil))))


(defun input-psrule nil
   (let
      ((input
          (prompt-if-necessary
             "PS rule declaration? ")))
      (when input
         (insert-idrule-declaration input t))))


(defun insert-idrule-declaration
   (input-list linear-rule)
   (let
      ((parsed-rule
          (parse-idrule-declaration input-list
             linear-rule)))
      (when
         (check-redefinition 'idrule *id-rules
            "rule" (car parsed-rule))
         (output-defining-message
            (if (id-rule-linear (cadr parsed-rule))
               "PS rule" "ID rule")
            (car parsed-rule))
         (setf (get (car parsed-rule) 'idrule)
            (cadr parsed-rule))
         (setf *id-rules
            (insert-declaration-at-end *id-rules
               (car parsed-rule)))
         (input-idrule-invalidations
            (ncons (car parsed-rule))
            'normalised))))


;;; The command handler called by top loop for inputting an LP
;;; rule declaration (the "Input lprule" command).

(defun input-lprule nil
   (let
      ((input
          (prompt-if-necessary
             "LP rule declaration? ")))
      (when input
         (insert-lprule-declaration input))))


(defun insert-lprule-declaration
   (input-list)
   (let
      ((parsed-rule
          (parse-lprule-declaration input-list)))
      (when
         (check-redefinition 'lprule *lp-rules
            "LP rule" (car parsed-rule))
         (output-defining-message "LP rule"
            (car parsed-rule))
         (setf (get (car parsed-rule) 'lprule)
            (cadr parsed-rule))
         (setf *lp-rules
            (insert-declaration-at-end *lp-rules
               (car parsed-rule)))
         (setf *reduced-lp-rules nil)
         (remprop (car parsed-rule)
            'normalised-lprule)
         ;; cannot re-use compiled ID rules since variables may
         ;; have been 'unnamed'
         (input-idrule-invalidations *id-rules
            'expanded))))


;;; The command handler called by top loop for inputting a Meta
;;; rule declaration (the "Input metarule" command). Even if new
;;; metarule is linear, must invalidate old expanded
;;; (non-linearised) ID rules since metarule may be redefining
;;; an old non-linear metarule.

(defun input-metarule nil
   (let
      ((input
          (prompt-if-necessary
             "Metarule declaration? ")))
      (when input
         (insert-metarule-declaration input))))


(defun insert-metarule-declaration
   (input-list)
   (let
      ((parsed-rule
          (parse-metarule-declaration input-list)))
      (when
         (check-redefinition 'metarule *meta-rules
            "metarule" (car parsed-rule))
         (output-defining-message "metarule"
            (car parsed-rule))
         (setf
            (get (car parsed-rule) 'metarule)
            (cadr parsed-rule))
         (setf *meta-rules
            (insert-declaration-at-end *meta-rules
               (car parsed-rule)))
         (remprop (car parsed-rule)
            'normalised-metarule)
         (input-idrule-invalidations
            (metarule-invalidated-idrules
               (cadr parsed-rule) (car parsed-rule))
            'expanded))))


;;; The command handler called by top loop for inputting a
;;; default rule declaration (the "Input defrule command).

(defun input-defrule nil
   (let
      ((input
          (prompt-if-necessary
             "Default rule declaration? ")))
      (when input
         (insert-defrule-declaration input))))


(defun insert-defrule-declaration
   (input-list)
   (let
      ((parsed-rule
          (parse-defrule-declaration input-list)))
      (when
         (check-redefinition 'defrule *default-rules
            "default rule" (car parsed-rule))
         (output-defining-message "default rule"
            (car parsed-rule))
         (setf (get (car parsed-rule) 'defrule)
            (cadr parsed-rule))
         (setf *default-rules
            (insert-declaration-at-end *default-rules
               (car parsed-rule)))
         (remprop (car parsed-rule)
            'normalised-defrule)
         (input-idrule-invalidations
            (defrule-invalidated-idrules
               (cadr parsed-rule) (car parsed-rule))
            'expanded))))


;;; The command handler called by top loop for inputting a
;;; feature propagation rule (the "Input proprule" command).

(defun input-proprule nil
   (let
      ((input
          (prompt-if-necessary
             "Propagation rule declaration? ")))
      (when input
         (insert-proprule-declaration input))))


(defun insert-proprule-declaration
   (input-list)
   (let
      ((parsed-rule
          (parse-proprule-declaration input-list)))
      (when
         (check-redefinition 'proprule *prop-rules
            "propagation rule" (car parsed-rule))
         (output-defining-message "propagation rule"
            (car parsed-rule))
         (setf
            (get (car parsed-rule) 'proprule)
            (cadr parsed-rule))
         (setf *prop-rules
            (insert-declaration-at-end *prop-rules
               (car parsed-rule)))
         (remprop (car parsed-rule)
            'normalised-proprule)
         (input-idrule-invalidations
            (proprule-invalidated-idrules
               (cadr parsed-rule) (car parsed-rule))
            'expanded))))


;;; The command handler called by top loop for inputting a word.
;;; The word goes into the "small lexicon" maintained by the
;;; GDE, rather than the morphology package's large dictionary.
;;; Slightly different since additionally asks if new definition
;;; is to be appended to old if exists.

(defun input-word nil
   (let
      ((input
          (prompt-if-necessary
             "Word with definition? ")))
      (when input
         (insert-word-definition input))))


(defun insert-word-definition (input-list)
   (let* ((on-disk-p
             (and *file-read (file-disk-resident-p *file-read)))
          (start-pos *file-read-position)
          (parsed-word (parse-word-definition input-list)))
      (cond
         ((and on-disk-p
               (not (member (car parsed-word) (append *words *cached-words))))
            ;; this is a disk-resident file and word is not already defined in
            ;; a non-disk-resident file
            (insert-word-definition1
               (car parsed-word)
               (or (get (car parsed-word) 'word) (make-word-definition :file *file-read))
               start-pos))
         ((check-redefinition 'word
             (append *words *cached-words *disk-resident-words) "word"
             (car parsed-word))
            (insert-word-definition1
               (car parsed-word) (cadr parsed-word) nil))
         ((yes-for-question "Add to existing definition")
            (insert-word-definition1
               (car parsed-word)
               (make-word-definition :senses
                  (append
                     (word-definition-senses
                        (cadr parsed-word))
                     (word-definition-senses
                        (get (car parsed-word) 'word)))
                  :file
                  (or
                     (word-definition-file
                        (get (car parsed-word) 'word))
                     (word-definition-file
                        (cadr parsed-word)))
                  :comment
                  (add-input-comment
                     (word-definition-comment
                        (cadr parsed-word))
                     (word-definition-comment
                        (get (car parsed-word)
                           'word))))
               nil)))))


(defun insert-word-definition1 (word definition start-pos)
   (setf *cached-words (remove-list-1 word *cached-words))
   (if start-pos
      (progn
         ;; reading disk-resident file
         (output-defining-message "word (disk-resident)" word)
         (push (list* *file-read (file-write-date *file-read) start-pos)
            (word-definition-senses definition))
         (setf *disk-resident-words
            (insert-declaration-at-end *disk-resident-words word))
         (setf *words
            (remove-list-1 word *words)))
      (progn
         ;; reading non-disk-resident file
         (output-defining-message "word" word)
         (setf *words
            (insert-declaration-at-end *words word))
         (setf *disk-resident-words
            (remove-list-1 word *disk-resident-words))))
   (setf (get word 'word) definition)
   (input-word-invalidations (ncons word) 'normalised))


;;; A general purpose function for checking the possible
;;; redefinition of any construct. If the instance of the
;;; construct is already defined and we're reading from the
;;; terminal, then give the user the option of redefining. If it
;;; is already defined and we're reading from file, then throw
;;; out with an error.

(defun check-redefinition
   (item construct-names type name)
   (cond
      ((not (member name construct-names :test #'eq))
         (if (get name item)
            (gde-ferror type " " name
               " is already, but inconsistently, defined
You are strongly advised to exit from the GDE and start again")
            t))
      (*file-read
         (gde-ferror type " " name
            " is already defined"))
      (t
         (check-redefinition-ok item type name))))


;;; A general purpose function for asking about the redefinition
;;; of any grammar item. "item" is the type of construct
;;; involved (e.g. feature, set, etc.).

(defun check-redefinition-ok (item type name)
   (terpri)
   (gde-warn type " is already defined")
   (format t "Current definition is: ~%")
   (cond
      ((eq item 'feature)
         (print-feature-definition name
            (get name 'feature) nil))
      ((eq item 'set)
         (print-set-definition name (get name 'set)
            nil))
      ((eq item 'alias)
         (print-alias-definition name
            (get name 'alias) nil))
      ((eq item 'category)
         (print-category-definition name
            (get name 'category) nil))
      ((eq item 'extension)
         (print-extension-definition name
            (get name 'extension) nil))
      ((eq item 'top)
         (print-top-definition name
            (get name 'top) nil))
      ((eq item 'idrule)
         (print-idrule-definition (get name 'idrule)
            nil))
      ((eq item 'lprule)
         (print-lprule-definition name
            (get name 'lprule) nil))
      ((eq item 'metarule)
         (print-metarule-definition name
            (get name 'metarule) nil))
      ((eq item 'defrule)
         (print-defrule-definition name
            (get name 'defrule) nil))
      ((eq item 'proprule)
         (print-proprule-definition name
            (get name 'proprule) nil))
      #+gde-morph
      ((eq item 'ecr)
         (print-ecr-definition name (get name 'ecr)
            nil))
      #+gde-morph
      ((eq item 'mr)
         (print-mr-definition name (get name 'mr)
            nil))
      #+gde-morph
      ((eq item 'cc)
         (print-cc-definition name (get name 'cc)
            nil))
      ((eq item 'word)
         (print-word-definition name
            (get name 'word) nil)))
   (yes-for-question
      "Do you want to replace existing definition"))


;;; End of file

