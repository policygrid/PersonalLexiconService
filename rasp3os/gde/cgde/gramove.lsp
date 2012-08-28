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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - GRAMMAR ITEM RENAMING
;;;
;;; Author: John Carroll
;;;
;;; Top level command handlers for renaming grammar definitions
;;; (the "Rename" command).
;;;
;;; Entry points:
;;;
;;;  * (defun Move-feature () ...
;;;  * (defun Move-set () ...
;;;  * (defun Move-alias () ...
;;;  * (defun Move-category (lexical) ...
;;;  * (defun Move-extension () ...
;;;  * (defun Move-top () ...
;;;  * (defun Move-idrule (linear-rule) ...
;;;  * (defun Move-metarule () ...
;;;  * (defun Move-defrule () ...
;;;  * (defun Move-proprule () ...
;;;  * (defun Move-lprule () ...
;;;  * (defun Move-word () ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Get the name of the definition for renaming from the user.
;;; If already-prompted is true, then all-names contains the
;;; initial list of names.

(defun get-names-for-moving
   (prompt all-names type already-prompted)
   (let
      ((input
          (or already-prompted
             (prompt-if-necessary prompt))))
      (when input
         (let
            ((names
                (if already-prompted all-names
                   (mapcan
                      #'(lambda (name)
                           (when (symbolp name) (ncons name)))
                      (get-items all-names input)))))
            (cond
               ((null names)
                  (gde-ferror "definition not found"))
               ((null (cdr names)) (car names))
               ((prompt-for-alternative type
                   (mapcar
                      #'(lambda (item)
                           (cons item item))
                      names)))
               (t
                  (get-names-for-moving prompt all-names type
                     nil)))))))


(defun get-filename-for-moving
   (existing-name)
   (let
      ((input
          (prompt-always
             "Move to file (return for same)? ")))
      (if input
         (let ((file (canonise-grammar-file-name input)))
            (cond
               ((and
                   (not
                      (member file *grammar-files :test
                         #'pathname-equal))
                   (probe-file file))
                  (gde-ferror
"file already exists but has not been read in"))
               (t (reorder-grammar-files file))))
         existing-name)))


;;; Move definitions of the various construct types.

(defun move-feature nil
   (let
      ((name
          (get-names-for-moving "Feature name? "
             *features "features" nil)))
      (if name
         (let
            ((input
                (and (is-valid-feature-deletion name)
                   (prompt-if-necessary
                      "Change name to (return for same)? "))))
            (if input
               (cond
                  ((member (car input) *features :test #'eq)
                     (gde-ferror
"a feature with that name already exists"))
                  (t
                     (setf (get (car input) 'feature)
                        (get name 'feature))
                     (rplaca (member name *features :test #'eq)
                        (car input))
                     (remprop name 'feature)
                     (remprop name 'altered)
                     (setf name (car input))
                     (setf (get name 'altered) t))))
            (setf (get name 'feature)
               (let
                  ((structure-42
                      (copy-feature-declaration
                         (get name 'feature))))
                  (setf
                     (feature-declaration-file structure-42)
                     (get-filename-for-moving
                        (feature-declaration-file
                           structure-42)))
                  structure-42))))))


(defun move-set nil
   (let
      ((name
          (get-names-for-moving "Set name? " *sets
             "sets" nil)))
      (if name
         (let
            ((input
                (and (is-valid-set-deletion name)
                   (prompt-if-necessary
                      "Change name to (return for same)? "))))
            (if input
               (cond
                  ((member (car input) *sets :test #'eq)
                     (gde-ferror
                        "a set with that name already exists"))
                  (t
                     (setf (get (car input) 'set)
                        (get name 'set))
                     (rplaca (member name *sets :test #'eq)
                        (car input))
                     (remprop name 'set)
                     (remprop name 'altered)
                     (setf name (car input))
                     (setf (get name 'altered) t))))
            (setf (get name 'set)
               (let
                  ((structure-43
                      (copy-set-declaration (get name 'set))))
                  (setf (set-declaration-file structure-43)
                     (get-filename-for-moving
                        (set-declaration-file structure-43)))
                  structure-43))))))


(defun move-alias nil
   (let
      ((name
          (get-names-for-moving "Alias name? "
             *aliases "aliases" nil)))
      (if name
         (let
            ((input
                (and (is-valid-alias-deletion name)
                   (prompt-if-necessary
                      "Change name to (return for same)? "))))
            (if input
               (cond
                  ((member (car input) *aliases :test #'eq)
                     (gde-ferror
"an alias with that name already exists"))
                  (t
                     (setf (get (car input) 'alias)
                        (get name 'alias))
                     (rplaca (member name *aliases :test #'eq)
                        (car input))
                     (setf *sorted-aliases nil)
                     (remprop name 'alias)
                     (remprop name 'normalised-alias)
                     (remprop name 'altered)
                     (setf name (car input))
                     (setf (get name 'altered) t))))
            (setf (get name 'alias)
               (let
                  ((structure-44
                      (copy-alias-declaration
                         (get name 'alias))))
                  (setf (alias-declaration-file structure-44)
                     (get-filename-for-moving
                        (alias-declaration-file structure-44)))
                  structure-44))))))


(defun move-category (lexical)
   (let
      ((name
          (get-names-for-moving
             "Category rule name? "
             (mapcan
                #'(lambda (decl)
                     (when
                        (eq
                           (category-declaration-lexical
                              (get decl 'category))
                           lexical)
                        (ncons decl)))
                *categories)
             "categories" nil)))
      (if name
         (let
            ((input
                (prompt-if-necessary
                   "Change name to (return for same)? ")))
            (if input
               (cond
                  ((member (car input) *categories :test #'eq)
                     (gde-ferror
"a category declaration with that name already exists"))
                  (t
                     (setf (get (car input) 'category)
                        (get name 'category))
                     (rplaca (member name *categories :test #'eq)
                        (car input))
                     (remprop name 'altered)
                     (input-idrule-invalidations *id-rules
                        'compiled)
                     (input-word-invalidations
                        (append *words *cached-words)
                        'compiled)
                     (remprop name 'category)
                     (remprop name 'normalised-category)
                     (setf name (car input))
                     (setf (get name 'altered) t))))
            (setf (get name 'category)
               (let
                  ((structure-45
                      (copy-category-declaration
                         (get name 'category))))
                  (setf
                     (category-declaration-file structure-45)
                     (get-filename-for-moving
                        (category-declaration-file
                           structure-45)))
                  structure-45))))))


(defun move-extension nil
   (if *extensions
      (let ((name (car *extensions)))
         (setf (get name 'extension)
            (let
               ((structure-42
                     (copy-extension-declaration
                        (get name 'extension))))
               (setf
                  (extension-declaration-file structure-42)
                  (get-filename-for-moving
                     (extension-declaration-file
                        structure-42)))
               structure-42)))
      (gde-cerror "no extension declared")))


(defun move-top nil
   (if *top
      (let ((name (car *top)))
         (setf (get name 'top)
            (let
               ((structure-42
                     (copy-top-declaration (get name 'top))))
               (setf
                  (top-declaration-file structure-42)
                  (get-filename-for-moving
                     (top-declaration-file
                        structure-42)))
               structure-42)))
      (gde-cerror "no top declared")))


(defun move-idrule (linear-rule)
   (let
      ((name
          (get-names-for-moving "Rule name? "
             (mapcan
                #'(lambda (rule)
                     (when
                        (eq (id-rule-linear (get rule 'idrule))
                           linear-rule)
                        (ncons rule)))
                *id-rules)
             "rules" nil)))
      (if name
         (let
            ((input
                (prompt-if-necessary
                   "Change name to (return for same)? ")))
            (if input
               (cond
                  ((member (car input) *id-rules :test #'eq)
                     (gde-ferror
"a rule with that name already exists"))
                  (t
                     (setf (get (car input) 'idrule)
                        (get name 'idrule))
                     (rplaca (member name *id-rules :test #'eq)
                        (car input))
                     (input-idrule-invalidations (ncons name)
                        'normalised)
                     (remprop name 'altered)
                     (remprop name 'idrule)
                     (setf name (car input))
                     (setf (get name 'altered) t)
                     (setf (get name 'idrule)
                        (let
                           ((structure-47
                               (copy-id-rule
                                  (get name 'idrule))))
                           (setf (id-rule-name structure-47)
                              (make-top-rule-name :base
                                 (make-sub-rule-name :base name
                                    :index nil :split nil)
                                 :meta-names nil))
                           structure-47)))))
            (setf (get name 'idrule)
               (let
                  ((structure-46
                      (copy-id-rule (get name 'idrule))))
                  (setf (id-rule-file structure-46)
                     (get-filename-for-moving
                        (id-rule-file structure-46)))
                  structure-46))))))


(defun move-lprule nil
   (let
      ((name
          (get-names-for-moving "LP rule name? "
             *lp-rules "LP rules" nil)))
      (if name
         (let
            ((input
                (prompt-if-necessary
                   "Change name to (return for same)? ")))
            (if input
               (cond
                  ((member (car input) *lp-rules :test #'eq)
                     (gde-ferror
"an LP rule with that name already exists"))
                  (t
                     (setf (get (car input) 'lprule)
                        (get name 'lprule))
                     (rplaca (member name *lp-rules :test #'eq)
                        (car input))
                     (remprop name 'altered)
                     (remprop name 'lprule)
                     (remprop name 'normalised-lprule)
                     (setf name (car input))
                     (setf (get name 'altered) t))))
            (setf (get name 'lprule)
               (let
                  ((structure-48
                      (copy-lp-rule (get name 'lprule))))
                  (setf (lp-rule-file structure-48)
                     (get-filename-for-moving
                        (lp-rule-file structure-48)))
                  structure-48))))))


(defun move-metarule nil
   (let
      ((name
          (get-names-for-moving "Metarule name? "
             *meta-rules "metarules" nil)))
      (if name
         (let
            ((input
                (prompt-if-necessary
                   "Change name to (return for same)? ")))
            (if input
               (cond
                  ((member (car input) *meta-rules :test #'eq)
                     (gde-ferror
"a metarule with that name already exists"))
                  (t
                     (setf (get (car input) 'metarule)
                        (get name 'metarule))
                     (rplaca (member name *meta-rules :test #'eq)
                        (car input))
                     (input-idrule-invalidations *id-rules
                        'compiled)
                     (input-idrule-invalidations
                        (metarule-invalidated-idrules
                           (get name 'metarule) name)
                        'expanded)
                     (remprop name 'altered)
                     (remprop name 'metarule)
                     (remprop name 'normalised-metarule)
                     (setf name (car input))
                     (setf (get name 'altered) t))))
            (setf (get name 'metarule)
               (let
                  ((structure-49
                      (copy-meta-rule (get name 'metarule))))
                  (setf (meta-rule-file structure-49)
                     (get-filename-for-moving
                        (meta-rule-file structure-49)))
                  structure-49))))))


(defun move-defrule nil
   (let
      ((name
          (get-names-for-moving "Default rule name? "
             *default-rules "default rules" nil)))
      (if name
         (let
            ((input
                (prompt-if-necessary
                   "Change name to (return for same)? ")))
            (if input
               (cond
                  ((member (car input) *default-rules :test #'eq)
                     (gde-ferror
"a default rule with that name already exists"))
                  (t
                     (setf (get (car input) 'defrule)
                        (get name 'defrule))
                     (rplaca (member name *default-rules :test #'eq)
                        (car input))
                     (input-idrule-invalidations
                        (defrule-invalidated-idrules
                           (get name 'defrule) name)
                        'expanded)
                     (remprop name 'altered)
                     (remprop name 'defrule)
                     (remprop name 'normalised-defrule)
                     (setf name (car input))
                     (setf (get name 'altered) t))))
            (setf (get name 'defrule)
               (let
                  ((structure-50
                      (copy-default-rule (get name 'defrule))))
                  (setf (default-rule-file structure-50)
                     (get-filename-for-moving
                        (default-rule-file structure-50)))
                  structure-50))))))


(defun move-proprule nil
   (let
      ((name
          (get-names-for-moving
             "Propagation rule name? " *prop-rules
             "propagation rules" nil)))
      (if name
         (let
            ((input
                (prompt-if-necessary
                   "Change name to (return for same)? ")))
            (if input
               (cond
                  ((member (car input) *prop-rules :test #'eq)
                     (gde-ferror
"a propagation rule with that name already exists"))
                  (t
                     (setf (get (car input) 'proprule)
                        (get name 'proprule))
                     (rplaca (member name *prop-rules :test #'eq)
                        (car input))
                     (input-idrule-invalidations
                        (proprule-invalidated-idrules
                           (get name 'proprule) name)
                        'expanded)
                     (remprop name 'altered)
                     (remprop name 'proprule)
                     (remprop name 'normalised-proprule)
                     (setf name (car input))
                     (setf (get name 'altered) t))))
            (setf (get name 'proprule)
               (let
                  ((structure-51
                      (copy-prop-rule (get name 'proprule))))
                  (setf (prop-rule-file structure-51)
                     (get-filename-for-moving
                        (prop-rule-file structure-51)))
                  structure-51))))))


(defun move-word nil
   (let
      ((patt (prompt-if-necessary "Word? ")))
      (if patt
         (let
            ((name
                (get-names-for-moving "Word? "
                   (get-items *words (canonise-word patt))
                   "words" t)))
            (if name
               (let
                  ((input
                      (canonise-word
                         (prompt-if-necessary
"Change name to (return for same)? "))))
                  (if input
                     (cond
                        ((member (car input) *words :test #'eq)
                           (gde-ferror
"a word with that name already exists"))
                        (t
                           (setf (get (car input) 'word)
                              (get name 'word))
                           (rplaca (member name *words :test #'eq)
                              (car input))
                           (input-word-invalidations
                              (ncons name) 'normalised)
                           (remprop name 'altered)
                           (remprop name 'word)
                           (setf name (car input))
                           (setf (get name 'altered) t))))
                  (setf (get name 'word)
                     (let
                        ((structure-52
                            (copy-word-definition
                               (get name 'word))))
                        (setf
                           (word-definition-file structure-52)
                           (get-filename-for-moving
                              (word-definition-file
                                 structure-52)))
                        structure-52))))))))


;;; End of file

