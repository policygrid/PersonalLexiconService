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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - GRAMMAR EDITING
;;;
;;; Author: John Carroll
;;;
;;; Top level command handlers for editing grammar definitions
;;; (the "Edit" command).
;;;
;;; Entry points:
;;;
;;;  * (defun Edit-feature () ...
;;;  * (defun Edit-set () ...
;;;  * (defun Edit-alias () ...
;;;  * (defun Edit-category (lexical) ...
;;;  * (defun Edit-extension () ...
;;;  * (defun Edit-top () ...
;;;  * (defun Edit-idrule (linear-rule) ...
;;;  * (defun Edit-metarule () ...
;;;  * (defun Edit-defrule () ...
;;;  * (defun Edit-proprule () ...
;;;  * (defun Edit-lprule () ...
;;;  * (defun Edit-word () ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Get the name of the definition for editing from the user. If
;;; already-prompted is true, then all-names contains the
;;; initial list of names.

(defun get-names-for-editing
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
                  (get-names-for-editing prompt all-names
                     type nil)))))))


(defun get-edited-definition (print-function print-args)
   (format t "Current definition is:~%")
   (apply print-function print-args)
   (prompt-always "New definition? "))


;;; Edit definitions of the various construct types.

(defun edit-feature nil
   (let
      ((name
          (get-names-for-editing "Feature name? "
             *features "features" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-feature-definition
                   (list name (get name 'feature) nil))))
            (when text
               (insert-feature-declaration text))))))


(defun edit-set nil
   (let
      ((name
          (get-names-for-editing "Set name? " *sets
             "sets" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-set-definition
                   (list name (get name 'set) nil))))
            (when text
               (insert-set-declaration text))))))


(defun edit-alias nil
   (let
      ((name
          (get-names-for-editing "Alias name? "
             *aliases "aliases" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-alias-definition
                   (list name (get name 'alias) nil))))
            (when text
               (insert-alias-declaration text))))))


(defun edit-category (lexical)
   (let
      ((name
          (get-names-for-editing
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
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-category-definition
                   (list name (get name 'category) nil))))
            (when text
               (insert-category-declaration lexical
                  text))))))


(defun edit-extension nil
   (when *extensions
      (let
         ((name (car *extensions)))
         (let
            ((text
                (get-edited-definition
                   'print-extension-definition
                   (list name (get name 'extension) nil))))
            (when text
               (insert-extension-declaration text))))))


(defun edit-top nil
   (when *top
      (let
         ((name (car *top)))
         (let
            ((text
                (get-edited-definition
                   'print-top-definition
                   (list name (get name 'top) nil))))
            (when text
               (insert-top-declaration text))))))


(defun edit-idrule (linear-rule)
   (let
      ((name
          (get-names-for-editing "Rule name? "
             (mapcan
                #'(lambda (rule)
                     (when
                        (eq (id-rule-linear (get rule 'idrule))
                           linear-rule)
                        (ncons rule)))
                *id-rules)
             "rules" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-idrule-definition
                   (list (get name 'idrule) nil))))
            (when text
               (insert-idrule-declaration text
                  linear-rule))))))


(defun edit-lprule nil
   (let
      ((name
          (get-names-for-editing "LP rule name? "
             *lp-rules "LP rules" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-lprule-definition
                   (list name (get name 'lprule) nil))))
            (when text
               (insert-lprule-declaration text))))))


(defun edit-metarule nil
   (let
      ((name
          (get-names-for-editing "Metarule name? "
             *meta-rules "metarules" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-metarule-definition
                   (list name (get name 'metarule) nil))))
            (when text
               (insert-metarule-declaration text))))))


(defun edit-defrule nil
   (let
      ((name
          (get-names-for-editing
             "Default rule name? " *default-rules
             "default rules" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-defrule-definition
                   (list name (get name 'defrule) nil))))
            (when text
               (insert-defrule-declaration text))))))


(defun edit-proprule nil
   (let
      ((name
          (get-names-for-editing
             "Propagation rule name? " *prop-rules
             "propagation rules" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-proprule-definition
                   (list name (get name 'proprule) nil))))
            (when text
               (insert-proprule-declaration text))))))


(defun edit-word nil
   (let
      ((pattern (prompt-if-necessary "Word? ")))
      (when pattern
         (setf pattern (canonise-word pattern))
         (let
            ((names
                (get-items (append *words *cached-words)
                   pattern)))
            (when
               (and (null names)
                  (get-word-definition (car pattern)
                     'normalised 'basic))
               (setf names (ncons (car pattern))))
            (let
               ((name
                   (get-names-for-editing "Word? " names
                      "words" t)))
               (when name
                  (let
                     ((text
                         (get-edited-definition
                            'print-word-definition
                            (list name (get name 'word) nil))))
                     (when text
                        (insert-word-definition text)))))))))


;;; End of file

