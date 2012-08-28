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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - GRAMMAR ITEM ORDERING
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code for ordering lists of rules
;;; where the order in which they are processed or applied makes
;;; a difference to the results they give.
;;;
;;; Entry points:
;;;
;;;  * (defun Order-features () ...
;;;  * (defun Order-sets () ...
;;;  * (defun Order-categories () ...
;;;  * (defun Order-aliases () ...
;;;  * (defun Order-idrules () ...
;;;  * (defun Order-metarules () ...
;;;  * (defun Order-proprules () ...
;;;  * (defun Order-defrules () ...
;;;  * (defun Order-lprules () ...
;;;  * (defun Order-words () ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defvar *order-top-commands nil)


(setf *order-top-commands
   (list
      (make-command-entry :shortest 1 :name 'view
         :action '(print-rule-ordering))
      (make-command-entry :shortest 1 :name 'move
         :action '(move-in-ordering))
      (make-command-entry :shortest 1 :name 'help
         :action '(give-order-help))
      (make-command-entry :shortest 1 :name '?
         :action '(give-order-help))
      (make-command-entry :shortest 1 :name '\!
         :action '(process-lisp-input))
      (make-command-entry :shortest 1 :name
         'shell :action '(shell))
      (make-command-entry :shortest 1 :name 'lisp
         :action '(lisp-top-loop))))


(defun give-order-help nil
   (give-help *order-help-file))


;;; The top level rule ordering command entry points. They call
;;; a higher level top loop for the user to interact in
;;; producing the new order. Reordering some construct types
;;; makes no difference to the grammar (i.e. pure declarative
;;; constructs like sets and words).

(defvar rule-ordering nil)


;;; If feature order has changed then invalidate expanded ID
;;; rules as well as compiled property since if there are no
;;; linear metarules expanded property categories do not get
;;; sorted before being added to compiled property.

(defun order-features nil
   (cond
      (*features
         (let
            ((rule-ordering *features))
            (order-top-loop)
            (unless (equal *features rule-ordering)
               (setf *features rule-ordering)
               (let
                  ((feature-order 0))
                  (dolist (feature *features)
                     (setf (get feature 'feature-order)
                        (setf feature-order (1+ feature-order)))))
               (input-idrule-invalidations *id-rules
                  'expanded)
               (input-word-invalidations
                  (append *words *cached-words) 'compiled))))
      (t (gde-cerror "no features defined"))))


(defun order-sets nil
   (cond
      (*sets
         (let
            ((rule-ordering *sets))
            (order-top-loop)
            (setf *sets rule-ordering)))
      (t (gde-cerror "no sets defined"))))


(defun order-categories (lexical)
   (cond
      (*categories
         (let
            ((rule-ordering *categories))
            (order-top-loop)
            (unless (eq rule-ordering *categories)
               (setf *categories rule-ordering)
               (input-idrule-invalidations *id-rules
                  'expanded)
               (if lexical
                  (input-word-invalidations
                     (append *words *cached-words)
                     'compiled)))))
      (t
         (gde-cerror
            "no category statements defined"))))


(defun order-aliases nil
   (cond
      (*aliases
         (let
            ((rule-ordering *aliases))
            (order-top-loop)
            (setf *aliases rule-ordering)))
      (t (gde-cerror "no aliases defined"))))


(defun order-idrules nil
   (cond
      (*id-rules
         (let
            ((rule-ordering *id-rules))
            (order-top-loop)
            (unless (eq rule-ordering *id-rules)
               (setf *id-rules rule-ordering)
               (input-idrule-invalidations nil nil))))
      (t (gde-cerror "no ID rules defined"))))


(defun order-metarules nil
   (cond
      (*meta-rules
         (let
            ((rule-ordering *meta-rules))
            (order-top-loop)
            (unless (eq rule-ordering *meta-rules)
               (setf *meta-rules rule-ordering)
               (input-idrule-invalidations *id-rules
                  'expanded))))
      (t (gde-cerror "no metarules defined"))))


(defun order-proprules nil
   (cond
      (*prop-rules
         (let
            ((rule-ordering *prop-rules))
            (order-top-loop)
            (unless (eq rule-ordering *prop-rules)
               (setf *prop-rules rule-ordering)
               (input-idrule-invalidations *id-rules
                  'expanded))))
      (t
         (gde-cerror
            "no propagation rules defined"))))


(defun order-defrules nil
   (cond
      (*default-rules
         (let
            ((rule-ordering *default-rules))
            (order-top-loop)
            (unless (eq rule-ordering *default-rules)
               (setf *default-rules rule-ordering)
               (input-idrule-invalidations *id-rules
                  'expanded))))
      (t
         (gde-cerror "no default rules defined"))))


(defun order-lprules nil
   (cond
      (*lp-rules
         (let
            ((rule-ordering *lp-rules))
            (order-top-loop)
            (setf *lp-rules rule-ordering)))
      (t (gde-cerror "no LP rules defined"))))


(defun order-words nil
   (cond
      (*words
         (let
            ((rule-ordering *words))
            (order-top-loop)
            (setf *words rule-ordering)))
      (t (gde-cerror "no words defined"))))


;;; Define a top loop for the order rule commands. The Top-loop
;;; function provides a cutomisable "read-eval-print" loop

(defun order-top-loop nil
   (print-rule-ordering)
   (top-loop #'gde-top-print
      #'order-top-eval "Order"
      "GDE Rule Re-ordering"))


(defun order-top-eval (x)
   (process-command (get-reply1 x) *order-top-commands)
   'nothing)


;;; Print the current rule ordering.

(defun print-rule-ordering nil
   (write-construct-names "" rule-ordering))


;;; Move a rule name from its current position and insert it
;;; after another given position.

(defun move-in-ordering nil
   (let
      ((input
          (prompt-if-necessary
             "Name of item to be moved? ")))
      (when input
         (let
            ((rule-input
                (member (car input) rule-ordering)))
            (cond
               (rule-input
                  (let
                     ((new-input
                         (or (cdr input)
                            (prompt-always
"Insert after? (* for before all) "))))
                     (cond
                        ((null new-input))
                        ((eq (car rule-input)
                            (car new-input))
                           (gde-cerror
                              "that's where the item is now"))
                        (t
                           (let
                              ((new-position
                                  (member (car new-input)
                                     rule-ordering)))
                              (cond
                                 ((eq (car new-input) '*)
                                    (setf rule-ordering
                                       (cons
                                          (car rule-input)
                                          (remove-list-1
                                             (car
                                                rule-input)
                                             rule-ordering)))
                                    (print-rule-ordering))
                                 ((null new-position)
                                    (gde-cerror
                                       "item not found"))
                                 (t
                                    (setf rule-ordering
                                       (move-in-ordering1
                                          (car rule-input)
                                          (car
                                             new-position)
                                          rule-ordering))
(print-rule-ordering))))))))
               (t
                  (gde-cerror
                     "could not find item to be moved")))))))


(defun move-in-ordering1
   (rule-name new-position rules)
   (cond
      ((null rules) nil)
      ((eq rule-name (car rules))
         (move-in-ordering1 rule-name new-position
            (cdr rules)))
      ((eq (car rules) new-position)
         (cons new-position
            (cons rule-name
               (move-in-ordering1 rule-name new-position
                  (cdr rules)))))
      (t
         (cons (car rules)
            (move-in-ordering1 rule-name new-position
               (cdr rules))))))


;;; End of file

