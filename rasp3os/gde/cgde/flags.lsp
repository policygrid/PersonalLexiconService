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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - FLAG COMMANDS AND HELP
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code for setting and displaying the
;;; various global options (the "Set" and "Show" commands), and
;;; for giving brief help (the "help" command).
;;;
;;; Entry points:
;;;
;;;  * (defun Set-terminal-width () ...
;;;  * (defun Set-defining-messages () ...
;;;  * (defun Set-warning-messages () ...
;;;  * (defun Set-prop-before-default () ...
;;;  * (defun Set-addition-checking () ...
;;;  * (defun Set-multiple-expansions () ...
;;;  * (defun Set-multiple-linearisations () ...
;;;  * (defun Set-morphology-system () ...
;;;  * (defun Set-fast-morph-lookup () ...
;;;  * (defun Set-word-structure () ...
;;;  * (defun Set-ecrs-before-multiply () ...
;;;  * (defun Set-term-unification () ...
;;;  * (defun Set-lr1-parse () ...
;;;  * (defun Set-show-parses () ...
;;;  * (defun Show-flags () ...
;;;  * (defun Give-help (help-file) ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defun set-width-of-terminal nil
   (setf *terminal-page-width
      (get-integer-flag-value *terminal-page-width 40)))


(defun set-defining-messages nil
   (setf *defining-messages
      (get-boolean-flag-value *defining-messages)))


(defun set-warning-messages nil
   (setf *warning-messages
      (get-boolean-flag-value *warning-messages)))


;;; Potentially invalidates all applications of propagation and
;;; default rules.

(defun set-prop-before-default nil
   (let
      ((old-value *prop-before-default))
      (setf *prop-before-default
         (get-boolean-flag-value
            *prop-before-default))
      (unless (eq *prop-before-default old-value)
         (input-idrule-invalidations *id-rules
            'expanded))))


;;; Note that if the value of this flag is changed then it
;;; potentially invalidates any metarule, propagation and
;;; default rule expanded ID rules.

(defun set-addition-checking nil
   (let
      ((old-value *addition-checking))
      (setf *addition-checking
         (get-boolean-flag-value
            *addition-checking))
      (when
         (not (eq *addition-checking old-value))
         (input-idrule-invalidations *id-rules
            'expanded))))


(defun set-multiple-expansions nil
   (let
      ((old-value *multiple-expansions))
      (setf *multiple-expansions
         (get-boolean-flag-value
            *multiple-expansions))
      (unless (eq *multiple-expansions old-value)
         (input-idrule-invalidations *id-rules
            'expanded))))


;;; Note that if the value of this flag is changed then it
;;; potentially invalidates any linearisations

(defun set-multiple-linearisations nil
   (let
      ((old-value *multiple-linearisations))
      (setf *multiple-linearisations
         (get-boolean-flag-value
            *multiple-linearisations))
      (unless
         (eq *multiple-linearisations old-value)
         (input-idrule-invalidations *id-rules
            'compiled))))


#+gde-morph
(progn

(defun set-morphology-system nil
   (let
      ((old-value *morph-system))
      (setf *morph-system
         (get-filename-flag-value *morph-system))
      (unless (equal *morph-system old-value)
         (input-word-invalidations *cached-words
            'normalised)
         (dolist (word *cached-words)
            (remprop word 'word))
         (setf *cached-words nil))
      (cond
         ((null *morph-system))
         ((every
               #'(lambda (part)
                  (equal *morph-system (car (last part))))
               d-loadedparts))
         (t
            ;; has been specified to be a path which is not the same as
            ;; what's currently loaded
            (d-unload)))))



(defun set-fast-morph-lookup nil
   (let
      ((old-value *fast-morph-lookup))
      (setf *fast-morph-lookup
         (get-boolean-flag-value
            *fast-morph-lookup))
      (unless (eq *fast-morph-lookup old-value)
         (input-word-invalidations *cached-words
            'normalised)
         (dolist (word *cached-words)
            (remprop word 'word))
         (setf *cached-words nil))))


(defun set-word-structure nil
   (let
      ((old-value *word-structure))
      (setf *word-structure
         (get-boolean-flag-value *word-structure))
      (when
         (and *word-structure
            (not (eq *word-structure old-value)))
         (input-word-invalidations *cached-words
            'normalised)
         (dolist (word *cached-words)
            (remprop word 'word))
         (setf *cached-words nil))))


(defun set-ecrs-before-multiply nil
   (let
      ((old-value *ecrs-before-multiply))
      (setf *ecrs-before-multiply
         (get-boolean-flag-value
            *ecrs-before-multiply))
      (unless
         (eq *ecrs-before-multiply old-value)
         (input-word-invalidations *words
            'compiled))))
)


(defun set-tagged-words nil
   (setf *tagged-words
      (get-boolean-flag-value *tagged-words)))


;;; If the values of the following flags are changed, then any
;;; parse trees are likely to be misleading, so get rid of them.

(defun set-term-unification nil
   (let*
      ((old-value *term-unification)
         (new-value (get-boolean-flag-value *term-unification)))
      (when (and *lr1-parse (null new-value))
         (gde-ferror "Term unification must be ON when LR1 parsing"))
      (setf *term-unification new-value)
      (unless (eq *term-unification old-value)
         (g-init-parse)
         (setf *generator-nodes nil))))


(defun set-lr1-parse nil
   (unless (fboundp 'lr1-parse)
      (gde-ferror "LR1 parser is not present"))
   (let
      ((old-value *lr1-parse)
         (new-value (get-boolean-flag-value *lr1-parse)))
      (when (and new-value (null *term-unification))
         (gde-ferror "LR1 parsing requires term unification to be ON"))
      (setf *lr1-parse new-value)
      ;; switching does not invalidate parse table / rule tree
      (unless (eq *lr1-parse old-value)
         (setf *current-parse-trees nil))))


(defun set-show-parses nil
   (setq *show-bracketings (get-integer-flag-value *show-bracketings -1)))


;;; Get a boolean flag value (ON / OFF) or a filename flag value
;;; (filename / OFF).

(defun get-boolean-flag-value
   (current-value)
   (let*
      ((reply
          (prompt-if-necessary "Current setting is "
             (return-flag-value current-value)
             " - enter new setting (ON or OFF): "))
         (new-value nil))
      (loop
         (cond
            ((null reply) (return nil)))
         (setf new-value (car reply))
         (cond
            ((or (uncased-eq-symbols new-value 'on)
                (uncased-eq-symbols new-value 'off))
               (return nil)))
         (setf reply
            (prompt-always
               "Valid settings are 'ON' or 'OFF'"
               " - enter new setting: ")))
      (cond
         (reply (uncased-eq-symbols new-value 'on))
         (t current-value))))


(defun get-filename-flag-value
   (current-value)
   (let
      ((reply
          (prompt-if-necessary "Current setting is "
             (or current-value "OFF")
             " - enter new setting: ")))
      (if reply
         (if
            (uncased-eq-symbols (car reply) 'off)
            nil (concatl-string reply))
         current-value)))


(defun get-integer-flag-value (current-value lower-bound)
   (let
      ((reply
          (prompt-if-necessary "Current setting is "
             current-value
             " - enter new setting: "))
         (new-value nil))
      (loop
         (cond
            ((null reply) (return nil)))
         (setf new-value (symbol-to-number (car reply)))
         (cond
            ((and new-value (> new-value lower-bound))
               (return nil)))
         (setf reply
            (prompt-always
               (format nil "Must be an integer > ~A - enter new setting: "
                  lower-bound))))
      (or new-value current-value)))


;;; Display the values of all the flags. If a flag is not
;;; boolean, then it is assumed to have possible values OFF or a
;;; filename.

(defun show-flags nil
   (format t
"Current flag settings:

WIdth of terminal       : ~A
Defining messages       : ~A
WArning messages        : ~A
Prop before default     : ~A
Addition checking       : ~A
Multiple Expansions     : ~A
Multiple Linearisations : ~A~%"
      (return-flag-value *terminal-page-width)
      (return-flag-value *defining-messages)
      (return-flag-value *warning-messages)
      (return-flag-value *prop-before-default)
      (return-flag-value *addition-checking)
      (return-flag-value *multiple-expansions)
      (return-flag-value *multiple-linearisations))
   #+gde-morph
   (format t
"MOrphology system       : ~A
Fast morph lookup       : ~A
WOrd structure          : ~A
Ecrs before multiply    : ~A~%"
      (return-flag-value *morph-system)
      (return-flag-value *fast-morph-lookup)
      (return-flag-value *word-structure)
      (return-flag-value *ecrs-before-multiply))
   (format t
"TAgged words            : ~A
TErm unification        : ~A
LR1 parse               : ~A
Show bracketings        : ~A~%"
      (return-flag-value *tagged-words)
      (return-flag-value *term-unification)
      (return-flag-value *lr1-parse)
      (return-flag-value *show-bracketings)))


(defun return-flag-value (flag-value)
   (cond
      ((null flag-value) "OFF")
      ((eq flag-value t) "ON")
      (t flag-value)))


;;; Copy the file with the given name character by character to
;;; the terminal.

(defun give-help (help-file)
   (cond
      ((probe-file help-file)
         (with-open-stream
            (instream
               (open help-file :direction :input))
            (loop
               (multiple-value-bind (line eof-p)
                  (read-line instream nil t nil)
                  (if (or eof-p (not (stringp line)))
                     (return nil))
                  (princ line) (terpri)))
            (terpri)))
      (t
         (gde-cerror "help file " help-file " does not exist"))))


;;; End of file


