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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - TOP LOOP
;;;
;;; Author: John Carroll
;;;
;;; This file contains code for inputing and matching user
;;; commands and prompting and reading input from the terminal
;;; and from file. Most of the globals used in the GDE are
;;; declared in this file
;;;
;;; Entry points:
;;;
;;;  * (defun Concat-string x ...
;;;  * (defun Concat-symbol x ...
;;;  * (defun Concatl-string (x) ...
;;;  * (defun Concatl-symbol (x) ...
;;;  * (defun Uncased-eq-symbols (a b) ...
;;;  * (defun Canonise-symbol (x) ...
;;;  * (defun Symbol-to-number (x) ...
;;;  * (defun Number-to-symbol (x) ...
;;;  * (defmacro Gde-warn lst ...
;;;  * (defmacro Gde-cerror lst ...
;;;  * (defmacro Gde-ferror lst ...
;;;  * (defun Prompt-if-necessary lst ...
;;;  * (defun Prompt-always lst ...
;;;  * (defun Prompt-never () ...
;;;  * (defun Yes-for-question lst ...
;;;  * (defun Prompt-for-alternative (type lst) ...
;;;  * (defun Gde-top-loop () ...
;;;  * (defun Gde-top-print () ...
;;;  * (defun Process-command (x command-table) ...
;;;  * (defun Process-command-option (prompt command-table) ...
;;;  * (defun Process-lisp-input () ...
;;;  * (defun Get-reply NIL ...
;;;  * (defun Write-construct-names (items) ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Constants.

(progn
   (defparameter *eol-string (string #\Newline))
   (defconstant *eof-marker t))


;;; Parameters.

(progn
   (defparameter *grammar-comment-char #\;)
   (defparameter *fill-grammar-comments t)
   (defparameter *gde-version-no "v1.42"))


;;; Variables.

(progn
   (defvar *grammar-comments nil)
   (defvar *user-command nil)
   (defvar *user-input nil)
   (defvar *top-level-commands nil)
   (defvar *input-commands nil)
   (defvar *edit-commands nil)
   (defvar *move-commands nil)
   (defvar *view-commands nil)
   (defvar *view-normalised-commands nil)
   (defvar *view-full-commands nil)
   (defvar *view-linearised-commands nil)
   (defvar *view-normalised-full-commands nil)
   (defvar
      *view-normalised-linearised-commands nil)
   (defvar *names-commands nil)
   (defvar *names-normalised-commands nil)
   (defvar *names-linearised-commands nil)
   (defvar
      *names-normalised-linearised-commands nil)
   (defvar *delete-commands nil)
   (defvar *set-commands nil)
   (defvar *order-commands nil)
   (defvar *dump-grammar-commands nil)
   (defvar *dump-word-commands nil))


(progn
   (defvar *backed-up-files nil)
   (defvar *grammar-files nil)
   (defvar *trace-reductions nil)
   (defvar *semantic-operator-property nil)
   (defvar *metarule-operator-property nil)
   (defvar *defining-messages nil)
   (defvar *warning-messages nil)
   (defvar *prop-before-default nil)
   (defvar *addition-checking nil)
   (defvar *multiple-expansions nil)
   (defvar *multiple-linearisations nil)
   (defvar *morph-system nil)
   (defvar *fast-morph-lookup nil)
   (defvar *word-structure nil)
   (defvar *ecrs-before-multiply nil)
   (defvar *tagged-words nil)
   (defvar *term-unification nil)
   (defvar *lr1-parse nil)
   (defvar *show-bracketings 100))


(progn
   (defvar *features nil)
   (defvar *sets nil)
   (defvar *aliases nil)
   (defvar *sorted-aliases nil)
   (defvar *categories nil)
   (defvar *extensions nil)
   (defvar *top nil)
   (defvar *id-rules nil)
   (defvar *meta-rules nil)
   (defvar *default-rules nil)
   (defvar *prop-rules nil)
   (defvar *lp-rules nil)
   (defvar *reduced-lp-rules nil)
   (defvar *ec-rules nil)
   (defvar *multiply-rules nil)
   (defvar *cc-rules nil)
   (defvar *words nil)
   (defvar *cached-words nil)
   (defvar *disk-resident-words nil)
   (defvar *current-parse-trees nil)
   (defvar *current-parse-weights nil)
   (defvar *parsed-sentences nil)
   (defvar *unsaved-parsed-sentences nil)
   (defvar *generator-nodes nil)
   (defvar *generator-rules nil)
   (defvar *generator-words nil)
   (defvar *category-index-dnet nil)
   (defvar *index-category-table nil)
   (defvar *current-category-index -1))


(progn
   (defvar variable-substitutions nil)
   (defvar highest-binding-no nil)
   (defvar output-to-buffer nil)
   (defvar definition-output-buffer nil)
   (defvar gde-dict-input nil)
   (defvar *suppress-dict-messages nil)
   (defvar *chart-edges nil)
   (defvar *file-read nil)
   (defvar *file-read-position nil)
   (defvar *inside-dci nil)
   (defvar *linelength* nil)
   (defvar d-lookupformat nil)
   (defvar d-traceflag nil)
   (defvar d-features nil)
   (defvar d-aliases nil)
   (defvar d-variables nil)
   (defvar d-catvalfeat nil)
   (defvar d-whead nil)
   (defvar d-wdaughter nil)
   (defvar d-morphologyonly nil)
   (defvar d-lexicon nil)
   (defvar d-simplelexicon nil)
   (defvar d-loadedparts nil)
   (defvar d-fileid nil)
   (defvar d-entryfileid nil)
   (defvar d-currentsym nil)
   (defvar d-semantics nil))


;;; Assign initial default values to lisp variables holding GDE
;;; flag values, and to other variables controlling its behaviour.

(progn
   (setf *defining-messages nil)
   (setf *warning-messages t)
   (setf *prop-before-default t)
   (setf *addition-checking t)
   (setf *multiple-expansions nil)
   (setf *multiple-linearisations nil)
   (setf *morph-system nil)
   (setf *fast-morph-lookup nil)
   (setf *word-structure t)
   (setf *ecrs-before-multiply t)
   (setf *tagged-words nil)
   (setf *term-unification t)
   (setf *lr1-parse nil))


(progn
   (setf *trace-reductions nil)
   (setf *semantic-operator-property 'semantic-operator)
   (setf *metarule-operator-property 'metarule-operator))


;;; Canonise-word takes a word (a symbol) and canonicalises it.
;;; Must also be able to cope with patterns (a list of symbols).

(defun canonise-word (word)
   word)


;;; Concatenating symbols and strings and finding their print
;;; lengths. Arguments to concatenating functions may be
;;; strings, symbols, characters or integers.

(defun concat-string (&rest x)
   (concatl-string x))


(defun concat-symbol (&rest x)
   (concatl-symbol x))


(defun atom-to-string (x)
   (cond
      ((stringp x) x)
      ((characterp x) (string x))
      ((symbolp x) (symbol-name x))
      ((atom x) (princ-to-string x))
      (t (error "Illegal argument to atom-to-string: ~S" x))))


(defun concatl-string (x)
   (apply #'concatenate 'string
      (mapcar #'atom-to-string x)))


(defun concatl-symbol (x)
   (intern
      (apply #'concatenate 'string
         (mapcar #'atom-to-string x))))


(defun symbol-to-number (x)
   (if (every #'digit-char-p (symbol-name x))
      (parse-integer (symbol-name x))
      nil))


(defun number-to-symbol (x)
   (intern (princ-to-string x)))


(defun semantic-kleene-marker-p (x)
   (getf
      '(1+ (1 . 1+) 2+ (2 . 2+) 3+ (3 . 3+)
         1* (1 . 1*) 2* (2 . 2*) 3* (3 . 3*))
      x))


;;; Test whether a symbol is a variable value generated by the
;;; morphology package.

(defun is-blank-variable (x)
   (and (symbolp x)
      (let ((str (symbol-name x)))
         (and (> (length str) 2)
            (eql (schar str 0) #\@)
            (eql (schar str 1) #\D)))))


;;; Create a new symbol during semantic reduction of a lambda
;;; expression. If it was previously a gensym, remove the digits
;;; in it.

(defun new-lambda-variable (var)
   (if (numberp var) (gensym)
      (gensym (remove-if #'digit-char-p (string var)))))


;;; Create a new symbol from alphabetic characters of argument, with
;;; given number appended

(defun strip-lambda-variable (var)
   (intern (remove-if #'digit-char-p (string var))))


;;; Most of the GDE reading is done by Get-reply in mixed case -
;;; the following functions are used when processing input in
;;; which the case does not matter, or putting input into case
;;; insensitive form.

(defun uncased-eq-symbols (a b)
   (and (string-equal (symbol-name a) (symbol-name b)) t))


(defun canonise-symbol (x) x)


;;; Macros for writing out error messages and if necessary
;;; throwing out of a computation with an error.

(defmacro gde-warn (&rest lst)
   `(when *warning-messages
      (format t "~&*** Warning, ~@{~A~}~%" ,@lst)))


(defmacro gde-cerror (&rest lst)
   `(format t "~&*** Error, ~@{~A~}~%" ,@lst))


(defmacro gde-ferror (&rest lst)
   `(progn
       (format t "~&*** Error, ~@{~A~}~%" ,@lst)
       (throw 'break 'nothing)))


;;; Macros for prompting user to enter a value. Only
;;; Yes-for-question repeatedly asks if the user types nothing -
;;; the others just return NIL in this case.
;;; Prompt-for-alternative takes a list of the form ((item .
;;; return-form) ...), displays the items, each prefixed with an
;;; integer, prompts for an integer in range and returns the
;;; appropriate return-form. Returns NIL if too many to choose
;;; from or user gives a bad reply.

(defun prompt-if-necessary (&rest lst)
   (cond
      ((null *user-input)
         (format t "~&~{~A~}" lst)
         (finish-output *standard-output*)
         (get-reply))
      (t
         (prog1 *user-input
            (setf *user-input nil)))))


(defun prompt-always (&rest lst)
   (setf *user-input nil)
   (apply #'prompt-if-necessary lst))


(defun prompt-never nil *user-input)


(defun yes-for-question (&rest lst)
   (clear-input)
   (prog1
      (y-or-n-p "~{~A~}? " lst)
      (fresh-line *standard-output*)))


(defun prompt-for-alternative (type lst)
   (cond
      ((= (list-length lst) 1)
         (format t
            "~&Only appropriate matching ~A is: ~A~%"
            type (caar lst))
         (when (yes-for-question "Do you want it")
            (cdar lst)))
      ((< (list-length lst) 101)
         (let
            ((n 0)
             (len (ceiling (log (list-length lst) 10))))
            (write-construct-names
               (format nil "Appropriate matching ~A are:  " type)
               (mapcar
                  #'(lambda (item)
                       (incf n)
                       (format nil "~VD. ~A" len n (car item)))
                  lst))
            (let
               ((input
                   (prompt-always
                      "Which one (give its number)? ")))
               (when input
                  (let
                     ((number
                         (symbol-to-number (car input))))
                     (cond
                        ((null number)
                           (gde-cerror "not a number") nil)
                        ((or
                            (< number 1)
                            (> number n))
                           (gde-cerror "number out of range")
                           nil)
                        (t
                           (cdr
                              (nth (1- number) lst)))))))))
      (t
         (write-construct-names
            (concat-string "Appropriate matching " type
               " are:  ")
            (mapcar #'car lst))
         nil)))


;;; Top level function of GDE. Top-loop provides a customisable
;;; "read-eval-print" loop, with programmer-supplied eval
;;; and print functions. It must in addition stop unwinding from
;;; errors. Ask the user before exiting if there is any unsaved
;;; data.

(defun gde-top-loop nil
   (loop
      (top-loop #'gde-top-print #'gde-top-eval "Gde"
         (format nil
            "Grammar Development Environment ~A"
            *gde-version-no))
      (unless
         (and (< *top-loop-level 2)
            (or (exists-altered-data) *unsaved-parsed-sentences)
            (not
               (y-or-n-p
"There are definitions or sentences which have not been saved.
Do you really want to exit? ")))
         (return nil))))


(defun exists-altered-data nil
   (dolist
      (constructs
         (list *features *sets *aliases *categories
            *extensions *top *id-rules *meta-rules
            *default-rules *prop-rules *lp-rules
            *ec-rules *multiply-rules *cc-rules
            *words))
      (if
         (dolist (name constructs)
            (if (get name 'altered) (return name)))
         (return constructs))))


(defun gde-top-print (x) x)


(defun gde-top-eval (x)
   (process-command (get-reply1 x) *top-level-commands)
   'nothing)


;;; Process a user command. Set up the global *user-input and
;;; take off its CAR for the command name. The rest of it is the
;;; arguments to be picked up by Prompt-if-necessary in the
;;; actual command functions. Do not protect aginst error
;;; unwinds here if reading from a file since this has already
;;; been done further up.

(defun process-command (x command-table)
   (setf *user-input x)
   (when *user-input
      (setf *user-command (pop *user-input))
      (let
         ((action
               (look-up-command *user-command command-table)))
         (if *file-read
            (if action
               (apply (car action) (cdr action))
               (gde-ferror "illegal construct type: " *user-command))
            (if action
               (catch 'break
                  (apply (car action) (cdr action)))
               (gde-cerror "invalid or ambiguous command or option: "
                  *user-command))))))


;;; Prompt for a command option, and with the resulting input
;;; continue processing the command.

(defun process-command-option (prompt command-table)
   (unless *user-input
      (unless *file-read
         (princ prompt)
         (finish-output *standard-output*))
      (setf *user-input (get-reply)))
   (process-command *user-input command-table))


;;; Search a command table for a given command and return its
;;; associated action.

(defun look-up-command (command command-table)
   (cond
      ((gde-comment-p command)
         (list 'insert-grammar-comment command))
      (t
         (dolist (entry command-table)
            (let
               ((action
                     (and (match-command command entry)
                        (command-entry-action entry))))
               (if action
                  (return
                     (cons (car action)
                        (mapcar
                           #'(lambda (arg)
                              (etypecase arg
                                 (cons (cadr arg))
                                 (string arg)
                                 (symbol (symbol-value arg))))
                           (cdr action))))))))))


;;; Check to see if a command matches an entry in a command
;;; table.

(defun match-command (command entry)
   (match-command-shortest command (command-entry-name entry)
      (command-entry-shortest entry)))


(defun match-command-shortest (command entry-name entry-shortest)
   (let
      ((index
          (mismatch
             (string entry-name)
             (string command) :test #'char-equal)))
      (or (null index)
         (zerop entry-shortest)
         (and (>= index entry-shortest)
            (eql index (length (string command)))))))


;;; Evaluate and print a lisp s-expression typed directly by the
;;; user. Do not print it out if the answer was NOTHING, since
;;; it must be the result of a redo on a GDE command.

(defun process-lisp-input nil
   (let
      ((ans (multiple-value-list (eval *user-input)))
         (*print-escape* t) (*print-pretty* t))
      (unless (eq (car ans) 'nothing)
         (mapc
            #'(lambda (x) (format t "~&~S~%" x))
            ans))))


;;; Read in an input line from the terminal. The input line is
;;; read as a text line, unless the first character is !, in
;;; which case it is a lisp command. If so, it is read by the
;;; ordinary read function and then packaged up
;;; with the ! symbol for interpretation by the command table.

(defun get-reply (&optional already-tokenised-p)
   ;; In Procyon update the mark indicating where the Toploop is expecting
   ;; to read user input
   #+PROCYON
   (when (synonym-stream-p *standard-input*)
      #+|2.1| (te:set-mark (synonym-stream-stream *standard-input*))
      #-|2.1| (:mark (synonym-stream-stream *standard-input*)))
   (setq *file-read-position
      (and *file-read (file-position *standard-input*)))
   (get-reply1
      (read-line *standard-input* nil "") already-tokenised-p))


(defun get-reply1 (line &optional already-tokenised-p)
   ;; Fix for CMU-CL bug returning two lines from read-line if the first
   ;; is empty
   #+CMU (setq line (string-left-trim '(#\newline) line))
   ;; Fix for Allegro PC bug returning linefeed character if line is empty
   #+ACLPC (setq line (string-left-trim '(#\linefeed) line))
   (cond
      ((zerop (length line))
         nil)
      ((eql (char line 0) #\!)
         (cons '\!
            (read-from-string line nil nil :start 1)))
      (t (gde-read-text-line line 0 nil already-tokenised-p))))


;;; Gde-whitespace-p is T if char must be ignored on input. Newline
;;; won't be in input so don't need to take it into account.

(defmacro gde-whitespace-p (char)
   `(or (eql ,char #\Space) (eql ,char #\Tab)))


;;; Gde-endword-p is T if the char ends words. Non-end word
;;; characters are alphanumerics, _, #, $, ^, |, /, -, +, '.
;;; The only characters that end words when inside a comment
;;; are whitespace characters.

(defvar *gde-endword-table
   (make-array 256 :initial-element nil))


(eval-when (load eval)
   (dolist
      (char
         (cons *grammar-comment-char
            (list #\Tab #\Space #\"
               #\! #\& #\( #\) #\= #\~ #\` #\@ #\{ #\[
               #\* #\: #\} #\] #\, #\< #\> #\. #\?)))
      (setf (svref *gde-endword-table (char-int char)) t)))


(defmacro gde-endword-p (char within-comment)
   `(cond
       (,within-comment
          (gde-whitespace-p ,char))
       (t
          (svref *gde-endword-table (char-int ,char)))))


(defmacro gde-escape-p (char)
   `(eql ,char #\\ ))


(defun gde-read-text-line (string index part-word already-tokenised-p)
   (if (>= index (length string))
      (cond
         ((null part-word) nil)
         ((eq already-tokenised-p '*comment)
            (ncons
               (coerce (nreverse part-word) 'string)))
         (t
            (ncons
               (intern (coerce (nreverse part-word) 'string)))))
      (let
         ((char (char string index)))
         (cond
            ((gde-endword-p char already-tokenised-p)
               (gde-read-text-line1 string index part-word
                  already-tokenised-p))
            ((and (gde-escape-p char) (not already-tokenised-p))
               (incf index)
               (gde-read-text-line string
                  (1+ index)
                  (cons
                     (if (eql index (length string))
                        #\Newline
                        (char string index))
                     part-word)
                  already-tokenised-p))
            (t
               (gde-read-text-line string (1+ index)
                  (cons char part-word) already-tokenised-p))))))


(defun gde-read-text-line1 (string index part-word already-tokenised-p)
   (let*
      ((char (char string index))
       (rest-of-line
          (cond
             ((gde-whitespace-p char)
                (gde-read-text-line string (1+ index) nil
                   already-tokenised-p))
             ((char= char *grammar-comment-char)
                (let ((depth 1))
                   (loop
                      (incf index)
                      (when (>= index (length string)) (return))
                      (setf char (char string index))
                      (if (char= char *grammar-comment-char)
                         (incf depth) (return)))
                   (ncons
                      (make-gde-comment depth
                         (gde-read-text-line string index nil '*comment)))))
             (t
                (cons (intern (string char))
                   (gde-read-text-line string (1+ index) nil
                      already-tokenised-p))))))
      (cond
         (part-word
            (cons
               (if (eq already-tokenised-p '*comment)
                  (coerce (nreverse part-word) 'string)
                  (intern (coerce (nreverse part-word) 'string)))
               rest-of-line))
         (t rest-of-line))))


;;; Mark single character symbols that function as construct
;;; delimeters - used in input syntax checking for sensible
;;; feature names etc.

(eval-when (load eval)
   (dolist
      (symb
         (coerce ";:,.[](){}=<>!?@" 'list))
     (setf (get (intern (string symb)) 'gde-category-symb) t)))


(defmacro gde-category-symb-p (x)
   `(and (symbolp ,x)
      (get ,x 'gde-category-symb)))


;;; Write all the items on the current line if there is space
;;; for them all, otherwise print them in columns starting at
;;; column 3 on the next line.

(defun write-construct-names (text items)
   (fresh-line)
   (princ text)
   (let
      ((item-lengths nil) (max-length 0)
         (total-length 0)
         (half-linelength
            (floor (gde-linelength) 2))
         (line-posn (length text)))
      (dolist (item items)
         (let
            ((item-length
                (1+
                   (length
                      (if (symbolp item)
                         (symbol-name item) item)))))
            (setf item-lengths
               (cons item-length item-lengths))
            (setf max-length
               (min half-linelength
                  (max max-length item-length)))
            (setf total-length
               (+ total-length item-length))))
      (cond
         ((< (+ line-posn total-length) (gde-linelength))
            (format t "~{~A ~}" items))
         (t
            (if (> line-posn 0) (terpri))
            (let
               ((col-tabs
                   (write-construct-get-tabs
                      (nreverse item-lengths)
                      (if (> line-posn 0) 3 0)
                      (1+ max-length)
                      (gde-linelength)))
                (line (make-string-output-stream)))
               (unwind-protect
                  (progn
                     (dolist (item items)
                        (when (<= (car col-tabs) 0)
                           (setf (car col-tabs) (- (car col-tabs)))
                           (princ (get-output-stream-string line))
                           (terpri))
                        (format line "~V@T~A" (car col-tabs) item)
                        (setf col-tabs (cdr col-tabs)))
                     (princ (get-output-stream-string line)))
                  (close line)))))
      (terpri)))


(defun write-construct-get-tabs
   (item-lengths lmar step final)
   (let*
      ((i lmar) (res (ncons lmar)) (item 0))
      (loop
         (cond
            ((null item-lengths) (return nil)))
         (setf item (pop item-lengths))
         (setf res
            (cond
               ((>= item step) (setf i lmar)
                  (cons (- lmar)
                     (cons (- lmar) (cdr res))))
               ((>= (+ i item) final)
                  (setf i (+ lmar step))
                  (cons (- step item)
                     (cons (- lmar) (cdr res))))
               (t
                  (setf i (+ i step))
                  (cons (- step item) res)))))
      (nreverse res)))


(defun give-gde-help nil
   (give-help *gde-help-file))


;;; End of file

