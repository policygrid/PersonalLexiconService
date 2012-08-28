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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - FILE HANDLING
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code for handling all the file
;;; related user commands, i.e. 'Read' and 'Write'
;;;
;;; Entry points:
;;;
;;;  * (defun Read-grammar (filename) ...
;;;  * (defun Read-grammar-file () ...
;;;  * (defun Write-grammar-file () ...
;;;  * (defun Backup-grammar-file (filename) ...
;;;  * (defun Show-files NIL ...
;;;  * (defun Clear-grammar-files () ...
;;;  * (defun Forget-gramar-file (file) ...
;;;  * (defun Add-grammar-comment () ...
;;;  * (defun Remove-grammar-comment () ...
;;;  * (defun Print-grammar-comment () ...
;;;  * (defun Get-new-construct-file () ...
;;;  * (defun Merge-grammar-comment (new-comment) ...
;;;  * (defun Canonise-grammar-file-name (user-input) ...
;;;
;;; Grammar items saved to file are removed from altered
;;; definition lists. Read-grammar-file does not reset -
;;; definitions read from a file are not allowed to redefine
;;; existing data.
;;;
;;; Read-grammar is an entry point to be called by the user from
;;; lisp, to read in a file of grammar definitions.
;;; Read-grammar-file is the function called by the GDE in
;;; response to the Read command - it prompts for the name of
;;; the file. All top level comments are merged into the
;;; association list *grammar-comments.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defvar read-terminated-ok nil)


;;; Determine if a file is disk-resident (i.e. word definitions in
;;; it stay on disk until the word as called for by view, the parser etc)
;;; by whether the file's top-level comment contains the word disk-resident
;;; (any casing), surrounded by whitespace.

(defun file-disk-resident-p (filename)
   (let
      ((file-comment
          (and filename
             (assoc filename *grammar-comments :test #'pathname-equal))))
      (and file-comment
         (find "DISK-RESIDENT" (gde-comment-text (cdr file-comment))
            :test #'string-equal))))


;;; The function called to handle a user "Read" command.

(defun read-grammar (filename)
   (let
      ((file
          (canonise-grammar-file-name
             (ncons filename))))
      (when (probe-file file)
         (let
            ((*file-read file))
            (reorder-grammar-files file)
            (read-grammar-file1 file)))))


(defun read-grammar-file nil
   (let
      ((input
          (prompt-if-necessary "File name? ")))
      (when input
         (let
            ((file (canonise-grammar-file-name input)))
            (cond
               ((probe-file file)
                  (let
                     ((*file-read file))
                     (reorder-grammar-files file)
                     (read-grammar-file1 file)))
               (t (gde-cerror "file does not exist")))))))


(defun read-grammar-file1 (filename)
   (let
      ((read-terminated-ok nil))
      (unwind-protect
         (with-open-stream
            (*standard-input*
               (open filename :direction :input))
            (loop
               (process-command (get-reply) *input-commands)
               (unless (peek-char nil *standard-input* nil nil)
                  ;; changed from (listen *standard-input*) due to bug in KCL
                  (return nil)))
            (setf read-terminated-ok t))
         (if read-terminated-ok
            (format t "File read~%")
            (gde-warn
               "file not completely read due to error")))))


;;; The function called to handle a user "Write" command. Write
;;; out the all the user defined constructs for the given
;;; file(s) using the pretty printers.

(defvar output-grammar-file nil)


(defun write-grammar-file nil
   (cond
      (*grammar-files
         (let
            ((pattern
                  (prompt-if-necessary
                     "File name (* for all)? ")))
            (when pattern
               (unless
                  (mapcan
                     #'(lambda (output-grammar-file)
                        (cond
                           ((and
                               (or (eq (car pattern) '*)
                                   (pathname-equal output-grammar-file
                                      (canonise-grammar-file-name
                                         pattern)))
                               (not (file-disk-resident-p
                                       output-grammar-file)))
                              (list
                                 (progn
                                    (backup-grammar-file
                                       output-grammar-file)
                                    (format t
                                       "Writing file ~A~%"
                                       (enough-namestring
                                          output-grammar-file))
                                    (with-open-stream
                                       (*standard-output*
                                          (open
                                             output-grammar-file
                                             :direction
                                             :output
                                             :if-exists
                                             :supersede
                                             :if-does-not-exist
                                             :create))
                                       (write-grammar-file1
                                          output-grammar-file))
                                    t)))))
                     *grammar-files)
                  (gde-cerror "not a writable grammar file")))))
      (t (gde-cerror "no files to write"))))


(defun write-grammar-file1 (file)
   (when
      (assoc file *grammar-comments :test #'pathname-equal)
      (print-grammar-comment1
         (cdr
            (assoc file *grammar-comments :test
               #'pathname-equal)))
      (terpri))
   (if
      (mapcan
         #'(lambda (feature)
              (cond
                 ((equal
                     (feature-declaration-file
                        (get feature 'feature))
                     file)
                    (print-feature-definition feature
                       (get feature 'feature) t)
                    (remprop feature 'altered) (ncons t))))
         *features)
      (terpri))
   (if
      (mapcan
         #'(lambda (set)
              (cond
                 ((equal
                     (set-declaration-file (get set 'set))
                     file)
                    (print-set-definition set (get set 'set) t)
                    (remprop set 'altered) (ncons t))))
         *sets)
      (terpri))
   (if
      (mapcan
         #'(lambda (alias)
              (cond
                 ((equal
                     (alias-declaration-file
                        (get alias 'alias))
                     file)
                    (print-alias-definition alias
                       (get alias 'alias) t)
                    (remprop alias 'altered) (ncons t))))
         *aliases)
      (terpri))
   (if
      (mapcan
         #'(lambda (category)
              (cond
                 ((and
                     (equal
                        (category-declaration-file
                           (get category 'category))
                        file)
                     (null
                        (category-declaration-lexical
                           (get category 'category))))
                    (print-category-definition category
                       (get category 'category) t)
                    (remprop category 'altered) (ncons t))))
         *categories)
      (terpri))
   (if
      (mapcan
         #'(lambda (category)
              (cond
                 ((and
                     (equal
                        (category-declaration-file
                           (get category 'category))
                        file)
                     (category-declaration-lexical
                        (get category 'category)))
                    (print-category-definition category
                       (get category 'category) t)
                    (remprop category 'altered) (ncons t))))
         *categories)
      (terpri))
   (if
      (mapcan
         #'(lambda (extension)
              (cond
                 ((equal
                     (extension-declaration-file
                        (get extension 'extension))
                     file)
                    (print-extension-definition extension
                       (get extension 'extension) t)
                    (remprop extension 'altered) (ncons t))))
         *extensions)
      (terpri))
   (if
      (mapcan
         #'(lambda (top)
              (cond
                 ((equal
                     (top-declaration-file (get top 'top))
                     file)
                    (print-top-definition top (get top 'top) t)
                    (remprop top 'altered) (ncons t))))
         *top)
      (terpri))
   (if
      (mapcan
         #'(lambda (idrule)
              (cond
                 ((and
                     (equal (id-rule-file (get idrule 'idrule))
                        file)
                     (null
                        (id-rule-linear (get idrule 'idrule))))
                    (print-idrule-definition
                       (get idrule 'idrule) t)
                    (remprop idrule 'altered) (ncons t))))
         *id-rules)
      (terpri))
   (if
      (mapcan
         #'(lambda (idrule)
              (cond
                 ((and
                     (equal (id-rule-file (get idrule 'idrule))
                        file)
                     (id-rule-linear (get idrule 'idrule)))
                    (print-idrule-definition
                       (get idrule 'idrule) t)
                    (remprop idrule 'altered) (ncons t))))
         *id-rules)
      (terpri))
   (if
      (mapcan
         #'(lambda (metarule)
              (cond
                 ((equal
                     (meta-rule-file (get metarule 'metarule))
                     file)
                    (print-metarule-definition metarule
                       (get metarule 'metarule) t)
                    (remprop metarule 'altered) (ncons t))))
         *meta-rules)
      (terpri))
   (if
      (mapcan
         #'(lambda (proprule)
              (cond
                 ((equal
                     (prop-rule-file (get proprule 'proprule))
                     file)
                    (print-proprule-definition proprule
                       (get proprule 'proprule) t)
                    (remprop proprule 'altered) (ncons t))))
         *prop-rules)
      (terpri))
   (if
      (mapcan
         #'(lambda (defrule)
              (cond
                 ((equal
                     (default-rule-file (get defrule 'defrule))
                     file)
                    (print-defrule-definition defrule
                       (get defrule 'defrule) t)
                    (remprop defrule 'altered) (ncons t))))
         *default-rules)
      (terpri))
   (if
      (mapcan
         #'(lambda (lprule)
              (cond
                 ((equal (lp-rule-file (get lprule 'lprule))
                     file)
                    (print-lprule-definition lprule
                       (get lprule 'lprule) t)
                    (remprop lprule 'altered) (ncons t))))
         *lp-rules)
      (terpri))
   #+gde-morph
   (if
      (mapcan
         #'(lambda (ecr)
              (cond
                 ((equal (ec-rule-file (get ecr 'ecr)) file)
                    (print-ecr-definition ecr (get ecr 'ecr) t)
                    (remprop ecr 'altered) (ncons t))))
         *ec-rules)
      (terpri))
   #+gde-morph
   (if
      (mapcan
         #'(lambda (mr)
              (cond
                 ((equal (multiply-rule-file (get mr 'mr))
                     file)
                    (print-mr-definition mr (get mr 'mr) t)
                    (remprop mr 'altered) (ncons t))))
         *multiply-rules)
      (terpri))
   #+gde-morph
   (if
      (mapcan
         #'(lambda (cc)
              (cond
                 ((equal (cc-rule-file (get cc 'cc)) file)
                    (print-cc-definition cc (get cc 'cc) t)
                    (remprop cc 'altered) (ncons t))))
         *cc-rules)
      (terpri))
   (dolist (word *words)
      (cond
         ((equal
             (word-definition-file (get word 'word))
             file)
            (print-word-definition word
               (get word 'word) t)
            (remprop word 'altered)))))


;;; If there exists on disc an old version of a file about to be
;;; saved, then back it up by renaming if it hasn't already been
;;; done in this session. Delete it first in case rename-file
;;; complains if file already exists.

(defun backup-grammar-file (filename)
   (let
      ((pathname (probe-file filename)))
      (when
         (and pathname
            (not
               (member pathname *backed-up-files :test
                  #'pathname-equal)))
         (setf *backed-up-files
            (cons pathname *backed-up-files))
         (let
            ((backed-up-name
               (backup-grammar-file-name pathname)))
            (unless (pathname-equal backed-up-name pathname)
               (format t "Backing up file ~A~%"
                  (enough-namestring filename))
               (if (probe-file backed-up-name)
                  (delete-file backed-up-name))
               (rename-file pathname backed-up-name))))))


(defun backup-grammar-file-name (pathname)
   (merge-pathnames
      (make-pathname :type *backup-file-type)
      pathname))


;;; The command handler for printing out the names of the
;;; currently loaded grammar files. Rely on Enough-namestring to
;;; return a readable version of the pathname.

(defun show-files nil
   (cond
      (*grammar-files
         (write-construct-names "Grammar files: "
            (mapcar
               #'(lambda (file)
                    (enough-namestring file))
               *grammar-files)))
      (t
         (format t
            "No grammar files have been read in~%"))))


(defun clear-grammar-files nil
   (setf *grammar-comments nil)
   (setf *grammar-files nil))


(defun forget-grammar-file (file)
   (setf *grammar-comments
      (remove-list-1
         (assoc file *grammar-comments :test
            #'pathname-equal)
         *grammar-comments))
   (setf *grammar-files
      (remove file *grammar-files :test #'pathname-equal
         :count 1)))


;;; The command handlers for inputting comments from the
;;; keyboard, and deleting them. Comments read from file go
;;; straight to Insert-grammar-comment

(defun add-grammar-comment nil
   (let
      ((input (prompt-if-necessary "Comment? ")))
      (cond
         ((and input
             (gde-comment-p (car input)))
            (insert-grammar-comment (car input)))
         (t
            (gde-cerror
               "not a comment - should begin with  '"
               *grammar-comment-char "'")))))


(defun remove-grammar-comment nil
   (let
      ((input
          (prompt-if-necessary "Filename? ")))
      (when input
         (if
            (member (canonise-grammar-file-name input)
               *grammar-files :test #'pathname-equal)
            (let
               ((com-pair
                   (assoc (canonise-grammar-file-name input)
                      *grammar-comments :test #'pathname-equal)))
               (cond
                  (com-pair
                     (print-grammar-comment1
                        (cdr com-pair))
                     (when (yes-for-question "Delete comment")
                        (setf *grammar-comments
                           (remove-list-1 com-pair
                              *grammar-comments))))
                  (t (gde-cerror "no comment in file"))))
            (gde-cerror
               "file has not been read in")))))


;;; The command handler for printing the comment associated with
;;; the current file.

(defun print-grammar-comment nil
   (if *grammar-files
      (let
         ((input
             (prompt-if-necessary "Filename? ")))
         (when input
            (let
               ((pairlis
                   (assoc (canonise-grammar-file-name input)
                      *grammar-comments :test #'pathname-equal)))
               (if pairlis
                  (print-grammar-comment1 (cdr pairlis))
                  (gde-ferror "no comment in file")))))
      (gde-cerror
         "no grammar files have been read in")))


(defun print-grammar-comment1
   (grammar-comment)
   (let
      ((depth
          (gde-comment-depth grammar-comment)))
      (dotimes (i depth)
         (princ *grammar-comment-char))
      (princ " ")
      (dolist
         (str
            (gde-comment-text
               (fill-gde-comment grammar-comment)))
         (progn
            (cond
               ((equal str *eol-string)
                  (terpri)
                  (dotimes (i depth)
                     (princ *grammar-comment-char)))
               (t (princ str)))
            (princ " ")))
      (terpri)))


;;; A top level comment has just been read from a file or the
;;; keyboard. It is put into the current file. Only one top
;;; level comment allowed per file - comment is printed out at
;;; top of file when it is saved.

(defun insert-grammar-comment (new-comment)
   (let
      ((file (get-new-construct-file)))
      (cond
         ((assoc file *grammar-comments :test
             #'pathname-equal)
            (let
               ((file-comment
                   (assoc file *grammar-comments :test
                      #'pathname-equal)))
               (rplacd file-comment
                  (make-gde-comment
                     (gde-comment-depth
                        (cdr file-comment))
                     (nconc
                        (gde-comment-text
                           (cdr file-comment))
                        (gde-comment-text new-comment))))))
         (t
            (setf *grammar-comments
               (cons (cons file new-comment)
                  *grammar-comments))))))


;;; Get a filename for a new construct to be put in when saved.
;;; Only ask for filename if terminal input. If the file already
;;; exists on disc but is not loaded, complain, otherwise disc
;;; file would get overwritten (instead of new definition being
;;; inserted into it which is presumably what is desired in this
;;; case).

(defun get-new-construct-file nil
   (cond
      (*file-read
         (reorder-grammar-files *file-read))
      ((and *grammar-files
          (yes-for-question "Add input item to file "
             (enough-namestring
                (car *grammar-files))))
         (car *grammar-files))
      (t
         (let
            ((input
                (prompt-always
                   "Filename for new input item? ")))
            (cond
               (input
                  (let
                     ((file
                         (canonise-grammar-file-name input)))
                     (cond
                        ((and
                            (not
                               (member file *grammar-files
                                  :test #'pathname-equal))
                            (probe-file file))
                           (gde-cerror
"file already exists but has not been read in"
                              " - try again")
                           (get-new-construct-file))
                        (t (reorder-grammar-files file)))))
               (t (gde-ferror "command abandoned")))))))


(defun reorder-grammar-files (file)
   (setf *grammar-files
      (cons file
         (remove file *grammar-files :test #'pathname-equal
            :count 1)))
   file)


;;; Make a canonicalised file name out of the user's input. Input
;;; will be in form of a list of symbols whose concatenation
;;; represents the name of the file. '\'s should be used to escape
;;; spaces and comment chars (;) in file names. All file names must be
;;; canonicalised (done here by merge-pathnames) so that the same file
;;; cannot get into the system with more than one name -
;;; otherwise chaos!
;;;
;;; Also provide a special equal test for pathnames - don't seem
;;; to be guaranteed even equalp in CL.

(defun canonise-grammar-file-name (user-input)
   (let
      ((name (parse-namestring (concatl-string user-input))))
      ;; POPLOG 1.3 has bug in merge-pathnames, so omit call (1.4 is OK)
      #+POPLOG name
      #-POPLOG (merge-pathnames name)))


(defun pathname-equal (p1 p2)
   (and
      (equalp (pathname-host p1) (pathname-host p2))
      (equalp (pathname-device p1) (pathname-device p2))
      (equalp (pathname-directory p1) (pathname-directory p2))
      (equalp (pathname-name p1) (pathname-name p2))
      (equalp (pathname-type p1) (pathname-type p2))
      (equalp (pathname-version p1) (pathname-version p2))))


;;; End of file


