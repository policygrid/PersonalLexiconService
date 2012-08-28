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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - COMMON LISP HEADER FILE
;;;
;;; Author: John Carroll
;;;
;;; This file contains relatively implementation independent
;;; global declarations and definitions.
;;;
;;; Most of the GDE source code was semi-automatically translated
;;; from Cambridge Lisp into Common Lisp around 1987. This accounts
;;; for some of the strange and non-idiomatic coding style. Some
;;; parts have been rewritten or extensively modified since then -
;;; they should look rather better!

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Say where the help files for the GDE are. Source pathname will
;;; be available at compile (read) time but fix so that the variable
;;; holding the pathname is not necessary at load time.

(progn
   (defvar *gde-help-file
      '#.(namestring
            (merge-pathnames (make-pathname :name "help")
               *gde-source-pathname*)))
   (defvar *parser-help-file
      '#.(namestring
            (merge-pathnames (make-pathname :name "phelp")
               *gde-source-pathname*)))
   (defvar *generator-help-file
      '#.(namestring
            (merge-pathnames (make-pathname :name "ghelp")
               *gde-source-pathname*)))
   (defvar *order-help-file
      '#.(namestring
            (merge-pathnames (make-pathname :name "ohelp")
               *gde-source-pathname*))))


;;; A simple but customisable top level loop for Lisp systems
;;; which do not have one built in. Clear-history should be
;;; called if possible if the state of the system is dumped as
;;; a reloadable core image.
;;;
;;; Some implementations output a
;;; prompt when waiting for input from terminal io. Stop them
;;; doing this as the program does it itself.

(progn
   (defvar *top-loop-level 0)
   (defvar *history-count 0))


(defun clear-history nil
   (setf *history-count 0)
   (setf *top-loop-level 0))


(defun top-loop-read (hist-count prompt arrows)
   (format *terminal-io*
      "~&~%~A ~A~A " hist-count prompt arrows)
   (finish-output *terminal-io*)
   (let ((ans nil))
      (loop
         #+(and procyon 2.1) (te:set-mark *terminal-io*)
         #+(and procyon (not 2.1)) (:mark *terminal-io*)
         #+(and mcl (not openmcl)) (evaluate-queued-forms)
         (setq ans
            (catch-all-errors 'read-line *standard-input* nil nil))
         (cond
            ((or (null ans) (equal ans ""))
               (format *terminal-io*
                  #+(or allegro lispworks openmcl)
                    "~A ~A~A " ; fresh-line gives unintended blank line
                  #-(or allegro lispworks openmcl)
                    "~&~A ~A~A "
                  hist-count prompt arrows)
               (finish-output *terminal-io*))
            (t
               (return
                  (string-left-trim '(#\Space) ans)))))))

#+(and mcl (not openmcl))
(defun evaluate-queued-forms ()
   ;; have to explicitly check for forms that are waiting from an external
   ;; evaluate command
   (loop
      (catch-all-errors
         #'(lambda nil
              (multiple-value-bind (form pendingp) (ccl:get-next-queued-form)
                 (when (or form pendingp)
                    (eval form)
                    (fresh-line *terminal-io*)))))
      (catch-all-errors
         #'(lambda nil
              (if (listen *terminal-io*)
                 (return-from evaluate-queued-forms)
                 (sleep 0.05))))))


(defun top-loop
   (top-print top-eval top-prompt banner)
   (format t
      "~&~%: Entering ~A ... (level ~A)~%" banner
      (+ *top-loop-level 1))
   (top-loop1 top-print top-eval top-prompt
      banner))


(defun top-loop1 (top-print top-eval top-prompt banner)
   (declare (ignore banner))
   #+POPLOG (setf *read-prompt* "")
   #+(and HP T) (setf impl:promptstring* "")
   (let
      ((*top-loop-level (+ *top-loop-level 1))
         (input nil) (answer nil))
      (loop
         (setf *history-count (+ *history-count 1))
         (setf input
            (top-loop-read
               *history-count top-prompt
               (make-string *top-loop-level :initial-element #\>)))
         (cond
            ((top-loop-quit-p input)
               (if
                  (or (> *top-loop-level 1)
                     (y-or-n-p
                        "Do you really want to exit? "))
                  (return nil)))
            (t
               (setf answer
                  (catch-all-errors top-eval input))
               (catch-all-errors top-print answer))))
      (when (= *top-loop-level 1)
         #+POPLOG (setf *read-prompt* "== ")
         #+(and HP T) (setf impl:promptstring* ">> ")
         nil)
      (format t "~&~%: Exiting ~A... (level ~A)~%"
         top-prompt *top-loop-level)))


(defun top-loop-quit-p (input)
   (let
      ((quit-p
          (mismatch input "quit" :test #'char-equal)))
      (or (null quit-p)
         (and (> quit-p 0) (= quit-p (length input))))))


;;; Configure the toploop so that #\Newline behaves the same as
;;; enter.

#+PROCYON
(eval-when (load eval)
   (comtab:set-event-function toploop::*toploop-comtab* #\Newline
      #'(lambda (x)
           (file-position *terminal-io* :end)
           (terpri *terminal-io*)
           (funcall
              '#.(comtab:event-function toploop::*toploop-comtab*
                   #+(and macintosh 2.1) enter
                   #+(and macintosh (not 2.1)) macintosh:vk-enter
                   #+pc pc:vk-enter)
              x))))


;;; Fast versions of some sequence functions. F-find is not
;;; general since it assumes that the first argument is a list
;;; rather than a sequence, and that the key and test arguments
;;; will be function names, not lambda expressions, so it can
;;; produce a direct call to a function and not have to use
;;; funcall.

(defmacro f-find (val lst &key key (test '(quote eql)))
   (let*
      ((lstvar (gensym)) (valvar (gensym))
       (key-form
         `(,(cadr key) (car (the cons ,lstvar)))))
      (if
         (or (symbolp val)
            (and (eq (car val) 'the) (symbolp (third val))))
         (setf valvar val))
      `(do
          (,@(unless (eq valvar val)
                (list (list valvar val)))
           (,lstvar (the list ,lst) (cdr (the cons ,lstvar))))
          ((or (atom ,lstvar)
              (,(cadr test)
                  ,(if (and (consp val) (eq (car val) 'the))
                      (list 'the (second val) valvar)
                      valvar)
                  ,(if (and (consp val) (eq (car val) 'the))
                      (list 'the (second val) key-form)
                      key-form)))
           (car ,lstvar))
         #+KCL
         (declare
            (object ,lstvar ,@(unless (eq valvar val) (list valvar))))
         )))


;;; (defmacro f-find (val lst &key key (test '(quote eql)))
;;;    `(find ,val ,lst :key ,key :test ,test))


(defun remove-list-1 (el lst)
   (cond
      ((atom lst) nil)
      ((eql (car (the cons lst)) el) (cdr (the cons lst)))
      (t
         (cons (car (the cons lst))
            (remove-list-1 el (cdr (the cons lst)))))))


(defmacro ncons (x) `(cons ,x nil))


;;; Portable embedding ('advising') existing function definitions.

(defmacro embed (name defn)
   (let ((embed-name
            (gentemp (concatenate 'string (string name) "-embedding"))))
      `(eval-when
         #+(or cltl2 x3j13 ansi-cl) (:load-toplevel :execute)
         #-(or cltl2 x3j13 ansi-cl) (load eval)
         (defun ,embed-name ,@(cdr (subst-embedding name defn)))
         (cond
            ((get ',name 'embedded-defn)
               (warn "Could not embed ~A - function is already embedded"
                  ',name))
            ((fboundp ',name)
               (setf (get ',name 'embedded-defn) (symbol-function ',name))
               (setf (symbol-function ',name)
                  (symbol-function ',embed-name)))
            (t
               (warn "Could not embed ~A - function is not defined"
                  ',name)))
         ',name)))


(defun subst-embedding (name x)
   (cond
      ((atom x) x)
      ((eq (car x) name)
         `(funcall
             (get ',name 'embedded-defn)
             ,@(mapcar #'(lambda (e) (subst-embedding name e)) (cdr x))))
      (t
         (mapcar #'(lambda (e) (subst-embedding name e)) x))))


(defmacro unembed (name)
   `(progn
      (cond
         ((get ',name 'embedded-defn)
            (setf (symbol-function ',name) (get ',name 'embedded-defn))
            (remprop ',name 'embedded-defn))
         ((fboundp ',name)
            (warn "Could not unembed ~A - function is not embedded" ',name))
         (t
            (warn "Could not unembed ~A - function is not defined" ',name)))
      ',name))


;;; If the morphology system functions to apply entry completion,
;;; multiplication and consistency rules are not defined, then
;;; define them as null functions, since words in the GDE
;;; lexicon go through these types of rule even if the full-blown
;;; morphology system is not in use.

(eval-when (load eval)
   (unless (fboundp 'd-applycrs)
      (setf (symbol-function 'd-applycrs)
         #'(lambda (rules entry) (declare (ignore rules)) entry))
      (setf (symbol-function 'd-applymrs)
         #'(lambda (rules entry) (declare (ignore rules)) (list entry)))
      (setf (symbol-function 'd-applyccs)
         #'(lambda (rules entry) (declare (ignore rules)) entry))
      (setf (symbol-function 'd-markunload)
         #'(lambda (type) (declare (ignore type)) nil))))


;;; Define the morphology system macros used by the GDE here,
;;; so that GDE and morph system may be compiled in either order
;;; (even though morph system must be loaded before GDE).

(defmacro dk-demands nil `(intern "demands"))

(defmacro dk-and nil `(intern "and"))

(defmacro dk-endtree nil `(intern "AA"))

(defmacro dk-le nil `(intern ".le"))

(defmacro dk-le-ma nil `(intern ".le.ma"))

(defmacro dk-category nil `(intern "category"))


;;; End of File
