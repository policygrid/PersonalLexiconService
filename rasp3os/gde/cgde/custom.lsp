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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - SYSTEM CUSTOMISATION
;;;
;;; Author: John Carroll
;;;
;;; This file contains code to customise a particular
;;; implementation of Common Lisp in order to be able to run
;;; the GDE.
;;;
;;; Conditional reading (#+ and #-) assumes the following versions
;;; of Common Lisp implementations:
;;;
;;;    (and HP T)   - HP CL I version 1.01
;;;    KCL          - Kyoto CL version 1.25 and AKCL 1.530 / 1.605
;;;    XEROX        - Xerox CL, Lyric release
;;;    POPLOG       - POPLOG CL version 1.0
;;;    (and ALLEGRO (not (or cltl2 x3j13)))
;;;                 - Franz Allegro CL version 3.1
;;;    (and ALLEGRO (or cltl2 x3j13))
;;;                 - Franz Allegro CL version 4.0, 4.1, 4.2
;;;    PROCYON      - Procyon CL release 2.1, 3.3
;;;    :CORAL       - Coral CL version 1.2 and MCL (see below)
;;;    (and LUCID (not (or LCL3.0 cltl2 x3j13)))
;;;                 - Sun (Lucid) CL version 2.1.1 & HP CL II rev A.02.16
;;;    LCL3.0       - Sun (Lucid) CL version 3.0.1
;;;    (and LUCID (or cltl2 x3j13))
;;;                 - Lucid CL version 4.0 & HP CL II rev A.04.00
;;;    LISPWORKS    - Harlequin LispWorks version 2.1
;;;    CMU          - CMU CL version 15, 17f
;;;    MCL          - Macintosh CL 4.0, 4.1, 4.2
;;;    OPENMCL      - OpenMCL Version (Beta: Darwin) 0.11
;;;
;;; Effort has been made to write this code so that it will work
;;; correctly with other versions of these implementations.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Global system-dependent compilation proclamations.

#+ALLEGRO
(eval-when (compile load eval)
   (shadow '(defmacro defstruct defvar defparameter defconstant)))


#+ALLEGRO
(eval-when (compile load eval)
   (cl:defmacro defmacro (&rest args)
      `(eval-when (compile load eval)
          (cl:defmacro ,@args)))
   (cl:defmacro defstruct (&rest args)
      `(eval-when (compile load eval)
          (cl:defstruct ,@args)))
   (cl:defmacro defvar (&rest args)
      `(eval-when (compile load eval)
          (cl:defvar ,@args)))
   (cl:defmacro defparameter (&rest args)
      `(eval-when (compile load eval)
          (cl:defparameter ,@args)))
   (cl:defmacro defconstant (&rest args)
      `(eval-when (compile load eval)
          (cl:defconstant ,@args))))


#+(and HP T)
(proclaim '(extn:upward-closures nil))


#+LUCID
(eval-when (compile load eval)
   (setf *redefinition-action* nil)
   (compiler-options :messages nil))


#+ALLEGRO
(eval-when (compile load eval)
   #+(or cltl2 x3j13)
   (progn
      (setq excl:*record-source-file-info* nil)
      (setq excl:*record-xref-info* nil))
   #-(or cltl2 x3j13)
   (setq excl:*record-source-files* nil))


;;; Setup storage usage, garbage collection message stuff. Print
;;; GC messages unless GC is frequent and reasonably quick.
;;; *** N.B. in KCL / AKCL with a large grammar loaded the storage
;;; allocated to certain types and certainly to relocatable pages
;;; will need to be explicitly increased - use functions
;;; system:allocate and system:allocate-relocatable-pages.

(eval-when (load eval)
   #+POPLOG
   (progn
      (setf pop11::popgctrace t)
      (setf pop11::popmemlim 1800000))
   #+LUCID
   (change-memory-management :growth-limit 750)
   #+ALLEGRO
   (setq *global-gc-behavior* :auto)
   #+KCL
   (progn
      ;; (system:allocate-relocatable-pages 300)
      (setf si:*notify-gbc* nil))
   #+LUCID
   (setf *gc-silence* nil))


;;; Control output formatting - line length / breaking etc. GDE
;;; output to the terminal is guaranteed to be within the linelength
;;; returned by the function gde-linelength (in file toploop).
;;; POPLOG is awkward in that lines of output are broken at any
;;; whitespace after poplinewidth - so variable should be set
;;; to at least the value of gde-linelength. Otherwise lines are
;;; unilaterally split at poplinemax (even in the middle of printing
;;; symbols), so to make sure files of s-expressions written using
;;; prin1 can be read back properly, poplinemax must be set to a value
;;; substantially larger that poplinewidth.

#+POPLOG
(eval-when (load eval)
   (setf pop11::poplinemax 100)
   (setf pop11::poplinewidth 78))


(eval-when (load eval)
   (setf *print-pretty* nil))


(progn
   (defvar *file-page-width 78)
   (defvar *terminal-page-width 78))


;;; Return the linelength for printing out rules etc to the
;;; terminal and to file. If filename is not supplied or nil,
;;; the output stream concerned will be *standard-output*. This
;;; may be either the terminal or bound temporarily to an open file
;;; stream.

(defun gde-linelength (&optional filename)
   (if filename *file-page-width
      #+PROCYON
         (if (cg:synonym-stream-p *standard-output*)
            (1- (line-length *terminal-io*))
            *file-page-width)
      #-PROCYON
         *terminal-page-width
      ))


;;; Set the linelength of an output stream so that Lisp pretty-printer
;;; inserts newlines and does not print very long lines.

(defun set-gde-linelength (stream)
   #-PROCYON (declare (ignore stream))
   #+PROCYON
      (set-right-margin stream *file-page-width)
   )


;;; Chop off top of transcript window when it gets large.
;;;
;;; #+PROCYON
;;; (setf (symbol-function 'text-edit::null-event-handler)
;;;    #'(lambda (window)
;;;         (when
;;;            (and (eq window *terminal-io*)
;;;               (> (file-length window) 20000))
;;;            (text-edit:set-region window 0 10000)
;;;            (text-edit:delete-to-kill-buffer window)
;;;            (common-graphics:pop-lisp-clipboard)
;;;            (text-edit:end-of-file window))))


;;; Control of input. Make sure that reading terminal eof does not
;;; stop the ability to carry on reading from the terminal io
;;; stream.

#+KCL
(eval-when (load eval)
   (setf si:*ignore-eof-on-terminal-io* t))


;;; Set up the file type for grammar files when they are backed
;;; up by the GDE after being changed for the first time in a session.

(defparameter *backup-file-type "bak")


;;; Initialisation forms: arrange for them to be executed on
;;; reloading the image, or alternatively by the core image
;;; saving function just before it does its work.
;;;
;;; Reset the GDE top loop history count, unmark files
;;; as backed up, clear saved sentences and re-open any
;;; closed morphology package lexicon streams.

(defvar *gde-init-forms*
   '(#+OPENMCL
     (progn (ccl:set-lisp-heap-gc-threshold 32000000)
     	(ccl:use-lisp-heap-gc-threshold)) ; on every startup
     #+SBCL
     (setf (sb-ext:bytes-consed-between-gcs)
        (if (> most-positive-fixnum (ash 2 32)) (* 96 1024 1024) (* 48 1024 1024)))
     (setf *backed-up-files nil)
     (setf *parsed-sentences nil)
     (setf *unsaved-parsed-sentences nil)
     #+gde-morph
     (ensure-lexicon-streams-open)
     #+(or OPENMCL SBCL)
     (parse-and-execute-command-line)
     ))


#+(or OPENMCL SBCL)
(defun parse-and-execute-command-line ()
   ;; the car of the command line arg list is the name of the lisp
   ;; kernel, and the rest should be strings -- assume option name / value
   ;; pairs
   (do
      ((lst (cdr #+SBCL sb-ext:*posix-argv* #+OPENMCL ccl::*command-line-argument-list*) 
            (cddr lst)))
      ((or (null lst) (null (cdr lst))))
      (when (equal (car lst) "-e")
         (format *standard-output* "~&~S~%"
            (eval (with-standard-io-syntax (read-from-string (cadr lst))))))
      (finish-output *standard-output*)))


;;; Definitions of system-dependent functions. Conscount should return the
;;; amount of heap storage so far allocated (or zero if not known), and gctime
;;; the number of milliseconds consumed so far by garbage collection and Lisp
;;; overheads (or zero if not known). Gctime is already defined in MCL

(defun conscount nil
   #+XEROX (il:conscount)
   #+PROCYON (- 130000000 (pro:free-store))
   #+CMU (extensions:get-bytes-consed)
   #+SBCL (sb-ext:get-bytes-consed)
   #-(or XEROX PROCYON CMU SBCL) 0)


#-MCL
(defun gctime nil
   #+XEROX (il:clock 3)
   #+PROCYON
   (floor (* (system:%gctime t) 1000) internal-time-units-per-second)
   #-(or XEROX PROCYON) 0)


;;; *** May need changing ***. The Shell function invokes the OS
;;; command interpreter. Lisp-top-loop starts an embedded lisp
;;; read-eval-print loop (a minimal function to do this called
;;; top-loop is defined in file hdr). Request-gc invokes the garbage
;;; collector.

#-(or ALLEGRO PROCYON LUCID)
   ;; Appropriate function already defined with this name in
   ;; ALLEGRO, PROCYON and LUCID
   (defun shell nil
      #+(and HP T) (system:host-command-function "sh")
      #+KCL (system "sh")
      #+POPLOG (pop11::sysobey "sh")
      #+XEROX (xcl:add-exec :profile "CL" :tty t)
      #-(or (and HP T) KCL POPLOG XEROX) nil
      )


(defun lisp-top-loop nil
   #+(and HP T)
      (unwind-protect
         (system:listener 'LISP ": Entering Lisp ... type !q to exit")
         (setf impl:promptstring* ""))
   #+POPLOG
      (progn
         (pop11-val "lisp_compile(charin)")
         (setf *read-prompt* "")
         (terpri))
   #+ALLEGRO
      (progn
         (format t "~%Entering Lisp ... type :return to exit~%")
         (tpl:top-level-read-eval-print-loop))
   #+XEROX (xcl:add-exec :profile "CL" :tty t)
   #+PROCYON
      (progn
         (format t "~%Entering Lisp ... call function quit to exit~%")
         (toploop:toploop)
         (terpri t))
   #+:CORAL
      (progn
         (format t "~%Entering Lisp ... call function gde-top-loop to exit~%")
         (ccl:%set-toplevel (symbol-function 'ccl:toplevel-loop))
         (ccl:toplevel))
   #+CMU
       (common-lisp::%top-level)
   #-(or (and HP T) POPLOG ALLEGRO XEROX PROCYON :CORAL)
       (break "Entering Lisp top loop")
   )


(defun request-gc nil
   #+(and HP T) (system:gc)
   #+KCL (gbc t)
   #+POPLOG (pop11::sysgarbage)
   #+ALLEGRO (excl:gc t)
   #+XEROX (il:reclaim)
   #+PROCYON (room t)
   #+:CORAL (ccl:gc)
   #+(and LUCID (not (or LCL3.0 cltl2 x3j13))) (system:gc)
   #+(and LUCID (or LCL3.0 cltl2 x3j13)) (gc)
   #+CMU (extensions:gc)
   nil
   )


;;; Turn off pretty-printing.

(eval-when (load eval)
   (setq *print-pretty* nil))


;;; *** May need changing ***. Control the format of error
;;; reporting / backtraces, and behaviour after interrupts.
;;;
;;; Make sure errors and interrupts are reported, and set up any
;;; unwinding from a resulting break loop to end up back in the
;;; GDE command loop.

(eval-when (load eval)
   #+POPLOG
      (progn
         (setf *break-on-errors* nil)
         (defun top-loop-continue nil
            (setf *standard-input* (make-synonym-stream '*terminal-io*))
            (setf *standard-output* (make-synonym-stream '*terminal-io*))
            (format t "Back to Command Loop~%")
            (top-loop1 #'gde-top-read #'gde-top-print
               #'gde-top-eval "Gde" "")
            (pop11::sysexit))
         (pop11-val
            "define global lisp_val with_nargs 1;
                lisp_compile(stringin())
             enddefine")
         (pop11-val
            "define global top_loop_continue;
                lisp_val('(TOP-LOOP-CONTINUE)')
             enddefine")
         (pop11-val
            "define global popsetpop;
                chain(top_loop_continue)
             enddefine"))
   #+(and HP T)
      (progn
         (setf system:*break-hook* 'gde-break-function)
         (defun gde-break-function ()
            (when
               (and (find-package 'debug) (fboundp 'debug:backtrace))
               (debug:backtrace))
            (extn:exception-quit)))
   #+(and LUCID (not (or LCL3.0 cltl2 x3j13)))
      (progn
         (defvar *top-loop-level 0)
         (defadvice
            (lucid::debugger-top-level-abort top-abort) (&rest x)
            (cond
               ((> *top-loop-level 0)
                  (format t "Back to Command Loop~%")
                  (throw nil nil))
               (t
                  (apply-advice-continue x)))))
   )


;;; *** May need changing ***. Funcall fn on args and prevent any
;;; errors unwinding further. Return nil if there was an error.
;;; Must actually print a message if there was an error - if this
;;; cannot be arranged then leave this function just as funcall and
;;; make sure unwinding will not go through GDE command loop.

(defmacro catch-all-errors (fn &rest args)
   #+(and HP T)
      `(extn:break-on-errors (funcall ,fn ,@args))
   #+KCL
      (let ((tag (gensym)) (result (gensym)))
         `(multiple-value-bind (,tag ,result)
             (si:error-set
                (list 'funcall (list 'quote ,fn)
                   ,@(mapcar #'(lambda (arg) `(list 'quote ,arg))
                        args)))
             (if ,tag nil ,result)))
   #+ALLEGRO
       `(catch 'top-level::top-level-break-loop
           (funcall ,fn ,@args))
   #+(and XEROX (not (or cltl2 x3j13)))
      (let ((result (gensym)))
         `(let
             ((,result
                 (il:errorset
                    (list 'funcall (list 'quote ,fn)
                       ,@(mapcar #'(lambda (arg) `(list 'quote ,arg))
                            args))
                    t)))
             (if ,result (car ,result))))
   #+(and PROCYON (not (or cltl2 x3j13)))
      (let ((result (gensym)))
         `(let
             ((,result
                 (procyon:trap-exits (funcall ,fn ,@args))))
             (if ,result (car ,result))))
   #+(and :CORAL (not (or cltl2 x3j13 ansi-cl)))
      `(ccl:catch-error
          (ccl:catch-abort (ccl:catch-cancel (funcall ,fn ,@args))))
   #+(and (not ALLEGRO) (or LCL3.0 (or cltl2 x3j13 ansi-cl)))
      ;; the following should work for all lisps containing the ANSI CL
      ;; condition system
      `(with-simple-restart
         (abort "Return to GDE command loop.")
         (funcall ,fn ,@args))
   #-(or (and HP T) KCL ALLEGRO XEROX PROCYON :CORAL LCL3.0 cltl2 x3j13 ansi-cl)
       `(catch nil (funcall ,fn ,@args))
   )


;;; End of file

