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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - LOAD FILE
;;;
;;; Author: John Carroll
;;;
;;; Loading the GDE. In systems with compile-file, the files loaded
;;; should have already have been compiled. On the other hand, in
;;; systems such as POPLOG/MCL/OpenMCL/PROCYON that compile on-the-fly
;;; in load it is the source files that are loaded here.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


#-gde-debug
(eval-when (compile load eval)
   (proclaim
      '(optimize (speed 2) (safety 1) (compilation-speed 0)
         (space 1) #+(or cltl2 x3j13 ansi-cl) (debug 1))))


;;; Common Lisp pathname representing all but the name and type of the
;;; files to be compiled/loaded.

(defvar *gde-source-pathname*
   (make-pathname :name nil :type nil :defaults (truename *load-pathname*)))


;;; Specify the default compiler output file type to load for
;;; those systems which do not try looking for files of that type.

(eval-when (load eval)
   (mapc
      #'(lambda (file)
         (load
            (merge-pathnames
               (make-pathname :name file .
                  #+(or PROCYON :CORAL POPLOG SBCL) (:type "lsp")
                  #+XEROX (:type "dfasl")
                  #-(or PROCYON :CORAL POPLOG XEROX SBCL) nil)
               *gde-source-pathname*)
            :verbose nil :print nil))
      '("custom" "hdr" "records" "defname" "toploop" "comment"
        "command" "order" "flags" "printer" "syntax"
        "files" "alias" "realias"
        "lpexp" "metaexp" "compile" "dictint"
        "dtree" "parse" "genrate" "semantics"
        "defns" "view" "names"
        "invalid" "graminp" "gramdel"
        "gramed" "gramove"
        . #+gde-morph ("dictfns" "dpatch") #-gde-morph nil)))


;;; Function to call to save a GDE core image to a specified file.
;;; Stash pathnames of open lexicon streams back where they are
;;; stored so that streams can be re-opened when image is restarted.
;;; Arrange for the forms in *gde-init-forms* (set up in file custom)
;;; to be evaluated when image is re-entered, and the gde top loop
;;; to be entered.

(defun save-gde-image (file &rest args)
   (dolist (lex d-lexicon)
      (setf (car lex) (namestring (truename (car lex)))))
   #+(and HP T)
      (sys:save-world file
         (append *gde-init-forms*
            '((gde-top-loop) (system:exit)))
         "")

   #+KCL
      (progn
         (setf (symbol-function 'si:top-level)
            `(lambda nil
                ,@ *gde-init-forms*
                (gde-top-loop)))
         (save file))

   #+POPLOG
      (when
         (savelisp
            (concatenate 'string (string file) ".psv"))
         (eval (cons 'progn *gde-init-forms*))
         (gde-top-loop)
         (bye))

   #+ALLEGRO
      (let ((fn
               (compile nil
                 `(lambda nil
                     ,@ *gde-init-forms* (gde-top-loop) (exit 0)))))
         (excl:gc t)
         (if (boundp '*restart-app-function*)
            (progn
               (setq *restart-app-function* fn) ; from v4.? onwards
               (dumplisp :name file))
            (dumplisp :name file :restart-function fn :read-init-file t)))

   #+XEROX
      (progn
         (unless (member 'gde-tidyup il:aroundexitfns)
            (setf (symbol-function 'gde-tidyup)
               `(lambda (event)
                   (if
                      (member event
                         '(il:afterlogout il:aftersysout il:aftermakesys
                             il:aftersavevm))
                      ,@ *gde-init-forms*)))
            (push 'gde-tidyup il:aroundexitfns))
         (if (consp (il:sysout file))
            (gde-top-loop)))

   #+PROCYON
      (save-image :image-file file :start-up-function
         (compile nil
            `(lambda nil
                ,@ *gde-init-forms*
                (gde-top-loop))))

   #+(and MCL (not OPENMCL))
      (save-application (pathname file) :toplevel-function
         (compile nil
            `(lambda nil
                (process-run-function "GDE"
                   #'(lambda ()
                        (let ((ccl::*listener-p* t))
                           (ccl::startup-ccl "init")
                           ,@ *gde-init-forms*
                           (gde-top-loop)))))))

   #+OPENMCL
      (apply #'save-application (pathname file) :toplevel-function
         (compile nil
            `(lambda nil
                ,@ *gde-init-forms* (gde-top-loop) (quit)))
         args)
   #+SBCL
      (apply #'sb-ext:save-lisp-and-die (pathname file) :purify t :toplevel
         (compile nil
            `(lambda nil
                ,@ *gde-init-forms* (gde-top-loop) (quit)))
         args)
   #+LUCID
      (disksave file :restart-function
         ;; Lucid 3.0 gives segmentation violation for restart function
         ;; below so use simpler treatment
         #+LCL3.0 #'gde-top-loop
         #-LCL3.0
         (compile nil
            `(lambda nil
                ,@ *gde-init-forms*
                (gde-top-loop) (quit)))
         #+(or LCL3.0 CLTL2) :full-gc #-(or LCL3.0 CLTL2) :gc t)

   #+LISPWORKS
      (system:save-image file :restart-function
         (compile nil
            `(lambda nil
                ,@ *gde-init-forms*
                (gde-top-loop) (bye))))

   #+CMU
      (extensions:save-lisp file :purify t :print-herald nil
         :init-function
         (compile nil
            `(lambda nil
                ,@ *gde-init-forms*
                (gde-top-loop) (quit))))
   )


;;; End of file
