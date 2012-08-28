#|----------------------------------------------------------------------------|
 | Copyright 2002, 2006, 2011 John Carroll, Ted Briscoe, Rebecca Watson       |
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

;;; PROB CODE - LOAD.LSP

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


#-gde-debug
(eval-when (compile load eval)
   (proclaim
      '(optimize (speed 2) (safety 1) (compilation-speed 0)
         (space 1) #+(or cltl2 x3j13 ansi-cl) (debug 1))))


;;;

(defparameter +prob-code-pathname+
   (make-pathname :name nil :type "lsp" :defaults (truename *load-pathname*)))
(defparameter +prob-data-pathname+
   (make-pathname
      :directory (append (pathname-directory (truename *load-pathname*)) '(:up "data"))
      :name nil :type nil
      :defaults (truename *load-pathname*)))
(defparameter +prob-greval-pathname+
   (make-pathname
      :directory (append (pathname-directory (truename *load-pathname*)) '(:up "greval"))
      :name nil :type nil
      :defaults (truename *load-pathname*)))


#+openmcl
(setq *default-external-format* :inferred)

(defun load-compiled-file (p)
   (unless (probe-file p) (error "Source file ~A does not exist" p))
   #+(or procyon openmcl mcl poplog cormanlisp sbcl) (load p)
   #-(or procyon openmcl mcl poplog cormanlisp sbcl)
   (let ((cp (compile-file-pathname p)))
      (when (or (not (probe-file cp))
                (< (file-write-date cp) (file-write-date p)))
         (compile-file p))
      (unless (probe-file cp)
         (error "Compilation of ~A did not yield a compiled file named ~A" p cp))
      (load cp)))


;;; code

(dolist (x '(#+mcl "mcl-timers"
             ;; #+openmcl "openmcl-timed-wait-patch" ; should only need for v1.0
             "hdr" "tools" "readcirc" "install" "in-text" "ask-stats"
             "dparse" "partial" "vphrasal" "vsubcat"
             "out-analysis" "gramrel" "extract-grs"))
   (load-compiled-file
      (merge-pathnames x +prob-code-pathname+)))


;;; data

(setq *tagged-words t)
(read-grammar (merge-pathnames "tsg15" +prob-data-pathname+))

(progn
  (install-unification-grammar t)
  (install-grammar-from-file
     (merge-pathnames "tsg15.backbone" +prob-data-pathname+))
  (install-machine-from-file
     (merge-pathnames "tsg15.machine" +prob-data-pathname+)))

(lr1-read-history-probs
   (merge-pathnames "tsg15.probs-ti" +prob-data-pathname+))


;;;

(read-subcat-scores
   (merge-pathnames "A1-short" +prob-data-pathname+))

(read-phrasal-verbs
   (merge-pathnames "phrasal-vle" +prob-data-pathname+))


;;; End of file
