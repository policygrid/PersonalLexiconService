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

;;; ANLT CHART PARSER - BOOT FILE
;;; 
;;; Compiling the parser for later loading by file 'load'. Compilation
;;; is not necessary in POPLOG/MCL/OpenMCL/PROCYON where the contents of
;;; loaded files are compiled by default. In these systems, the source files
;;; themselves should be loaded using file 'load.lsp' and this file ignored.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


#-gde-debug
(eval-when (compile load eval)
   (proclaim
      '(optimize (speed 2) (safety 1) (compilation-speed 0)
         (space 1) #+(or cltl2 x3j13) (debug 1))))


;;; Common Lisp pathname representing all but the name and type of the
;;; files to be compiled/loaded.

(defvar *parser-source-pathname*
   (make-pathname :name nil :type nil :defaults (truename *load-pathname*)))


(eval-when (load eval)
   (let
      ((files
          (mapcar
             #'(lambda (file)
                (merge-pathnames (make-pathname :name file :type "lsp")
                   *parser-source-pathname*))
             '("hdr" "gindex" "unify" "parse"))))
         (mapc
            #'(lambda (file)
                 (load (compile-file file)))
            files)))


;;; End of file
