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

;;; ANLT CHART PARSER - LOAD FILE
;;; 
;;; Loading the parser. In systems with compile-file, the files loaded
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

(defvar *parser-source-pathname*
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
               *parser-source-pathname*)
            :verbose nil :print nil))
      '("hdr" "gindex" "unify" "parse")))


;;; End of file
