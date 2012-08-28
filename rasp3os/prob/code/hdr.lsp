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

;;; Header file for LALR(1) parser system - common macros,
;;; structure definitions, utility and top-level functions

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defvar *sentence-end-marker* '$$$)


;;; Wrapper around without-interrupts

(eval-when (compile load eval)
(defmacro with-uninterrupted-execution (&rest forms)
   `(#+CMU system:without-interrupts
     #+SBCL sb-sys:without-interrupts
     #-(or CMU SBCL) without-interrupts 
      ,@forms))
)


;;; Alternative optimised versions of car and cdr for
;;; application to arguments which are known to be
;;; non-atomic and non-null.

(eval-when (compile load eval)
(defmacro cons-car (x) `(car (the cons ,x)))

(defmacro cons-cdr (x) `(cdr (the cons ,x)))
)


;;; Fast, unsafe versions of vector and bit-vector element access. Inline
;;; vector and bit-vector length. Fast fixnum tests.

(eval-when (compile load eval)
(defmacro vector-svref (v n)
   `(locally
      (declare (optimize (speed 3) (safety 0) (space 0))
         (inline svref))
      (svref (the simple-vector ,v) (the fixnum ,n))))

(defsetf vector-svref (v n) (val)
   `(locally (declare (optimize (speed 3) (safety 0) (space 0)))
      (setf (svref (the simple-vector ,v) (the fixnum ,n)) ,val)))

(defmacro simple-vector-length (v)
   `(locally
      (declare (optimize (speed 3) (safety 0) (space 0))
         (inline length))
      (length (the simple-vector ,v))))

(defmacro fixnum-add-1 (var)
   ;; KCL seems to generate worse code if there are too many
   ;; (the fixnum <form>) declarations around, so omit some. Procyon compiler
   ;; seems not to recognise 1+ with fixnum arg and result as a special case
   `(the fixnum
      #+kcl (1+ ,var)
      #+procyon (i1+ ,var)
      #-(or kcl procyon) (1+ (the fixnum ,var))))

(defmacro bit-vector-sbit (v n)
   `(locally
      (declare (optimize (speed 3) (safety 0) (space 0))
         (inline sbit))
      (sbit (the simple-bit-vector ,v) (the fixnum ,n))))

(defmacro bit-vector-length (v)
   `(locally
      (declare (optimize (speed 3) (safety 0) (space 0))
         (inline length))
      (length (the simple-bit-vector ,v))))

(defmacro lr-fixnum-eql (x y)
   ;; Allegro pre-4.0 compiler produces much better code if test is =
   `(locally
      (declare (optimize (speed 3) (safety 0) (space 0)))
      (#+(and allegro (not (or cltl2 x3j13))) =
       #-(and allegro (not (or cltl2 x3j13))) eql
         (the fixnum ,x) (the fixnum ,y))))

(defmacro lr-fixnum-< (x y)
   `(locally
      (declare (optimize (speed 3) (safety 0) (space 0)) (inline <))
      (< (the fixnum ,x) (the fixnum ,y))))
)


;;; Versions of GDE macro definitions

(eval-when (compile load eval)
(defmacro lr-make-fv-pair (feature value)
   `(cons ,feature ,value))

(defmacro lr-fv-pair-feature (fvpair)
   `(car ,fvpair))

(defmacro lr-fv-pair-value (fvpair)
   `(cdr ,fvpair))
)


(eval-when (compile load eval)
   (defmacro gde-loaded-p nil `(fboundp 'make-fv-pair)))


;;; Versions of parser macro definitions

(eval-when (compile load eval)
(defmacro lr-varp (x)
  ;; arg must be known not to be a category value
  `(locally
      (declare (optimize (speed 3) (space 0) (safety 0))
         (inline symbolp))
      (not (symbolp ,x))))

(defmacro lr-category-index (cat) `(svref ,cat 0))

(defmacro lr-categoryp (x) `(simple-vector-p ,x))

(defmacro lr-unnamed-variable nil ''\@)
)


;;;

(eval-when (compile load eval)
(defstruct cfrule
   name mother daughters)
)


;;; A fast version of getf - but probably less safe than the built-in version

(eval-when (compile load eval)
#+gde-debug
(defmacro lr-fast-getf (lst key default)
   `(getf ,lst ,key ,default))
#-gde-debug
(defun lr-fast-getf (lst key default)
   (declare (optimize (speed 3) (safety 0) (space 0)))
   (tagbody
      (go end)
      lp
      (when (eq key (cons-car lst))
         (return-from lr-fast-getf (cons-car (cons-cdr lst))))
      (setq lst (cons-cdr (cons-cdr lst)))
      end
      (if lst (go lp) (return-from lr-fast-getf default)))))


(defun lr-fast-last (x)
   (declare (optimize (speed 3) (safety 0) (space 0)))
   (let ((prev x))
      (loop
         (unless (consp x) (return))
         (setq prev x)
         (setq x (cons-cdr x)))
      prev))


;;; End of file
