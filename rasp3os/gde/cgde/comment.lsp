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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - COMMENT PROCESSING
;;;
;;; Author: John Carroll
;;;
;;; Code to make and justify comments read in the middle of
;;; input declarations. A comment block is constructed by
;;; Get-reply on encountering the comment character (value of
;;; global *grammar-comment-char) - a comment block is a list
;;; whose CAR is *comment and whose CDR is a list of strings,
;;; one per word.
;;;
;;; Entry points:
;;;
;;;  * (defun Make-gde-comment (depth words) ...
;;;  * (defun Gde-comment-p (x) ...
;;;  * (defmacro Gde-comment-depth (x) ...
;;;  * (defmacro Gde-comment-text (x) ...
;;;  * (defun Fill-gde-comment (comment) ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defvar *grammar-comment-marker nil)


(setf *grammar-comment-marker
   (concat-symbol "*comment"))


;;; Comment record definition. Every time a comment is created,
;;; it is optionally filled to the current linelength
;;; (*linelength*).

(defun make-gde-comment (depth words)
   (let
      ((comment
         (cons *grammar-comment-marker
            (cons depth words))))
      (cond
         (*fill-grammar-comments
            (fill-gde-comment comment))
         (t comment))))


(defun gde-comment-p (x)
   (and (consp x)
      (eq (car x) *grammar-comment-marker)))


(defun gde-comment-depth (x)
   (cadr x))


(defun gde-comment-text (x)
   (cddr x))


;;; Fill a comment block.

(defun fill-gde-comment (comment)
   (cons *grammar-comment-marker
      (cons (gde-comment-depth comment)
         (fill-gde-comment1 (gde-comment-text comment) nil 0
            (- (or *linelength* (gde-linelength)) 3)
            *eol-string))))


(defun fill-gde-comment1
   (in out line-length right-margin
      eol-string)
   (let
      ((word-length 0))
      (loop
         (cond
            ((null
                (and in
                   (not (equal (car in) eol-string))))
               (return nil)))
         (setf word-length
            (1+ (length (car in))))
         (setf line-length
            (+ line-length word-length))
         (cond
            ((> line-length right-margin)
               (when
                  (and out
                     (not (equal (car out) eol-string)))
                  (push eol-string out))
               (push (car in) out)
               (setf line-length word-length))
            (t (push (car in) out)))
         (setf in (cdr in)))
      (cond
         ((and in (cdr in))
            (setf in (cdr in))
            (cond
               ((equal (car in) eol-string)
                  (fill-gde-comment1 (cdr in)
                     (cons eol-string (cons eol-string out)) 0
                     right-margin eol-string))
               ((gde-whitespace-p
                   (car
                      (coerce (princ-to-string (car in))
                         'list)))
                  (fill-gde-comment1 in (cons eol-string out)
                     0 right-margin eol-string))
               (t
                  (fill-gde-comment1 in out line-length
                     right-margin eol-string))))
         (t (nreverse out)))))


;;; End of file

