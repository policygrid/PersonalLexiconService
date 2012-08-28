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

;;; Special reader for large circular / re-entrant list structures. Note
;;; that this is not general-purpose since ignores re-entrancies inside
;;; non-list objects, requires that read-circular is called (rather
;;; than read) and assumes use of #n syntax is well-formed. Also deals
;;; with #. occuring in limited contexts.
;;;
;;; Had to write this since otherwise just calling read, Allegro 3.1 locks
;;; up into a loop, generating more and more garbage. Procyon 2.1.4 gets
;;; stack overflow on long lists.
;;;
;;; This would be definition if there were no problems with reading such
;;; lists:
;;;
;;; (defun read-circular (stream) (read stream))

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defvar *read-circle-readtable* (copy-readtable))
(defvar *read-circle-table*)
(defvar *read-circular-hash-marker* (gensym))
(defvar *read-circular-eval-marker* (gensym))


(eval-when (load eval)
   (set-dispatch-macro-character #\# #\.
      #'(lambda (stream subchar arg)
         (declare (ignore subchar arg))
         (cons *read-circular-eval-marker* (read stream t nil t)))
      *read-circle-readtable*))


(eval-when (load eval)
   (set-dispatch-macro-character #\# #\#
      #'(lambda (stream subchar arg)
         (declare (ignore stream subchar))
         (cons *read-circular-hash-marker* arg))
      *read-circle-readtable*))


(eval-when (load eval)
   (set-dispatch-macro-character #\# #\=
      #'(lambda (stream subchar arg)
         (declare (ignore subchar) (simple-vector *read-circle-table*))
         (unless (integerp arg) (error "invalid argument ~S to #\\=" arg))
         (let ((form (read stream t nil t)))
            (when (>= arg (length *read-circle-table*))
               (let ((new (make-array (+ arg 50))))
                  (replace new *read-circle-table*)
                  (setq *read-circle-table* new)))
            (setf (svref *read-circle-table* arg) form)
            (cons *read-circular-hash-marker* arg)))
      *read-circle-readtable*))
      
      
(defun read-circular (stream)
   (let*
      ((*readtable* *read-circle-readtable*)
         (*read-circle-table* (make-array 50))
         (res (read stream)))
      (dotimes (n (length (the simple-vector *read-circle-table*)))
         (read-circular-expand (svref *read-circle-table* n)))
      (read-circular-expand res)
      res))


(defun read-circular-expand (x)
   (loop
      (when (atom x) (return))
      (cond
         ((atom (car x)))
         ((eq (caar x) *read-circular-hash-marker*)
            (rplaca x (svref *read-circle-table* (cdar x))))
         ((eq (caar x) *read-circular-eval-marker*)
            (read-circular-expand (cdar x))
            (rplaca x (eval (cdar x))))
         (t
            (read-circular-expand (car x))))
      (if (and (consp (cdr x)) (eq (cadr x) *read-circular-hash-marker*))
         (return
            (rplacd x (svref *read-circle-table* (cddr x))))
         (setq x (cdr x)))))


#|
;;; Test case

(let ((*print-circle* t))
   (with-input-from-string (str "(#1=(a . #2=(b #1# c . t)) #3=(#3# . #3#) #2#)")
      (print (read-circular str))
      nil))
|#


;;; End of file
