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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - SEMANTIC INTERPRETATION
;;;
;;; Author: John Carroll
;;;
;;; This file contains simple example code for scoping the LFs
;;; produced by the big grammar and post-processing them into a
;;; more readable form. It defines an 'interpret-sentence' function
;;; that gets called as a result of invoking the GDE parser command
;;; loop function 'interpret'.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

#-gde-debug
(eval-when (compile load eval)
   (proclaim
      '(optimize (speed 2) (safety 1) (compilation-speed 0)
         (space 0) #+(or cltl2 x3j13 ansi-cl) (debug 1))))


(defun interpret-sentence (forms)
   (dolist (form forms)
      (let
         ((#+procyon *print-shared* #-procyon *print-circle* t)
            (*print-escape* nil) (*print-gensym* nil))
         (fresh-line)
         (write (interpret-single-form form) :pretty t :escape nil)
         (terpri) (terpri))))


(defun interpret-single-form (form)
   (if (consp form)
      (case (car form)
         ((DECL IMP YNQU WHQU)
            (list (car form) (interpret-single-form1 (cadr form))))
         (t
            (interpret-single-form1 form)))
      form))


(defun interpret-single-form1 (form)
   (post-process-scoped-form (linear-scope form) nil))


;;;; SCOPING

(defun semantic-quantifier-form-p (x)
   ;; True for pro and rpro otherwise if they occur as free-floating list
   ;; entities cannot guarantee identity where necessary (and if could would
   ;; be difficult for subsequent processing)
   (member (car x) '(|qe| |dd| |uq| |pro| |rpro|) :test #'eq))


(defun semantic-event-form-p (x) (eq (car x) '|qe|))


(defun get-determiner (quant)
   (car (cadr quant)))


(defun get-var (quant)
   (caadr (cadr quant)))


(defun get-restriction (quant)
   (case (car quant)
      ;; for NP quantifiers insert definiteness specification into restriction
      ((|dd| |uq|)
         (if (member (car (cadr quant)) '(|the| |some| |every|))
            (caddr (cadr quant))
            `(|and|
               (,(ecase (car quant) (|dd| '|def|) (|uq| '|indef|))
                  ,(caadr (cadr quant))) ,(caddr (cadr quant)))))
      (t (caddr (cadr quant)))))


;;; Straight-forward linear scoping with all quantifiers enclosing, and
;;; event quantifiers preceding others for same predicate:
;;;
;;; (defun linear-scope (form)
;;;    (let ((quant (get-first-quant form)))
;;;       (if quant
;;;          (linear-scope-with-quant quant form)
;;;          form)))
;;;
;;;
;;; (defun get-first-quant (form)
;;;    (cond
;;;       ((atom form) nil)
;;;       ((semantic-quantifier-form-p form) form)
;;;       (t
;;;          (dolist (item form nil)
;;;             (let ((res (get-first-quant item)))
;;;                (when res (return res)))))))


(defun linear-scope (form)
   (multiple-value-bind (quant inside)
      (get-first-quant form nil)
      (cond
         ((null quant) form)
         (inside
            (linear-scope
               (linear-scope-nsubst
                  (linear-scope-with-quant quant inside) inside form)))
         (t (linear-scope-with-quant quant form)))))


(defun get-first-quant (form inside)
   (cond
      ((atom form) nil)
      ((semantic-event-form-p form)
         ;; (qe ...) wants embedded quantifiers to be pulled out, down to
         ;; level of next qe form
         (dolist (item (cddadr form) nil)
            (multiple-value-bind (rform rinside)
               (get-first-quant item form)
               (when rform
                  (return-from get-first-quant (values rform rinside))))))
      ((semantic-quantifier-form-p form)
         (values form inside))
      (t
         (dolist (item (cdr form) nil)
            (multiple-value-bind (rform rinside)
               (get-first-quant item inside)
               (when rform (return (values rform rinside))))))))


(defun linear-scope-with-quant (quant form)
   `(,(get-determiner quant)
      (,(get-var quant))
      ,(linear-scope (get-restriction quant))
      ,(linear-scope
         (linear-scope-nsubst (get-var quant) quant form))))


(defun linear-scope-nsubst (new old form)
   ;; Destructively substitute all occurrences of new for old in form
   (cond
      ((eql form old) new)
      ((atom form) form)
      (t
         (mapl
            #'(lambda (tail)
               (setf (car tail) (linear-scope-nsubst new old (car tail))))
            form))))


;;;; POST-PROCESSING

(defun post-process-scoped-form (form done)
   (cond
      ((atom form) form)
      ((member (car form) done :test #'eq)
         (mapl
            #'(lambda (tail)
               (setf (car tail)
                  (post-process-scoped-form (car tail) nil)))
            (cdr form))
         form)
      ((and (symbolp (car form)) ; rule out e.g. "<SEMANTICS FOR ...>"
            (get (car form) 'post-process-operator))
         (post-process-scoped-form
            (funcall (get (car form) 'post-process-operator) form)
            (cons (car form) done)))
      (t
         (mapl
            #'(lambda (tail)
               (setf (car tail)
                  (post-process-scoped-form (car tail) nil)))
            form))))


(eval-when (load eval)
   (mapc
      #'(lambda (item)
         (setf (get (car item) 'post-process-operator) (cdr item)))
      (list
         ;; collapse (name (the (..) (and ... (named x y) ...))) to y
         (cons '|name|
            #'(lambda (x) (caddr (assoc '|named| (cddr (third (cadr x)))))))
         (cons 'NN
            #'(lambda (x) (post-process-numeric-form (cadr x))))
         ;; transform (CP x y ...) into x-y-...
         (cons 'CP
            #'(lambda (x)
               (intern (format nil "~{~A~^-~}" (cdr x)))))
         ;; transform (compound x y ...) into x_y_... Can occur nested.
         (cons '|compound|
            #'(lambda (x)
               (intern
                  (format nil "~{~A~^_~}"
                     (mapcar
                        #'(lambda (sub) (post-process-scoped-form sub nil))
                        (cdr x))))))
         ;; transform (and ... (and ...)) into (and ... ...),
         (cons '|and|
            #'(lambda (x)
               (let ((subforms
                        (mapcan
                           #'(lambda (sub)
                              (cond
                                 ((atom sub) (list sub))
                                 ((eq (car sub) '|and|)
                                    (copy-list (cdr sub)))
                                 (t (list sub))))
                           (cdr x))))
                  (if (cdr subforms)
                     (cons '|and| subforms) (car subforms)))))
         ;; transform (lambda (Rn) ...) into the result of applying
         ;; the function to POSSESSED-BY
         (cons '|lambda|
            #'(lambda (x)
               (if
                  (and (consp (cdr x)) (consp (cadr x))
                     (symbolp (caadr x))
                     (eql (char (symbol-name (caadr x)) 0) #\R))
                  (reduce-lambda-formula (list x 'POSSESSED-BY))
                  x)))
         ;; transform (qe (exists (e) (and <tense-info> <pred>))) into
         ;; (exists (e) <tense-info> <pred>) and other (qe (exists (e) ...))
         ;; into (exists (e) ...)
         (cons '|qe|
            #'(lambda (x)
               (let ((form (cadr x)))
                  (if (and (consp (caddr form)) (eq (car (caddr form)) '|and|))
                     `(,(car form) ,(cadr form) ,@(cdr (caddr form)))
                     form))))
         )))


(defun post-process-numeric-form (form)
   (cond
      ((and (symbolp form) (eql (char (symbol-name form) 0) #\\))
         (parse-integer (subseq (symbol-name form) 1)))
      ((atom form)
         (error "Ill-formed numeric semantic form"))
      ((member (car form) '(+ * /) :test #'eq)
         (apply (car form) (mapcar #'post-process-numeric-form (cdr form))))
      (t
         (error "Ill-formed numeric semantic form"))))
        

;;; End of file

