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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - REALIASING
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code for restoring aliases into
;;; idrules and single categories which have previously been
;;; normalised.
;;;
;;; Entry points:
;;;
;;;  * (defun Realias-idrule-definition (definition) ...
;;;  * (defun Realias-category-specification (binding-list) ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defun realias-idrule-definition (definition)
   (let
      ((structure-63 (copy-id-rule definition)))
      (setf (id-rule-binding-list structure-63)
         (realias-category-specification
            (id-rule-binding-list structure-63)))
      (setf (id-rule-semantic-forms structure-63)
         (mapcar
            #'(lambda (form)
               (if (and (consp form) (semantic-form-pattern-p (car form)))
                  (append
                     (mapcar
                        #'(lambda (no-and-bindings)
                           (cons (car no-and-bindings)
                              (realias-category-specification
                                 (cdr no-and-bindings))))
                        (butlast form))
                     (last form))
                  form))
            (id-rule-semantic-forms structure-63)))
      structure-63))


;;; Copy the category-binding records to be realiased, since the
;;; category fields get destructively removed during realiasing.
;;; But first sort aliases, excluding those of the form X=[].

(defun realias-category-specification
   (binding-list)
   (unless *sorted-aliases
      (setf *sorted-aliases
         (sort
            (copy-list
               (mapcan
                  #'(lambda (alias)
                       (when
                          (category-binding-category
                             (car
                                (normalised-alias-category
                                   alias)))
                          (ncons alias)))
                  *aliases))
            #'is-better-alias)))
   (realias-category-bindings
      (mapcar
         #'(lambda (binding)
              (make-category-binding :number
                 (category-binding-number binding) :category
                 (category-binding-category binding)
                 :repetition
                 (category-binding-repetition binding)))
         binding-list)))


;;; Predicate for sorting all the alias names in the system into
;;; a preferred order. Prefer aliases which cover more features
;;; - if same number, then prefer alias which comes first in the
;;; alias declarations.

(defun is-better-alias (a1 a2)
   (let
      ((a1-feature-length
          (list-length
             (category-binding-category
                (car
                   (normalised-alias-category a1)))))
         (a2-feature-length
            (list-length
               (category-binding-category
                  (car
                     (normalised-alias-category a2))))))
      (cond
         ((> a1-feature-length a2-feature-length)
            t)
         ((< a1-feature-length a2-feature-length)
            nil)
         (t
            (member a2 (member a1 *aliases :test #'eq)
               :test #'eq)))))


(defun realias-category-bindings
   (binding-list)
   (mapcar
      #'(lambda (aliases)
           (let
              ((binding (pop binding-list)))
              (if aliases
                 (make-category-binding :number
                    (category-binding-number binding) :category
                    (make-alias-instantiation :name
                       (car aliases) :bundle
                       (nconc
                          (mapcar
                             #'(lambda (alias)
                                  (make-alias-instantiation
                                     :name alias :bundle nil))
                             (cdr aliases))
                          (category-binding-category binding)))
                    :repetition
                    (category-binding-repetition binding))
                 binding)))
      (mapcar
         #'(lambda (binding)
              (realias-category-binding
                 (category-binding-number binding)
                 binding-list))
         binding-list)))


;;; Return names of aliases which cover part of category
;;; specified by binding-no. Features in a matching alias get
;;; deleted from the top level category that they cover. (Higher
;;; level category values may be pointed to from other
;;; categories and so must remain unchanged). An alias matches
;;; if the base level category is an extension of the alias
;;; definition and any category values are exact matches.

(defun realias-category-binding
   (binding-no binding-list)
   (mapcan
      #'(lambda (alias)
           (when
              (let
                 ((alias-binding-list
                     (normalised-alias-category alias)))
                 (when
                    (and
                       (match-category 0 alias-binding-list
                          binding-no binding-list)
                       (realias-match-permissable 0
                          alias-binding-list binding-no
                          binding-list t))
                    (remove-realiased-features 0
                       alias-binding-list binding-no
                       binding-list)
                    t))
              (ncons alias)))
      *sorted-aliases))


(defun realias-match-permissable
   (alias-binding-no alias-binding-list
      binding-no binding-list top-level)
   (let
      ((alias-bundle
          (category-binding-category
             (f-find (the fixnum alias-binding-no)
                alias-binding-list :key
                #'category-binding-number :test #'eql)))
         (bundle
            (category-binding-category
               (f-find (the fixnum binding-no)
                  binding-list :key #'category-binding-number
                  :test #'eql))))
      (and
         (or top-level
            (= (list-length alias-bundle)
               (list-length bundle)))
         (dolist (alias-fvpair alias-bundle t)
            (unless
               (if (numberp (fv-pair-value alias-fvpair))
                  (let
                     ((fvpair
                         (f-find (fv-pair-feature alias-fvpair)
                            bundle :key #'fv-pair-feature :test
                            #'eq)))
                     (if (numberp (fv-pair-value fvpair))
                        (realias-match-permissable
                           (fv-pair-value alias-fvpair)
                           alias-binding-list
                           (fv-pair-value fvpair) binding-list
                           nil)
                        t))
                  t)
               (return nil))))))


(defun remove-realiased-features
   (alias-no alias-binding-list binding-no
      binding-list)
   (let
      ((alias-bundle
          (category-binding-category
             (f-find (the fixnum alias-no)
                alias-binding-list :key
                #'category-binding-number :test #'eql))))
      (setf
         (category-binding-category
            (f-find (the fixnum binding-no)
               binding-list :key #'category-binding-number
               :test #'eql))
         (mapcan
            #'(lambda (fvpair)
                 (unless
                    (f-find (fv-pair-feature fvpair)
                       alias-bundle :key #'fv-pair-feature
                       :test #'eq)
                    (ncons fvpair)))
            (category-binding-category
               (f-find (the fixnum binding-no)
                  binding-list :key #'category-binding-number
                  :test #'eql))))))


(defun normalised-alias-category
   (alias-name)
   (alias-declaration-cat-bindings
      (normalise-alias-definition alias-name)))


;;; End of file

