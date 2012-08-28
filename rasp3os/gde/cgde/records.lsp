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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - STRUCTURE DEFINITIONS
;;;
;;; Author: John Carroll
;;;
;;; This module contains the structure definitions for the main
;;; data structures used in the GDE.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(defstruct (command-entry)
   shortest
   name
   action)


(defstruct (feature-declaration)
   values
   file
   comment)


(defstruct (set-declaration)
   features
   file
   comment)


(defstruct (alias-declaration)
   cat-bindings
   file
   comment)


(defstruct (category-declaration)
   feature-path
   cat-bindings
   features
   lexical
   semantic-types
   file
   comment)


(defstruct (basic-type)
   name)


(defstruct (complex-type)
   arg
   res)


(defstruct (extension-declaration)
   features
   file
   comment)


(defstruct (top-declaration)
   categories
   file
   comment)


;;; The rules-applied field in an ID rule contains a list of the
;;; default and propagation rules applied only since the last
;;; metarule.

(defstruct (id-rule)
   name
   binding-nos
   highest-binding-no
   binding-list
   lexical
   linear
   rules-applied
   semantic-forms
   file
   comment)


;;; An ID rule name is potentially quite complex. Multiple
;;; versions of the same rule come from metarule expansion
;;; (meta-names), multiple metarule matches (index in
;;; meta-names), multiple linearisations (index in name), and
;;; expansion of optional or repeated categories (split in name
;;; and meta-names).

(defstruct (top-rule-name)
   base
   meta-names)


(defstruct (sub-rule-name)
   base
   index
   split)


(defstruct (lp-rule)
   lp-terms
   file
   comment)


(defstruct (meta-rule)
   lhs-binding-nos
   rhs-binding-nos
   cat-bindings
   lhs-rhs-corresponds
   lexical
   linear
   semantic-forms
   file
   comment)


(defstruct (default-rule)
   binding-nos
   cat-bindings
   category-index
   feature-names
   value
   lexical
   linear
   file
   comment)


(defstruct (category-index)
   binding-no
   cat-feature)


(defstruct (prop-rule)
   binding-nos
   cat-bindings
   ident-specs
   lexical
   linear
   file
   comment)


(defstruct (prop-ident-spec)
   category-index
   feature-names)


(defstruct (word-definition)
   senses
   file
   comment)


(defstruct (word-sense)
   cat-bindings
   structure
   semantic-forms)


;;; Following 3 records must be lists without a header, since
;;; they are passed into the morphology system.

(defstruct (ec-rule (:type list))
   name
   precond
   action
   features
   file
   comment)


(defstruct (multiply-rule (:type list))
   name
   precond
   skeletons
   file
   comment)


(defstruct (cc-rule (:type list))
   name
   precond
   postcond
   file
   comment)


;;; A common node represents a shared subtree in the results
;;; produced by the parser.

(defstruct (common-node)
   category
   start-vertex
   end-vertex
   tree-nos)


;;; A category may be either an alias instantiation or a feature
;;; bundle: a feature bundle is just a list of Fv-pairs, not a
;;; record. A category is usually kept in a Category-binding
;;; record - the number field is an index for use in dealing
;;; with category valued features, and category matching.

(defstruct
   (category-binding)
   number
   category
   repetition)


(defstruct (alias-instantiation)
   name
   bundle)


;;; A set of Match-binding records encode the result of matching
;;; two categories - the indices of the top level and category
;;; value categories that match.

(defstruct (match-binding (:predicate nil))
   pattern-binding-no
   matched-binding-nos)


;;; Parser chart edge constructor and access functions -
;;; defined in parser files.
;;; 
;;; (defstruct
;;;   (g-chart-edge
;;;      (:constructor make-g-chart-edge
;;;         (needed found end vts res start)))
;;;   needed found end vts res start)
;;; 
;;; (defmacro g-chart-edge-rvt (edge)
;;;   `(car (g-chart-edge-vts ,edge)))
;;; 
;;; (defmacro g-chart-edge-nvt (edge)
;;;   `(cdr (g-chart-edge-vts ,edge)))

(defmacro chart-edge-rvt (edge)
  `(car (g-chart-edge-vts ,edge)))

(defmacro chart-edge-nvt (edge)
  `(cdr (g-chart-edge-vts ,edge)))


;;; Feature-value pairs must match up with the parser's idea of
;;; this data-structure, but not necessarily the morphology
;;; system's. Versions also defined in parser files. Note that
;;; the accessors cannot be funcalled since they are macros -
;;; use f-find instead of find to avoid problems.

(defmacro make-fv-pair (feature value)
   `(cons ,feature ,value))

(defmacro fv-pair-feature (fvpair)
   `(car ,fvpair))

(defmacro fv-pair-value (fvpair)
   `(cdr ,fvpair))


;;; Feature-value pair in morphology system format

(defmacro make-d-fv-pair (&key feature value)
   `(list ,feature ,value))

(defmacro d-fv-pair-feature (x) `(car ,x))

(defmacro d-fv-pair-value (x) `(cadr ,x))


;;; Variables are structures with a print function to output them
;;; in form @22 etc (or &55 etc for optional variables in unrestricted
;;; unification).

(defvar *current-variable-name* 0)
(defvar *varstring*
   (make-array 255
      :element-type #+(or cltl2 x3j13 ansi-cl) 'character #-(or cltl2 x3j13 ansi-cl) 'string-char
      :fill-pointer 0))


(defstruct
   (grammar-variable
      (:constructor make-grammar-variable (node-p optional-p name))
      (:print-function
         (lambda (item stream level)
            (declare (ignore level))
            (let ((name (grammar-variable-name item))
                  (first-char
                     (if (grammar-variable-optional-p item) #\& #\@)))
               (write-string
                  (etypecase name
                     (string
                        (concatenate 'string (string first-char) name))
                     (integer
                        (integer-to-varstring name first-char *varstring*)))
                  stream))))
      (:predicate varp))
   node-p optional-p name)


(defun integer-to-varstring (x first-char str)
   (setf (fill-pointer str) 0)
   (loop
      (when (< x 10) (vector-push (digit-char x) str)
         (vector-push first-char str)
         (return (reverse str)))
      (multiple-value-bind (quo rem)
         (truncate x 10)
         (vector-push (digit-char rem) str)
         (setq x quo))))


;;; End of file

