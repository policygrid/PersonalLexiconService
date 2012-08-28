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

;;; ANLT CHART PARSER - DECLARATIONS HEADER FILE
;;;
;;; Needs external definitions for:
;;;
;;;   semantics-for-null
;;;
;;;   g-defns
;;;
;;; G-defns, given a word (a lisp symbol) should return a list
;;; of the form:
;;;
;;;   (((<category1> . |word|) . <word>) ...)
;;;
;;; each element representing one sense of the word. Format of a
;;; category is:
;;;
;;;   ((<feature1> . <value1>) (<feature2> . <value2>) ...)
;;;
;;; (sorted into a canonical order) where each value is either a
;;; symbol, variables having a non-nil value on their property
;;; list under indicator 'variable' (i.e. satisfying G-varp).

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; The parser should be invoked by calling the function
;;; g-parse, giving as argument a list of words. It returns a
;;; list of parses, or () if none.
;;;
;;; The special variable *term-unification should be set to t
;;; for strict category matching, otherwise nil. After a parse,
;;; the special variable *chart-edges may be inspected to see
;;; what edges were generated.

;;; Specials

(defvar g-packing t)
(defvar g-search-bindings t)
(defvar g-gramtree nil)
(defvar g-tracerules nil)
(defvar g-result)

(defvar *chart-edges)
(defvar *term-unification)
(defvar *current-category-index)
(defvar *current-variable-name*)
(defvar *g-compute-all-edges nil)


(defun g-init-parse nil
   (setq g-gramtree nil) 
   (setq g-tracerules nil)
   (setq *chart-edges nil))


;;; Alternative optimised versions of car and cdr for application
;;; to arguments which are known to be non-atomic and non-null.

(eval-when (compile load eval)
(defmacro cons-car (x)
   #-gde-debug `(car (the cons ,x)) #+gde-debug `(car ,x))
   
(defmacro cons-cdr (x)
   #-gde-debug `(cdr (the cons ,x)) #+gde-debug `(cdr ,x))
)


;;; Fast, unsafe versions of vector element access and setting. Inline
;;; vector length and creation. Fast fixnum-indexed iteration.

(eval-when (compile load eval)
(defmacro vector-svref (v n)
   `(locally
      #-gde-debug
      (declare (optimize (speed 3) (safety 0) (space 0)) (inline svref))
      #-gde-debug (svref (the simple-vector ,v) (the fixnum ,n))
      #+gde-debug (svref ,v ,n)))

(defsetf vector-svref (v n) (val)
   `(locally 
      #-gde-debug
      (declare (optimize (speed 3) (safety 0) (space 0)))
      #-gde-debug (setf (svref (the simple-vector ,v) (the fixnum ,n)) ,val)
      #+gde-debug (setf (svref ,v ,n) ,val)))

(defmacro simple-vector-length (v)
   `(locally
      #-gde-debug
      (declare (optimize (speed 3) (safety 0) (space 0)) (inline length))
      (length #-gde-debug (the simple-vector ,v) #+gde-debug ,v)))


(defmacro make-simple-vector (len)
   ;; This needs to be pretty fast - KCL doesn't perform any compiler
   ;; optimisation for this sort of make-array call, so do it for it.
   #+kcl `(si:make-vector t ,len nil nil nil nil nil)
   #-kcl `(make-array #-gde-debug (the fixnum ,len) #+gde-debug ,len))


(defmacro fixnum-add-1 (var)
   ;; KCL seems to generate worse code if there are too many
   ;; (the fixnum <form>) declarations around, so omit some. Procyon compiler
   ;; seems not to recognise 1+ with fixnum arg and result as a special case
   #-gde-debug
   `(the fixnum
      #+kcl (1+ ,var)
      #+procyon (i1+ ,var)
      #-(or kcl procyon) (1+ (the fixnum ,var)))
   #+gde-debug `(1+ ,var))

(defmacro fixnum-dotimes-from-1 ((var limit . result) . body)
   ;; Similar to dotimes except leaves out zero, and limit must be a
   ;; fixnum.
   `(do ((.limit. ,limit)
         (,var 1 (fixnum-add-1 ,var)))
      ((#+kcl eql #-kcl g-fixnum-eql ,var .limit.) ,@result)
      #-gde-debug (declare (fixnum .limit. ,var))
      ,@body))
)


;;; Chart edges

(eval-when (compile load eval)
(defstruct
   (g-chart-edge
      (:constructor make-g-chart-edge
         (needed found end vts res start)))
   needed found end vts res start)

(defmacro g-chart-edge-rvt (edge)
   `(cons-car (g-chart-edge-vts ,edge)))

(defmacro g-chart-edge-nvt (edge)
   `(cons-cdr (g-chart-edge-vts ,edge)))
)


;;; Variables are named structures - name is grammar-variable, slots are
;;; node-p, optional-p, and name. The structure is defined in GDE sources.
;;; If a non-category value is not a symbol then it is a variable.

(eval-when (compile load eval)
(defmacro g-genvar nil
   '(locally
      #-gde-debug
      (declare (optimize (speed 3) (safety 0) (space 0))
         (inline make-grammar-variable))
      (make-grammar-variable t nil
         (setq *current-variable-name*
            (1+ #-gde-debug (the integer *current-variable-name*)
                #+gde-debug *current-variable-name*)))))


(defmacro g-genoptvar nil
   '(locally
      #-gde-debug
      (declare (optimize (speed 3) (safety 0) (space 0))
         (inline make-grammar-variable))
      (make-grammar-variable t t
         (setq *current-variable-name*
            (1+ #-gde-debug (the integer *current-variable-name*)
                #+gde-debug *current-variable-name*)))))


(defmacro g-varp (x)
   ;; arg must already be known not to be a category (a simple-vector)
   ;; NB cannot use (typep x 'structure) since structure isn't a CL
   ;; class. (typep x 'grammar-variable) would be slow since implementation
   ;; has to check for structure, fetch its name and do an eq test on it
   `(locally
      #-gde-debug
      (declare (optimize (speed 3) (safety 0) (space 0))
         (inline symbolp))
      (not (symbolp ,x))))


(defmacro g-node-varp (x)
   ;; arg must be known to be a variable
   `(locally
      #-gde-debug
      (declare (optimize (speed 3) (safety 0) (space 0))
         (inline grammar-variable-node-p))
      (grammar-variable-node-p ,x)))


(defmacro g-optionalp (x)
   ;; arg must be known to be a variable
   `(locally
      #-gde-debug
      (declare (optimize (speed 3) (safety 0) (space 0))
         (inline grammar-variable-optional-p))
      (grammar-variable-optional-p ,x)))


(defmacro g-unnamed-variable nil ''\@)


(defmacro g-categoryp (x)
   ;; Some implementations (e.g. Allegro pre-4.0, kcl/akcl, mcl) cannot inline
   ;; simple-vector-p so this test could turn out pretty expensive - instead
   ;; test for array type, or test for none of other possible alternatives
   ;; if they have inline expansions
   #+(and allegro (not (or cltl2 x3j13)))
      `(not (or (symbolp ,x) (excl::structurep ,x)))
   #+kcl
      `(not (or (symbolp ,x) (system:structurep ,x)))
   #+(or openmcl mcl)
      `(arrayp ,x)
   #-(or (and allegro (not (or cltl2 x3j13))) kcl openmcl mcl)
      `(simple-vector-p ,x)) ; 
   )


;;; A fast version of assoc with test eq and alist elements all conses -
;;; but less safe than the built-in version

(eval-when (compile load eval)
#+gde-debug
(defmacro g-fast-assoc-eq (key lst)
   `(assoc ,key ,lst :test #'eq))
#-gde-debug
(defun g-fast-assoc-eq (key lst)
   (declare (optimize (speed 3) (safety 0) (space 0)))
   (tagbody
      (go end)
      lp
      (when (eq key (cons-car (cons-car lst)))
         (return-from g-fast-assoc-eq (cons-car lst)))
      (setq lst (cons-cdr lst))
      end
      (if lst (go lp))))


(defmacro g-perform-stack-check nil
   '(locally (declare (notinline identity))
      ;; Try to get compiler to insert a stack overflow check
      (identity nil)))


(defmacro g-fixnum-eql (x y)
   ;; Allegro pre-4.0 compiler produces much better code if test is =
   `(locally
      #-gde-debug (declare (optimize (speed 3) (safety 0) (space 0)))
      (#+(and allegro (not (or cltl2 x3j13))) = #-(and allegro (not (or cltl2 x3j13))) eql
         #-gde-debug (the fixnum ,x) #+gde-debug ,x
         #-gde-debug (the fixnum ,y) #+gde-debug ,y)))
)


(defun g-remove-segment (sub x)
   ;; remove bindings from x that occur (in same order) in sub
   ;; must always end up copying top-level structure of 2nd arg
   (do ((tail x (cdr tail)) (res nil))
      ((null tail) (nreverse res))
      (cond
        ((null sub) (return (nreconc res (copy-list tail))))
        ((eq (car sub) (car tail)) (setq sub (cdr sub)))
        (t (push (car tail) res)))))


(defun g-combine-vts (vt1 vt2)
   ;; suppress any bindings in vt2 that could cause re-entry into vt1
   ;; minimising copying
   (nconc vt1
      (do ((tail vt2 (cdr tail)) (to-remove nil))
         ((null tail)
            (if to-remove
               (do ((tail vt2 (cdr tail)) (res nil))
                  ((null tail) (nreverse res))
                  (unless (member (car tail) to-remove :test #'eq)
                     (push (car tail) res)))
               vt2))
         (when (g-fast-assoc-eq (cons-cdr (car tail)) vt1)
            (push (car tail) to-remove)))))


(defun g-tconc (a b)
   (let ((cell (cons b nil)))
      (cond
         ((and (consp a) (car a))
            (rplacd (cdr a) cell) (rplacd a cell) a)
         ((consp a) (rplaca a cell) (rplacd a cell)
            a)
         (t (cons cell cell)))))


;;; End of file
