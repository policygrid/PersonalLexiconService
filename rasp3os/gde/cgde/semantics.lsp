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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - SEMANTIC OPERATIONS
;;;
;;; Author: John Carroll
;;
;;; This file contains the code to extract and display semantic
;;; information in parse and generator trees, reduce the lambdas
;;; in a semantic formula, and translate forms between referring
;;; to rule daughters by binding numbers and by textual
;;; positions.
;;;
;;; Entry points:
;;;
;;;  * (defun Display-parse-semantics (trees) ...
;;;  * (defun Display-gen-semantics (node) ...
;;;  * (defun Extract-semantics-from-parse-tree (tree) ...
;;;  * (defun Extract-semantics-from-gen-tree (node) ...
;;;  * (defun Reduce-lambda-formula (form) ...
;;;  * (defun Translate-positions-to-bindings
;;;       (form bound-nos nos-for-bound nos-for-free) ...
;;;  * (defun Translate-bindings-to-positions
;;;       (form bound-nos nos-for-bound nos-for-free) ...
;;;  * (defun Combine-idrule-metarule-semantics
;;;       (idrule-forms metarule-forms match-bindings) ...
;;;  * (defun Semantics-for-null () ...
;;;  * (defun Type-check-semantic-form
;;;       (form construct-name mother-binding-no
;;;          binding-list) ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Lambda expression is of the form (lambda (v) ...), a lambda
;;; call is ((lambda (v) ...) ...), and a binding expression is
;;; of the form (p (v) ...).

(defmacro sem-lambda-expression-p (x)
   `(and (consp ,x) (symbolp (car ,x))
       (uncased-eq-symbols (car ,x) 'lambda)
       (consp (cdr ,x)) (consp (cadr ,x))
       (atom (caadr ,x)) (null (cdadr ,x))
       (consp (cddr ,x))))


(defmacro lambda-call-p (x)
   `(and (consp ,x)
       (sem-lambda-expression-p (car ,x))
       (consp (cdr ,x))))


(defmacro binding-expression-p (x)
   `(and (consp ,x)
       ;; (symbolp (car ,x)) - allow conses as the predicate (as
       ;; in e.g. ((NN \3) (x) (dog x))
       (consp (cdr ,x)) (consp (cadr ,x))
       (atom (caadr ,x)) (null (cdadr ,x))))


(defmacro sem-lambda-expression-bv (x)
   `(caadr ,x))


(defmacro sem-lambda-expression-body (x)
   `(caddr ,x))


(defmacro binding-expression-bv (x)
   `(caadr ,x))


(defmacro binding-expression-bv-list (x)
   `(cadr ,x))


;;; Apply mapcar 'conservatively', only returning a new list when
;;; corresponding old and new elements not eql

(defmacro mapcar-conserve (fn lst)
   (let ((var (gensym)))
      `(let ((,var (mapcar ,fn ,lst)))
         (if (every #'eql ,var ,lst) ,lst ,var))))


;;; Print out a semantic form from parser or generator

(defparameter +gr-semantics-p+ nil)

(defun display-semantic-form (form unreduced-p)
   (let
      ((#+procyon *print-shared* #-procyon *print-circle* t)
         (*print-gensym* nil) (*print-escape* nil)
         (*print-length* nil) (*print-level* nil))
      (if +gr-semantics-p+
         (dolist (f form)
            (write (if unreduced-p f (simplify-lambda-formula f))
               :pretty t)
            (terpri))
         (progn
            (write (if unreduced-p form (simplify-lambda-formula form))
               :pretty t)
            (terpri)))
      (terpri)))


;;; Called by parser 'view semantics' command. Extract and
;;; reduce semantics in parse trees for current sentence. A
;;; daughter may be atomic, indicating that the 2 adjacent
;;; siblings were licensed by a one-repeated category in a rule.
;;; The semantics for these daughters are spliced into the
;;; resulting semantic form at the appropriate point.

(defvar *cached-converted-categories*)

(defun display-parse-semantics (trees unreduced-p)
   (let ((n 0))
      (dolist (tree trees)
         (incf n)
         (dolist
            (form
               (or (extract-semantics-from-parse-tree (cdr tree) (car tree))
                  (progn
                     (gde-warn "no semantics for parse tree number " n "
")
                     nil)))
            (display-semantic-form form unreduced-p)))))


(defvar *gr-semantic-forms*)

(defun extract-semantics-from-parse-tree (tree &optional variable-table)
   (let*
      ((*cached-converted-categories* nil)
       (*gr-semantic-forms* nil)
       (sem (extract-semantics-from-parse-tree1 tree variable-table)))
      (if +gr-semantics-p+ (ncons *gr-semantic-forms*) sem)))

      
(defun extract-semantics-from-parse-tree1 (tree variable-table)
   (cond
      ((null (cdr tree))
         (semantic-forms-present (cddar tree) (cadar tree)))
      ((eq (cdar tree) (null-index-name))
         (list nil))
      ((atom (cadr tree))
         (cond
            ((and *word-structure (cdar tree))
               (compute-morph-word-semantics (cdar tree)))
            ((and *tagged-words (null (cddr tree)))
               (compute-tagged-word-semantics (cadr tree)))
            (t (semantic-forms-present (cddr tree) (cadr tree)))))
      (t
         (let* ((n 0)
               (n-and-categories
                  (cons (list 0 (car tree))
                     (mapcar
                        #'(lambda (daughter) (cons (incf n) daughter))
                        (reverse (cdr tree)))))
               (mother-forms
                  (filter-semantic-forms
                     (semantic-forms-present (cddar tree) (cadar tree))
                     n-and-categories variable-table)))
            (dolist (n-and-daughter (cdr n-and-categories))
               (setf (cdr n-and-daughter)
                  (extract-semantics-from-parse-tree1 (cdr n-and-daughter)
                     variable-table)))
            (setq n-and-categories
               (nconc n-and-categories
                  (kleene-daughter-substitutions n-and-categories mother-forms)))
            (mapcan
               #'(lambda (mother-form)
                  (let ((forms (ncons mother-form)))
                     (dolist (n-and-daughter (cdr n-and-categories))
                        (setq forms
                           (mapcan
                              #'(lambda (form)
                                 (if (member-any-level-form (car n-and-daughter) form)
                                    (maplist
                                       #'(lambda (tail)
                                          (let ((form
                                                   (if (cdr tail)
                                                      (copy-reentrant-tree form nil)
                                                      form)))
                                             (if (symbolp (car n-and-daughter))
                                                ;; it's a kleene n+ marker
                                                (nsplice-into-form
                                                   (car tail) (car n-and-daughter) form
                                                   nil)
                                                (nsubstitute-into-form
                                                   (car tail) (car n-and-daughter) form))))
                                       (cdr n-and-daughter))
                                    (list form)))
                              forms)))
                     (copy-out-semantic-forms forms)))
               mother-forms)))))


(defun copy-out-semantic-forms (forms)
   (if +gr-semantics-p+
      (let ((res nil))
         (dolist (f forms)
            (if (and (consp f) (atom (car f)) (not (eq (car f) '|lambda|)))
               (push f *gr-semantic-forms*)
               (push f res)))
         res)
      (copy-list forms)))

#|
;;; 8-dec-04 was the following

(defun copy-out-semantic-forms (forms)
   (if +gr-semantics-p+
      (let ((res nil))
         (dolist (f forms)
            (if (or (atom f) (eq (car f) '|lambda|))
               (push f res)
               (push f *gr-semantic-forms*)))
         res)
      (copy-list forms)))
|#


(defun compute-tagged-word-semantics (word)
   ;; (let ((tail-pos (position #\_ (string word) :from-end t)))
   ;;    (list (if tail-pos (intern (subseq (string word) 0 tail-pos)) word)))
   (list word))


(defun semantic-forms-present (forms name)
   (cond
      (forms
         (mapcan
            #'(lambda (form)
               (if (and (consp form) (eq (car form) (alt-semantics-marker)))
                  (mapcar #'copy-tree (cdr form))
                  (list (copy-tree form))))
            forms))
      (t
         (ncons
            (concat-string "<SEMANTICS FOR NODE " name ">")))))


(defun copy-reentrant-tree (x lsts)
   (cond
      ((atom x) (values x lsts))
      ((assoc x lsts :test #'eq)
         (let ((item (assoc x lsts :test #'eq)))
            (values (cdr item) lsts)))
      (t
         (multiple-value-bind (head head-lsts)
            (copy-reentrant-tree (car x) lsts)
            (multiple-value-bind (tail tail-lsts)
               (copy-reentrant-tree (cdr x) head-lsts)
               (let ((res (cons head tail)))
                  (values res (cons (cons x res) tail-lsts))))))))

        
(defun kleene-daughter-substitutions (n-and-categories mother-forms)
   (mapcar
      #'(lambda (n-and-marker)
         (cons (cdr n-and-marker)
            (let ((forms
                     (mapcar #'list
                        (cdr (assoc (car n-and-marker) n-and-categories)))))
               (dolist
                  (n-and-exts
                     (nthcdr (1+ (car n-and-marker)) n-and-categories)
                     (mapcar #'reverse forms))
                  (setq forms
                     (mapcan
                        #'(lambda (form)
                           (mapcar
                              #'(lambda (ext) (cons ext form))
                              (cdr n-and-exts)))
                        forms))))))
      (kleene-markers-in-semantics mother-forms nil)))
      

(defun kleene-markers-in-semantics (x res)
   (cond
      ((semantic-kleene-marker-p x)
         (pushnew (semantic-kleene-marker-p x) res :test #'eq) res)
      ((atom x) res)
      (t
         (kleene-markers-in-semantics (car x)
            (kleene-markers-in-semantics (cdr x) res)))))


;;; Take semantic clauses of a ID rule and return semantic forms which are
;;; applicable in current context (mother and daughter categories in local tree).
;;; Special processing for sets of clauses containing kleene marker where
;;; a condition in the clause refers to the kleene daughter.

(defun filter-semantic-forms (forms n-and-categories variable-table)
   (let*
      ((conj-forms nil)
         (normal-forms
            (mapcan
               #'(lambda (form)
                  (if (and (consp form) (semantic-form-pattern-p (car form)))
                     (let ((marker
                              (car (kleene-markers-in-semantics
                                    (find-if-not #'semantic-form-pattern-p form)
                                    nil))))
                        (if marker
                           (mapcan
                              #'(lambda (x)
                                 (if (and (consp x) (eq (car x) '*kleene))
                                    (progn
                                       (push
                                          (mapcar
                                             #'(lambda (res-form)
                                                (list* res-form (cadr x) marker))
                                             (cddr x))
                                          conj-forms) nil)
                                    (list x)))
                              (filter-patterned-semantic-form
                                 form n-and-categories variable-table
                                 (member (car marker) n-and-categories :key #'car)))
                           (filter-patterned-semantic-form
                              form n-and-categories variable-table nil)))
                     (list (copy-tree form))))
               forms)))
      (if conj-forms
         ;; (((<form> <n> . (1 . 1+)) ...) ...)
         (nconc normal-forms
            (apply #'mapcar
               #'(lambda (&rest forms-and-markers)
                  (nsplice-into-form
                     (mapcar
                        #'(lambda (form-and-marker)
                           (let ((args
                                    ;; assume only 1 occurence of a kleene 
                                    ;; marker in a particular form
                                    (semantic-form-marker-args (car form-and-marker)
                                       (cdddar forms-and-markers))))
                              (if args
                                 (cons (cadr form-and-marker) (copy-tree args))
                                 (cadr form-and-marker))))
                        (sort (copy-list forms-and-markers) #'< :key #'cadr))
                     (cdddar forms-and-markers)
                     (caar forms-and-markers) t))
               conj-forms))
         normal-forms)))


(defun filter-patterned-semantic-form (form n-and-categories variable-table
      kleene-n-and-categories)
   ;; kleene-n-and-categories is nil or a tail of n-and-categories when
   ;; form contains a kleene marker - tail starts at daughter corresponding to
   ;; marker
   (if
      (and kleene-n-and-categories
         (eql (caar form) (caar kleene-n-and-categories)))
      ;; semantic condition is on the kleene marker daughter in form
      (mapcan
         #'(lambda (n-and-category)
            (let
               ((res-forms
                     (filter-patterned-semantic-form1 form
                        n-and-category n-and-categories variable-table
                        kleene-n-and-categories)))
               (when res-forms
                  (list (list* '*kleene (car n-and-category) res-forms)))))
            kleene-n-and-categories)
      (filter-patterned-semantic-form1 form
         (assoc (caar form) n-and-categories) n-and-categories
         variable-table kleene-n-and-categories)))


(defun filter-patterned-semantic-form1 (form n-and-category 
      n-and-categories variable-table kleene-n-and-categories)
   (when
      (and n-and-category
         (match-category 0 (cdar form) 0
            (or (getf *cached-converted-categories* (cadr n-and-category))
               (setf
                  (getf *cached-converted-categories* (cadr n-and-category))
                  (convert-from-parser-format
                     variable-table (cadr n-and-category) t)))))
      (if (semantic-form-pattern-p (cadr form))
         (filter-patterned-semantic-form
            (cdr form) n-and-categories variable-table
            kleene-n-and-categories)
         (mapcar #'copy-tree (cdr form)))))


(defun semantic-form-marker-args (form marker)
   (cond
      ((atom form) nil)
      ((eql (car form) marker) (cdr form))
      (t
         (or (semantic-form-marker-args (car form) marker)
            (semantic-form-marker-args (cdr form) marker)))))


;;; Called by generator 'view semantics' command. Extract and
;;; reduce semantics in generator tree.

(defun display-gen-semantics (node unreduced-p)
   (mapc
      #'(lambda (form) (display-semantic-form form unreduced-p))
      (extract-semantics-from-gen-tree node
         (cdr (gen-node-table (car *generator-nodes))))))


(defun extract-semantics-from-gen-tree (node variable-table)
   (let ((*cached-converted-categories* nil))
      (extract-semantics-from-gen-tree1 node variable-table)))


(defun extract-semantics-from-gen-tree1 (node variable-table)
   (let ((name (gen-node-rule-name node)))
      (cond
         ((null name)
            (semantic-forms-present nil node))
         ((eq name 'e)
            (semantic-forms-present
               (gen-node-semantics node) name))
         ((symbolp name)
            (semantic-forms-present
               (gen-node-semantics name)
               (gen-node-rule-name name)))
         (t
            (let*
               ((n 0)
                  (n-and-categories
                     (cons (cons 0 node)
                        (mapcar
                           #'(lambda (daughter) (cons (incf n) daughter))
                           (gen-node-daughters node)))))
               (mapcan
                  #'(lambda (mother-form)
                     (let ((forms (ncons mother-form)))
                        (dolist (n-and-daughter (cdr n-and-categories))
                           (setf forms
                              (mapcan
                                 #'(lambda (form)
                                    (mapcar
                                       #'(lambda (ext)
                                          (substitute-into-form
                                             ext (car n-and-daughter) form))
                                       (extract-semantics-from-gen-tree1
                                          (cdr n-and-daughter) variable-table)))
                                 forms)))
                        (copy-list forms)))
                  (filter-semantic-forms
                     (semantic-forms-present (gen-node-semantics node) name)
                     (mapcar
                        #'(lambda (n-and-category)
                           (list (car n-and-category)
                              (list
                                 (gen-node-category (cdr n-and-category))
                                 '*once*)))
                        n-and-categories)
                     variable-table)))))))


;;; Substitute occurrences of an object

(defun substitute-into-form (new old form)
   (cond
      ((eql form old) new)
      ((atom form) form)
      (t
         (mapcar-conserve
            #'(lambda (item)
               (substitute-into-form new old item))
            form))))


(defun nsubstitute-into-form (new old form)
   (cond
      ((eql form old) new)
      ((atom form) form)
      (t
         (mapl
            #'(lambda (tail)
               (setf (car tail)
                  (nsubstitute-into-form new old (car tail))))
            form))))


(defun nsplice-into-form (new old form preformed-p)
   ;; splice, but additionally treat the application ... (<n>+ x) ... as
   ;; ... (<n1> x) (<n2> x) (<n3> x) ... . Copy the x's so that event vars
   ;; etc inside them become distinct. The splice-in may be preformed
   ;; in which case just use it as is.
   (cond
      ((atom form) form)
      (t
         (mapl
            #'(lambda (tail)
               (cond
                  ((and (consp (car tail)) (eql (caar tail) old))
                     (cond
                        (preformed-p
                           (setf (car tail) (car new))
                           (setf (cdr tail) (cdr new)))
                        (t
                           (setf (car tail) (cons (car new) (cdar tail)))
                           (setf (cdr tail)
                              (nconc
                                 (mapcar
                                    #'(lambda (n)
                                       (cons n (copy-tree (cdar tail))))
                                    (cdr new))
                                 (cdr tail))))))
                  ((eql (car tail) old)
                     (setf (car tail) (car new))
                     (setf (cdr tail) (append (cdr new) (cdr tail))))
                  (t (nsplice-into-form new old (car tail) preformed-p))))
            form))))


(defun member-any-level-form (x form)
   (cond
      ((eql x form) t)
      ((atom form) nil)
      (t
         (or (member-any-level-form x (car form))
            (member-any-level-form x (cdr form))))))


;;; Reduce a lambda expression as far as possible. Each time a
;;; reduction is made, go back up to the top level and start
;;; going down expression looking for next reduction. Stop when
;;; no more found (expression unchanged).
;;;
;;; Ordinary reduction is ((lambda (x) ...) y) -> ... but the
;;; reduction ((lambda (x) ...) a ...) -> (... ...) is also
;;; allowed.
;;;
;;; Rename variables as required. Need to do this when:
;;;
;;;  1. substituting for x in the expression (p (x) ...) -
;;;     rename x's in expression and do not substitute further.
;;;
;;;  2. substituting into (p (x) ...) where ... mentions x
;;;     and at the same time, x is also bound
;;;     higher up in the expression or x occurs in the
;;;     substituting expression - rename x's in expression
;;;     and continue substitution. Prevents any variables in
;;;     substituting form being captured unless they were free
;;;     in the initial semantic form.

(defvar *reduction-applied*)
(defvar *cached-reductions*)
(defvar *cached-singles*)
(defvar *cached-substitutions*)


(defun reduce-lambda-formula (form)
   (let ((*reduction-applied* nil)
         (*cached-reductions* nil))
      (loop
         (setf form
            (reduce-lambda-formula1 form nil "form"))
         (cond
            ((not *reduction-applied*) (return form)))
         (setf *reduction-applied* nil))))


(defun reduce-lambda-formula1 (form bound name)
   (cond
      ((atom form) form)
      ((getf *cached-reductions* form))
      (t
         (let
            ((res (reduce-lambda-formula2 form bound name)))
            (setf (getf *cached-reductions* form) res)))))


(defun reduce-lambda-formula2 (form bound name)
   (cond
      ((or (basic-type-p form) (complex-type-p form))
         form)
      ((and (basic-type-p (car form)) (cdr form)
            (eq (basic-type-name (car form)) '*))
         (car form))
      ((and (complex-type-p (car form)) (cdr form))
         (let
            ((arg (reduce-type-formula (cadr form) name)))
            (unless
               (match-type-expression
                  (complex-type-arg (car form)) arg name)
               (gde-warn "incompatible argument of type "
                  (semantic-type-string
                     (type-of-semantic-form arg))
                  " to function of type "
                  (semantic-type-string (car form))
                  " in " (idrule-name-string name)))
            (let
               ((res (complex-type-res (car form))))
               (when (cddr form)
                  (setf res (cons res (cddr form))))
               (setf *reduction-applied* t)
               (reduce-lambda-formula1 res bound name))))
      ((lambda-call-p form)
         (when *trace-reductions
            (format t "--- Reducing:~%")
            (write form :pretty t) (terpri))
         (let
            ((res
                  (let ((*cached-substitutions* nil)
                        (*cached-singles* nil))
                     (reduce-single-lambda
                        (sem-lambda-expression-bv (car form))
                        (sem-lambda-expression-body (car form))
                        (cadr form) bound))))
            (when (cddr form)
               (setf res (cons res (cddr form))))
            (when *trace-reductions
               (format t "--- giving:~%")
               (write res :pretty t) (terpri))
            (setf *reduction-applied* t)
            (reduce-lambda-formula1 res bound name)))
      (t
         (when (binding-expression-p form)
            (setf bound
               (cons (binding-expression-bv form) bound)))
         (mapcar-conserve
            #'(lambda (item) (reduce-lambda-formula1 item bound name))
            form))))


(defun reduce-single-lambda (old body new bound)
   (cond
      ((atom body)
         (if (eql body old) new body))
      ((getf *cached-singles* body))
      (t
         (let
            ((res (reduce-single-lambda1 old body new bound)))
            (setf (getf *cached-singles* body) res)))))


(defun reduce-single-lambda1 (old body new bound)
   (cond
      ((and (binding-expression-p body)
            (eql (binding-expression-bv body) old))
         (cached-substitute-into-form
            (new-lambda-variable old) old body))
      ((and (binding-expression-p body)
            (occurs-in-semantic-expression (binding-expression-bv body)
               (cddr body))
            (or
               (let ((where
                        (occurs-in-semantic-expression
                           (binding-expression-bv body) new)))
                  (and where
                     ;; make sure that if the bv occurs in new, then that
                     ;; part of new does not share structure with body 
                     ;; otherwise will create circularity
                     (not (occurs-in-semantic-expression where body))))
               (member (binding-expression-bv body) bound)))
         (reduce-single-lambda old
            (cached-substitute-into-form
               (new-lambda-variable (binding-expression-bv body))
               (binding-expression-bv body) body)
            new bound))
      (t
         (mapcar-conserve
            #'(lambda (item) (reduce-single-lambda old item new bound))
            body))))


(defun cached-substitute-into-form (new old form)
   (cond
      ((eql form old) new)
      ((atom form) form)
      ((getf (getf (getf *cached-substitutions* new) old) form))
      (t
         (let
            ((res (mapcar-conserve
                     #'(lambda (item)
                        (cached-substitute-into-form new old item))
                     form)))
            (setf (getf (getf (getf *cached-substitutions* new) old) form)
               res)))))


(defun occurs-in-semantic-expression (x y)
   ;; return innermost cons containing first occurrence of y in x
   (cond
      ((atom y) (eql x y))
      (t
         (let ((where
                  (or (occurs-in-semantic-expression x (car y))
                     (occurs-in-semantic-expression x (cdr y)))))
            (if (consp where) where y)))))


;;; Reduce a semantic form and rename variables to make it more readable.
;;;
;;; Rename different instances of variables which have same name. Do this by
;;; keeping a list of the 'base' names of all the variables found so far
;;; in 'binding' expressions. When a base name that has already been seen
;;; crops up again, substitute <base><n> for it in the current expression
;;; where <n> is one plus the number of times the base name has been seen
;;; so far.

(defvar *simplify-variables-found*)
(defvar *cached-simplifications*)


(defun simplify-lambda-formula (form)
   (let ((*simplify-variables-found* nil)
         (*cached-simplifications* nil))
      (simplify-lambda-formula1 (reduce-lambda-formula form) nil)))


(defun simplify-lambda-formula1 (form substs)
   (cond
      ((atom form) (or (cdr (assoc form substs)) form))
      ((getf *cached-simplifications* form))
      (t
         (let
            ((res (simplify-lambda-formula2 form substs)))
            (setf (getf *cached-simplifications* form) res)))))


(defun simplify-lambda-formula2 (form substs)
   (cond
      ((binding-expression-p form) 
         (let*
            ((old-var (binding-expression-bv form)) 
               (stripped-var (strip-lambda-variable old-var)) 
               (new-var 
                  (concat-symbol stripped-var 
                     (1+ (count stripped-var *simplify-variables-found*))))
               (new-substs (cons (cons old-var new-var) substs)))
            (push stripped-var *simplify-variables-found*)
            (mapcar 
               #'(lambda (item)
                  (simplify-lambda-formula1 item new-substs))
               form)))
      (t 
         (mapcar 
            #'(lambda (item)
               (simplify-lambda-formula1 item substs))
            form))))


;;; Convert a semantic formula read in containing indices to
;;; rule categories to instead refer to the corresponding rule
;;; binding numbers.

(defun translate-positions-to-bindings
   (form bound-nos nos-for-bound nos-for-free
      illegal-nos)
   (cond
      ((numberp form)
         (translate-position-to-binding form
            (if (member form bound-nos) nos-for-bound
               nos-for-free)
            illegal-nos))
      ((atom form) form)
      ((sem-lambda-expression-p form)
         (cons (car form)
            (translate-positions-to-bindings
               (cdr form)
               (if (numberp (sem-lambda-expression-bv form))
                  (cons (caadr form) bound-nos) bound-nos)
               nos-for-bound nos-for-free illegal-nos)))
      (t
         (cons
            (translate-positions-to-bindings (car form)
               bound-nos nos-for-bound nos-for-free
               illegal-nos)
            (translate-positions-to-bindings (cdr form)
               bound-nos nos-for-bound nos-for-free
               illegal-nos)))))


(defun translate-position-to-binding
   (index binding-nos illegal-nos)
   (cond
      ((null binding-nos)
         (gde-ferror
"semantics for a word may not contain a numeric index"))
      (t
         (let
            ((no (nth index binding-nos)))
            (cond
               ((null no)
                  (gde-ferror "category index " index
" is larger than the number of daughters"))
               ((member no illegal-nos :test #'equal)
                  (gde-ferror "category index " index
                     " refers to a W or U metavariable"))
               (t no))))))


;;; Convert binding numbers in form to ones representing fixed
;;; PS rule argument positions

(defun translate-bindings-to-positions
   (form bound-nos nos-for-bound nos-for-free)
   (cond
      ((numberp form)
         (let
            ((nos
                  (if (member form bound-nos) nos-for-bound
                     nos-for-free)))
            (- (list-length nos)
               (list-length (member form nos)))))
      ((atom form) form)
      ((sem-lambda-expression-p form)
         (cons (car form)
            (translate-bindings-to-positions
               (cdr form)
               (if (numberp (sem-lambda-expression-bv form))
                  (cons (caadr form) bound-nos) bound-nos)
               nos-for-bound nos-for-free)))
      (t
         (cons
            (translate-bindings-to-positions (car form)
               bound-nos nos-for-bound nos-for-free)
            (translate-bindings-to-positions (cdr form)
               bound-nos nos-for-bound nos-for-free)))))


;;; Apply metarule semantics to those of an ID or PS rule, in
;;; the process translating from the metarule binding numbers to
;;; the corresponding ID/PS rule numbers.
;;;
;;; Correspondences (bindings) are between metarule binding nos
;;; and ID rule ones. Inconsistency if one of the metarule ones
;;; does not have exactly one ID rule correspondance (i.e. a W
;;; variable - this should have been disallowed on input).

(defun combine-idrule-metarule-semantics (idrule-forms untrans-metarule-forms
      match-bindings metarule-rhs-nos idrule-binding-nos)
   (let*
      ((metarule-rhs-patterns nil)
         (metarule-forms
            (mapcar
               #'(lambda (form)
                  (if (and (consp form) (semantic-form-pattern-p (car form)))
                     (append
                        (mapcar
                           #'(lambda (no-and-bindings)
                              (if (member (car no-and-bindings) metarule-rhs-nos)
                                 (progn
                                    (push no-and-bindings metarule-rhs-patterns)
                                    no-and-bindings)
                                 (cons
                                    (translate-metarule-semantic-form
                                       (car no-and-bindings) match-bindings
                                       idrule-binding-nos)
                                    (cdr no-and-bindings))))
                           (butlast form))
                        (translate-metarule-semantic-form (last form)
                           match-bindings idrule-binding-nos))
                     (translate-metarule-semantic-form form match-bindings
                        idrule-binding-nos)))
               untrans-metarule-forms)))
      (mapcan
         #'(lambda (idrule-form)
            (mapcan
               #'(lambda (metarule-form)
                  (let ((res
                           (combine-idrule-metarule-semantics1 idrule-form
                              metarule-form match-bindings metarule-rhs-patterns
                              idrule-binding-nos)))
                     (if res (list res))))
               metarule-forms))
         idrule-forms)))


(defun combine-idrule-metarule-semantics1 (idrule-form metarule-form
      match-bindings metarule-rhs-patterns idrule-binding-nos)
   (let ((metarule-patterns nil) (idrule-patterns nil))
      (when
         (and (consp metarule-form)
            (semantic-form-pattern-p (car metarule-form)))
         (setq metarule-patterns (butlast metarule-form))
         (setq metarule-form (car (last metarule-form))))
      (when
         (and (consp idrule-form)
            (semantic-form-pattern-p (car idrule-form)))
         (setq idrule-patterns (butlast idrule-form))
         (setq idrule-form (car (last idrule-form))))
      (when
         (every
            #'(lambda (metarule-pattern)
               (let ((idrule-pattern
                        (assoc (car metarule-pattern) idrule-patterns)))
                  (cond
                     ((null idrule-pattern) t)
                     ((match-category
                           (category-binding-number (cadr metarule-pattern))
                           (cdr metarule-pattern)
                           (category-binding-number (cadr idrule-pattern))
                           (cdr idrule-pattern))
                        (setq idrule-patterns
                           (remove-list-1 idrule-pattern idrule-patterns))
                        t))))
            metarule-patterns)
         (let ((form
                  (if (compiled-function-p metarule-form)
                     (funcall metarule-form (copy-tree idrule-form))
                     (reduce-lambda-formula
                        (list (copy-tree metarule-form) (copy-tree idrule-form))))))
            (setq metarule-patterns
               (mapcan
                  #'(lambda (p)
                     (when (member p metarule-rhs-patterns :test #'eq)
                        (list
                           (translate-metarule-semantic-form
                              p match-bindings nil))))
                  metarule-patterns))
            (setq idrule-patterns
               (remove-if-not
                  #'(lambda (pat) (member (car pat) idrule-binding-nos))
                  idrule-patterns))
            (if (or metarule-patterns idrule-patterns)
               (nconc
                  (combine-idrule-metarule-patterns idrule-patterns
                     metarule-patterns)
                  (list form))
               form)))))
           

(defun combine-idrule-metarule-patterns (idrule-patterns metarule-patterns)
   ;; conflate categories carried through from idrule and new
   ;; metarule conditions to preserve single condition for each daughter
   (let ((res nil))
      (dolist (pat (nconc idrule-patterns metarule-patterns))
         (let ((already (assoc (car pat) res)))
            (if already
               (let
                  ((variable-substitutions nil)
                     (highest-binding-no
                        (1+
                           (reduce #'max (cdr already)
                              :key #'category-binding-number)))
                     (cat-b (copy-category-binding (cadr already))))
                  (multiple-value-bind (new-cat new-bindings)
                     (merge-category
                        (copy-list
                           (category-binding-category (cadr already)))
                        (cddr already)
                        (category-binding-category (cadr pat))
                        (cddr pat) t)
                     (setf (category-binding-category cat-b) new-cat)
                     (setq res
                        (top-subst-1
                           (cons (car already) (cons cat-b new-bindings))
                           already res))))
               (push pat res))))
      (nreverse res)))


(defun translate-metarule-semantic-form (form bindings idrule-binding-nos)
   (cond
      ((numberp form)
         (let
            ((trans
                (f-find (the fixnum form) bindings :key
                   #'match-binding-pattern-binding-no :test #'eql)))
            (setf trans
               (and trans
                  (match-binding-matched-binding-nos trans)))
            (if (and trans (null (cdr trans))) (car trans)
               (gde-ferror
"inconsistency detected applying metarule semantic form"))))
      ((compiled-function-p form)
         #'(lambda (idrule-form)
            (funcall form idrule-form idrule-binding-nos)))
      ((atom form) form)
      (t
         (cons
            (translate-metarule-semantic-form
               (car form) bindings idrule-binding-nos)
            (translate-metarule-semantic-form
               (cdr form) bindings idrule-binding-nos)))))


;;; Take word structure derivation (actually CDR - top category
;;; is not included) for a word retrieved from morph system and
;;; compute a set of semantic formulae from semantic fields in
;;; lexical entries and the semantic rules corresponding to
;;; morphological rules in word grammar

(defun compute-morph-word-semantics (tree)
   (cond
      ((eq (car tree) 'entry)
         (let
            ((entry (cadr tree)))
            (semantic-forms-present
               (ncons (cadddr entry)) (car entry))))
      (t
         (mapcan
            #'(lambda (mother-form)
                 (copy-list
                    (let
                       ((n 0) (forms (ncons mother-form)))
                       (dolist (daughter-form (cdr tree))
                          (progn
                             (setf n (1+ n))
                             (setf forms
                                (mapcan
                                   #'(lambda (form)
                                        (mapcar
                                           #'(lambda (ext)
                                              (substitute-into-form ext n form))
                                             (compute-morph-word-semantics
                                              (cdr daughter-form))))
                                   forms))))
                       forms)))
            (semantic-forms-present
               (and (assoc (car tree) d-semantics :test #'eq)
                  (cdr
                     (assoc (car tree) d-semantics :test #'eq)))
               (car tree))))))


;;; May define 'E' to have a semantic formula which gets
;;; substituted into tree with each 'E'. This is an alternative
;;; to specifying the semantics of the trace in each rule which
;;; contains a trace daughter.

(defun semantics-for-null nil
   (let
      ((null-word (get 'e 'word)))
      (if null-word
         (mapcan
            #'(lambda (sense)
                 (copy-list
                    (word-sense-semantic-forms sense)))
            (word-definition-senses null-word)))))


;;; Verify that the type of a semantic formula (computed using
;;; semantic types declared for syntactic categories in category
;;; declarations) agrees with the type declared corresponding to
;;; the syntactic category of the ID rule mother or word.
;;;
;;; There may be several types for each category - compute type
;;; of whole formula for all combinations of types and issue a
;;; warning only if no alternative type is equivalent to any
;;; possible result type.

(defun type-check-semantic-form
   (form construct-name mother-binding-no
      binding-list)
   (let
      ((mother-types
          (translate-category-to-types
             mother-binding-no binding-list)))
      (dolist
         (composed-daughter-type
            (translate-bindings-to-types form
               binding-list))
         (let
            ((reduced-daughter-type
                (reduce-type-formula composed-daughter-type
                   construct-name)))
            (if
               (dolist (mother-type mother-types t)
                  (if
                     (match-type-expression mother-type
                        reduced-daughter-type construct-name)
                     (return nil)))
               (if (top-rule-name-p construct-name)
                  (gde-warn
"type mismatch between mother and daughters in "
                     (idrule-name-string construct-name))
                  (gde-warn
"type mismatch between category and semantics of "
                     construct-name)))))))


(defun type-of-semantic-form (form)
   (cond
      ((or (basic-type-p form)
          (complex-type-p form))
         form)
      ((sem-lambda-expression-p form)
         (make-complex-type :arg
            (make-basic-type :name '*) :res
            (type-of-semantic-form
               (sem-lambda-expression-body form))))
      (t (make-basic-type :name '*))))


(defun translate-bindings-to-types
   (form binding-list)
   (cond
      ((numberp form)
         (translate-category-to-types form
            binding-list))
      ((or (atom form) (basic-type-p form)
          (complex-type-p form))
         (ncons form))
      (t
         (mapcan
            #'(lambda (car-trans)
                 (mapcar
                    #'(lambda (cdr-trans)
                         (cons car-trans cdr-trans))
                    (translate-bindings-to-types
                       (cdr form) binding-list)))
            (translate-bindings-to-types
               (car form) binding-list)))))


(defun translate-category-to-types
   (form binding-list)
   (or
      (mapcan
         #'(lambda (cat-dec)
              (let
                 ((dec
                     (normalise-category-definition cat-dec)))
                 (if
                    (and
                       (null
                          (category-declaration-feature-path
                             dec))
                       (match-category
                          (category-binding-number
                             (car
                                (category-declaration-cat-bindings
                                   dec)))
                          (category-declaration-cat-bindings
                             dec)
                          form binding-list))
                    (copy-list
                       (category-declaration-semantic-types
                          dec)))))
         *categories)
      (ncons (make-basic-type :name '*))))


(defun reduce-type-formula (form name)
   (let ((*reduction-applied* nil)
         (*cached-reductions* nil))
      (loop
         (setf form
            (reduce-lambda-formula1 form nil name))
         (cond
            ((not *reduction-applied*) (return form)))
         (setf *reduction-applied* nil))))


;;; Check if two expressions have same type. First will always
;;; be a basic or complex type. If both are complex types then
;;; just check for equality. If the second is a lambda
;;; expression then its bound variable should be equivalent to
;;; the first's argument and its result equivalent to the
;;; first's result:
;;;
;;;    <a, b>      (lambda (v) body)
;;;
;;; if v is of type a then body must be of type b, so check
;;; this.
;;;
;;; If second type is not a lambda expression then it cannot be
;;; equivalent to a complex type (i.e. a function).

(defun match-type-expression
   (type1 type2 name)
   (cond
      ((basic-type-p type1)
         (cond
            ((eq (basic-type-name type1) '*) t)
            ((basic-type-p type2)
               (or (eq (basic-type-name type2) '*)
                  (eq (basic-type-name type1)
                     (basic-type-name type2))))
            ((or (complex-type-p type2)
                (sem-lambda-expression-p type2))
               nil)
            (t t)))
      ((complex-type-p type1)
         (cond
            ((basic-type-p type2)
               (eq (basic-type-name type2) '*))
            ((complex-type-p type2)
               (and
                  (match-type-expression
                     (complex-type-arg type1)
                     (complex-type-arg type2) name)
                  (match-type-expression
                     (complex-type-res type1)
                     (complex-type-res type2) name)))
            ((sem-lambda-expression-p type2)
               (match-type-expression
                  (complex-type-res type1)
                  (reduce-type-formula
                     (list type2 (complex-type-arg type1))
                     name)
                  name))
            (t t)))))


;;; End of file

