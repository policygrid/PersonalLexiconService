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

;;; Non-deterministically parses a sentence with respect to a LR(1)
;;; parsing table. Parsing process is guided by atomic categories, but
;;; is augmented with unification of complex categories.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

;;; A vertex holds the links coming in from parent vertices on the left -
;;; each link has a set of possible alternative analyses.

(eval-when (compile load eval)
(defstruct
   (vertex
      (:constructor make-vertex (end state parents+analyses)))
   end state parents+analyses)

(defstruct
   (analysis
      (:constructor make-analysis (ntrans score nvt-tree)))
   ntrans score nvt-tree)

(defstruct
   (node-rec
      (:constructor make-node-rec
         (node norm-score ntrans score path packed)))
   node norm-score ntrans score path packed)
)


(eval-when (compile load eval)
(defmacro node-category (n) `(car,n))
(defmacro node-ntrans (n) `(cadr,n))
(defmacro node-score (n) `(caddr ,n))
(defmacro node-rest (n) `(cdddr ,n))
)


(eval-when (compile load eval)
(defmacro mapcan-lambda-unordered (fn lst)
   (let*
      ((fn-exp (cadr (macroexpand fn)))
         (var (caadr fn-exp))
         (lstvar (gensym))
         (lstret (gensym))
         (lsttemp (gensym)))
      `(let
         ((,lstvar ,lst)
            (,lstret nil)
            ,var)
         (loop
            (when (atom ,lstvar) (return ,lstret))
            (setq ,var (cons-car ,lstvar))
            (let ((,lsttemp (progn ,@(cddr fn-exp))))
               (when (consp ,lsttemp) (setq ,lstret (nconc ,lsttemp ,lstret))))
            (setq ,lstvar (cons-cdr ,lstvar))))))
)


;;; Global data structures

(defvar *productions*)
(defvar *state-actions*)
(defvar *shift-action-vector*)
(defvar *terminals*)
(defvar *terminal-categories*)
(defvar *null-categories*)
(defvar *sentence-end-marker*)
(defvar +lr1-pf-unpack-fn+)

;;; Bound here, internal

(defvar *n-unifications* 0)
(defvar *n-failures* 0)
(defvar *accepted-vertex-analyses*)
(defvar *vertices-done*)
(defvar *analyses-to-add*)
(defvar *last-lookaheads-list*)
(defvar *last-top-stack*)


;;; Set here, internal

(defvar *cached-unification-daughters* nil)


;;; Global parameters

(defvar *lr1-warnings-p* nil)
(defvar g-packing t)
(defvar *lr1-parse-timeouts*)

(defvar *phrasal-verbs-p* nil)
(defvar *subcat-probs-p* nil)

(defparameter +n-best-retained+ 1)


;;; Look up word in lexicon to get list of complex categories - then
;;; from mapping of generalised complex categories to atomic terminal
;;; category symbols, determine atomic category(s) for word. Returns
;;; ((<atomic1> . <complex-cats1>) (<atomic2> . <complex-cats2>) ...)

(defun lr1-compute-word-categories (word)
   (when (eq word *sentence-end-marker*)
      (return-from lr1-compute-word-categories
         (list (list (position *sentence-end-marker* (the list *terminals*)
                        :test #'eq)))))
   (multiple-value-bind (defns probs)
       (g-defns word)
      (do*
         ((defn (pop defns) (pop defns))
            (prob (or (pop probs) 0.0) (or (pop probs) 0.0))
            (cat-no 1 (1+ cat-no))
            (res nil))
         ((null defn)
            (unless res
               (warn
"Unable to assign an atomic terminal to any category of word '~A'"
                  word))
            res)
         (setq defn
            (if (lr-category-index (caar defn))
               (cons
                  (list* (g-copy-category (caar defn) nil nil nil nil)
                     0 prob (cdar defn))
                  (cdr defn))
               nil))
         (let ((cat-indices
                  (and defn
                     (find-atomic-term (caar defn) *terminal-categories*))))
            (cond
               ((not *lr1-warnings-p*))
               ((null cat-indices)
                  (warn
                     "Unable to assign an atomic terminal to category ~S of ~:
                      word '~A'"
                     cat-no word))
               ((cdr cat-indices)
                  ;; A word generalises 2 categories previously thought 
                  ;; (from looking at grammar) to be distinct and not to unify.
                  (warn
                     "More than one atomic category ~S for category ~S of word '~A'"
                     (mapcar #'(lambda (index) (nth index *terminals*)) cat-indices)
                     cat-no word)))
            (dolist (cat-index cat-indices)
               (declare (fixnum cat-index))
               (let ((pair (assoc cat-index res)))
                  (if pair
                     (lr1-parse-lexical-pack defn pair)
                     (push (list cat-index (cons nil defn)) res))))))))


(defun find-atomic-term (cat tree-vec)
   ;; Must return all possible atomic categories in case word definition
   ;; unifies with more than 1 backbone category derived from grammar -
   ;; i.e. word cat generalises these themselves generalised derived
   ;; categories
   (let ((tree
            (cdr (vector-svref tree-vec (vector-svref cat 0)))))
      (if (lr-fixnum-eql (simple-vector-length cat) 1)
         tree
         (find-atomic-term1 cat 1 tree nil))))


(defun find-atomic-term1 (cat offset tree res)
   (declare (fixnum offset))
   (loop
      (if tree
         (progn
            (when
               (find-atomic-matchvalues
                  (cons-car tree) (vector-svref cat offset))
               (setq res
                  (let ((new-offset (fixnum-add-1 offset)))
                     (declare (fixnum new-offset))
                     (if (lr-fixnum-eql new-offset (simple-vector-length cat))
                        (cons
                           (cons-car (cons-car (cons-cdr tree)))
                           res)
                        (find-atomic-term1 cat new-offset
                           (cons-car (cons-cdr tree)) res)))))
            (setq tree (cons-cdr (cons-cdr tree))))
         (return res))))


(defun find-atomic-matchvalues (rv nv)
   (or (eq rv nv)
      (eq rv (lr-unnamed-variable)) (lr-varp rv) (lr-varp nv)))


(defun lr1-parse-lexical-pack (defn pair)
   (if g-packing
      (do
         ((old-tail (cdr pair) (cdr old-tail)))
         ((null old-tail)
            (push (cons nil defn) (cdr pair)))
         (when (g-equal-p (caadr (car old-tail)) (caar defn) nil)
            (setf (node-rest (cadar old-tail))
               (cons (list* '*packed nil nil defn)
                  (node-rest (cadar old-tail))))
            (return)))
      (push (cons nil defn) (cdr pair))))


;;; Entry point

(defun lr1-parse (words)
   (let ((*last-lookaheads-list*
            (cons nil
               (mapcar #'lr1-compute-word-categories
                 (append words (list *sentence-end-marker*))))))
      (if (member nil (cdr *last-lookaheads-list*) :test #'eq)
         (values nil 0 0 nil) ; no atomic category for one or more of the words
         (let
            ((*n-unifications* 0)
             (*n-failures* 0)
             (*last-top-stack* nil)
             ;;(*accepted-vertex-analyses* nil)
             (nvt-trees nil)
             (root-weights nil))
            (if (and (boundp '*lr1-parse-timeouts*) (consp *lr1-parse-timeouts*))
               (when
                  (eq
                     (let ((+parse-timeout+ (caar *lr1-parse-timeouts*))
                           #+(or allegro sbcl)
                           (+parse-storeout+ (cdar *lr1-parse-timeouts*)))
                        (execute-with-timeout
                           (progn
                              (format *error-output*
                                 "~&Halted in ~A / ~A~%" 'lr1-parse-1 'lr1-parse-2)
                              :timeout)
                           (lr1-parse-1)
                           (multiple-value-setq (nvt-trees root-weights) (lr1-parse-2))))
                     :timeout)
                  (let ((+parse-timeout+ (cadr *lr1-parse-timeouts*))
                        #+(or allegro sbcl)
                        (+parse-storeout+ (cddr *lr1-parse-timeouts*)))
                     (execute-with-timeout
                        (format *error-output* "~&Halted in ~A~%" 'lr1-parse-2)
                        (multiple-value-setq (nvt-trees root-weights) (lr1-parse-2)))))
               (progn
                  (lr1-parse-1)
                  (multiple-value-setq (nvt-trees root-weights) (lr1-parse-2))))
            (values nvt-trees *n-unifications* *n-failures* root-weights)))))


(defun lr1-parse-1 (&aux (recoveredp nil))
   ;; construct parse forest, recovering from dead-ends as necessary
   (unwind-protect
      (let ((stack (list (list (make-vertex 0 0 nil))))
            (word-no 0))
         (dolist (lookaheads (cdr *last-lookaheads-list*))
            (incf word-no)
            (push nil stack) ; top of stack holds vertices for current word-no
            (do
               ((*vertices-done* nil)
                (lst (cadr stack) (cadr stack))
                (prev-lst nil))
               ((eq prev-lst lst)
                  (dolist (v lst)
                     (lr1-parse-shift-vertex v stack word-no lookaheads)))
               (do*
                  ((v-tail lst (cdr v-tail))
                   (v (car v-tail) (car v-tail)))
                  ((eq v-tail prev-lst))
                  (push v *vertices-done*)
                  (let ((*analyses-to-add* nil))
                     (lr1-parse-reduce/accept-vertex v stack word-no lookaheads)
                     (dolist (add *analyses-to-add*)
                        (let ((already
                                 (assoc (caar add) (vertex-parents+analyses (cdr add))
                                    :test #'eq)))
                           (if already
                              (setf (cdr already) (append (cdar add) (cdr already)))
                              (push (car add) (vertex-parents+analyses (cdr add))))))))
               (setq prev-lst lst))
            (when (or (null *last-top-stack*)
                      (lr1-trailing-non-lexical-p (cadr stack)))
               (with-uninterrupted-execution
                  (setq *last-top-stack* (cadr stack))
                  (setq *last-lookaheads-list* (member lookaheads *last-lookaheads-list* :test #'eq))))
            (when (and *accepted-vertex-analyses* (not recoveredp))
               (return nil))
            (unless (car stack) ; do we need to recover from a dead end?
               (setq recoveredp t)
               (let ((*vertices-done* nil) (*analyses-to-add* nil))
                  (dolist (v (cadr stack))
                     (push v *vertices-done*)
                     (lr1-parse-reduce/accept-vertex
                        v stack word-no (car (last *last-lookaheads-list*)))))
               (dolist (v (cadr stack))
                  (dolist (la lookaheads)
                     (lr1-parse-shift v 0 stack word-no
                        (mapcar
                           #'(lambda (nvt-tree)
                               (cons (car nvt-tree)
                                 (cons
                                    (list* (node-category (cadr nvt-tree)) 1
                                       (+ 0.0 (node-score (cadr nvt-tree)))
                                       (node-rest (cadr nvt-tree)))
                                    (cddr nvt-tree))))
                           (cdr la))))))
            (setf (cadr stack) nil) ; free up some space
            (lr1-parse-invalidate-cached-unifications nil)))
     (progn
       (when recoveredp (setq *accepted-vertex-analyses* nil))
       (lr1-parse-invalidate-cached-unifications t)) ; ensure it gets done whatever happens
     ))


(defun lr1-parse-2 ()
   ;; construct an analysis from parse forest - if incomplete append lookaheads
   ;; to best hypothesis
   (let*
      ((all-analyses
         (sort
	  (if *accepted-vertex-analyses*
	      (funcall +lr1-pf-unpack-fn+);;(lr1-parse-result-analyses)
	      (lr1-parse-collect-partial-analyses *last-top-stack*
						  (if *last-lookaheads-list*
						      (butlast *last-lookaheads-list* 1) nil)))
	  #'> :key #'analysis-score))
       (analyses
	(if (eql +n-best-retained+ 0) all-analyses
            (ldiff all-analyses (nthcdr +n-best-retained+ all-analyses))))
       (root-weights 
	;; don't exponentiate - avoid possible underflow
	(mapcar #'analysis-score analyses)))
     ;;(terpri)
     ;;(pprint root-weights)
      #+debug
      (format t "~&~A~%"
         (mapcar
            #'(lambda (a) (cons (analysis-score a) (analysis-ntrans a)))
            analyses))
      (values
         (mapcar #'analysis-nvt-tree analyses) root-weights)))


(defun lr1-trailing-non-lexical-p (vertices)
   ;; is there an arc covering last word that's more than just a single lexical node?
   (some
      #'(lambda (v)
          (some
             #'(lambda (pa)
                (some
                   #'(lambda (vt-and-tree) (not (atom (cadr (cdr vt-and-tree)))))
                   (cdr pa)))
             (vertex-parents+analyses v)))
      vertices))

(defun lr1-parse-invalidate-cached-unifications (remove-p)
   ;; if remove-p then completely remove caches, otherwise just invalidate their
   ;; contents - by clearing them
   (with-uninterrupted-execution
      (dolist (daughter *cached-unification-daughters*)
         (when (and (consp daughter) ; in case junk has ended up there
                    (cdr daughter))
            (if (or remove-p (listp (cdr daughter)))
               (setf (cdr daughter) nil)
               (clrhash (cdr daughter)))))
      (when remove-p (setq *cached-unification-daughters* nil))))


;;; Manage the unpacking process. Use a beam size of +n-best-retained+, but
;;; temporarily widen the beam if some analyses fail in unification consistency
;;; verification.

(defun lr1-parse-result-analyses (&optional (last-verify-fails 0))
   ;; (setq aa *accepted-vertex-analyses*)
   ;; (return-from lr1-parse-result-analyses nil)
   ;; (pprint *accepted-vertex-analyses*)
   (let*
      ((verify-fails 0)
       (analyses
         (mapcan
            #'(lambda (vt-and-tree)
                (multiple-value-bind (unps unp-scores)
                     (lr1-parse-unpack (cdr vt-and-tree) (car vt-and-tree) nil)
                   (mapcan
                      #'(lambda (unp)
                          (let ((unp-score (pop unp-scores)))
                             (let ((v-t
                                     (lr1-pf-verify-tree
                                        (cdr vt-and-tree) (car vt-and-tree) unp)))
                                (if v-t
                                   (list (make-analysis 0 unp-score v-t))
                                   (progn (incf verify-fails) nil)))))
                      unps)))
            *accepted-vertex-analyses*)))
      (when (> verify-fails 0)
         (warn "~A tree(s) failed unpacking verify test" verify-fails))
      analyses))


(defun lr1-parse-unpack-clear-cache (tree)
   ;; clear subtree caches - called before a repeated attempt to unpack, perhaps
   ;; with a greater number of analyses to be left after pruning
   (when (consp (node-ntrans (car tree)))
      (setf (node-ntrans (car tree)) nil)
      (let ((tail (node-rest (car tree))))
         (loop
            (when (or (atom tail) (atom (car tail))) (return))
            (let ((packed (cdar tail)))
               (lr1-parse-unpack-clear-cache (cddr packed)))
            (setq tail (cdr tail)))
         (cond
            ((and (cdr tree) (atom (cadr tree)))
               ;; a word node
               nil)
            (t
               (dolist (d (cdr tree)) (lr1-parse-unpack-clear-cache d)))))))


;;; Unpack probabilistic parse forest with pruning at each node. Cache unpacking
;;; results at each node so that unpacking done only once at a node in
;;; presence of subtree sharing. No checking of unifications at packed nodes
;;; since packing is under equality, not subsumption

(defun lr1-parse-unpack (tree vt rule-cat)
   (g-perform-stack-check)
   (when (consp (node-ntrans (car tree)))
      (let ((cached (node-ntrans (car tree))))
         (return-from lr1-parse-unpack 
           (values (car cached) (cdr cached)))))
   (let
       ((tail (node-rest (car tree)))
        (unpacked nil) (unpacked-scores nil))
     (loop
      (when (or (atom tail) (atom (car tail))) (return))
      (let ((packed (cdar tail)))
        (multiple-value-bind (unps unp-scores)
            (lr1-parse-unpack (cddr packed) vt rule-cat)
          (dolist (u unps)
            ;; must include host tree so nodes packed into packed nodes will
            ;; be found later
            (push (cons tree u) unpacked)
            (push (pop unp-scores) unpacked-scores))))
      (setq tail (cdr tail)))
     (cond
       ((and (cdr tree) (atom (cadr tree))) ; a word node
        (push (list tree) unpacked) 
        (push (node-score (car tree)) unpacked-scores))
       (t
        (let* ((rname
                (and (consp tail) (car tail)))
               (d-cats ; allow for elide case
                (and rname (cdr (get (intern rname) 'lr1-psrule))))
               (daughters-alts (list (list tree)))
               (daughters-alts-scores 
                (list (lr1-parse-unpack-score tree rname))))
          (dolist (d (cdr tree))
            (multiple-value-setq (daughters-alts daughters-alts-scores)
              (lr1-parse-unpack-daughter
               d vt (and d-cats (car (pop d-cats))) daughters-alts
               daughters-alts-scores)))
          (setq unpacked (append unpacked daughters-alts))
          (setq unpacked-scores (append unpacked-scores daughters-alts-scores)))))
     (multiple-value-setq (unpacked unpacked-scores)
       (lr1-parse-unpack-prune unpacked unpacked-scores))
     (setf (node-ntrans (car tree))
           (cons unpacked unpacked-scores))
     (values unpacked unpacked-scores)))

(defun lr1-parse-unpack-score (tree rname)
   (let ((score (node-score (car tree))))
      (when *subcat-probs-p*
         (setq score (lr1-parse-unpack-score-comp tree rname score)))
      (when *phrasal-verbs-p*
         (setq score (lr1-parse-unpack-score-phrasal tree rname score)))
      score))
      

(defun lr1-parse-unpack-stem (form)
   (let ((orig form))
      (when (and (> (length form) 1)
                 (eql (char form 0) #\<) (eql (char form 1) #\w))
         (let*
            ((wtag-end (position #\> form :start 2))
             (endwtag-start
                (and wtag-end (position #\< form :start (1+ wtag-end)))))
            (when (and wtag-end endwtag-start)
               (setq form
                  (subseq form (1+ wtag-end) endwtag-start)))))
      (let*
        ;; ((epos (or (position #\+ form :from-end t)
                  ;;  (position #\: form :from-end t)
                  ;;  (position #\_ form :from-end t))))
	((pos-p (position #\_ form :from-end t))
	 (affix-p (position #\+ form :from-end t :end pos-p))
	 (num-p (position #\: form :from-end t :end pos-p))
	 (epos (or affix-p num-p pos-p)))
         (when epos (setq form (subseq form 0 epos))))
      (if (eq form orig)
         (string-downcase form) (nstring-downcase form))))


(defun lr1-parse-unpack-daughter (d vt d-cat daughters-alts daughters-alts-scores)
   (let ((unpacked nil) (unpacked-scores nil))
      (dolist (alt daughters-alts)
         (let ((alt-score (pop daughters-alts-scores)))
             (multiple-value-bind (unps unp-scores)
                (lr1-parse-unpack d vt d-cat)
               (dolist (u unps)
                  ;; equivalent to (append alt u) but can save a lot of space
                  ;; alt first so first tree is the enclosing one
                  (push (list :embedded alt u) unpacked)
                  (push (+ (pop unp-scores) alt-score) unpacked-scores)))))
      (values unpacked unpacked-scores)))


;;;

(defun lr1-parse-unpack-prune (vt-and-trees scores) 
   ;; choose n best for each node - can do it efficiently when number to
   ;; process is <=n, or n=1
   (cond
      ((null (cdr vt-and-trees)))
      ((eql +n-best-retained+ 1)
         (let ((max-score most-negative-single-float)
               (max-v-and-t nil)
               (v-and-ts vt-and-trees))
            (dolist (s scores)
               (let ((v-and-t (pop v-and-ts)))
                  (when (> s max-score)
                     (setq max-score s max-v-and-t v-and-t))))
            (setf (car scores) max-score (car vt-and-trees) max-v-and-t)
            (setf (cdr scores) nil (cdr vt-and-trees) nil)))
      ((or (eql +n-best-retained+ 0)
          (<= (length vt-and-trees) +n-best-retained+)))
      (t
         (do
            ((threshold
                (- (lr1-parse-nth-largest +n-best-retained+ scores)
                   least-positive-single-float))
             (vtail vt-and-trees (cdr vtail))
             (stail scores (cdr stail))
             (kept +n-best-retained+))
            ((null vtail)
             (setq vt-and-trees (delete nil vt-and-trees :test #'eq))
             (setq scores (delete nil scores :test #'eq)))
            ;; if score better than that of +n-best-retained+th update kept count
            ;; and if we've got enough chop off rest of input lists - and if score
            ;; worse then mark for deletion
            (if (>= (car stail) threshold)
               (when (eql (decf kept) 0)
                  (setf (cdr vtail) nil) (setf (cdr stail) nil))
               (setf (car vtail) nil (car stail) nil)))))
   (values vt-and-trees scores))

(defvar *nth-largest-vector* nil)

(defun lr1-parse-nth-largest (n scores)
   ;; return nth largest value in scores list
   (let ((vec 
            (if (and *nth-largest-vector* (eql (length *nth-largest-vector*) n))
               (progn
                  (fill *nth-largest-vector* most-negative-single-float)
                  *nth-largest-vector*)
               (setq *nth-largest-vector*
                  (make-array n :initial-element most-negative-single-float))))
         (end (1- n)))
      (declare (simple-vector vec))
      (dolist (s scores)
         (when (> s (svref vec end)) ; better than current nth?
            (dotimes (v n)           ; work upward from zeroth element
               (declare (fixnum v n))
               (when (> s (svref vec v))
                  (do ((i end (1- i)))
                      ((eql i v) (setf (svref vec v) s))
                      (declare (fixnum i end))
                      (setf (svref vec i) (svref vec (1- i))))
                  (return)))))
      (svref vec end)))


;;; Verify unifications in an unpacked tree (specified as a set of tree nodes
;;; within the forest). Return nil if tree fails a unification, otherwise
;;; the tree itself - with any packed nodes removed

(defun lr1-pf-verify-tree (tree vt node-set)
   (g-perform-stack-check)
   (let ((tail (node-rest (car tree))) mother)
      (loop
         (when (or (atom tail) (atom (car tail)))
            (setq mother (list* (node-category (car tree)) tail))
            (return))
         (let ((packed (cdar tail)))
            (when (lr1-pf-node-present-p (cddr packed) node-set)
               (return-from lr1-pf-verify-tree
                  (multiple-value-bind (success new-rvt new-nvt)
                     (g-unify
                        (caar tree) (caar (cddr packed)) vt (cadr packed))
                     (declare (ignore new-rvt))
                     ;;#+ignore
                     (unless success
                        (format t "~&Verify failure ~A~%~A~%~A~%" (find-if #'atom tail)
                           (g-copy-category (caar tree) vt nil nil nil)
                           (g-copy-category (caar (cddr packed)) (cadr packed) nil nil nil)))
                     (when success
                        (let ((u (lr1-pf-verify-tree (cddr packed) new-nvt node-set)))
                           (when u
                              ;; (car packed) is the nvt of packed-into node at
                              ;; the time of packing
                              (cons
                                 (g-combine-vts (g-remove-segment (car packed) vt) (car u))
                                 (cdr u)))))))))
         (setq tail (cdr tail)))
      (cond
         ((and (cdr tree) (atom (cadr tree)))
            ;; a word node
            (unless (lr1-pf-node-present-p tree node-set)
               (warn "d verify")
                  (return-from lr1-pf-verify-tree nil))
            (cons vt (cons mother (cdr tree))))
         (t
            (let ((daughters nil))
               (dolist (d (cdr tree))
                  (unless (lr1-pf-node-present-p d node-set)
                     (warn "verify ~A ~A" d node-set)
                     (return-from lr1-pf-verify-tree nil))
                  (let ((vt-and-d (lr1-pf-verify-tree d vt node-set)))
                     (unless vt-and-d (return-from lr1-pf-verify-tree nil))
                     (setq vt (car vt-and-d)
                           daughters (cons (cdr vt-and-d) daughters))))
               (cons vt (cons mother (nreverse daughters))))))))


(defun lr1-pf-node-present-p (tree node-set)
   ;; like member but knows about 'fake' 2-ary appends marked with :embedded
   ;; i.e. (a b c d e f) might be represented as (a :embedded (b c) (d e) f)
   (cond
      ((atom node-set) nil)
      ((eq tree (car node-set)) t)
      ((eq (car node-set) :embedded)
         (or (lr1-pf-node-present-p tree (cadr node-set))
             (lr1-pf-node-present-p tree (caddr node-set))
             (lr1-pf-node-present-p tree (cdddr node-set))))
      (t (lr1-pf-node-present-p tree (cdr node-set)))))


;;; Look for a vertex in list with a given state; and is there a vertex
;;; in vertices+analyses which has same end and state as given vertex

(eval-when (compile load eval)
(defmacro lr1-parse-state-vertex (state vertex-list)
   `(do
      ((.vs. ,vertex-list (cdr .vs.)))
      ((null .vs.) nil)
      (when (lr-fixnum-eql (vertex-state (car .vs.)) ,state)
         (return (car .vs.)))))


(defmacro lr1-parse-vertex-end-state-vertex-p (vertex vertices+analyses)
   `(do
      ((.v+as. ,vertices+analyses (cdr .v+as.)))
      ((null .v+as.) nil)
      (let ((.v. (caar .v+as.)))
         (when
            (and (lr-fixnum-eql (vertex-end .v.) (vertex-end ,vertex))
               (lr-fixnum-eql (vertex-state .v.) (vertex-state ,vertex)))
            (return t)))))
)


;;; Perform all shifts on one vertex

(defun lr1-parse-shift-vertex (v stack word-no lookaheads)
  ;;(format t "actions: ~A~%" (vector-svref *state-actions* (vertex-state v)))
   (dolist
       (action-pair (cddr (vector-svref *state-actions* (vertex-state v))))
     ;;(format t "vertex-state : ~A action-pair : ~A~%" (vertex-state v) action-pair)
      (unless (cdr action-pair)
         (let ((action-bv-length
                  (bit-vector-length (car action-pair))))
            (dolist (la lookaheads)
	      (dolist (shift-pair (vector-svref *shift-action-vector* (car la)))
		;;(format t "shift-pair : ~A using: ~A~%" shift-pair la)
                  (when
                     (and (lr-fixnum-< (car shift-pair) action-bv-length)
                        (not
                           (lr-fixnum-eql
                              (bit-vector-sbit (car action-pair) (car shift-pair))
                              0)))
                     (let ((score
                              (lr1-parse-action-prob
			       (vertex-state v) 'shift (car la) nil (cdr shift-pair))))
		       ;;(format t "Going to shift with prob: ~A~%" score)
		       (lr1-parse-shift
                           v (cdr shift-pair) stack word-no
                           (mapcar
                              #'(lambda (nvt-tree)
                                  (cons (car nvt-tree)
                                    (cons
                                       (list* (node-category (cadr nvt-tree)) 1
                                          (+ score (node-score (cadr nvt-tree)))
                                          (node-rest (cadr nvt-tree)))
                                       (cddr nvt-tree))))
                              (cdr la)))))))))))


(defun lr1-parse-shift (parent state stack word-no analyses)
 ;; (format t "Shift state ~A, start ~A to state ~A end ~A ~%"
	;;   (vertex-state parent) (- word-no 1) state word-no)
  (let ((already
            (lr1-parse-state-vertex state (car stack))))
      (if already
         (let ((from
                  (assoc parent (vertex-parents+analyses already) :test #'eq)))
            (if from
               (setf (cdr from) (nconc analyses (cdr from)))
               (push (cons parent analyses)
                  (vertex-parents+analyses already))))
         (push
            (make-vertex word-no state (list (cons parent analyses)))
            (car stack)))))


;;; Perform all reduce / accept on one vertex

(defun lr1-parse-reduce/accept-vertex (v stack word-no lookaheads)
   (dolist
      (action-pair (cddr (vector-svref *state-actions* (vertex-state v))))
      (cond
         ((symbolp (car action-pair)) 
            (dolist (vertex+analyses (vertex-parents+analyses v))
               (when
                  (and (eql (vertex-state (car vertex+analyses)) 0)
                     (eq (caar lookaheads)
                        (position *sentence-end-marker* (the list *terminals*)
                           :test #'eq))) 
                  (dolist (analysis (cdr vertex+analyses))
                     (pushnew analysis *accepted-vertex-analyses* :test #'eq)))))
         ((cdr action-pair) 
            (dolist (la lookaheads)
               (unless
                  (lr-fixnum-eql
                     (bit-vector-sbit (car action-pair) (car la))
                     0)
                  (lr1-parse-reduce 
                     v (vector-svref *productions* (- (cdr action-pair)))
                     stack word-no lookaheads (- (cdr action-pair)) (car la))
                  ;; Must not perform same reduce action on any state more 
                  ;; than once (caused by ambiguous lookaheads asking for 
                  ;; same action) otherwise will get spurious parses  
                  (return)))))))


(defun lr1-parse-reduce (vertex cfrule stack word-no lookaheads cfrule-no la)
;;;    (format t "~&Attempt to reduce state ~A, end ~A with ~A~%"
;;;       (vertex-state vertex) (vertex-end vertex) (cfrule-name cfrule))
   (dolist
      (av-and-analyses
         (lr1-parse-reduce-ancestors vertex cfrule))
      (when (cdr av-and-analyses)
         (let*
            ((av (car av-and-analyses)) ; v1'
               (to-state ; s''
                  (cdr
                     (assoc (cfrule-mother cfrule)
                        (cadr (vector-svref *state-actions* (vertex-state av)))
                        :test #'eq)))
               (score
                  (lr1-parse-action-prob (vertex-state vertex) cfrule-no la
                     (vertex-state av) to-state))
               (already ; v''
                  (lr1-parse-state-vertex to-state (cadr stack))))
;;;          (format t "~&Reduce state ~A, start ~A, end ~A with ~A (~A) -> state ~A~%"
;;;             (vertex-state vertex) (vertex-end av) (vertex-end vertex)
;;;             (cfrule-name cfrule) (vertex-state av) to-state)
            (dolist (nvt-and-tree (cdr av-and-analyses))
               (incf (node-score (cadr nvt-and-tree)) score)
               (when (and g-packing
                        (consp (node-rest (cadr nvt-and-tree))))
                  ;; put reduce score in packed analyses as well
                  (dolist (p (node-rest (cadr nvt-and-tree)))
                     (when (and (consp p) (eq (car p) '*packed))
                        (incf (node-score (cadddr p)) score)))))
            (if already
               (let ((parent-already
                        (assoc av (vertex-parents+analyses already) :test #'eq)))
                  (if parent-already
                     (let ((new-analyses
                              (lr1-parse-reduce-pack
                                 parent-already (cdr av-and-analyses))))
                        (when new-analyses
                           (if (eq (car *vertices-done*) already)
                              ;; add to (vertex-parents+analyses already) later
                              (push (cons (cons av new-analyses) already)
                                 *analyses-to-add*)
                              (setf (cdr parent-already)
                                 (append (cdr parent-already) new-analyses)))
                           (when (member already *vertices-done* :test #'eq)
                              (lr1-parse-reduce-clone
                                 already (cons av new-analyses) stack word-no
                                 lookaheads))))
                     (let
                        ((v2 ; v2'
                              (lr1-parse-vertex-end-state-vertex-p av
                                 (vertex-parents+analyses already))))
                        (if v2
                           (lr1-parse-reduce-clone
                              already av-and-analyses stack word-no lookaheads)
                           (progn
                              (if (eq (car *vertices-done*) already)
                                 ;; add to (vertex-parents+analyses already) later
                                 (push (cons av-and-analyses already)
                                    *analyses-to-add*)
                                 (push av-and-analyses
                                    (vertex-parents+analyses already)))
                              (when (member already *vertices-done* :test #'eq)
                                 (lr1-parse-reduce-clone
                                    already av-and-analyses stack word-no
                                    lookaheads)))))))
               (push (make-vertex (1- word-no) to-state (list av-and-analyses))
                  (cadr stack)))))))


(defun lr1-parse-reduce-clone (already av-and-analyses stack word-no lookaheads)
   (lr1-parse-reduce/accept-vertex
      (make-vertex
         (vertex-end already) (vertex-state already) (list av-and-analyses))
      stack word-no lookaheads))


(defun lr1-parse-reduce-pack (vertex-and-analyses new-analyses)
   (if g-packing
      (do
         ((old-tail (cdr vertex-and-analyses) (cdr old-tail)))
         ((or (null old-tail) (null new-analyses))
            new-analyses)
         (unless (eq (node-rest (cadar old-tail)) '|elide|)
            ;; Mustn't pack into nodes which will be elided on next reduction
            (setq new-analyses
               (delete-if
                  #'(lambda (a)
                     (if (eq (node-category (cadar old-tail)) (node-category (cadr a)))
                        ;; attempt to pack a node into itself
                        (progn
                           ;; (warn "EQ in packing - duplicate backbone rule?")
                           t)
                        (when
                           (and (not (eq (node-rest (cadr a)) '|elide|))
                              (g-equal-p (node-category (cadar old-tail))
                                 (node-category (cadr a))
                                 (car (car old-tail))))
                           (push (list* '*packed (car (car old-tail)) a)
                              (node-rest (cadar old-tail)))
                           t)))
                  new-analyses))))
      new-analyses))

;;;

(defvar *avs-and-analyses*)

(defparameter +disabled-rules+ nil)
(defparameter +rule-application-counts+ nil) ; set to t if want stats

(defun lr1-summarise-rule-application-counts ()
   (do ((lst nil)
        (tl +rule-application-counts+ (cddr tl)))
       ((null tl)
          (mapc #'print
             (sort lst
                #'(lambda (x y)
                   (cond ((> (cadr x) (cadr y)))
                         ((eql (cadr x) (cadr y)) (> (cddr x) (cddr y)))))))
          (values))
       (when (consp (cadr tl))
          (push (cons (car tl) (cadr tl)) lst))))


(defun lr1-parse-reduce-ancestors (vertex production)
   ;; return an alist of ancestor vertices and analyses wrt given production
   (when (member (cfrule-name production) +disabled-rules+ :test #'eq)
      (return-from lr1-parse-reduce-ancestors nil))
   (let*
      ((rule-name (cfrule-name production))
       (rule (get rule-name 'lr1-psrule)))
      (cond
         ((null (cfrule-daughters production))
            (list
               (cons vertex
                  (list
                     (cons nil
                        (if rule
                           ;; unification grammar rule with no daughters
                           (cons
                              (list*
                                 (g-copy-category (caar rule) nil nil nil nil)
                                 0 0.0 (cdar rule))
                              nil)
                           ;; rule made up by backbone computation to do [NULL +] gap
                           (cons
                              (list*
                                 (g-copy-category
                                    (lr-fast-getf *null-categories*
                                       (cfrule-mother production) nil)
                                    nil nil nil nil)
                                 0 0.0 '|null|)
                              (cons 'e (semantics-for-null)))))))))
         ((null rule)
            (error
               "Inconsistency - could not find unification grammar definition for rule ~A"
               rule-name))
         (t
            (let ((*avs-and-analyses* nil))
               (lr1-parse-reduce-ancestors1
                  vertex (car rule) (cdr rule)
                  (if (and (cddr rule) (not (eq (cdar rule) '|elide|))) 1 0)
                  0.0
                  '(nil . nil) nil)
               (when +rule-application-counts+
                  (when (atom +rule-application-counts+) (setq +rule-application-counts+ nil))
                  (let ((pair (getf +rule-application-counts+ rule-name)))
                     (unless pair
                        (setf (getf +rule-application-counts+ rule-name)
                           (setq pair (cons 0 0))))
                     (if *avs-and-analyses* (incf (car pair)) (incf (cdr pair)))))
               *avs-and-analyses*)))))


(defun lr1-parse-reduce-ancestors1 (vertex mother daughters ntrans score
      vts nodes)
   ;; Nodes is a list of nodes traversed so far. Assume that initially 
   ;; daughters is not empty. On final recursion vts contains new
   ;; (nvt . mother-cat) rather than (rvt . nvt) since a
   ;; cached unification for last rule daughter can give this straight off.
   ;; Must create a copy of mother cons each time otherwise packing could
   ;; affect cached items - e.g. if later there is reduction with a (distinct)
   ;; backbone rule corresponding to same unif rule with daughter nodes
   ;; identical (i.e. lexical entries generalising 2 backbone cats). Also
   ;; daughter list must be consed up each time since caching takes no
   ;; account of packing within daughters
   (if daughters
      (dolist (parent+nodes (vertex-parents+analyses vertex))
         (dolist (n (cdr parent+nodes))
            (let*
               ;; d: (dcat . <hash-table/plist ncat1:(nvts1 (avts1 cached1 ...) nvts2 ...) ...>)
               ((d (car daughters))
                (cached
                   (if (cdr d)
                      (lr-fast-getf
                         (lr-fast-getf
                            (if (listp (cdr d)) (lr-fast-getf (cdr d) (caadr n) nil)
                               (gethash (caadr n) (cdr d)))
                            (car n) nil)
                         vts t)
                      (progn
                         (push d *cached-unification-daughters*)
                         t))))
;;;                (if (eq cached t)
;;;                   (format t "- ") (format t "+ ")) ; miss=-, hit=+
               (when (eq cached t)
                  (setq cached
                     (lr1-parse-reduce-daughter n d (car vts) (cdr vts)))
                  (when (and cached (null (cdr daughters)))
                     (setq cached
                        (cons (cdr cached)
                           (g-copy-category (car mother) (car cached) nil nil nil))))
                  (when (and (listp (cdr d)) (> (length (cdr d)) 100)) ; 50 elements
                     (setf (cdr d) (make-hash-table :test #'eq :size 100)))
                  (if (listp (cdr d))
                     (setf (getf (getf (getf (cdr d) (caadr n)) (car n)) vts) 
                        cached)
                     (setf (getf (getf (gethash (caadr n) (cdr d)) (car n)) vts) 
                        cached)))
               (when cached
                  (lr1-parse-reduce-ancestors1 (car parent+nodes)
                     mother (cdr daughters)
                     (+ (node-ntrans (cadr n)) ntrans)
                     score
                     cached (cons (cdr n) nodes))))))
      (multiple-value-bind (daughters elide-score)
          (lr1-parse-reduce-daughters nodes)
         (let ((analysis
                  (cons (car vts)
                     (cons
                        (list* (cdr vts) ntrans (+ score elide-score) (cdr mother))
                        daughters)))
               (found (assoc vertex *avs-and-analyses* :test #'eq)))
            (if found
               (block packing
                  (when (and g-packing
                           (not (eq (node-rest (cadr analysis)) '|elide|)))
                     ;; mustn't pack (into) nodes which will be elided on next reduce
                     (dolist (old (cdr found))
                        (when
                           (and (not (eq (node-rest (cadr old)) '|elide|))
                              (g-equal-p (node-category (cadr old))
                                 (node-category (cadr analysis))
                                 (car old)))
                           (push (list* '*packed (car old) analysis)
                              (node-rest (cadr old)))
                           (return-from packing))))
                  (push analysis (cdr found)))
               (push (list vertex analysis) *avs-and-analyses*))))))


(defun lr1-parse-reduce-daughters (nodes)
   ;; for a dummy node with 'elide' as the node-rest, splice in its daughters. Also
   ;; return second value of the sum of scores of any elided nodes, to be added
   ;; in to new mother
   (cond
      ((cdr nodes)
         (do ((ns nodes (cdr ns))
              (res nil)
              (elide-score 0.0))
            ((null ns) (values res elide-score))
            (if (eq (node-rest (caar ns)) '|elide|)
               (progn
                  (setq res (append (cdar ns) res))
                  (incf elide-score (node-score (caar ns))))
               (push (car ns) res))))
      ((eq (node-rest (caar nodes)) '|elide|)
         (values (cdar nodes) (node-score (caar nodes))))
      (t (values nodes 0.0))))


(defun lr1-parse-reduce-daughter (node rule-daughter rvt nvt)
   (declare (integer *n-unifications* *n-failures*))
   (incf *n-unifications*)
   (let ((nvt-tail (lr-fast-last nvt)))
      (when nvt-tail (setf (cdr nvt-tail) (car node)))
      (multiple-value-bind (succeed new-rvt new-nvt)
         (g-unify
            (car rule-daughter) (caadr node) rvt (or nvt (car node)))
         (when nvt-tail (setf (cdr nvt-tail) nil))
         (cond
            (succeed
               (when nvt-tail (setq new-nvt (append new-nvt (car node))))
               (cons new-rvt new-nvt))
            (t (incf *n-failures*) nil)))))

;;; End of file
