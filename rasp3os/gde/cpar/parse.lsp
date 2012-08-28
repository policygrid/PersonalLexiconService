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

;;; ANLT CHART PARSER - MAIN PARSE FUNCTIONS
;;;
;;; Implements parsing algorithm, local ambiguity packing and
;;; unpacking.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(eval-when (compile load eval)
(defmacro g-trace-node-p (node) `(eq (cdr ,node) '|null|))

(defmacro g-kleene-one-node-p (node) `(eq (cdr ,node) '+))

(defmacro g-category-index (cat) `(vector-svref ,cat 0))
)


;;; Parse a list of words. GDE passes optional variable top-indexes so parser
;;; won't unpack categories which cannot possibly be top. Argument passed as
;;; t if no top constraint is in force.
;;; Active edges indexed on vertex number and category index of next needed
;;; category, inactive on start vertex and category of constituent.

(defun g-parse (words &optional (top-indexes t))
   (let*
      ((nvertices (1+ (length words)))
         (ncats (1+ *current-category-index))
         (chart
            (cons
               (make-array (list nvertices ncats) :initial-element nil)
               (and g-packing
                  (make-array (list nvertices ncats) :initial-element nil))))
         (word-no 0)
         (g-result nil))
      (setq *chart-edges nil)
      (loop
         (cond
            ((null words)
               (g-insert-traces word-no chart t)
               (return
                  (if g-packing
                     (let
                        ((res
                              (mapcan
                                 #'(lambda (p)
                                    (when
                                       (or (not (listp top-indexes))
                                          (member (g-category-index (caadr p))
                                             top-indexes))
                                       (g-unpack (cdr p) (car p))))
                                 g-result)))
                        (dolist (p res) (g-rempack (cdr p)))
                        res)
                     g-result)))
            ((and (not *g-compute-all-edges)
                  (> word-no 0)
                  (dotimes (n ncats t)
                     (when (aref (car chart) word-no n) (return nil))))
               (return nil)))
         (g-insert-traces word-no chart nil)
         (dolist (defn (g-defns (car words)))
            (let ((cat (caar defn)))
               (when (g-category-index cat)
                  (multiple-value-bind (renamed-cat re-entrant-p)
                     (g-copy-category cat nil nil nil nil)
                     (g-process-inactive
                        (make-g-chart-edge nil
                           (cons (cons renamed-cat (cdar defn)) (cdr defn))
                           (1+ word-no) (cons nil nil) re-entrant-p word-no)
                        chart (null (cdr words)))))))
         (setq word-no (1+ word-no))
         (setq words (cdr words)))))

      
(defun g-insert-traces (word-no chart finishing)
   (dolist (rule g-tracerules)
      (multiple-value-bind (cat re-entrant-p)
         (g-copy-category (caar rule) nil nil nil nil)
         (g-process-inactive
            (make-g-chart-edge nil
               (cons (cons cat (cdar rule)) (cdr rule))
               word-no (cons nil nil) re-entrant-p word-no)
            chart finishing))))


;;; Unpack packed nodes in a single parse tree, returning a list of
;;; nvt - tree pairs. Work top-down, recursively unpacking any packed trees
;;; at this node (doing this with the packed tree variable table augmented by
;;; the mappings of variables from the packed tree top category back into the
;;; packed-into node) and returning the results together with the result of
;;; unpacking the main tree with its daughters recursively unpacked.
;;;
;;; When combining packed tree variable bindings with those from rest
;;; of parse, add those created after packing took place to packed tree
;;; bindings.

(defun g-unpack (tree vt)
   (g-perform-stack-check)
   (let
      ((tail (cdar tree)) (unpacked nil) mother)
      (loop
         (when (or (atom tail) (atom (car tail)))
            (setq mother (cons (caar tree) tail)) (return))
         (let ((packed (cdar tail)))
            (multiple-value-bind (success new-rvt new-nvt)
               (g-unify
                  (caar tree) (caar (cddr packed)) vt (cadr packed))
               (declare (ignore new-rvt))
               (when success
                  (dolist (u (g-unpack (cddr packed) new-nvt))
                     (push
                        ;; (car packed) is the nvt of packed-into node at
                        ;; the time of packing
                        (cons
                           (g-combine-vts (g-remove-segment (car packed) vt) (car u))
                           (cdr u))
                        unpacked)))))
         (setq tail (cdr tail)))
      (cond
         ((and (cdr tree) (atom (cadr tree)))
            ;; a word node
            (cons (cons vt (cons mother (cdr tree))) unpacked))
         (t
            (let ((daughters-alts (list (list vt))))
               (dolist (d (cdr tree))
                  (setq daughters-alts
                     (g-unpack-daughter d daughters-alts)))
               (append unpacked
                  (if (cdr daughters-alts)
                     (mapcar
                        #'(lambda (alt)
                           (list* (car alt) mother (reverse (cdr alt))))
                        daughters-alts)
                     (list (cons vt tree)))))))))


(defun g-unpack-daughter (d daughters-alts)
   (mapcan
      #'(lambda (alt)
         (mapcar
            #'(lambda (u)
               (list* (car u) (cdr u) (cdr alt)))
            (g-unpack d (car alt))))
      daughters-alts))


;;; Destructively remove all node packing from a single tree

(defun g-rempack (tree)
   (when (and (consp (cdar tree)) (consp (cadar tree)))
      (do
         ((tail (cdar tree) (cdr tail)))
         ((atom (car tail))
            (setf (cdar tree) tail))))
   (cond
      ((and (cdr tree) (atom (cadr tree)))
         ;; a word node
         )
      (t
         (dolist (p (cdr tree)) (g-rempack p)))))


;;; If last word and spans chart, then this a valid parse, so
;;; save it. Next see if it can extend any active edges which
;;; finish where this starts, and then if it can start any new
;;; actives.

(defun g-process-inactive (edge chart finishing)
   (g-perform-stack-check)
   (when g-packing
      (dolist
         (inactedge
            (aref (cdr chart) (g-chart-edge-start edge)
               (g-category-index (caar (g-chart-edge-found edge)))))
         (when
            (and
               (g-fixnum-eql (g-chart-edge-end inactedge) (g-chart-edge-end edge))
               (g-subsume (caar (g-chart-edge-found inactedge))
                  (caar (g-chart-edge-found edge))
                  (g-chart-edge-nvt inactedge)))
            (setf (cdar (g-chart-edge-found inactedge))
               (cons
                  (list* '*packed (g-chart-edge-nvt inactedge)
                     (g-chart-edge-nvt edge) (g-chart-edge-found edge))
                  (cdar (g-chart-edge-found inactedge))))
            (return-from g-process-inactive)))
      (unless (g-chart-edge-res edge)
         ;; only available for being packed into if not re-entrant
         (push edge
            (aref (cdr chart) (g-chart-edge-start edge)
               (g-category-index (caar (g-chart-edge-found edge)))))))
   (setq *chart-edges (g-tconc *chart-edges edge))
   (let
      ((end (g-chart-edge-end edge))
         (defn (g-chart-edge-found edge))
         (nvt (g-chart-edge-nvt edge))
         (start (g-chart-edge-start edge)))
      (cond
         ((and finishing (eql start 0))
            (setq g-result
               (cons (cons nvt defn) g-result))))
      (dolist
         (actedge (aref (car chart) start (g-category-index (caar defn))))
         (multiple-value-bind (succeed new-rvt new-nvt)
            (g-can-extend-edge
               (caar (g-chart-edge-needed actedge))
               (caar defn) (g-chart-edge-vts actedge)
               nvt)
            (when succeed
               (g-extend-active actedge defn (cons new-rvt new-nvt)
                  end chart finishing))))
      (dolist
         (table-and-defns (g-retrieve-rules (caar defn) nil nvt g-gramtree))
         (dolist (rule (cdr table-and-defns))
            (let ((discharged-edge
                     (g-discharge-traces (cdr rule) nil start
                        (car table-and-defns) (car rule) start)))
               (g-extend-active discharged-edge defn
                  (g-chart-edge-vts discharged-edge)
                  end chart finishing))))))


(defun g-can-extend-edge (cat-needed defn-cat vts nvt)
   ;; Following is equivalent to (g-unify cat-needed defn-cat (car vts)
   ;; (append (cdr vts) nvt)) but generates less garbage when nvt is empty,
   ;; or when (cdr vts) and nvt are not empty, but unification fails. In
   ;; latter case have to ensure that (cdr vts) and nvt do not share tails
   ;; otherwise end up with circularity.
   (cond
      ((null nvt)
         (g-unify
            cat-needed defn-cat (car vts) (cdr vts)))
      ((null (cdr vts))
         (g-unify
            cat-needed defn-cat (car vts) nvt))
      (t
         (let ((nvt-tail (last (cdr vts))))
            (if t ;(eq nvt-tail (last nvt))
               (g-unify cat-needed defn-cat (car vts)
                  (append (cdr vts) nvt))
               (progn
                  (setf (cdr nvt-tail) nvt)
                  (multiple-value-bind (succeed new-rvt new-nvt)
                     (g-unify
                        cat-needed defn-cat (car vts) (cdr vts))
                     (setf (cdr nvt-tail) nil)
                     (when succeed
                        (values t new-rvt
                           (g-recombine-nvt new-nvt (cdr vts) nvt))))))))))


(defun g-recombine-nvt (whole tail new)
   (if (eq whole tail)
      (append whole new)
      (do
         ((lst whole (cdr lst)))
         ((eq (cdr lst) tail)
            (setf (cdr lst) (append tail new))
            whole))))


(defun g-incorporate-active (edge chart)
   (setq *chart-edges
      (g-tconc *chart-edges edge))
   (push edge
      (aref (car chart) (g-chart-edge-end edge)
         (g-category-index (caar (g-chart-edge-needed edge))))))


(defun g-extend-active (edge defn vts end chart finishing)
   (g-finish-edge
      (g-discharge-traces
         (cdr (g-chart-edge-needed edge))
         (cons defn (g-chart-edge-found edge)) end
         vts (g-chart-edge-res edge)
         (g-chart-edge-start edge))
      chart finishing)
   (cond
      ((and (g-kleene-one-node-p (car (g-chart-edge-needed edge)))
            (not finishing))
         (g-incorporate-active
            (make-g-chart-edge
               (g-chart-edge-needed edge)
               (cons defn (g-chart-edge-found edge)) end vts
               (g-chart-edge-res edge)
               (g-chart-edge-start edge))
            chart))))


;;; Test an edge to see if it is active or inactive. If active
;;; (and not at end of sentence with no possibility of extending
;;; it with a trace rule) add to chart. If inactive, rename or
;;; remove variables in found category.

(defun g-finish-edge (edge chart finishing)
   (cond
      ((g-chart-edge-needed edge)
         (unless (and finishing (null g-tracerules))
            (g-incorporate-active edge chart)))
      (t
         (multiple-value-bind (cat re-entrant-p)
            (g-copy-category (car (g-chart-edge-res edge))
               (g-chart-edge-rvt edge) nil nil nil)
            (setf (g-chart-edge-found edge)
               (cons
                  (cons cat (cdr (g-chart-edge-res edge)))
                  (g-chart-edge-found edge)))
            (setf (g-chart-edge-res edge) re-entrant-p)
            (g-process-inactive edge chart finishing)))))


;;; Return an edge with a non-trace first needed category by popping
;;; and copying any leading trace categories and pushing them onto
;;; the found list.

(defun g-discharge-traces (needed found end vts res start)
   (loop
      (cond
         ((and needed (g-trace-node-p (car needed)))
            (setq found
               (cons
                  (cons
                     (cons
                        (g-copy-category (caar needed) (car vts) nil nil nil)
                        (null-index-name))
                     (cons 'e (semantics-for-null)))
                  found))
            (setq needed (cdr needed)))
         (t
            (return
               (make-g-chart-edge needed found end vts res start))))))


;;; End of file
