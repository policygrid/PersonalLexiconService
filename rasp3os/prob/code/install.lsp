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

;;; Reads in a backbone grammar from a file with following format:
;;;
;;;   ; comments
;;;   Terminals t+ .
;;;   Tcats {t cat}+ .
;;;   Non-terminals nt+ .
;;;   NTcats {t cat}+ .
;;;   Let name nt --> t/nt+ .
;;;   End
;;;
;;; Backbone grammar once read in resides in special variables
;;; *productions*, *terminals*, *terminal-categories*,
;;; *terminal-alist*, *non-terminals*, *null-categories*

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Global

(defvar *productions* nil)
(defvar *terminals* nil)
(defvar *terminal-categories* nil)
(defvar *terminal-alist*)
(defvar *non-terminals* nil)
(defvar *null-categories* nil)

(defvar *state-actions*)
(defvar *shift-action-vector*)
(defvar *sentence-end-marker*)


;;; Bound here

(defvar *definition-line*)
(defvar *definition-line-index*)
(defvar *definition-stream*)
(defvar *non-terminal-alist*)


;;; From GDE

(defvar *id-rules)
(defvar *current-category-index)
(defvar *category-index-dnet)


;;; Install a backbone grammar from file for LR parsing. File contents
;;; as described above. 

(defun install-grammar-from-file (file)
   (with-open-file (*definition-stream* file :direction :input)
      (setq *productions* nil)
      (setq *terminals* nil)
      (setq *terminal-categories* nil)
      (setq *terminal-alist* nil)
      (setq *non-terminals* nil)
      (setq *non-terminal-alist* nil) 
      (setq *null-categories* nil)
      (let
         ((*definition-line* "")
            (*definition-line-index* 0)
            item)
         (loop
            (setq item (read-single-definition))
            (when (eq item '|End|)
               (unless (and *terminals* *non-terminals* *productions*)
                  (error "Backbone grammar is incomplete"))
               (when (intersection *terminals* *non-terminals* :test #'eq)
                  (error "Terminal and non-terminal symbols not disjoint"))
               (setq *productions*
                  (make-array (length *productions*)
                     :initial-contents (reverse *productions*)))
               (return nil))))))


(defun read-single-definition ()
   (cond
      ((check-for-item "Let" nil)
         (push (read-production) *productions*))
      ((check-for-item "Terminals" nil)
         (setq *terminals* (read-category-symbols))
         (pushnew *sentence-end-marker* *terminals*))
      ((check-for-item "Tcats" nil)
         (setq *terminal-alist* (read-terminals-definition)))
      ((check-for-item "Non-terminals" nil)
         (setq *non-terminals* (read-category-symbols)))
      ((check-for-item "NTcats" nil)
         (setq *non-terminal-alist* (read-non-terminals-definition)))
      ((check-for-item "End" nil) '|End|)
      (t 
         (error "unexpected item in grammar file"))))


;;; Read a backbone grammar production.

(defun read-production nil
   (let*
      ((name-str (get-next-item))
         (name (intern name-str))
         (mother-str (get-next-item))
         (mother (intern mother-str))
         (daughters nil) item)
      (check-for-item "-->" t)
      (loop
         (setq item (get-next-item))
         (when (equal item ".") (return))
         (push (intern item) daughters))
      (setq daughters (nreverse daughters))
      (cond
         ((not (gde-loaded-p)))
         ((eql (char mother-str (1- (length mother-str))) #\+)
            ;; the kleene category (mother) is the more tightly constrained:
            ;; having same daughters also guarantees correct propagation 
            (setf (get name 'lr1-psrule)
               (cons
                  (cons (atomic-complex-category mother) '|elide|)
                  (mapcar
                     #'(lambda (d) (declare (ignore d))
                        (list (atomic-complex-category mother)))
                     (reverse daughters)))))
         ((eql (char name-str (1- (length name-str))) #\@)
            (setf (get name 'lr1-psrule)
               (list
                  (cons (atomic-complex-category mother) '|elide|)
                  (list (atomic-complex-category (car (last daughters))))))))
      (make-cfrule
         :name name :mother mother :daughters daughters)))


(defun atomic-complex-category (at)
   (car
      (or (rassoc at *terminal-alist* :test #'eq)
         (rassoc at *non-terminal-alist* :test #'eq))))

      
;;; Read a set of atomic category names terminated by period. Return them
;;; in a list

(defun read-category-symbols nil
   (let ((categories nil) item)
      (loop
         (setq item (get-next-item))
         (when (equal item ".")
            (return (nreverse categories)))
         (push (intern item) categories))))


;;; Read in definitions of terminal and non-terminal categories

(defun read-terminals-definition ()
   (let ((cats-and-names (read-atomic-complex-categories)))
      (when (gde-loaded-p)
         (setq *terminal-categories*
            (g-makeruletree
               (mapcar
                  #'(lambda (cat-and-name)
                     (cons (car cat-and-name)
                        (or
                           (position (cdr cat-and-name) *terminals* :test #'eq)
                           (error "Terminal name ~S not found"
                              (cdr cat-and-name)))))
                  cats-and-names)
               (make-array (1+ *current-category-index) :initial-element nil)))
         cats-and-names)))


(defun read-non-terminals-definition ()
   (let ((cats-and-names (read-atomic-complex-categories)))
      (when (gde-loaded-p)
         (dolist (cat-and-name cats-and-names)
            (when (eql (search "NULL-" (string (cdr cat-and-name))) 0)
               (setq *null-categories*
                  (list* (cdr cat-and-name)
                     (make-indexed-null-category (car cat-and-name))
                     *null-categories*)))))
      cats-and-names))


(defun make-indexed-null-category (cat)
   ;; If category is not re-entrant, turn all variables into the unnamed
   ;; variable so that later g-remvars on this category will be faster
   (if (g-reentrant-p cat nil)
      cat
      (dotimes (offset (length cat) cat)
         (cond
            ((eql offset 0))
            ((lr-categoryp (svref cat offset))
               (setf (svref cat offset)
                  (make-indexed-null-category (svref cat offset))))
            ((lr-varp (svref cat offset))
               (setf (svref cat offset) (unnamed-variable)))))))


;;; Read a set of atomic category - unification category pairs terminated
;;; by period. Return as an alist keyed on unif category.

(defun read-atomic-complex-categories nil
   (let ((alist nil) item)
      (loop
         (setq item (get-next-item))
         (when (equal item ".")
            (return alist))
         (check-for-item "[" t)
         (push
            (cons
               (if (gde-loaded-p)
                  (index-complex-category (read-complex-category))
                  (read-complex-category))
               (intern item))
            alist))))


(defun read-complex-category nil
   (let ((category nil))
      (loop
         (when (check-for-item "]" nil) (return (nreverse category)))
         (let*
            ((feature (intern (get-next-item)))
               (value (get-next-item)))
            (when (equal value "[") (setq value (read-complex-category)))
            (when (gde-loaded-p)
               (push
                  (cond
                     ((consp value)
                        (lr-make-fv-pair feature value))
                     ((and (> (length value) 1) (eql (char value 0) #\@))
                        (or
                           (car (get feature 'variable-list))
                           (let ((fvpair
                                    (lr-make-fv-pair feature
                                       (generate-named-variable
                                          (intern (subseq value 1))))))
                              (push fvpair (get feature 'variable-list))
                              fvpair)))
                     (t
                        (or
                           (find (intern value) (get feature 'proper-value-list)
                              :key #'(lambda (fvpair) (lr-fv-pair-value fvpair))
                              :test #'eq)
                           (let ((fvpair (lr-make-fv-pair feature (intern value))))
                              (push fvpair (get feature 'proper-value-list))
                              fvpair))))
                  category))
            (check-for-item "," nil)))))


(defun index-complex-category (category)
   (index-bundle-ensure-initialised)
   (let ((len-and-index
            (find-index-for-bundle
               category *category-index-dnet (cddr *category-index-dnet)
               category t)))
      (cond
         ((null len-and-index)
            (error "inconsistency in function ~S" 'index-bundle-for-parser1))
         (t
            (let ((n 0)
                  (res (make-array (1+ (car len-and-index)))))
               (setf (svref res 0) (cdr len-and-index))
               (dolist (fvpr category)
                  (setf (svref res (incf n))
                     (if (consp (lr-fv-pair-value fvpr))
                        (index-complex-category (lr-fv-pair-value fvpr))
                        (lr-fv-pair-value fvpr))))
               res)))))


;;; Low-level input stream reading and tokenisation

(defun get-next-item ()
   (fill-definition-buffer)
   (let
      ((pos
            (do*
               ((n *definition-line-index* (1+ n))
                  (limit (length *definition-line*)))
               ((>= n limit) nil)
               (case (char *definition-line* n)
                  ((#\space #\tab) (return n))
                  ((#\; #\[ #\] #\.) (return n))
                  (#\,
                     (if
                        (and (< n limit)
                           (member (char *definition-line* (1+ n))
                              '(#\space #\tab)))
                        (return n)))
                  (t nil)))))
      (cond
         (pos
            (when (eql pos *definition-line-index*) (incf pos))
            (prog1 (subseq *definition-line* *definition-line-index* pos)
               (setq *definition-line-index* pos)
               (skip-leading-whitespace)))
         (t
            (prog1 (subseq *definition-line* *definition-line-index*)
               (setq *definition-line* ""))))))


(defun check-for-item (str errorp)
   (fill-definition-buffer)
   (let ((end-pos (+ *definition-line-index* (length str))))
      (cond
         ((and (>= (length *definition-line*) end-pos)
             (string= *definition-line* str
                :start1 *definition-line-index*
                :end1 end-pos))
            (setq *definition-line-index* end-pos)
            (skip-leading-whitespace)
            t)
         (errorp
            (error "~A expected but not found near ~A"
               str (subseq *definition-line* *definition-line-index*)))
         (t nil))))


(defun fill-definition-buffer ()
   (loop
      (when (< *definition-line-index* (length *definition-line*))
         (return nil))
      (setq *definition-line* (read-line *definition-stream* nil nil))
      (if (null *definition-line*)
         (error "unexpected end of file in grammar file")
         (let ((comment (position #\; *definition-line*)))
            (when comment
               (setq *definition-line*
                  (subseq *definition-line* 0 comment)))
            (setq *definition-line-index* 0)
            ;; Fix for CMU-CL bug returning two lines from read-line if
            ;; the first is empty
            #+CMU
            (setq *definition-line*
               (string-left-trim '(#\newline) *definition-line*))
            ;; Fix for Allegro PC bug returning linefeed character if line
            ;; is empty
            #+ACLPC
            (setq *definition-line*
               (string-left-trim '(#\linefeed) *definition-line*))
            (skip-leading-whitespace)))))


(defun skip-leading-whitespace ()
   (setq *definition-line-index*
      (do
         ((n *definition-line-index* (1+ n))
            (len (length *definition-line*)))
         ((>= n len) n)
         (unless
            (member (char *definition-line* n) '(#\space #\tab))
            (return n)))))


;;; Print internal representation of an LALR machine to file.

(defun dump-machine-to-file (file)
   (with-open-file
      (str file :direction :output :if-exists :supersede
         :if-does-not-exist :create)
      (let ((#-procyon *print-circle* #+procyon *print-shared* t)
            (*print-array* t) (*print-escape* t)
            (*print-length* nil) (*print-level* nil)
            (*print-pretty* nil))
         (print *shift-action-vector* str)
         (print *state-actions* str))))


;;; Install an LALR machine from file (originally put there by function
;;; dump-machine-to-file). Convert shift bignums into bit vectors.

(defun install-machine-from-file (file)
   (with-open-file (str file :direction :input)
      (setq *shift-action-vector* (read str))
      (peek-char #\( str)
      (let ((elements (read-circular str)))
         (setq *state-actions*
            (make-array (length elements) :initial-contents elements))
         (install-machine-shift-bit-vectors))))


(defun install-machine-shift-bit-vectors nil
   (dotimes (a (length *state-actions*))
      (dolist (action (cddr (svref *state-actions* a)))
         (when (and (integerp (car action)) (null (cdr action)))
            (let*
               ((int (car action))
                  (nbits (integer-length int))
                  (vec
                     (make-array nbits :element-type 'bit :initial-element 0)))
               (dotimes (n nbits)
                  (when (logbitp n int) (setf (sbit vec n) 1)))
               (setf (car action) vec))))))


;;; Compile unification grammar currently resident into form directly useable
;;; by LR parsers.

(defun install-unification-grammar (remove-cached)
   (unless (gde-loaded-p)
      (error "GDE is not present in system"))
   (unless *id-rules
      (error "No grammar has been loaded"))
   (let ((ds nil))
      (mapc
         #'(lambda (rule-name)
            (mapc
               #'(lambda (compiled-id-rule)
                  (let ((ps-rule
                           (convert-idrule-to-parser compiled-id-rule)))
                     (setf (get (intern (cadar ps-rule)) 'lr1-psrule)
                        (cons (car ps-rule)
                           ;; daughters to have null cdr and be in reverse order
                           ;; also share all but first daughters when possible
                           (do* ((dtail (cddr ps-rule) (cdr dtail))
                                 (res (if (cdr ps-rule) (list (cons (caadr ps-rule) nil)))))
                              ((null dtail) res)
                              (let ((d (assoc (caar dtail) ds :test #'equalp))) 
                                 (unless d
                                    (push (setq d (cons (caar dtail) nil)) ds))
                                 (push d res)))))))
               (compile-idrule rule-name))
            (when remove-cached
               (remprop rule-name 'compiled-idrules)
               (remprop rule-name 'expanded-idrules)))
         *id-rules))
   (when remove-cached (clear-cached-grammar nil)))
            

;;; End of file
