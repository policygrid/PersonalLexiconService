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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - DISPLAY PARSETREE
;;;
;;; Author: John Carroll
;;;
;;; Code for displaying a parse tree in graphical form - only
;;; assumes a dumb terminal - does not try any fancy cursor
;;; addressing etc. Assumes each node is an atom with a property
;;; 'daughters' (a list of node names); category-chars-function
;;; is a function which, when given a node name as argument
;;; returns either NIL (meaning don't print), or two lists of
;;; characters representing the category on that node.
;;; Label-chars-function should return a list of the characters
;;; to be printed as the node label. If optional-file is
;;; non-NIL, output is sent to the files <optional-file>1,
;;; <optional-file>2 etc, each file having a linelength equal to
;;; the integer returned by Gde-linelength when given the
;;; argument <optional-file>. Adapted from code originally
;;; written by Arthur Cater.
;;;
;;; Entry points:
;;;
;;;  * (defun Display-parse-tree (top-node category-chars-function
;;; label-chars-function optional-file) ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Contents of global variables:
;;;
;;; *nodes-by-levels
;;;
;;; ((1 . <nodes-at-top-level>) (2 . <nodes-at-2nd-level>) ...)
;;;
;;; *high-water-marks
;;;
;;; ((<level> . <pos-of-next-right-sibling> ...)
;;;
;;; Generates and uses (but does not remove) the properties
;;; d-level, x-posn, width, category-chars, label-chars on each
;;; node name.

(progn
   (defvar *nodes-by-levels nil)
   (defvar *high-water-marks nil)
   (defvar *levels-touched nil)
   (defvar *line-buffers nil)
   (defvar *lines-printed nil)
   (defvar *output-file nil)
   (defvar *page-width nil))


(defstruct (print-tree-buffer)
   index
   file
   handle
   buffer)


;;; --- Entry point ---

(defun display-parse-tree
   (top-node category-chars-function
      label-chars-function optional-file)
   (let
      ((*nodes-by-levels nil)
         (*high-water-marks nil))
      (give-levels top-node 1)
      (find-width top-node
         category-chars-function
         label-chars-function)
      (allocate-x-position top-node 1)
      (plot-out-nodes optional-file)))


(defun give-levels (node display-level)
   (setf (get node 'd-level) display-level)
   (cond
      ((assoc display-level *nodes-by-levels)
         (nconc
            (assoc display-level *nodes-by-levels)
            (list node)))
      (t
         (setf *nodes-by-levels
            (cons (list display-level node)
               *nodes-by-levels))))
   (dolist (child (get node 'daughters))
      (give-levels child (1+ display-level))))


;;; Calculate the width for each parse tree node. Cache the
;;; characters in the category and label associated with each
;;; node - otherwise would have to recompute when printing out
;;; and this may be expensive in the case of the category.

(defun find-width
   (node category-chars-function
      label-chars-function)
   (setf (get node 'width)
      (+ 2
         (max 4
            (let
               ((category-chars
                     (apply category-chars-function
                        (ncons node))))
               (setf (get node 'category-chars)
                  category-chars)
               (cond
                  (category-chars
                     (list-length (car category-chars)))
                  (t 0)))
            (let
               ((label-chars
                     (apply label-chars-function
                        (ncons node))))
               (setf (get node 'label-chars) label-chars)
               (list-length label-chars)))))
   (dolist (daughter (get node 'daughters))
      (find-width daughter
         category-chars-function
         label-chars-function)))


(defun allocate-x-position
   (node display-level)
   (let
      ((minimum (minimum-at display-level))
         (mid-point nil))
      (cond
         ((consp (get node 'daughters))
            (dolist (item (get node 'daughters))
               (allocate-x-position item (1+ display-level)))
            (setf mid-point
               (get-midpoint (get node 'daughters)))
            (cond
               ((>= mid-point minimum)
                  (setf (get node 'x-posn) mid-point)
                  (update-minimum display-level
                     (+ (get node 'width)
                        (- mid-point minimum))))
               (t
                  (let
                     ((shift (- minimum mid-point))
                        (*levels-touched nil))
                     (dolist (item (get node 'daughters))
                        (shift-over item shift))
                     (dolist (level *levels-touched)
                        (update-minimum level shift))
                     (setf (get node 'x-posn) minimum)
                     (update-minimum display-level
                        (get node 'width))))))
         (t
            (setf (get node 'x-posn) minimum)
            (update-minimum display-level
               (get node 'width))))))


(defun minimum-at (level)
   (let
      ((pr (assoc level *high-water-marks)))
      (cond
         (pr (cdr pr))
         (t (push (cons level 1) *high-water-marks)
            1))))


(defun update-minimum (level width)
   (let
      ((pr (assoc level *high-water-marks)))
      (setf (cdr pr)
         (+ width (cdr pr)))))


(defun get-midpoint (node-cluster)
   (truncate
      (+ (get (car node-cluster) 'x-posn)
         (get (car (last node-cluster)) 'x-posn))
      2))


(defun shift-over (node shift)
   (let
      ((level (get node 'd-level)))
      (when (null (member level *levels-touched))
         (push level *levels-touched))
      (setf (get node 'x-posn)
         (+ shift (get node 'x-posn)))
      (dolist (item (get node 'daughters))
         (shift-over item shift))))


;;; Print out the tree. Do not print out node categories on a
;;; line unless they are all non-NIL.

(defun plot-out-nodes (*output-file)
   (let
      ((*line-buffers nil) (*lines-printed 0)
         (*page-width
            (gde-linelength *output-file)))
      (setf *nodes-by-levels
         (sort (copy-list *nodes-by-levels)
            #'(lambda (a b)
                 (< (car a) (car b)))))
      (print-nodes (car *nodes-by-levels))
      (dolist (level (cdr *nodes-by-levels))
         (progn
            (print-arrows level)
            (print-nodes level)))
      (mapcar
         #'(lambda (buffer)
              (progn
                 (when (print-tree-buffer-handle buffer)
                    (close (print-tree-buffer-handle buffer)))
                 (print-tree-buffer-file buffer)))
         *line-buffers)))


(defun print-nodes (node-cluster)
   (let
      ((prs
          (pairlis
             (mapcar
                #'(lambda (node)
                     (cons (get node 'x-posn)
                        (get node 'width)))
                (cdr node-cluster))
             (cdr node-cluster))))
      (dolist (pr prs)
         (print-node-line (caar pr) (cdar pr)
            (get (cdr pr) 'label-chars)))
      (flush-buffer)
      (when
         (dolist (pr prs t)
            (unless (get (cdr pr) 'category-chars)
               (return nil)))
         (dolist (pr prs)
            (print-node-line (caar pr) (cdar pr)
               (car
                  (get (cdr pr) 'category-chars))))
         (flush-buffer)
         (when
            (member-if
               #'(lambda (pr)
                    (cdr
                       (get (cdr pr) 'category-chars)))
               prs)
            (dolist (pr prs)
               (print-node-line (caar pr) (cdar pr)
                  (cdr
                     (get (cdr pr) 'category-chars))))
            (flush-buffer)))))


(defun print-node-line
   (x-posn width output-chars)
   (let
      ((spare-space
          (- width (list-length output-chars))))
      (let
         ((current-tab
               (+ x-posn
                  (truncate
                     (cond
                        ((zerop (rem spare-space 2))
                           spare-space)
                        (t
                           (1+ spare-space)))
                     2))))
         (dolist (char output-chars)
            (progn
               (print-buffer-putv char current-tab)
               (setf current-tab (1+ current-tab)))))))


(defun print-arrows (node-cluster)
   (let
      ((positions
          (mapcar
             #'(lambda (node)
                  (+ (get node 'x-posn)
                     (truncate (get node 'width) 2)))
             (cdr node-cluster)))
         (parent-position
            (mapcar
               #'(lambda (node)
                    (+ (get (get node 'mother) 'x-posn)
                       (truncate
                          (get (get node 'mother) 'width)
                          2)))
               (cdr node-cluster))))
      (dolist
         (mapping '((1 . 2) (2 . 1) (3 . 0)))
         (let
            ((sonw (car mapping))
               (parw (cdr mapping)))
            (dolist
               (x (pairlis parent-position positions))
               (print-buffer-putv #\.
                  (truncate
                     (+ (* (car x) parw) (* (cdr x) sonw))
                     3)))
            (flush-buffer)))))


;;; Routines for managing the output line buffers. Flush buffer
;;; prints them out when required, throwing away all characters
;;; which are past the current linelength if output is not to
;;; file, so that overwide trees do not mess the vdu screen up
;;; by overflowing onto the next line.

(defun print-buffer-putv (char current-tab)
   (let
      ((buffer-entry
          (f-find
             (the fixnum (truncate current-tab *page-width))
             *line-buffers :key
             #'print-tree-buffer-index :test #'eql)))
      (unless buffer-entry
         (let
            ((file
                (new-plot-file *output-file
                   (1+ (length *line-buffers)))))
            (setf buffer-entry
               (make-print-tree-buffer :index
                  (truncate current-tab *page-width)
                  :file file :handle
                  (open-print-tree-file file) :buffer
                  (make-string *page-width :initial-element
                     #\space)))
            (setf *line-buffers
               (nconc *line-buffers
                  (ncons buffer-entry)))))
      (setf
         (char (print-tree-buffer-buffer buffer-entry)
            (rem current-tab *page-width))
         char)))


;;; Take a pathname and concatenate an index onto it to create
;;; the name of a new file. Cannot do this if Pathname-name
;;; doesn't return a string or a symbol - in which case use the
;;; original name if the index is 1, or ask the user for a new
;;; name.

(defun new-plot-file (optional-file index)
   (cond
      (optional-file
         (let
            ((name (pathname-name optional-file)))
            (cond
               ((or (stringp name) (symbolp name))
                  (merge-pathnames
                     (make-pathname :directory
                        (pathname-directory optional-file)
                        :name (concat-string name index) :type
                        (pathname-type optional-file))
                     optional-file))
               ((= index 1) optional-file)
               (t
                  (let
                     ((input
                         (prompt-always "File name for part "
                            index " of tree? ")))
                     (cond
                        (input
                           (merge-pathnames
                              (canonise-grammar-file-name
                                 input)
                              optional-file))
                        (t
                           (gde-ferror
"parse tree printing terminated"))))))))))


(defun open-print-tree-file (file)
   (when file
      (let
         ((output-stream
             (open file :direction :output :if-exists
                :supersede :if-does-not-exist :create)))
         (format output-stream "~V%" *lines-printed)
         output-stream)))


;;; If no output file then send to standard output stream.

(defun flush-buffer nil
   (setf *lines-printed (1+ *lines-printed))
   (cond
      (*output-file
         (dolist (buffer-entry *line-buffers)
            (flush-buffer1
               (print-tree-buffer-buffer buffer-entry)
               (print-tree-buffer-handle buffer-entry))))
      (t
         (flush-buffer1
            (print-tree-buffer-buffer
               (car *line-buffers))
            *standard-output*))))


(defun flush-buffer1 (buffer-entry stream)
   (let
      ((end
          (last-buffer-non-blank buffer-entry (1- *page-width))))
      (write-line buffer-entry stream :end (1+ end))
      (loop
         (cond
            ((< end 0) (return nil)))
         (setf (schar buffer-entry end) #\Space)
         (setf end (1- end)))))


(defun last-buffer-non-blank (string n)
   (cond
      ((< n 0)
         n)
      ((char= (schar string n) #\Space)
         (last-buffer-non-blank string (1- n)))
      (t n)))


;;; End of file


