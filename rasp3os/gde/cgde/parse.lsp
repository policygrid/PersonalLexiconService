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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - PARSER INTERFACE
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code to interface the GDE with the
;;; parser. Compilation of ID rules into form required by parser
;;; is done by code in another file. After a parse, the
;;; resulting trees are in *current-parse-trees - the user may
;;; write further processing functions working on the value of
;;; this variable.
;;;
;;; Entry points:
;;;
;;;  * (defun Parse-sentences () ...
;;;  * (defun Convert-category-to-parser (category ...
;;;
;;; If word structure option is on, variables bound by the
;;; parser do not get carried down into the internal word
;;; structure.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(progn
   (defvar g-gramtree)
   (defvar *state-actions*))


(progn
   (defvar *parser-commands nil)
   (defvar *parser-view-commands nil)
   (defvar *parser-write-commands nil)
   (defvar *parser-fparse-commands nil)
   (defvar *previous-sentence nil))


(setf *parser-fparse-commands
   (list
      (make-command-entry :shortest 1 :name
         'numbers :action '(parse-file 'nil))
      (make-command-entry :shortest 1 :name
         'bracketings :action
         '(parse-file 'bracketings))
      (make-command-entry :shortest 2 :name
         'lrules :action
         '(parse-file 'lrules))
      (make-command-entry :shortest 2 :name
         'lcategories :action
         '(parse-file 'lcategories))
      (make-command-entry :shortest 1 :name
         'rules :action
         '(parse-file 'rules))
      (make-command-entry :shortest 2 :name
         'full :action
         '(parse-file 'full))
      (make-command-entry :shortest 1 :name
         'semantics :action
         '(parse-file 'semantics))
      (make-command-entry :shortest 1 :name
         'interpret :action
         '(parse-file 'interpret))))


(setf *parser-view-commands
   (list
      (make-command-entry :shortest 1 :name
         'bracketings :action
         '(display-parse-trees 'bracketings))
      (make-command-entry :shortest 2 :name
         'lrules :action
         '(display-parse-trees 'lrules))
      (make-command-entry :shortest 2 :name
         'lcategories :action
         '(display-parse-trees 'lcategories))
      (make-command-entry :shortest 1 :name
         'rules :action
         '(display-parse-trees 'rules))
      (make-command-entry :shortest 2 :name 'full
         :action '(display-parse-trees 'full))
      (make-command-entry :shortest 2 :name
         'category :action '(display-tree-node 't))
      (make-command-entry :shortest 1 :name
         'semantics :action
         '(display-parse-trees 'semantics))
      (make-command-entry :shortest 2 :name
         'form :action
         '(display-tree-node 'nil))
      (make-command-entry :shortest 2 :name
         'common :action
         '(display-parse-trees 'common))
      (make-command-entry :shortest 1 :name
         'vertices :action
         '(display-vertices-and-counts))
      (make-command-entry :shortest 1 :name
         'parsed :action
         '(display-parsed-sentences))
      (make-command-entry :shortest 1 :name
         'edges :action '(display-simple-edges 'nil))
      (make-command-entry :shortest 2 :name
         'inactive :action
         '(display-simple-edges 'inactive))
      (make-command-entry :shortest 2 :name
         'active :action
         '(display-simple-edges 'active))
      (make-command-entry :shortest 2 :name 'ai
         :action '(display-complex-edges 'ai))
      (make-command-entry :shortest 2 :name 'ia
         :action '(display-complex-edges 'ia))))


(setf *parser-write-commands
   (list
      (make-command-entry :shortest 1 :name
         'parsed :action '(write-parsed-sentences))
      (make-command-entry :shortest 1 :name
         'bracketings :action '(write-parse-trees 'bracketings))
      (make-command-entry :shortest 2 :name
         'lrules :action '(write-parse-trees 'lrules))
      (make-command-entry :shortest 2 :name
         'lcategories :action '(write-parse-trees 'lcategories))
      (make-command-entry :shortest 1 :name
         'rules :action '(write-parse-trees 'rules))
      (make-command-entry :shortest 2 :name 'full
         :action '(write-parse-trees 'full))
      (make-command-entry :shortest 1 :name
         'semantics :action
         '(write-parse-trees 'semantics))))


(setf *parser-commands
   (list
      (make-command-entry :shortest 1 :name
         'previous :action '(parse-previous))
      (make-command-entry :shortest 1 :name
         'fparse :action
         '(process-command-option
             "Numbers /B/LR/LC/R/FU/S /I? " *parser-fparse-commands))
      (make-command-entry :shortest 3 :name 'help
         :action '(give-parser-help))
      (make-command-entry :shortest 1 :name '?
         :action '(give-parser-help))
      (make-command-entry :shortest 1 :name 'view
         :action
         '(process-command-option
             "B/LR/LC/R/FU/S /CA/FO /CO /P /V/E/AC/IN/AI/IA? "
             *parser-view-commands))
      (make-command-entry :shortest 1 :name
         'write :action
         '(process-command-option "B/LR/LC/R/FU/S /P? "
             *parser-write-commands))
      (make-command-entry :shortest 3 :name
         'interpret :action
         '(invoke-parser-interpretation))
      (make-command-entry :shortest 1 :name '\!
         :action '(process-lisp-input))
      (make-command-entry :shortest 4 :name
         'shell :action '(shell))
      (make-command-entry :shortest 1 :name 'lisp
         :action '(lisp-top-loop))
      (make-command-entry :shortest 1 :name 'gc
         :action '(request-gc))
      (make-command-entry :shortest 0 :name (intern "")
         :action '(parse-words))))


(defun give-parser-help nil
   (give-help *parser-help-file))


;;; The toplevel command handler for the "parse" command. If any
;;; ID rules have to be compiled, then the grammar tree will
;;; have been reset to NIL. Don't worry about invalidating old
;;; parse trees or chart edges - they cannot cause
;;; inconsistencies.

(defun parse-sentences nil
   (cond
      (*features
         (let ((remove-cached (prompt-never)))
            (setf remove-cached
               (and remove-cached
                  (match-command-shortest
                     (car remove-cached) "uncache" 1)))
            (cond
               ((if *lr1-parse *state-actions* g-gramtree)
                  (if remove-cached
                     (clear-cached-grammar nil)))
               (t (setf *current-parse-trees nil)
                  (setf *chart-edges nil)
                  (compile-world-stats1)
                  (if *lr1-parse
                     (make-lr1-parse-states remove-cached)
                     (make-parse-rule-tree remove-cached))
                  (if remove-cached
                     (clear-cached-grammar nil)
                     (or *lr1-parse (compile-world-stats2))))))
         (parse-top-loop))
      (t
         (gde-cerror
            "no grammar has been defined"))))


(defun make-parse-rule-tree (remove-cached)
   (g-indexrules
      (mapcan
         #'(lambda (idrule-name)
            (prog1
               (mapcar
                  #'(lambda (compiled-idrule)
                     (convert-idrule-to-parser
                        (if *term-unification
                           compiled-idrule
                           (fill-unrestricted-idrule compiled-idrule))))
                  (compile-idrule idrule-name))
               (when remove-cached
                  (remprop idrule-name 'compiled-idrules)
                  (remprop idrule-name 'expanded-idrules))))
         *id-rules)))


(defun parse-top-loop nil
   (top-loop #'gde-top-print
      #'parse-top-eval "Parse"
      (concat-string "GDE Parser ("
         (if *lr1-parse "LR1" "chart")
         " parser, top category " (if *top "" "not ") "defined)")))


(defun parse-top-eval (x)
   ;; if tagged words flag is on then sentences are expected already to be
   ;; tokenised. Parser commands also processed here, but tokenisation or
   ;; lack of it should make no difference for them
   (process-command (get-reply1 x *tagged-words) *parser-commands)
   'nothing)


;;; --- Top level parser commands ---

(defun parse-words nil
   (setf *previous-sentence
      (cons *user-command (prompt-never)))
   (setf *parsed-sentences
      (nconc *parsed-sentences
         (ncons
            (cons *history-count *previous-sentence))))
   (setf *unsaved-parsed-sentences t)
   (invoke-parser *previous-sentence
      'bracketings))


(defun parse-previous nil
   (cond
      (*previous-sentence
         (format t "~A~%" *previous-sentence)
         (invoke-parser *previous-sentence
            'bracketings))
      (t (gde-cerror "no previous sentence"))))


;;; Take a file consisting of a set of sentences (in free
;;; format, separated by full-stop, ! or ?) and parse them,
;;; outputting results to terminal or another file. User can
;;; specify either brief (only numbers of parses) or verbose
;;; (stats and bracketings) for output format.

(defun parse-file (verbose)
   (let*
      ((command-options (prompt-never))
         (input (prompt-always "Input file? ")))
      (when input
         (let
            ((parse-sentence-file
                (canonise-grammar-file-name input)))
            (cond
               ((probe-file parse-sentence-file)
                  (let
                     ((output (prompt-always "Output file? ")))
                     (cond
                        (output
                           (let
                              ((output-file
                                  (canonise-grammar-file-name output)))
                              (when (probe-file output-file)
                                 (gde-warn
                                    "appending output to existing file "
                                    output-file))
                              (with-open-stream
                                 (*standard-output*
                                    (open output-file
                                       :direction :output
                                       :if-exists :append
                                       :if-does-not-exist :create))
                                 (set-gde-linelength *standard-output*)
                                 (parse-file1
                                    parse-sentence-file verbose command-options))))
                        (t
                           (parse-file1
                              parse-sentence-file verbose command-options)))))
               (t
                  (gde-cerror
                     "sentence file does not exist")))))))


(defun parse-file1 (input-file verbose command-options)
   (let
      ((*suppress-dict-messages t))
      (with-open-stream
         (*standard-input*
            (open input-file :direction :input))
         (let* ((sent nil) (tail-sent nil))
            (loop
               (setf sent
                  (nconc sent
                     (mapcan
                        #'(lambda (word)
                             (unless (gde-comment-p word)
                                (ncons word)))
                        (get-reply *tagged-words))))
               (setf tail-sent (parse-file-get-tail sent))
               (cond
                  (tail-sent
                     (setf sent (ldiff sent tail-sent))
                     (setf *previous-sentence sent)
                     (parse-file-sentence sent verbose command-options)
                     (setf sent (cdr tail-sent))))
               (cond
                  ((eql
                      (peek-char nil *standard-input* nil
                         *eof-marker)
                      *eof-marker)
                     (return nil))))
            (if sent
               (parse-file-sentence
                  (ldiff sent (parse-file-get-tail sent))
                  verbose command-options))))))


(defun parse-file-sentence (sent verbose command-options)
   ;; flush system output buffer before invoking parser in
   ;; case a parse crashes the system through being too large etc
   (cond
      (verbose
         (format t "~%~%~A " sent))
      (t (format t "~A " sent)))
   (finish-output *standard-output*)
   (let
      ((n-parses (invoke-parser sent nil)))
      (format t "~A~%" n-parses)
      (when (and verbose *current-parse-trees)
         (display-parse-trees1 *current-parse-trees
            verbose command-options nil))
      n-parses))


(defun parse-file-get-tail (x)
   (let
      ((tails
          (list (member '\. x) (member '? x)
             (member '\! x))))
      (when tails
         (car
            (sort (copy-list tails)
               #'(lambda (t1 t2)
                    (> (list-length t1) (list-length t2))))))))


;;; Run the parser on a list of words. The result is a list (in
;;; reverse order to order found) or parses, each parse being a
;;; pair of variable bindings and skeleton tree.

(defun invoke-parser (words verbose)
   (setq words (remove-leading-star words))
   (setf *current-parse-trees nil)
   (setq *current-parse-weights nil)
   (setf *chart-edges nil)
   (let*
      ((res nil) (weights nil)
         n-unif n-fail 
         (gc-time (gctime))
         (elapsed-time
            (truncate (* (get-internal-real-time) 1000)
               internal-time-units-per-second))
         (cpu-time
            (truncate (* (get-internal-run-time) 1000)
               internal-time-units-per-second))
         (heap (conscount)))
      (multiple-value-setq (res n-unif n-fail weights)
         (invoke-parser1 words))
      (setq heap (- (conscount) heap))
      (setq cpu-time
         (- (truncate (* (get-internal-run-time) 1000)
               internal-time-units-per-second)
            cpu-time))
      (setq elapsed-time
         (- (truncate (* (get-internal-real-time) 1000)
               internal-time-units-per-second)
            elapsed-time))
      (setq gc-time (- (gctime) gc-time))
      (when verbose
         (format t "~%~A msec CPU"
            #+(or PROCYON :CORAL) (- cpu-time gc-time)
            #-(or PROCYON :CORAL) cpu-time)
         (when (> gc-time 0)
            (format t " (+ ~A msec GC)" gc-time))
         (unless (= cpu-time elapsed-time)
            (format t ", ~A msec elapsed" elapsed-time))
         (when (> heap 0)
            (format t ", ~A heap memory" heap))
         (if *lr1-parse
            (format t "~%~A unifications, ~A failures~%" n-unif n-fail)
            (format t "~%~A edges generated~%"
               (if *chart-edges (length (car *chart-edges)) 0))))
      (setf res
         (if *top
            (mapcan
               #'(lambda (r)
                  (when (top-category-p (cadr r) (car r))
                     (list r)))
               res)
            res))
      (when verbose
         (let ((n-parses (list-length res)))
            (cond
               ((eql n-parses 0)
                  (format t "No parses~%~%"))
               ((eql n-parses 1) (format t "1 parse~%~%"))
               (t
                  (format t "~A parses~%~%" n-parses)))))
      (setf *current-parse-trees res)
      (setf *current-parse-weights weights)
      (when (and verbose *current-parse-trees)
         (display-parse-trees1 *current-parse-trees
            verbose nil nil))
      (list-length *current-parse-trees)))


(defun invoke-parser1 (words)
   (if *lr1-parse
      (lr1-parse words)
      (g-parse words (top-category-indexes))))


(defun remove-leading-star (words)
   (cond
      ((null words) nil)
      ((member (car words) '(* ? \!))
         (remove-leading-star (cdr words)))
      (t words)))


(defun top-category-indexes nil
   (if *top
      (do ((n 0 (1+ n))
            (len (length *index-category-table))
            (res nil))
         ((eql n len) res)
         (when
            (some
               #'(lambda (binding-list)
                  (every
                     #'(lambda (fvpair)
                        (member (fv-pair-feature fvpair)
                           (svref *index-category-table n) :test #'eq))
                     (category-binding-category (car binding-list))))
               (top-declaration-categories
                  (normalise-top-definition (car *top))))
            (push n res)))
      t))


(defun top-category-p (category vt)
   (or (null *top)
      (let*
         ((p-bindings
               (convert-from-parser-format vt category t))
            (p-binding-no (category-binding-number (car p-bindings))))
         (some
            #'(lambda (bindings)
               (match-category
                  (category-binding-number (car bindings)) bindings
                  p-binding-no p-bindings))
            (top-declaration-categories
               (normalise-top-definition (car *top)))))))


;;; --- Display all the parse trees. If mode is words then just
;;; print word bracketing; if rules, then display rule names and
;;; words; if full, then include the categories in each node.
;;; For displaying as trees, first have to put each parse tree
;;; in the form of a list onto the properties of generated nodes

(defun display-parse-trees (mode)
   (let
      ((input (prompt-never)))
      (if *current-parse-trees
         (let
            ((index
                (and input
                   (symbol-to-number (car input)))))
            (display-parse-trees1
               (let
                  ((tree
                      (and (numberp index)
                         (nth (1- index) *current-parse-trees))))
                  (if tree (ncons tree)
                     *current-parse-trees))
               mode
               (cond
                  ((numberp index) (cdr input))
                  (input))
               nil))
         (gde-cerror "no current parse trees"))))


(defun display-parse-trees1
   (trees mode command-options optional-file)
   (if (member mode '(rules full))
      (let
         ((index 0))
         (mapcan
            #'(lambda (bindings-and-tree)
               (unless optional-file (terpri))
               (display-parse-tree
                  (make-parse-tree-nodes
                     (if (eq mode 'full)
                        (car bindings-and-tree))
                     (cdr bindings-and-tree)
                     (eq mode 'full) (gensym))
                  (cond
                     ((eq mode 'full)
                        #'(lambda (node)
                           (chars-in-realiased-category
                              (get node 'category)
                              t)))
                     (t
                        #'(lambda (node)
                           (declare (ignore node))
                           nil)))
                  #'(lambda (node)
                     (coerce
                        (princ-to-string
                           (idrule-name-string
                              (get node 'rule-name)))
                        'list))
                  (when optional-file
                     (new-plot-file optional-file (incf index)))))
            trees))
      (if optional-file
         (with-open-file
            (*standard-output* optional-file :direction :output
                :if-exists :overwrite :if-does-not-exist :create)
            (display-parse-trees2 trees mode command-options)
            (list optional-file))
         (display-parse-trees2 trees mode command-options))))


(defun display-parse-trees2 (trees mode command-options)
   (fresh-line *standard-output*)
   (ecase mode
      (semantics
         (display-parse-semantics trees
            (and command-options
               (match-command-shortest
                  (car command-options) 'unreduced 1))))
      (bracketings
         (display-parse-bracketing trees))
      (lrules
         (display-parse-rule-labelling trees))
      (lcategories
         (display-parse-cat-labelling trees))
      (common
         (display-parse-common trees nil 0))
      (interpret
         (invoke-parser-interpretation))))


;;; Find shared parse tree nodes if there are more than one
;;; parse. Display in tabular form. Does not go into internal
;;; word structure.

(defvar common-nodes nil)


(defun display-parse-common (bindings-and-trees common-nodes tree-no)
   (cond
      ((> (list-length bindings-and-trees) 1)
         (dolist
            (binding-and-tree bindings-and-trees)
            (find-common-in-tree
               (cdr binding-and-tree)
               (setf tree-no (1+ tree-no)) 0))))
   (let
      ((c-nodes
          (mapcan
             #'(lambda (node)
                  (when
                     (> (list-length (common-node-tree-nos node))
                        1)
                     (ncons node)))
             common-nodes)))
      (cond
         (c-nodes (display-sentence-with-vertices)
            (format t
               "Shared sub-trees (parse numbers across top):~%")
            (let
               ((parse-spaces
                   (display-parse-numbers
                      (list-length bindings-and-trees) 3 10)))
               (dolist
                  (node
                     (sort (copy-list c-nodes)
                        #'is-common-node-before))
                  (let
                     ((tree-nos (common-node-tree-nos node))
                        (output nil))
                     (format t "~A~%"
                        (cadar (common-node-category node)))
                     (push
                        (format nil "   ~A-~A~10T"
                           (common-node-start-vertex node)
                           (common-node-end-vertex node))
                        output)
                     (dolist (parse-space parse-spaces)
                        (when tree-nos
                           (cond
                              ((minusp (cdr parse-space))
                                 (format t "~{~A~}~%~10T" (nreverse output))
                                 (setq output
                                    (list
                                       (format nil "~V@T" (- (cdr parse-space))))))
                              (t
                                 (push
                                    (format nil "~V@T" (cdr parse-space))
                                    output)))
                           (cond
                              ((= (car parse-space) (car tree-nos))
                                 (pop tree-nos) (push "+" output))
                              (t (push " " output)))))
                     (format t "~{~A~}~%" (nreverse output))))))
         (t
            (format t "--- No common nodes in parse trees~%")))))


(defun display-sentence-with-vertices nil
   (format t "Sentence (with corresponding vertex numbers):~%")
   (let*
      ((words (remove-leading-star *previous-sentence))
         (n -1)
         (words-and-spaces
            (mapcan
               #'(lambda (word)
                  (list (if (> (incf n) 9) "  " " ")
                     (string word)))
               words))
         (m -1)
         (numbers-and-spaces
            (cons (incf m)
               (mapcan
                  #'(lambda (word)
                     (list (format nil "~V@T" (length (string word)))
                        (incf m)))
                  words))))
      (format t "~{~A~}~%~{~A~}~%~%"
         words-and-spaces numbers-and-spaces)))


(defun display-parse-numbers (n width initial-tab)
   (let*
      ((output (list (format nil "~V@T" initial-tab)))
         (res nil) (nn 1) (current-tab initial-tab)
         (line-length (gde-linelength)))
      (loop
         (when (> nn n) (return nil))
         (incf current-tab (1+ width))
         (push
            (cons nn
               (cond
                  ((> current-tab line-length)
                     (format t "~{~A~}~%" (nreverse output))
                     (setq output
                        (list (format nil "~V@T" initial-tab)))
                     (setf current-tab initial-tab)
                     (- width))
                  (t width)))
            res)
         (push
            (format nil "~V@T~A"
               (if (>= nn 10) (1- width) width) nn)
            output)
         (incf nn))
      (format t "~{~A~}~%" (nreverse output))
      (nreverse res)))


(defun find-common-in-tree (tree tree-no vertex)
   (cond
      ((and (cdr tree) (atom (cadr tree)))
         (1+ vertex))
      (t
         (let
            ((node
                (f-find tree common-nodes :key
                   #'common-node-category :test #'eq)))
            (cond
               (node
                  (setf (common-node-tree-nos node)
                     (nconc (common-node-tree-nos node)
                        (ncons tree-no)))
                  (common-node-end-vertex node))
               (t
                  (let
                     ((start-vertex vertex))
                     (push
                        (make-common-node :category tree
                           :start-vertex start-vertex
                           :end-vertex
                           (progn
                              (dolist
                                 (daughter
                                    (reverse (cdr tree)))
                                 (cond
                                    ((consp daughter)
                                       (setf vertex
                                          (find-common-in-tree
                                             daughter tree-no
                                             vertex)))))
                              vertex)
                           :tree-nos (ncons tree-no))
                        common-nodes)
                     vertex)))))))


;;; Order common parses on basis of position of first word. If a
;;; tie, decide in favour of commonality covering more words to
;;; the right.

(defun is-common-node-before (n1 n2)
   (let
      ((start1 (common-node-start-vertex n1))
         (start2 (common-node-start-vertex n2)))
      (cond
         ((= start1 start2)
            (> (common-node-end-vertex n1)
               (common-node-end-vertex n2)))
         (t
            (< start1 start2)))))


;;; Just print a bracketing of words for each parse. Descend
;;; into word structure if flag is set. When there's more than 1 parse
;;; print numbers

(defun display-parse-bracketing (bindings-and-trees)
   (let
      ((n 0) (*print-escape* nil)
         (*print-length* nil) (*print-level* nil))
      (dolist (bindings-and-tree bindings-and-trees)
         (when (>= n *show-bracketings)
            (format t "...[~A~A trees]...~%~%" (- (length bindings-and-trees) n)
               (if (> n 0) " more" ""))
            (return-from nil nil))
         (when (cdr bindings-and-trees) (princ (incf n)))
         (write
            (get-bracketing-from-parse-tree (cdr bindings-and-tree))
            :pretty t)
         (terpri) (terpri))))


(defun get-bracketing-from-parse-tree (tree)
   (cond
      ((and (cdr tree) (atom (cadr tree)))
         (cond
            ((or (not *word-structure)
                (atom (cdar tree)))
               (cadr tree))
            (t
               (get-bracketing-from-morph-tree
                  (cdar tree)))))
      (t
         (reverse
            (mapcan
               #'(lambda (daughter)
                    (cond
                       ((consp daughter)
                          (let
                             ((var-59
                                 (get-bracketing-from-parse-tree
                                    daughter)))
                             (if var-59 (ncons var-59))))))
               (cdr tree))))))


(defun get-bracketing-from-morph-tree (tree)
   (cond
      ((eq (car tree) 'entry)
         (car (cadr tree)))
      (t
         (mapcan
            #'(lambda (daughter)
                 (let
                    ((var-60
                        (get-bracketing-from-morph-tree
                           (cdr daughter))))
                    (if var-60 (ncons var-60))))
            (cdr tree)))))


;;;

(defun display-parse-cat-labelling (bindings-and-trees)
   (dolist
      (bindings-and-tree bindings-and-trees)
      (let
         ((*print-escape* nil)
            (*print-length* nil) (*print-level* nil))
         (progn
            (write
               (get-cat-labelling-from-parse-tree
                  (cdr bindings-and-tree)
                  (car bindings-and-tree))
               :pretty t)
            (terpri))
         (terpri))))


(defun get-cat-labelling-from-parse-tree (tree vt)
   (let
      ((category
            (concatl-string
               (chars-in-realiased-category
                  (convert-from-parser-format vt
                     (car tree) nil)
                  nil t))))
      (cons category
         (cond
            ((and (cdr tree) (atom (cadr tree)))
               (cond
                  ((or (not *word-structure)
                      (atom (cdar tree)))
                     (ncons (cadr tree)))
                  (t
                     (get-cat-labelling-from-morph-tree
                        (cdar tree)))))
            (t
               (reverse
                  (mapcan
                     #'(lambda (daughter)
                          (cond
                             ((consp daughter)
                                (let
                                   ((var-61
                                       (get-cat-labelling-from-parse-tree
                                          daughter vt)))
                                   (if var-61
                                      (ncons var-61))))))
                     (cdr tree))))))))


(defun get-cat-labelling-from-morph-tree (tree)
   (cond
      ((eq (car tree) 'entry)
         (ncons (car (cadr tree))))
      (t
         (mapcar
            #'(lambda (daughter)
                 (cons
                    (concatl-string
                       (chars-in-realiased-category
                          (convert-from-morph-format
                             (car daughter))
                          nil t))
                    (get-cat-labelling-from-morph-tree
                       (cdr daughter))))
            (cdr tree)))))


;;;

(defun display-parse-rule-labelling (bindings-and-trees)
   (dolist
      (bindings-and-tree bindings-and-trees)
      (let
         ((*print-escape* nil)
            (*print-length* nil) (*print-level* nil))
         (progn
            (write
               (get-rule-labelling-from-parse-tree
                  (cdr bindings-and-tree))
               :pretty t)
            (terpri))
         (terpri))))


(defun get-rule-labelling-from-parse-tree (tree)
   (cond
      ((and (cdr tree) (atom (cadr tree)))
         (cadr tree))
      (t
         (cons (cadar tree)
            (reverse
               (mapcan
                  #'(lambda (daughter)
                     (cond
                        ((consp daughter)
                           (let ((d (get-rule-labelling-from-parse-tree daughter)))
                              (if d (list d))))))
                  (cdr tree)))))))


;;;

(defun make-parse-tree-nodes
   (table tree mode mother-node)
   (setf (get mother-node 'daughters) nil)
   (cond
      ((and (cdr tree) (atom (cadr tree)))
         (setf (get mother-node 'category)
            (and mode
               (convert-from-parser-format table
                  (car tree) nil)))
         (cond
            ((or (not *word-structure)
                (atom (cdar tree)))
               (setf (get mother-node 'rule-name)
                  (cadr tree)))
            (t
               (make-morph-tree-nodes table (cdar tree)
                  mode mother-node))))
      (t
         (setf (get mother-node 'category)
            (and mode
               (convert-from-parser-format table
                  (car tree) nil)))
         (setf (get mother-node 'rule-name)
            (cadar tree))
         (dolist (daughter (cdr tree))
            (cond
               ((consp daughter)
                  (let
                     ((daughter-node (gensym "N")))
                     (setf (get mother-node 'daughters)
                        (cons daughter-node
                           (get mother-node 'daughters)))
                     (setf (get daughter-node 'mother)
                        mother-node)
                     (make-parse-tree-nodes table daughter mode
                        daughter-node)))))))
   mother-node)


(defun make-morph-tree-nodes
   (table tree mode mother-node)
   (setf (get mother-node 'daughters) nil)
   (cond
      ((eq (car tree) 'entry)
         (setf (get mother-node 'rule-name)
            (car (cadr tree))))
      (t
         (setf (get mother-node 'rule-name)
            (car tree))
         (setf (get mother-node 'daughters)
            (mapcar
               #'(lambda (daughter)
                    (let
                       ((daughter-node (gensym "N")))
                       (setf (get daughter-node 'mother)
                          mother-node)
                       (if mode
                          (setf (get daughter-node 'category)
                             (convert-from-morph-format
                                (car daughter))))
                       (make-morph-tree-nodes table
                          (cdr daughter) mode
                          daughter-node)
                       daughter-node))
               (cdr tree))))))


(defun write-parse-trees (mode)
   (cond
      (*current-parse-trees
         (let
            ((input
                (prompt-if-necessary "File name? ")))
            (when input
               (let
                  ((files
                        (display-parse-trees1
                           *current-parse-trees mode nil
                           (canonise-grammar-file-name input))))
                  (write-construct-names
                     "Parse trees written to files "
                     (mapcar #'enough-namestring files))))))
      (t (gde-cerror "no current parse trees"))))


;;; --- Display parse tree categories --- Does not go into
;;; internal word structure.

(defun display-tree-node (syntax-p)
   (if *current-parse-trees
      (let*
         ((tree-input (prompt-never))
          (index
            (cond
               ((and tree-input (symbol-to-number (car tree-input)))
                  (symbol-to-number (pop tree-input)))
               ((null (cdr *current-parse-trees)) 1)
               (t
                  (symbol-to-number
                     (car
                        (prompt-always "Parse tree number (1 to "
                           (list-length *current-parse-trees)
                           ")? ")))))))
         (when index
            (if
               (or (< index 1)
                  (> index (list-length *current-parse-trees)))
               (gde-cerror "invalid parse tree number")
               (let
                  ((tree
                        (nth (1- index) *current-parse-trees)))
                  (multiple-value-bind (value rest-input)
                     (display-tree-value-chosen
                        tree-input
                        (collect-tree-rules-and-values
                           (cdr tree) syntax-p))
                     (if value
                        (if syntax-p
                           (display-tree-node-category (car tree) value)
                           (display-tree-node-semantics (car tree) value
                              rest-input))))))))
      (gde-ferror "no current parse trees")))


(defun display-tree-node-category (vt value)
   (let
      ((bindings
         (convert-from-parser-format vt value t)))
      (print-category-binding
         (category-binding-category
            (car bindings))
         (cdr bindings) 0)))
        

(defun display-tree-node-semantics (vt value input)
   (display-parse-semantics (list (cons vt value))
      (and input
         (match-command-shortest
            (car input) 'unreduced 1))))


(defun display-tree-value-chosen (tree-input rules-and-values)
   (cond
      ((and tree-input (symbol-to-number (car tree-input)))
         (let
            ((pairlis
                (nth
                   (1- (symbol-to-number (pop tree-input)))
                   rules-and-values)))
            (if pairlis
               (values (cdr pairlis) tree-input)
               (gde-ferror
                  "invalid parse tree node number"))))
      (t
         (values
            (prompt-for-alternative "tree nodes"
               rules-and-values)
            tree-input))))


(defun collect-tree-rules-and-values (tree syntax-p)
   (cond
      ((null (cdr tree))
         (ncons
            (cons (cadar tree)
               (if syntax-p (car tree) tree))))
      ((atom (cadr tree))
         (ncons
            (cons (cadr tree)
               (if syntax-p (car tree) tree))))
      (t
         (cons
            (cons (cadar tree)
               (if syntax-p (car tree) tree))
            (mapcan
               #'(lambda (daughter)
                    (cond
                       ((consp daughter)
                          (collect-tree-rules-and-values
                             daughter syntax-p))))
               (reverse (cdr tree)))))))


;;; --- Display and save sentences parsed so far ---

(defun display-parsed-sentences nil
   (dolist (s *parsed-sentences)
      (format t "(~A) ~{~A ~}.~%" (car s) (cdr s))))


(defun write-parsed-sentences nil
   (let
      ((input
          (prompt-if-necessary "File name? ")))
      (when input
         (let
            ((file (canonise-grammar-file-name input)))
            (backup-grammar-file file)
            (with-open-stream
               (*standard-output*
                  (open file :direction :output :if-exists
                     :supersede :if-does-not-exist :create))
               (dolist (s *parsed-sentences)
                  (format *standard-output* "~{~A ~}.~%" (cdr s))))
            (setf *unsaved-parsed-sentences nil)
            (format t "Sentences written~%")))))


(defun display-vertices-and-counts nil
   (cond
      (*previous-sentence
         (display-sentence-with-vertices)
         (display-edge-counts))
      (t (gde-cerror "no previous sentence"))))


(defun display-edge-counts nil
   (if *chart-edges
      (let
         ((table (collect-edges-for-counts)))
         (write-construct-names
            "Chart edge counts (start-end active inactive): "
            (mapcan
               #'(lambda (entry)
                    (mapcar
                       #'(lambda (sub-entry)
                            (concat-string (car entry) "-"
                               (car sub-entry) "  "
                               (list-length
                                  (mapcan
                                     #'(lambda (edge)
                                          (when (g-chart-edge-needed edge)
                                             (ncons edge)))
                                     (cdr sub-entry)))
                               " "
                               (list-length
                                  (mapcan
                                     #'(lambda (edge)
                                          (unless
                                             (g-chart-edge-needed edge)
                                             (ncons edge)))
                                     (cdr sub-entry)))
                               "  "))
                       (cdr entry)))
               table)))
      (format t "No chart edges~%")))


(defun collect-edges-for-counts nil
   (let
      ((table nil))
      (dolist (edge (car *chart-edges))
         (let
            ((start (g-chart-edge-start edge))
               (end (g-chart-edge-end edge)))
            (cond
               ((assoc start table)
                  (let
                     ((sub-entry
                         (assoc end
                            (cdr (assoc start table)))))
                     (if sub-entry
                        (push edge (cdr sub-entry))
                        (push (cons end (ncons edge))
                           (cdr (assoc start table))))))
               (t
                  (push
                     (cons start
                        (ncons (cons end (ncons edge))))
                     table)))))
      (sort
         (copy-list
            (mapcar
               #'(lambda (entry)
                    (cons (car entry)
                       (sort (copy-list (cdr entry))
                          #'(lambda (x y)
                               (< (car x) (car y))))))
               table))
         #'(lambda (x y) (< (car x) (car y))))))


;;; --- Display chart edges for debugging ---
;;;
;;; Expected modes for functions are {NIL, inactive, active} or
;;; {ai, ia}.

(defun display-simple-edges (mode)
   (if *chart-edges
      (let
         ((input (prompt-never)))
         (fresh-line)
         (let
            ((opt-start
                (when input
                   (symbol-to-number (pop input)))))
            (let
               ((opt-end
                   (when input
                      (symbol-to-number (pop input)))))
               (dolist (edge (car *chart-edges))
                  (cond
                     ((and (member mode '(nil active))
                         (g-chart-edge-needed edge))
                        (print-active (g-chart-edge-needed edge)
                           (g-chart-edge-end edge)
                           (g-chart-edge-res edge)
                           (chart-edge-rvt edge)
                           (g-chart-edge-start edge) opt-start
                           opt-end))
                     ((and (member mode '(nil inactive))
                         (not (g-chart-edge-needed edge)))
                        (print-inactive (g-chart-edge-found edge)
                           (chart-edge-nvt edge)
                           (g-chart-edge-start edge)
                           (g-chart-edge-end edge) opt-start
                           opt-end)))))))
      (gde-cerror "no current chart edges")))


(defun display-complex-edges (mode)
   (if *chart-edges
      (let
         ((input
             (prompt-if-necessary "Vertex number? ")))
         (when input
            (let
               ((vertex
                   (symbol-to-number (car input))))
               (let
                  ((start-a (when (eq mode 'ia) vertex))
                     (end-a (when (eq mode 'ai) vertex))
                     (start-i (when (eq mode 'ai) vertex))
                     (end-i (when (eq mode 'ia) vertex)))
                  (dolist (edge (car *chart-edges))
                     (cond
                        ((g-chart-edge-needed edge)
                           (print-active
                              (g-chart-edge-needed edge)
                              (g-chart-edge-end edge)
                              (g-chart-edge-res edge)
                              (chart-edge-rvt edge)
                              (g-chart-edge-start edge) start-a
                              end-a))
                        (t
                           (print-inactive
                              (g-chart-edge-found edge)
                              (chart-edge-nvt edge)
                              (g-chart-edge-start edge)
                              (g-chart-edge-end edge) start-i
                              end-i))))))))
      (gde-cerror "no current chart edges")))


(defun print-active
   (needed end res rule-table start opt-start
      opt-end)
   (when
      (edge-satisfies-boundaries start end
         opt-start opt-end)
      (format t "A  ~A -> ~A~VT~A --> ~%" start
         end 14 (cadr res))
      (dolist (cat needed)
         (print-edge-category rule-table cat 17))))


(defun edge-satisfies-boundaries
   (start end opt-start opt-end)
   (and
      (or (not (numberp opt-start))
         (= start opt-start))
      (or (not (numberp opt-end))
         (= end opt-end))))


(defun print-inactive (defn node-table start end opt-start
      opt-end)
   (when (edge-satisfies-boundaries start end opt-start opt-end)
      (let
         ((name
             (find-if #'atom
                (if (and (cdr defn) (atom (cadr defn))) (cdr defn) (cdar defn)))))
         (format t "I  ~A -> ~A~VT~A~%" start end 14 name)
         (print-edge-category node-table (car defn) 17)
         (mapc
            #'(lambda (x)
               (cond
                  ((eq x name) (return-from print-inactive))
                  ((and (consp x) (eq (car x) '*packed))
                     ;; this is a packed constituent
                     (let ((tail (cddddr x)))
                        (format t "~14T~A (packed)~%"
                           (if (and (consp tail) (atom (car tail)))
                              (car tail) (cadr (cadddr x)))))
                     (print-edge-category (caddr x) (cadddr x) 17))))
            (cdar defn)))))


(defun print-edge-category
   (table category line-posn)
   (let
      ((category
          (convert-from-parser-format table category
             t)))
      (print-category-binding
         (category-binding-category
            (car category))
         (cdr category) line-posn)))


;;; --- Establish hook for sentence interpretation ---

(defun invoke-parser-interpretation nil
   (if *current-parse-trees
      (if (fboundp 'interpret-sentence)
         (let*
            ((input (prompt-never))
               (index
                  (and input
                     (symbol-to-number (car input)))))
            (interpret-sentence
               (mapcan
                  #'(lambda (tree)
                     (mapcar
                        #'simplify-lambda-formula
                        (extract-semantics-from-parse-tree
                           (cdr tree) (car tree))))
                  (let
                     ((tree
                           (and (numberp index)
                              (nth (1- index) *current-parse-trees))))
                     (if tree (list tree) *current-parse-trees)))))
         (gde-cerror
            "interpretation function (interpret-sentence) is undefined"))
      (gde-cerror "no current parse trees")))



;;; Return an idrule in the internal format required by the
;;; parser. Category-binding records are converted to feature
;;; bundles in required format.

(defun convert-idrule-to-parser (idrule)
   (cons
      (cons
         (index-bundle-for-parser
            (category-binding-category
               (f-find
                  (car (id-rule-binding-nos idrule))
                  (id-rule-binding-list idrule) :key
                  #'category-binding-number :test #'eql))
            (id-rule-binding-list idrule) t)
         (cons
            (idrule-name-string (id-rule-name idrule))
            (mapcar
               #'(lambda (form)
                  (convert-semantic-form-to-parser form
                     (id-rule-binding-nos idrule)))
               (id-rule-semantic-forms idrule))))
      (mapcar
         #'(lambda (rhs-binding-no)
              (convert-category-to-parser
                 (f-find (the fixnum rhs-binding-no)
                    (id-rule-binding-list idrule) :key
                    #'category-binding-number :test #'eql)
                 (id-rule-binding-list idrule) nil))
         (cdr (id-rule-binding-nos idrule)))))


(defun convert-semantic-form-to-parser (form binding-nos)
   (translate-bindings-to-positions form nil
      binding-nos binding-nos))


(defun convert-category-to-parser (category binding-list word-p)
   (cons
      (index-bundle-for-parser
         (category-binding-category category)
         binding-list (not word-p))
      (if word-p word-p
         (let
            ((repetition (category-binding-repetition category))
               (null-fvpair
                  (f-find (null-feature-name)
                     (category-binding-category category) :key
                     #'fv-pair-feature :test #'eq)))
            (cond
               ((and null-fvpair
                     (eq (fv-pair-value null-fvpair) '+))
                  (null-index-name))
               ((eq repetition '*once*) nil)
               ((eq repetition '*rep1*) '+)
               (t
                  (gde-ferror "internal inconsistency -"
                     " unrecognised category repetition")))))))


;;; Find index for a category in a discrimination net keyed on feature
;;; names (ignoring head and null features). Return category in form expected
;;; by parser: as a vector holding just the feature values with the zeroth
;;; element containing the index.
;;;
;;; Addp flag should be true for rule categories and embedded categories inside
;;; word definitions, and false for top level word categories.
;;; For a lexical category, a combination of top level features
;;; not encountered in a rule category indicates an inconsistency between
;;; grammar and lexicon. An warning may be printed in this case. However,
;;; previously unencountered category value categories can quite legitimately
;;; exist only in word definitions.

(defun index-bundle-for-parser (bundle binding-list addp)
   (index-bundle-ensure-initialised)
   (index-bundle-for-parser1 bundle binding-list addp))


(defun index-bundle-ensure-initialised nil
   (unless *category-index-dnet
      (setq *category-index-dnet (list nil nil))
      (setq *current-category-index -1)
      (setq *index-category-table (make-array 10 :initial-element nil))))


(defun index-bundle-for-parser1 (bundle binding-list addp)
   (let ((len-and-index
            (find-index-for-bundle
               bundle *category-index-dnet (cddr *category-index-dnet)
               bundle addp)))
      (cond
         ((null len-and-index)
            (gde-warn
               (format nil
"lexical category [~{~A ~A~^, ~}] cannot be consumed by any rule~%"
                  (mapcan
                     #'(lambda (fvpair)
                        (list (fv-pair-feature fvpair)
                           (if (numberp (fv-pair-value fvpair))
                              "..." (fv-pair-value fvpair))))
                     bundle)))
            (make-array 1 :initial-element nil))
         (t
            (let ((n 0)
                  (res (make-array (1+ (car len-and-index)) :initial-element nil)))
               (setf (svref res 0) (cdr len-and-index))
               (dolist (fvpr bundle)
                  (unless
                     (or (eq (fv-pair-feature fvpr) (null-feature-name))
                        (eq (fv-pair-feature fvpr) (head-feature-name)))
                     (setf (svref res (incf n))
                        (if (numberp (fv-pair-value fvpr))
                           (index-bundle-for-parser1
                              (category-binding-category
                                 (f-find (fv-pair-value fvpr) binding-list
                                    :key #'category-binding-number :test #'eql))
                              binding-list t)
                           (fv-pair-value fvpr)))))
               res)))))


(defun index-bundle-for-category-index (bundle)
   (index-bundle-ensure-initialised)
   (cdr
      (find-index-for-bundle
         bundle *category-index-dnet (cddr *category-index-dnet)
         bundle t)))


(defun find-index-for-bundle (fvpairs prev tree bundle addp)
   (cond
      ((null fvpairs)
         (cond
            ((null tree)
               (if addp (add-index-for-bundle nil prev bundle)))
            ((eq (car tree) 'zz) (cadr tree))
            (t
               (find-index-for-bundle nil tree (cddr tree) bundle addp))))
      ((or (eq (fv-pair-feature (car fvpairs)) (null-feature-name))
            (eq (fv-pair-feature (car fvpairs)) (head-feature-name)))
         (find-index-for-bundle
            (cdr fvpairs) prev tree bundle addp))
      ((null tree)
         (if addp (add-index-for-bundle fvpairs prev bundle)))
      ((eq (car tree) 'zz)
         (find-index-for-bundle fvpairs tree (cddr tree) bundle addp))
      ((eq (car tree) (fv-pair-feature (car fvpairs)))
         (find-index-for-bundle
            (cdr fvpairs) tree (cadr tree) bundle addp))
      (t
         (find-index-for-bundle fvpairs tree (cddr tree) bundle addp))))


(defun add-index-for-bundle (fvpairs tree bundle)
   (let
      ((data (cons nil (incf *current-category-index))))
      (setf (cddr tree)
         (add-index-for-bundle1 fvpairs data bundle))
      data))


(defun add-index-for-bundle1 (fvpairs data bundle)
   (cond
      ((null fvpairs)
         (let ((feats
                  (mapcan
                     #'(lambda (fvpr)
                        (unless
                           (or (eq (fv-pair-feature fvpr) (null-feature-name))
                              (eq (fv-pair-feature fvpr) (head-feature-name)))
                           (list (fv-pair-feature fvpr))))
                     bundle)))
            (when (>= (length *index-category-table) (cdr data))
               (let
                  ((new-tree
                        (make-array (+ (cdr data) 10) :initial-element nil)))
                  (replace new-tree *index-category-table)
                  (setq *index-category-table new-tree)))
            (setf (svref *index-category-table (cdr data)) feats)
            (setf (car data) (length feats))
            (list 'zz data)))
      ((or (eq (fv-pair-feature (car fvpairs)) (null-feature-name))
            (eq (fv-pair-feature (car fvpairs)) (head-feature-name)))
         (add-index-for-bundle1 (cdr fvpairs) data bundle))
      (t
         (list (fv-pair-feature (car fvpairs))
            (add-index-for-bundle1 (cdr fvpairs) data bundle)))))



;;; Convert a feature bundle of the form used by the parser
;;; system to a category binding. Delete variables in the process.

(defvar new-parser-cat-bindings nil)


(defun convert-from-parser-format (variable-table category keep-variables)
   (let ((highest-binding-no 0))
      (convert-from-parser-format1 variable-table (car category)
         (if (eq (cdr category) (null-index-name))
            (list
               (feature-proper-value-pair (null-feature-name) '+)))
         keep-variables
         (cond
            ((eq (cdr category) '+) '*rep1*)
            ((or (null (cdr category))
                  (eq (cdr category) (null-index-name))
                  (consp (cdr category)))
               '*once*)
            (t
               (gde-ferror
"internal inconsistency - unrecognised category repetition"))))))


(defun convert-from-parser-format1 (variable-table cat bundle
      keep-variables repetition)
   (do
      ((n 1 (1+ n))
         (len (length cat))
         (main-binding-no highest-binding-no)
         (new-parser-cat-bindings nil)
         (features (svref *index-category-table (svref cat 0))))
      ((eql n len)
         (cons
            (make-category-binding :number main-binding-no
               :category (nreverse bundle) :repetition repetition)
            new-parser-cat-bindings))
      (let
         ((feature (pop features))
            (value (svref cat n)))
         (when (varp value)
            (setq value (g-dereference value variable-table)))
         (cond
            ((simple-vector-p value) ; a category value
               (let ((new-binding-no (incf highest-binding-no)))
                  (setq new-parser-cat-bindings
                     (append new-parser-cat-bindings
                        (convert-from-parser-format1
                           variable-table value nil keep-variables
                           '*once*)))
                  (push (make-fv-pair feature new-binding-no) bundle)))
            ((optvarp value))
            ((and (not keep-variables)
                  (or (varp value)
                     (eq value (unnamed-variable)))))
            (t
               (push (feature-proper-value-pair feature value)
                  bundle))))))


;;; End of file


