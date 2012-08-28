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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - GENERATOR
;;;
;;; Author: John Carroll
;;;
;;; A user-controlled generator of syntax trees. The user builds
;;; up the tree herself, specifying which node to expand with
;;; which particular rule. Automatic generation may be invoked
;;; at any point - down to nodes which are filled in with an
;;; appropriate word from the GDE lexicon or one that has
;;; previously been retrieved from the morphology system. In
;;; automatic mode no ID rule is applied more than once down any
;;; particular branch of the tree.
;;;
;;; Entry points:
;;;
;;;  * (defun Generate-sentences NIL ...
;;;
;;; Repeated daughters in rules are taken to occur only once.
;;; Internal word structure is not displayed in generator trees.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


(progn
   (defvar *generator-commands nil)
   (defvar *generator-view-commands nil)
   (defvar *generator-auto-commands nil))


(setf *generator-view-commands
   (list
      (make-command-entry :shortest 1 :name
         'bracketing :action
         '(display-gen-tree 'bracketing))
      (make-command-entry :shortest 1 :name
         'categories :action
         '(display-gen-tree 'categories))
      (make-command-entry :shortest 1 :name
         'rules :action '(display-gen-tree 'rules))
      (make-command-entry :shortest 1 :name 'full
         :action '(display-gen-tree 'full))
      (make-command-entry :shortest 1 :name
         'semantics :action
         '(display-gen-tree 'semantics))))


(setf *generator-auto-commands
   (list
      (make-command-entry :shortest 1 :name
         'bracketing :action
         '(gen-sentence 'bracketing))
      (make-command-entry :shortest 1 :name
         'categories :action
         '(gen-sentence 'categories))
      (make-command-entry :shortest 1 :name
         'rules :action '(gen-sentence 'rules))
      (make-command-entry :shortest 1 :name 'full
         :action '(gen-sentence 'full))
      (make-command-entry :shortest 1 :name
         'semantics :action
         '(gen-sentence 'semantics))))


(setf *generator-commands
   (list
      (make-command-entry :shortest 1 :name
         'expand :action '(gen-expand))
      (make-command-entry :shortest 1 :name
         'automatic :action
         '(process-command-option "B/C/R/F /S? "
             *generator-auto-commands))
      (make-command-entry :shortest 1 :name
         'clear :action '(gen-clear))
      (make-command-entry :shortest 1 :name 'view
         :action
         '(process-command-option "B/C/R/F /S? "
             *generator-view-commands))
      (make-command-entry :shortest 1 :name 'undo
         :action '(gen-undo 'full))
      (make-command-entry :shortest 1 :name
         'interpret :action
         '(invoke-gen-interpretation))
      (make-command-entry :shortest 1 :name 'help
         :action '(give-generator-help))
      (make-command-entry :shortest 1 :name '?
         :action '(give-generator-help))
      (make-command-entry :shortest 1 :name '\!
         :action '(process-lisp-input))
      (make-command-entry :shortest 1 :name
         'shell :action '(shell))
      (make-command-entry :shortest 1 :name 'lisp
         :action '(lisp-top-loop))))


(progn
   (defmacro gen-definition-category (defn)
      `(caar ,defn))
   (defmacro gen-definition-name (defn)
      `(cadar ,defn))
   (defmacro gen-definition-semantics (defn)
      `(cddar ,defn)))


(defun give-generator-help nil
   (give-help *generator-help-file))


;;; Creation and manipulation of generator tree nodes.

(defstruct (generator-node)
   mother
   daughters
   rule-name
   category
   table
   semantics
   word-structure)


(progn
   (defmacro gen-node-mother (node-name)
      `(generator-node-mother
          (get ,node-name 'gen-node)))
   (defmacro gen-node-daughters (node-name)
      `(generator-node-daughters
          (get ,node-name 'gen-node)))
   (defmacro gen-node-rule-name (node-name)
      `(generator-node-rule-name
          (get ,node-name 'gen-node)))
   (defmacro gen-node-category (node-name)
      `(generator-node-category
          (get ,node-name 'gen-node)))
   (defmacro gen-node-table (node-name)
      `(generator-node-table
          (get ,node-name 'gen-node)))
   (defmacro gen-node-semantics (node-name)
      `(generator-node-semantics
          (get ,node-name 'gen-node)))
   (defmacro gen-node-word-structure
      (node-name)
      `(generator-node-word-structure
          (get ,node-name 'gen-node))))


(progn
   (defmacro set-gen-node-mother
      (node-name new-mother)
      `(setf
          (generator-node-mother
             (get ,node-name 'gen-node))
          ,new-mother))
   (defmacro set-gen-node-daughters
      (node-name new-daughters)
      `(setf
          (generator-node-daughters
             (get ,node-name 'gen-node))
          ,new-daughters))
   (defmacro set-gen-node-rule-name
      (node-name new-rule-name)
      `(setf
          (generator-node-rule-name
             (get ,node-name 'gen-node))
          ,new-rule-name))
   (defmacro set-gen-node-category
      (node-name new-cat)
      `(setf
          (generator-node-category
             (get ,node-name 'gen-node))
          ,new-cat))
   (defmacro set-gen-node-table
      (node-name new-table)
      `(setf
          (generator-node-table
             (get ,node-name 'gen-node))
          ,new-table))
   (defmacro set-gen-node-semantics
      (node-name new-semantics)
      `(setf
          (generator-node-semantics
             (get ,node-name 'gen-node))
          ,new-semantics))
   (defmacro set-gen-node-word-structure
      (node-name new-structure)
      `(setf
          (generator-node-word-structure
             (get ,node-name 'gen-node))
          ,new-structure)))


(defun make-gen-non-lexical-node
   (node-name rule-name mother-category
      daughters table semantics)
   (set-gen-node-daughters node-name
      (mapcar
         #'(lambda (daughter)
              (let
                 ((daughter-name (make-gen-node-name))
                    (null-p
                       (eq (cdr daughter)
                          (null-index-name))))
                 (setf (get daughter-name 'gen-node)
                    (make-generator-node :mother node-name
                       :daughters nil :rule-name (if null-p 'e)
                       :category
                       (if null-p
                          (cons
                             (make-fv-pair 
                                (null-feature-name) '+)
                             (car daughter))
                          (car daughter))
                       :table table :semantics
                       (if null-p (semantics-for-null))
                       :word-structure nil))
                 daughter-name))
         daughters))
   (set-gen-node-rule-name node-name
      rule-name)
   (set-gen-node-category node-name
      mother-category)
   (set-gen-node-semantics node-name
      semantics)
   node-name)


(defun make-gen-lexical-node
   (mother word category table semantics
      structure)
   (let
      ((node-name (make-gen-node-name)))
      (set-gen-node-rule-name mother node-name)
      (setf (get node-name 'gen-node)
         (make-generator-node :mother mother
            :daughters nil :rule-name word :category
            category :table table :semantics semantics
            :word-structure structure))
      node-name))


(defun gen-root-node (category)
   (let
      ((node-name (make-gen-node-name)))
      (setf (get node-name 'gen-node)
         (make-generator-node :mother nil :daughters
            nil :rule-name nil :category category
            :table (cons nil nil) :semantics nil
            :word-structure nil))
      node-name))


(defun make-gen-node-name nil
   (let
      ((name
          (cond
             (*generator-nodes
                (or
                   (get (car *generator-nodes)
                      'next-number-atom)
                   (setf
                      (get (car *generator-nodes)
                         'next-number-atom)
                      (number-to-symbol
                         (1+
                            (symbol-to-number
                               (car *generator-nodes)))))))
             (t '\1))))
      (push name *generator-nodes)
      name))


;;; Generator entry point.
;;;
;;; If there is not already a tree in existence, set top category
;;; to be first TOP category, or [] if it is not defined.

(defun generate-sentences nil
   (cond
      (*features
         (generate-sentences1))
      (t
         (gde-cerror
            "no grammar has been defined"))))


(defun generate-sentences1 nil
   (cond
      (*features
         (unless *generator-rules (make-gen-rule-tree))
         (unless *generator-words (make-gen-word-tree))
         (unless *generator-nodes
            (gen-root-node (make-gen-top-category)))
         (generate-top-loop))
      (t
         (gde-cerror "no grammar has been defined"))))


(defun generate-top-loop nil
   (top-loop #'gde-top-print
      #'gen-top-eval "Gen" "GDE Generator"))


(defun gen-top-eval (x)
   (unless *generator-nodes
      (gen-root-node (make-gen-top-category)))
   (process-command (get-reply1 x) *generator-commands)
   'nothing)


(defun make-gen-top-category nil
   (let*
      ((category
            (if *top
               (car
                  (top-declaration-categories
                     (normalise-top-definition (car *top))))
               (list
                  (make-category-binding :number 0
                     :category nil :repetition '*once*))))
         (cat-bindings
            (fully-instantiate-category
               (normalise-category-specification category) nil)))
      (unless *term-unification
         (setq cat-bindings (fill-unrestricted-category cat-bindings)))
      (car
         (convert-category-to-parser
            (car cat-bindings) (cdr cat-bindings) nil))))


;;; Rules and words known to the GDE are stored in 2 trees for
;;; quick access (c.f. the parser - in fact uses g-makeruletree from
;;; parser). For words, indexing is on each category in word
;;; definitions, and for rules on the category of the mother.
;;;
;;; Items stored in rule tree are of the form:
;;;
;;;    ((<mother-cat> <rule-name> . <semantics>)
;;;        (<daughter-cat1> . <repetition1>) ...)
;;;
;;; and in word tree are of form
;;;
;;;    ((<word-cat> <word> . <semantics>) . <word-structure>)
;;;
;;; The rule name will always be a string and the word a symbol,
;;; so can pass round both types of definition, process them
;;; uniformly, but tell which is which when necessary.

(defun make-gen-rule-tree nil
   (index-gen-rules
      (mapcan
         #'(lambda (idrule-name)
            (mapcar
               #'(lambda (compiled-idrule)
                  (convert-idrule-to-parser
                     (if *term-unification
                        compiled-idrule
                        (fill-unrestricted-idrule compiled-idrule))))
               (compile-idrule idrule-name)))
         *id-rules)))


(defun index-gen-rules (rules)
   (setq *generator-rules
      (g-makeruletree
         (mapcar
            #'(lambda (rule)
               (cons (caar rule) rule))
            rules)
         (make-array (1+ *current-category-index) :initial-element nil))))


(defun make-gen-word-tree nil
   (setq *generator-words
      (g-makeruletree
         (mapcan
            #'(lambda (word)
               (mapcar
                  #'(lambda (word-defn)
                     (list* (caar word-defn)
                        (cons (caar word-defn) (cdr word-defn))
                        (cdar word-defn)))
                  (g-defns word)))
            (append *words *cached-words))
         (make-array (1+ *current-category-index) :initial-element nil))))



;;; Expand a generator tree node. The tree will always exist.
;;; The definition returned by Ask-for-definition is a cons of
;;; (<definition-from-tree> . <variable-tables>) where the
;;; tables contain the variable bindings resulting from the
;;; unification of the definition category with the tree node
;;; category.

(defun gen-expand nil
   (let
      ((node-to-expand
          (ask-for-node-name *generator-nodes)))
      (when node-to-expand
         (let
            ((definition
                (ask-for-definition
                   (gen-node-category node-to-expand))))
            (when definition
               (cond
                  ((symbolp
                      (gen-definition-name
                         (car definition)))
                     (gen-expand-with-word node-to-expand
                        definition))
                  (t
                     (gen-expand-with-rule node-to-expand
                        (gen-node-category node-to-expand)
                        definition)))
               (display-gen-tree 'full))))))


(defun ask-for-node-name
   (possible-node-names)
   (let
      ((name (prompt-if-necessary "Node? ")))
      (when name
         (let
            ((tree-node (car name)))
            (cond
               ((not
                   (member tree-node possible-node-names))
                  (gde-cerror
                     "node is not in the tree - try again")
                  (ask-for-node-name possible-node-names))
               ((gen-node-rule-name tree-node)
                  (gde-cerror
"node has already been expanded - try again")
                  (ask-for-node-name possible-node-names))
               (t (setf *user-input (cdr name))
                  tree-node))))))


(defun ask-for-definition (category)
   (let
      ((pattern
          (prompt-if-necessary
             "Rule name or word? ")))
      (if pattern
         (let
            ((appropriate-defns
                (get-appropriate-expanding-definitions
                   category pattern (canonise-word pattern))))
            (cond
               ((null appropriate-defns)
                  (gde-cerror
"rule or word not appropriate or not found - try again")
                  (ask-for-definition category))
               ((and (null (cdr appropriate-defns))
                   (not (equal pattern '(*))))
                  (car appropriate-defns))
               ((prompt-for-alternative
                   "rule names and words"
                   (mapcar
                      #'(lambda (def)
                           (cons
                              (gen-definition-name
                                 (car def))
                              def))
                      appropriate-defns)))
               (t (ask-for-definition category)))))))


;;; Return definitions for rules and words which match the
;;; pattern specified by the user, and which also may expand the
;;; current tree node category. If no rules or words found which
;;; match pattern, try morphology system, calling G-defns and
;;; remake word tree (for Word-definitions-for-category to pick
;;; up word) if successful.

(defun get-appropriate-expanding-definitions
   (category pattern canonised-pattern)
   (let
      ((matching-rules
          (get-items *id-rules pattern)))
      (let
         ((matching-words
             (cond
                ((get-items (append *words *cached-words)
                    canonised-pattern))
                ((and (null matching-rules)
                    (g-defns (car canonised-pattern)))
                   (make-gen-word-tree)
                   (ncons (car canonised-pattern))))))
         (nconc
            (and matching-rules
               (rule-definitions-for-category category
                  matching-rules))
            (and matching-words
               (word-definitions-for-category category
                  matching-words))))))


;;; Filter rules and words, returning those that unify with category
;;; and thus may be used to expand it. Return paired with a pair
;;; of variable binding tables.

(defun rule-definitions-for-category
   (category idrule-names)
   (let
      ((full-names
          (mapcan
             #'(lambda (name)
                  (cond
                     ((symbolp name)
                        (mapcar
                           #'(lambda (rule)
                                (idrule-name-string
                                   (id-rule-name rule)))
                           (get name 'compiled-idrules)))
                     (t (ncons (idrule-name-string name)))))
             idrule-names)))
      (mapcan
         #'(lambda (table-and-defns)
              (mapcan
                 #'(lambda (definition)
                      (cond
                         ((member
                             (gen-definition-name definition)
                             full-names :test #'equal)
                            (list
                               (cons definition
                                  (car
                                     table-and-defns))))))
                 (cdr table-and-defns)))
         (g-retrieve-rules
            (gen-remove-variables
               (cdr
                  (gen-node-table
                     (car *generator-nodes)))
               category)
            (car
               (gen-node-table
                  (car *generator-nodes)))
            (cdr
               (gen-node-table
                  (car *generator-nodes)))
            *generator-rules))))


(defun word-definitions-for-category
   (category words)
   (mapcan
      #'(lambda (table-and-defns)
           (mapcan
              #'(lambda (definition)
                   (cond
                      ((member (gen-definition-name definition)
                          words)
                         (list
                            (cons definition
                               (car table-and-defns))))))
              (cdr table-and-defns)))
      (g-retrieve-rules
         (gen-remove-variables
            (cdr
               (gen-node-table
                  (car *generator-nodes)))
            category)
         (car
            (gen-node-table
               (car *generator-nodes)))
         (cdr
            (gen-node-table
               (car *generator-nodes)))
         *generator-words)))


;;; Instantiate a node with the definition of a word, or expand
;;; a node with a rule. If with a rule, need to rename all
;;; variables in daughters which are not bound in the current
;;; variable table. Words are OK, since the same word on
;;; successive occasions will always come out with different
;;; variables.

(defun gen-expand-with-word
   (node definition-and-tables)
   (let
      ((definition
          (car definition-and-tables))
         (tables (cdr definition-and-tables)))
      (make-gen-lexical-node node
         (gen-definition-name definition)
         (gen-remove-variables (car tables)
            (gen-definition-category definition))
         (cons nil (cdr tables))
         (gen-definition-semantics definition)
         (cdr definition))))


(defun gen-remove-variables (subs category)
   (if subs
      (do*
         ((offset 1 (1+ offset))
            (len (length category))
            (res (make-array len)))
         ((eql offset len)
            (setf (svref res 0) (svref category 0))
            res)
         (setf (svref res offset)
            (let ((val (svref category offset)))
               (cond
                  ((varp val)
                     (let ((new-pair (assoc val subs)))
                        (if new-pair (cdr new-pair) val)))
                  ((simple-vector-p val)
                     (gen-remove-variables subs val))
                  (t val)))))
      category))


(defvar renaming-bindings nil)


(defun gen-expand-with-rule
   (node category definition-and-tables)
   (let
      ((definition
          (car definition-and-tables))
         (tables (cdr definition-and-tables))
         (renaming-bindings nil))
      (make-gen-non-lexical-node node
         (gen-definition-name definition) category
         (mapcar
            #'(lambda (daughter)
                 (cons
                    (gen-substitute-category (car tables)
                       (car daughter)) (cdr daughter)))
            (cdr definition))
         (cons nil (cdr tables))
         (gen-definition-semantics definition))))


(defun gen-substitute-category (subs category)
   (do*
      ((offset 1 (1+ offset))
         (len (length category))
         (res (make-array len)))
      ((eql offset len)
         (setf (svref res 0) (svref category 0))
         res)
      (setf (svref res offset)
         (let ((val (svref category offset)))
            (cond
               ((optvarp val)
                  (generate-optvariable))
               ((varp val)
                  (let
                     ((new-pair (assoc val subs)))
                     (cond
                        (new-pair (cdr new-pair))
                        (t
                           (gen-substitute-category1 val)))))
               ((simple-vector-p val)
                  (gen-substitute-category subs val))
               (t val))))))


(defun gen-substitute-category1 (value)
   (let
      ((rename-pair
          (assoc value renaming-bindings)))
      (cond
         (rename-pair (cdr rename-pair))
         (t
            (let
               ((v (generate-variable)))
               (push (cons value v) renaming-bindings)
               v)))))


;;; View the complete generator tree so far. Mode is one of
;;; bracketing, categories, rules, full or semantics.

(defun display-gen-tree (mode)
   (cond
      (*generator-nodes
         (cond
            ((member mode '(bracketing categories))
               (let
                  ((*print-escape* nil))
                  (progn
                     (write
                        (collect-words-in-gen-tree
                           (car (last *generator-nodes)) mode)
                        :pretty t)
                     (terpri))
                  (terpri)))
            ((eq mode 'semantics)
               (let
                  ((input (prompt-never)))
                  (display-gen-semantics
                     (car (last *generator-nodes))
                     (and input
                        (match-command-shortest (car input)
                           'unreduced 1)))))
            (t
               (display-parse-tree
                  (make-gen-tree-nodes
                     (cdr
                        (gen-node-table
                           (car *generator-nodes)))
                     (car (last *generator-nodes)) mode
                     (gensym "N"))
                  (if (eq mode 'rules)
                     #'(lambda (node)
                          (declare (ignore node))
                          nil)
                     #'(lambda (node)
                          (chars-in-realiased-category
                             (get node 'category) t)))
                  #'(lambda (node)
                       (coerce
                          (princ-to-string
                             (get node 'rule-name))
                          'list))
                  nil))))
      (t (gde-cerror "tree does not exist"))))


(defun make-gen-tree-nodes
   (table mother-node mode new-node)
   (setf (get new-node 'daughters) nil)
   (let
      ((word-node
          (gen-node-rule-name mother-node)))
      (cond
         ((and word-node (symbolp word-node)
             (gen-node-rule-name word-node))
            (setf (get new-node 'category)
               (and mode
                  (convert-from-parser-format table
                     (ncons (gen-node-category word-node))
                     nil)))
            (cond
               ((or (not *word-structure)
                   (atom (gen-node-word-structure word-node)))
                  (setf (get new-node 'rule-name)
                     (gen-node-rule-name word-node)))
               (t
                  (make-word-gen-tree-nodes table
                     (gen-node-word-structure word-node) mode
                     new-node))))
         (t
            (setf (get new-node 'category)
               (and mode
                  (convert-from-parser-format table
                     (ncons (gen-node-category mother-node))
                     nil)))
            (setf (get new-node 'rule-name)
               (or (gen-node-rule-name mother-node)
                  mother-node))
            (setf (get new-node 'daughters)
               (mapcar
                  #'(lambda (daughter)
                       (let
                          ((daughter-node (gensym "N")))
                          (setf (get daughter-node 'mother)
                             new-node)
                          (make-gen-tree-nodes table daughter
                             mode daughter-node)
                          daughter-node))
                  (gen-node-daughters mother-node))))))
   new-node)


(defun make-word-gen-tree-nodes
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
                       (make-word-gen-tree-nodes table
                          (cdr daughter) mode
                          daughter-node)
                       daughter-node))
               (cdr tree))))))


(defun collect-words-in-gen-tree
   (mother-node mode)
   (let
      ((word-node
          (gen-node-rule-name mother-node)))
      (cond
         ((null word-node)
            (if (eq mode 'categories)
               (concatl-string
                  (chars-in-realiased-category
                     (convert-from-parser-format
                        (cdr
                           (gen-node-table
                              (car *generator-nodes)))
                        (ncons (gen-node-category mother-node))
                        nil)
                     nil))
               (concat-string "_" mother-node "_")))
         ((symbolp word-node)
            (cond
               ((eq word-node 'e) word-node)
               ((or (not *word-structure)
                   (atom (gen-node-word-structure word-node)))
                  (gen-node-rule-name word-node))
               (t
                  (collect-morphemes-in-gen-tree
                     (gen-node-word-structure word-node)))))
         (t
            (mapcan
               #'(lambda (daughter)
                    (let
                       ((var-41
                           (collect-words-in-gen-tree daughter
                              mode)))
                       (if var-41 (ncons var-41))))
               (gen-node-daughters mother-node))))))


(defun collect-morphemes-in-gen-tree (tree)
   (cond
      ((eq (car tree) 'entry)
         (car (cadr tree)))
      (t
         (mapcar
            #'(lambda (daughter)
                 (collect-morphemes-in-gen-tree
                    (cdr daughter)))
            (cdr tree)))))


;;; Clear the generator tree after asking for confirmation. Undo
;;; finds mother of most recent node, deletes all daughters of
;;; that mother, and displays new tree if mode is non-NIL.

(defun gen-clear nil
   (when
      (yes-for-question
         "Do you really want to clear the tree")
      (setf *generator-nodes nil)
      (format t "Generator tree cleared~%")))


(defun gen-undo (mode)
   (let
      ((mother-of-last
          (gen-node-mother
             (car *generator-nodes))))
      (setf *generator-nodes
         (mapcan
            #'(lambda (node)
                 (unless
                    (eq (gen-node-mother node) mother-of-last)
                    (ncons node)))
            *generator-nodes))
      (set-gen-node-daughters mother-of-last nil)
      (set-gen-node-rule-name mother-of-last nil)
      (when mode (display-gen-tree mode))))


;;; Automatically generate a set of sentences up to a given
;;; length, from the current set of rules and words.

(defun gen-sentence (mode)
   (let
      ((input
          (prompt-if-necessary "Maximum length? ")))
      (when input
         (let
            ((input-max
                (symbol-to-number (car input))))
            (cond
               ((numberp input-max)
                  (gen-sentence1
                     (mapcan
                        #'(lambda (node)
                             (cond
                                ((null
                                    (gen-node-rule-name node))
                                   (list
                                      (cons node
                                         (gen-rules-up-path
                                            (gen-node-mother
                                               node)))))))
                        *generator-nodes)
                     mode
                     (- input-max
                        (length
                           (mapcan
                              #'(lambda (node)
                                 (when
                                    (symbolp
                                       (gen-node-rule-name
                                          node))
                                    (ncons node)))
                              *generator-nodes)))))
               (t
                  (gde-cerror
                     "maximum length should be a number")))))))


(defun gen-rules-up-path (node)
   (when node
      (cons (gen-node-rule-name node)
         (gen-rules-up-path
            (gen-node-mother node)))))


;;; Open-nodes is a list of the form ((node-name . rules-used)
;;; ...) where rules-used is a list of all the rule names in the
;;; path from the root node to the current node. These rules
;;; must not be used again in this path. Only leave a node
;;; assuming it to be lexical if no rules apply to it.

(defun gen-sentence1
   (open-nodes mode maxlen)
   (cond
      ((null open-nodes) (display-gen-tree mode))
      ((< maxlen 0))
      (t
         (let
            ((node (caar open-nodes))
               (rules-used (cdar open-nodes))
               (node-cat
                  (gen-node-category (caar open-nodes))))
            (let
               ((rule-defs
                   (look-for-rule-definitions node-cat
                      rules-used)))
               (let
                  ((word-def
                      (look-for-word-definition node-cat)))
                  (when word-def
                     (gen-expand-with-word node word-def))
                  (if (null rule-defs)
                     (gen-sentence1 (cdr open-nodes) mode
                        maxlen))
                  (when word-def (gen-undo nil)))
               (dolist (rule-def rule-defs)
                  (progn
                     (gen-expand-with-rule node node-cat
                        rule-def)
                     (gen-sentence1
                        (nconc
                           (mapcar
                              #'(lambda (daughter)
                                   (cons daughter
                                      (cons
                                         (gen-definition-name
                                            (car
                                               rule-def))
                                         rules-used)))
                              (gen-node-daughters node))
                           (cdr open-nodes))
                        mode
                        (- maxlen
                           (1- (length (gen-node-daughters node)))))
                     (gen-undo nil))))))))


(defun look-for-rule-definitions
   (category rule-exceptions)
   (mapcan
      #'(lambda (table-and-defns)
           (mapcan
              #'(lambda (definition)
                   (cond
                      ((not
                          (member
                             (gen-definition-name definition)
                             rule-exceptions :test #'equal))
                         (list
                            (cons definition
                               (car table-and-defns))))))
              (cdr table-and-defns)))
      (g-retrieve-rules
         (gen-remove-variables
            (cdr
               (gen-node-table
                  (car *generator-nodes)))
            category)
         (car
            (gen-node-table
               (car *generator-nodes)))
         (cdr
            (gen-node-table
               (car *generator-nodes)))
         *generator-rules)))


(defun look-for-word-definition (category)
   (let
      ((tables-and-defns
          (g-retrieve-rules
             (gen-remove-variables
                (cdr
                   (gen-node-table
                      (car *generator-nodes)))
                category)
            (car
               (gen-node-table
                  (car *generator-nodes)))
            (cdr
               (gen-node-table
                  (car *generator-nodes)))
             *generator-words)))
      (if tables-and-defns
         (cons (cadar tables-and-defns)
            (caar tables-and-defns)))))


;;; --- Establish hook for sentence interpretation ---

(defun invoke-gen-interpretation nil
   (if (fboundp 'interpret-sentence)
      (interpret-sentence
         (mapcar
            #'(lambda (form)
               (simplify-lambda-formula form))
            (extract-semantics-from-gen-tree
               (car (last *generator-nodes))
               (cdr
                  (gen-node-table (car *generator-nodes))))))
      (gde-cerror
         "interpretation function (interpret-sentence) is undefined")))


;;; End of file

