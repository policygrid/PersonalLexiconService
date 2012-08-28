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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - DICTIONARY INTERFACE
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code to fully instantiate GDE word
;;; definitions, and for looking up word definitions in the
;;; morphology system.
;;;
;;; Entry points:
;;;
;;;  * (defun G-defns (word) ...
;;;  * (defun Get-word-definitions (word format) ...
;;;  * (defun Compile-word (word) ...
;;;  * (defun Canonise-word (word) ...
;;;  * (defun Lookup-words-in-file NIL ...
;;;  * (defun Compile-dictionary () ...
;;;  * (defun Dict-top-loop () ...

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Functions to access the GDE lexicon and the morphology
;;; system. G-defns is called by the parser and generator, and
;;; returns a list of word-senses, each of the form (<category>
;;; <word>).

(defun g-defns (word)
   (setf word (canonise-word word))
   (let ((definition (get-word-definition word 'normalised 'full)))
      (if definition
         (values
            (let ((word-alts
                     (if (consp (word-definition-file definition))
                        (mapcar #'car (word-definition-file definition))
                        nil)))
               (mapcar
                  #'(lambda (sense)
                       (let ((bindings
                                (if *term-unification
                                   (word-sense-cat-bindings sense)
                                   (fill-unrestricted-category
                                      (word-sense-cat-bindings sense)))))
                          (cons
                             (cons
                                (car
                                   (convert-category-to-parser
                                      (car bindings) (cdr bindings) t))
                                (word-sense-structure sense))
                             (cons
                                (if word-alts (pop word-alts) word)
                                (word-sense-semantic-forms sense)))))
                   (word-definition-senses definition)))
            (if (consp (word-definition-file definition))
               (mapcar #'cdr (word-definition-file definition))
               nil))
         (progn (gde-cerror "word " word " not found")
            nil))))


;;; Get-word-definitions is the entry point for the GDE. Returns
;;; a list of categories (each being a list of category-binding
;;; records), one category for each word sense. Looks first in
;;; the GDE lexicon, then the morphology system for word. It
;;; returns NIL if the word cannot be found.
;;;
;;; Mode can be one of basic, normalised, propagated, or
;;; normalised-propagated.
;;;
;;; The word is expected to be in the correct case, i.e. already
;;; canonised.
;;;
;;; When tagged words flag is on look up words first on whole string
;;; (with segment before the underline character lowercased)

(defun get-word-definition (word format mode)
   (flet
      ((lookup (w)
          (cond
             ((null w) nil)
             ((and (get w 'word)
                 (word-definition-file (get w 'word)))
               (when (some #'consp (word-definition-senses (get w 'word)))
                  (get-gde-word-disk-resident w))
               (get-gde-word-definition w format mode))
             #+gde-morph
             ((and *morph-system
                 (get-morph-word-definition1 w))
               (get-morph-word-definition w format mode)))))
      (let ((tag-pos nil)
            (str (string word)))
         (if (and *tagged-words
                (setq tag-pos (get-word-tag-position str)))
            (let ((etag-pos
                    (or (position #\< str :start tag-pos) (length str))))
               (or
                  ;; try lookup as word_tag
                  (lookup
                     (find-symbol
                        (format nil "~(~A~)_~A"
                           (subseq str 0 (1- tag-pos)) (subseq str tag-pos etag-pos))))
                  ;; try lookup just of tag
                  (lookup (intern (subseq str tag-pos etag-pos)))))
             (lookup word)))))


(defun get-word-tag-position (str)
   ;; look for last underscore - there also must be at least 1 char after it
   (let ((found
            (position #\_ str :from-end t :end (1- (length str)))))
      (and found (1+ found))))


(defun get-gde-word-disk-resident (word)
   (when
      (some
         #'(lambda (sense)
             (and (consp sense)
                (not (eql (file-write-date (car sense)) (cadr sense)))))
         (word-definition-senses (get word 'word)))
      (gde-ferror "cannot retrieve disk-resident word '" word
"' since the file it is in has been modified since it was read in"))
   (let ((senses nil) (file nil) (comment nil))
      (dolist (sense (word-definition-senses (get word 'word)))
         (if (consp sense)
            (let ((parsed-word
                    (with-open-file (*standard-input* (car sense) :direction :input)
                       (let ((*file-read (car sense)))
                          (file-position *standard-input* (cddr sense))
                          (parse-word-definition (cdr (get-reply)))))))
               (setq senses
                  (nconc senses (word-definition-senses (cadr parsed-word))))
               (setq file (word-definition-file (cadr parsed-word)))
               (setq comment
                  (add-input-comment comment
                     (word-definition-comment (cadr parsed-word)))))
            (setq senses (nconc senses (list sense)))))
      (setf (get word 'word)
         (make-word-definition
            :senses senses :file file :comment comment))))


;;; Get a word straight from the GDE lexicon

(defun get-gde-word-definition
   (word format mode)
   (cond
      ((eq mode 'basic)
         (cond
            ((eq format 'aliased) (get word 'word))
            (t (normalise-word-definition word))))
      ((eq mode 'full)
         (let
            ((compiled-word (compile-gde-word word)))
            (cond
               ((eq format 'aliased)
                  (let
                     ((structure-29
                         (copy-word-definition compiled-word)))
                     (setf
                        (word-definition-senses structure-29)
                        (mapcar
                           #'(lambda (sense)
                                (let
                                   ((structure-30
                                       (copy-word-sense
                                          sense)))
                                   (setf
                                      (word-sense-cat-bindings
                                         structure-30)
                                      (realias-category-specification
                                         (word-sense-cat-bindings
                                            sense)))
                                   structure-30))
                           (word-definition-senses
                              structure-29)))
                     structure-29))
               (t compiled-word))))))


;;; Compile a word in the GDE lexicon - get its normalised
;;; categories, apply entry crs, multiplication rules, delete
;;; morph-only features, turn the feature value @D.. into a
;;; proper unique variable, and apply lexical category rules.

(defun compile-gde-word (word)
   (or (get word 'compiled-word)
      (progn
         (setf *generator-words nil)
         #+gde-morph (setup-morph-features)
         (setf (get word 'compiled-word)
            (let
               ((structure-31
                   (copy-word-definition
                      (normalise-word-definition word))))
               #+gde-morph
               (setf (word-definition-senses structure-31)
                  (mapcan
                     #'(lambda (sense)
                          (mapcar
                             #'(lambda (entry)
                                  (let
                                     ((structure-32
                                         (copy-word-sense
                                            sense)))
                                     (setf
                                        (word-sense-cat-bindings
                                           structure-32)
                                        (process-lexical-category
                                           (convert-from-morph-format
                                              (caddr entry))
                                           word
                                           (word-sense-semantic-forms
                                              sense)))
                                     structure-32))
                             (apply-dict-rules
                                (list word word
                                   (convert-to-morph-format
                                      (car
                                         (word-sense-cat-bindings
                                            sense))
                                      (cdr
                                         (word-sense-cat-bindings
                                            sense)))
                                   word nil))))
                     (word-definition-senses structure-31)))
               #-gde-morph
               (setf (word-definition-senses structure-31)
                  (mapcar
                     #'(lambda (sense)
                          (let
                             ((structure-32
                                 (copy-word-sense sense)))
                             (setf
                                (word-sense-cat-bindings structure-32)
                                (process-lexical-category
                                   (word-sense-cat-bindings sense)
                                   word
                                   (word-sense-semantic-forms sense)))
                             structure-32))
                     (word-definition-senses structure-31)))
               structure-31)))))


;;; Creating variables copies top level list structure of
;;; category (as a by-product) - this is relied on later during
;;; the full instantiation and sorting of the category which are
;;; destructive.

(defun process-lexical-category (cat-bindings word semantic-forms)
   (reset-variable-value-pairs)
   (let
      ((category
          (fully-instantiate-category
             (rename-variables-in-category
                (delete-morph-features cat-bindings))
             t)))
      (dolist (form semantic-forms)
         (type-check-semantic-form form word
            (category-binding-number
               (car category))
            category))
      category))


(defun check-category-semantic-forms nil
   nil)


(defun rename-variables-in-category (category)
   ;; morph system connot produce re-entrant variable values
   (mapcar
      #'(lambda (cat-binding)
           (let
              ((structure-33
                  (copy-category-binding cat-binding)))
              (setf
                 (category-binding-category structure-33)
                 (mapcar
                    #'(lambda (fvpr)
                         (cond
                            ((is-blank-variable
                                (fv-pair-value fvpr))
                               (feature-variable-value-pair
                                  (fv-pair-feature fvpr)))
                            (t fvpr)))
                    (category-binding-category structure-33)))
              structure-33))
      category))


(defun delete-morph-features (bindings)
   (let
      ((morphology-features
          (and (get (morphologyonly-set-name) 'set)
             (set-declaration-features
                (get (morphologyonly-set-name) 'set)))))
      (mapcar
         #'(lambda (binding)
              (let
                 ((structure-34
                     (copy-category-binding binding)))
                 (setf
                    (category-binding-category structure-34)
                    (mapcan
                       #'(lambda (fvpr)
                            (unless
                               (cond
                                  ((get (fv-pair-feature fvpr)
                                      'feature)
                                     (member
                                        (fv-pair-feature fvpr)
                                        morphology-features))
                                  (t
                                     (gde-ferror "feature "
                                        (fv-pair-feature fvpr)
" has not been declared")))
                               (ncons fvpr)))
                       (category-binding-category
                          structure-34)))
                 structure-34))
         bindings)))


;;; Retrieve a word from the morphology package.

#+gde-morph
(progn

(defun get-morph-word-definition
   (word format mode)
   (let
      ((mode-word
          (if (eq mode 'basic) (get word 'word)
             (compile-morph-word word))))
      (cond
         ((eq format 'aliased)
            (let
               ((structure-35
                   (copy-word-definition mode-word)))
               (setf (word-definition-senses structure-35)
                  (mapcar
                     #'(lambda (sense)
                          (let
                             ((structure-36
                                 (copy-word-sense sense)))
                             (setf
                                (word-sense-cat-bindings
                                   structure-36)
                                (realias-category-specification
                                   (word-sense-cat-bindings
                                      sense)))
                             structure-36))
                     (word-definition-senses structure-35)))
               structure-35))
         (t mode-word))))


(defun get-morph-word-definition1 (word)
   (or (get word 'word)
      (setf (get word 'word)
         (let
            ((res (get-word-from-morphan word)))
            (when res (pushnew word *cached-words)
               (make-word-definition :senses
                  (mapcar
                     #'(lambda (item)
                          (make-word-sense :cat-bindings
                             (convert-from-morph-format
                                (if *word-structure
                                   (car item) item))
                             :structure
                             (if *word-structure
                                (cdr item) nil)
                             :semantic-forms
                             (if *word-structure
                                (mapcar
                                   #'(lambda (form)
                                        (reduce-lambda-formula
                                           form))
                                   (normalise-semantic-forms
(compute-morph-word-semantics
                                         (cdr item))
                                      nil nil))
                                nil)))
                     res)
                  :file nil :comment nil))))))


(defun compile-morph-word (word)
   (and *morph-system
      (or (get word 'compiled-word)
         (progn
            (setf *generator-words nil)
            (setf (get word 'compiled-word)
               (let
                  ((structure-37
                      (copy-word-definition (get word 'word))))
                  (setf (word-definition-senses structure-37)
                     (mapcar
                        #'(lambda (sense)
                             (let
                                ((compiled-bindings
                                    (process-lexical-category
                                       (word-sense-cat-bindings
                                          sense)
                                       word
(word-sense-semantic-forms
                                          sense))))
                                (let
                                   ((structure-38
                                       (copy-word-sense
                                          sense)))
                                   (setf
                                      (word-sense-cat-bindings
                                         structure-38)
                                      compiled-bindings)
                                   structure-38)))
                        (word-definition-senses structure-37)))
                  structure-37))))))
)


;;; Interface to morphology system.

#+gde-morph
(defun get-word-from-morphan (word)
   (d-load) (setup-morph-features)
   (let*
      ((d-traceflag nil)
         (d-lookupformat
            (if *word-structure 'd-wordstructure
               'd-categoryform))
         (word-def nil)
         (gc-time (gctime))
         (cpu-time
            (truncate (* (get-internal-run-time) 1000)
               internal-time-units-per-second))
         (heap (conscount)))
      (setf word-def
         (if *fast-morph-lookup (d-fastlookup word) (d-lookup word)))
      (setq heap (- (conscount) heap))
      (setq cpu-time
         (- (truncate (* (get-internal-run-time) 1000)
               internal-time-units-per-second)
            cpu-time))
      (setq gc-time (- (gctime) gc-time))
      (when
         (and word-def (not *suppress-dict-messages))
         #-ALLEGRO (fresh-line)
         (format t "--- ~A: ~A msec CPU" word
            #+(or PROCYON :CORAL) (- cpu-time gc-time)
            #-(or PROCYON :CORAL) cpu-time)
         (when (> gc-time 0)
            (format t " (+ ~A msec GC)" gc-time))
         (when (> heap 0)
            (format t ", ~A heap" heap))
         (terpri)
         (finish-output))
      word-def))


;;; Convert feature bundles to and from morphology system
;;; format. Coming back value should not be a number - but -1
;;; seems to appear, so allow for this.

#+gde-morph
(progn

(defun convert-to-morph-format
   (category category-list)
   (mapcan
      #'(lambda (fvpair)
           (let
              ((var-39
                  (let
                     ((feat (fv-pair-feature fvpair))
                        (val (fv-pair-value fvpair)))
                     (cond
                        ((numberp val)
                           (make-d-fv-pair :feature feat :value
                              (convert-to-morph-format
                                 (f-find (the fixnum val)
                                    category-list :key
                                    #'category-binding-number
                                    :test #'eql)
                                 category-list)))
                        (t
                           (make-d-fv-pair :feature feat :value
                              val))))))
              (if var-39 (ncons var-39))))
      (category-binding-category category)))


(defun convert-from-morph-format
   (feature-bundle)
   (let
      ((highest-binding-no 0))
      (convert-from-morph-format1
         feature-bundle)))


(defun convert-from-morph-format1
   (feature-bundle)
   (let
      ((main-binding-no highest-binding-no)
         (binding-list nil))
      (let
         ((main-cat-binding
             (make-category-binding :number
                main-binding-no :category
                (mapcar
                   #'(lambda (fvpr)
                        (let
                           ((feature (d-fv-pair-feature fvpr))
                              (value (d-fv-pair-value fvpr)))
                           (cond
                              ((or (null value) (consp value))
                                 (let
                                    ((main-binding-no
                                        (setf
                                           highest-binding-no
                                           (1+ highest-binding-no))))
                                    (setf binding-list
                                       (append binding-list
                                          (convert-from-morph-format1
                                             value)))
                                    (make-fv-pair 
                                       feature
                                       main-binding-no)))
                              ((numberp value)
                                 (feature-proper-value-pair
                                    feature
                                    (number-to-symbol value)))
                              (t
                                 (feature-proper-value-pair
                                    feature value)))))
                   feature-bundle)
                :repetition '*once*)))
         (cons main-cat-binding binding-list))))


(defun setup-morph-features nil
   (setf d-morphologyonly nil)
   (unless
      (and d-features d-aliases d-whead
         d-wdaughter)
      (setf d-catvalfeat nil)
      (setf d-features
         (mapcar
            #'(lambda (name)
                 (list name
                    (let
                       ((values
                           (feature-declaration-values
                              (get name 'feature))))
                       (cond
                          ((eq values (cat-feature-value))
                             (setf d-catvalfeat
                                (cons name d-catvalfeat))
                             'category)
                          (t values)))))
            *features))
      (setf d-aliases
         (mapcar
            #'(lambda (name)
                 (list name
                    (let
                       ((bindings
                           (alias-declaration-cat-bindings
                              (normalise-alias-definition
                                 name))))
                       (convert-to-morph-format
                          (car bindings)
                          (cdr bindings)))))
            *aliases))
      (setf d-morphologyonly
         (when (get (morphologyonly-set-name) 'set)
            (set-declaration-features
               (get (morphologyonly-set-name) 'set))))
      (setf d-whead
         (when (get (whead-set-name) 'set)
            (set-declaration-features
               (get (whead-set-name) 'set))))
      (setf d-wdaughter
         (when (get (wdaughter-set-name) 'set)
            (set-declaration-features
               (get (wdaughter-set-name) 'set))))))


(defun apply-dict-rules (entry)
   (let
      ((crs
          (mapcan
             #'(lambda (name)
                  (cond
                     ((ec-rule-features (get name 'ecr))
                        (mapcar
                           #'(lambda (feature)
                                (subst feature
                                   (f-variable-name)
                                   (get name 'ecr) :test
                                   #'equal))
                           (ec-rule-features (get name 'ecr))))
                     (t (ncons (get name 'ecr)))))
             *ec-rules))
         (mrs
            (mapcar
               #'(lambda (name)
                    (get name 'mr))
               *multiply-rules))
         (ccs
            (mapcar
               #'(lambda (name)
                    (get name 'cc))
               *cc-rules)))
      (setf d-variables
         (nconc
            (variables-in-morph-syntax-field
               (caddr entry))
            d-variables))
      (d-applyccs ccs
         (cond
            (*ecrs-before-multiply
               (d-applymrs mrs (d-applycrs crs entry)))
            (t
               (mapcar
                  #'(lambda (e)
                       (d-applycrs crs e))
                  (d-applymrs mrs entry)))))))


(defun variables-in-morph-syntax-field
   (fvprs)
   (mapcan
      #'(lambda (fvpr)
           (let
              ((value (d-fv-pair-value fvpr)))
              (cond
                 ((consp value)
                    (variables-in-morph-syntax-field value))
                 ((and (varp value)
                     (not (assoc value d-variables)))
                    (ncons
                       (list value
                          (if
                             (is-category-valued
                                (d-fv-pair-feature fvpr))
                             (dk-category) nil)))))))
      fvprs))
)


;;; --- Lookup words in file ---
;;;
;;;  Read a set of words from a file and try to look up their
;;; definitions. Optionally output results to another file.

(defvar lookup-words-file nil)


(defun lookup-words-in-file nil
   (let
      ((input
          (prompt-if-necessary "Input file? ")))
      (when input
         (let
            ((lookup-words-file
                (canonise-grammar-file-name input)))
            (cond
               ((probe-file lookup-words-file)
                  (let
                     ((input (prompt-always "Output file? ")))
                     (cond
                        (input
                           (let
                              ((output-file
                                  (canonise-grammar-file-name
                                     input)))
                              (backup-grammar-file output-file)
                              (with-open-stream
                                 (*standard-output*
                                    (open output-file
                                       :direction :output
                                       :if-exists :supersede
                                       :if-does-not-exist
                                       :create))
                                 (lookup-words-in-file1
                                    lookup-words-file))))
                        (t
                           (lookup-words-in-file1
                              lookup-words-file)))))
               (t
                  (gde-cerror
                     "words file does not exist")))))))


(defun lookup-words-in-file1 (input-file)
   (with-open-stream
      (*standard-input*
         (open input-file :direction :input))
      (loop
         (dolist (word (get-reply))
            (cond
               ((not (gde-comment-p word)) (terpri)
                  (view-word1 (canonise-word word) 'aliased
                     'basic))))
         (cond
            ((eql
                (peek-char nil *standard-input* nil *eof-marker)
                *eof-marker)
               (return nil))))))


;;; --- Making a new dictionary ---

#+gde-morph
(progn

(defun compile-dictionary nil
   (cond
      ((null *morph-system)
         (gde-cerror
            "can't compile - morphology system flag is OFF"))
      (t (format t "Compiling dictionary...~%")
         (d-markunload 'di)
         (morph-compilation-invalidations)
         (d-makelexicon *morph-system)
         (when *words
            (format t
               "~%There is now the opportunity to move the ~:
words in the GDE lexicon~%into the morphology ~:
package '...le' input file~%")
            (when
               (yes-for-question
                  "Do you want this to happen")
               (merge-lexicons-into-file
                  (parse-namestring
                     (concat-string *morph-system (dk-le))))
               (dolist (word *words)
                  (delete-word-invalidations word)))))))


(defun morph-compilation-invalidations nil
   (dolist (word *cached-words)
      (progn
         (remprop word 'word)
         (remprop word 'compiled-word)))
   (setf *cached-words nil)
   (setf *generator-words nil))


(defun merge-lexicons-into-file
   (lexicon-merge-file)
   (with-open-stream
      (*standard-output*
         (open lexicon-merge-file :direction :output
            :if-exists :append :if-does-not-exist
            :create))
      (terpri)
      (dolist (word *words)
         (dolist
            (sense
               (word-definition-senses
                  (normalise-word-definition word)))
            (let
               ((category
                   (convert-to-morph-format
                      (car
                         (word-sense-cat-bindings sense))
                      (cdr
                         (word-sense-cat-bindings sense)))))
               (dolist
                  (semantic-form
                     (word-sense-semantic-forms sense))
                  (progn
                     (princ
                        (list word word category semantic-form
                           nil))
                     (terpri))))))))


(embed d-parselexiconfile
   (lambda nil
       (cond
          (*inside-dci (d-parselexiconfile))
          (t
             (when
                (probe-file
                   (concat-string *morph-system (dk-le-ma)))
                (close
                   (open
                      (concat-string *morph-system
                         (dk-le-ma))
                      :direction :input)))
                             ;;; Don't know why this is
                             ;;; needed, but it stops OPEN
                             ;;; CREATE falling over if
                             ;;; lexicon has previously been
                             ;;; compiled then loaded.
              (when
                (null
                   (catch-all-errors 'd-parselexiconfile1))
                (close d-entryfileid) (close d-fileid)
                (gde-ferror
                   "dictionary compilation failed"))))))


(defun d-parselexiconfile1 nil
   (let* ((words *words) (word nil))
      (loop
         (cond
            ((null words) (return nil)))
         (setf word (pop words))
         (dolist
            (sense
               (word-definition-senses
                  (normalise-word-definition word)))
            (let
               ((bindings
                   (word-sense-cat-bindings sense)))
               (setf d-simplelexicon
                  (d-entryconv
                     (apply-dict-rules
                        (list word word
                           (convert-to-morph-format
                              (car bindings)
                              (cdr bindings))
                           (word-sense-semantic-forms sense)
                           nil))
                     d-simplelexicon))))))
   (let ((entry nil))
      (loop
         (cond
            ((eq d-currentsym 'eof) (return nil)))
         (setf entry (d-parseentry))
         (unless
            (member (canonise-word (car entry))
               *words)
            (setf d-simplelexicon
               (d-entryconv (apply-dict-rules entry)
                  d-simplelexicon))))))
)


;;; --- Making new spelling rules and word grammar ---

#+gde-morph
(progn

(defun compile-spelling nil
   (cond
      ((null *morph-system)
         (gde-cerror
            "can't compile - morphology system flag is OFF"))
      (t
         (format t "Compiling spelling rules... ~%")
         (d-markunload 'sp)
         (morph-compilation-invalidations)
         (when
            (null
               (catch-all-errors 'd-makesprules
                  *morph-system))
            (close d-fileid)
            (gde-cerror
               "spelling rule compilation failed")))))


(defun compile-word-grammar nil
   (cond
      ((null *morph-system)
         (gde-cerror
            "can't compile - morphology system flag is OFF"))
      (t
         (format t "Compiling word grammar... ~%")
         (d-markunload 'gr)
         (morph-compilation-invalidations)
         (when
            (null
               (catch-all-errors 'd-makewordgrammar
                  *morph-system))
            (close d-fileid)
            (gde-cerror
               "word grammar compilation failed")))))
)


;;; Call morphology system top loop. No facilities for
;;; compiling.

#+gde-morph
(defun dict-top-loop nil
   (cond
      (*morph-system
         (let ((*inside-dci t))
            (d-initreader)
            ;; d-restart expects to clear a newline from input before
            ;; doing anything, so put last one back
            (unread-char #\Newline *terminal-io*)
            (d-restart)))
      (t
         (gde-cerror
            "morphology system flag is OFF"))))


;;; End of file

