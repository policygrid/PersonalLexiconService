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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - DICTIONARY INPUT ETC.
;;;
;;; Author: John Carroll
;;;
;;; This file contains the code to input, delete and view all
;;; morphology system constructs.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; --- Input functions ---

(defun input-ecr nil
   (let
      ((input
          (prompt-if-necessary
             "Entry CR declaration? ")))
      (when input
         (insert-ecr-declaration input))))


(defun insert-ecr-declaration (input-list)
   (let
      ((parsed-ecr
          (parse-ecr-declaration input-list)))
      (when
         (check-redefinition 'ecr *ec-rules
            "entry completion rule"
            (car parsed-ecr))
         (format t "~%Defining entry CR: ~A~%"
            (car parsed-ecr))
         (setf (get (car parsed-ecr) 'ecr)
            (cadr parsed-ecr))
         (setf *ec-rules
            (insert-declaration-at-end *ec-rules
               (car parsed-ecr)))
         (input-word-invalidations *words
            'compiled))))


(defun parse-ecr-declaration (input-list)
   (setup-morph-features)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (ecr-name nil) (ecr-pre nil)
         (ecr-action nil) (features nil)
         (gde-dict-input t) (d-fileid t))
      (setf *current-item (pop *input-text))
      (setf ecr-name
         (parse-word "entry CR name"))
      (parse-literal '\: ": expected")
      (setf d-currentsym *current-item)
      (setf ecr-pre (d-parseprec))
      (d-musthave '=)
      (d-musthave '>)
      (setf ecr-action (d-parseskeleton))
      (when (eq *current-item '\,) (d-nextatom)
         (parse-literal (f-variable-name)
            "F variable name expected")
         (parse-literal (element-variable-name)
            "invalid set membership specification")
         (setf features (parse-set-body)))
      (list ecr-name
         (make-ec-rule :name ecr-name :precond
            ecr-pre :action ecr-action :features
            features :file
            (cond
               ((get ecr-name 'ecr)
                  (ec-rule-file (get ecr-name 'ecr)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


(defun input-mr nil
   (let
      ((input
          (prompt-if-necessary
             "Multiplication rule declaration? ")))
      (when input
         (insert-mr-declaration input))))


(defun insert-mr-declaration (input-list)
   (let
      ((parsed-mr
          (parse-mr-declaration input-list)))
      (when
         (check-redefinition 'mr *multiply-rules
            "multiplication rule" (car parsed-mr))
         (format t
            "~%Defining multiplication rule: ~A~%"
            (car parsed-mr))
         (setf (get (car parsed-mr) 'mr)
            (cadr parsed-mr))
         (setf *multiply-rules
            (insert-declaration-at-end *multiply-rules
               (car parsed-mr)))
         (input-word-invalidations *words
            'compiled))))


(defun parse-mr-declaration (input-list)
   (setup-morph-features)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (mr-name nil) (mr-pre nil) (mr-skels nil)
         (gde-dict-input t) (d-fileid t))
      (setf *current-item (pop *input-text))
      (setf mr-name
         (parse-word "multiplication rule name"))
      (parse-literal '\: ": expected")
      (setf d-currentsym *current-item)
      (setf mr-pre (d-parseprec))
      (d-musthave '=)
      (d-musthave '>)
      (d-musthave '>)
      (setf mr-skels (d-parseskeletons))
      (list mr-name
         (make-multiply-rule :name mr-name :precond
            mr-pre :skeletons (reverse mr-skels) :file
            (cond
               ((get mr-name 'mr)
                  (multiply-rule-file (get mr-name 'mr)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


(defun input-cc nil
   (let
      ((input
          (prompt-if-necessary
             "Consistency check declaration? ")))
      (when input
         (insert-cc-declaration input))))


(defun insert-cc-declaration (input-list)
   (let
      ((parsed-cc
          (parse-cc-declaration input-list)))
      (when
         (check-redefinition 'cc *cc-rules
            "consistency check" (car parsed-cc))
         (format t
            "~%Defining consistency check: ~A~%"
            (car parsed-cc))
         (setf (get (car parsed-cc) 'cc)
            (cadr parsed-cc))
         (setf *cc-rules
            (insert-declaration-at-end *cc-rules
               (car parsed-cc)))
         (input-word-invalidations *words
            'compiled))))


(defun parse-cc-declaration (input-list)
   (setup-morph-features)
   (let
      ((*input-text input-list)
         (*input-comments nil) (*current-item nil)
         (cc-name nil) (cc-pre nil)
         (cc-postcond nil) (gde-dict-input t)
         (d-fileid t))
      (setf *current-item (pop *input-text))
      (setf cc-name
         (parse-word "consistency check name"))
      (parse-literal '\: ": expected")
      (setf d-currentsym *current-item)
      (setf cc-pre (d-parseprec))
      (d-musthave (dk-demands))
      (setf cc-postcond (d-parseprec))
      (list cc-name
         (make-cc-rule :name cc-name :precond cc-pre
            :postcond cc-postcond :file
            (cond
               ((get cc-name 'cc)
                  (cc-rule-file (get cc-name 'cc)))
               (t (get-new-construct-file)))
            :comment *input-comments))))


(defun d-parseprec nil
   (let
      ((precond nil))
      (cond
         ((eq d-currentsym '\~) (d-musthave '\~)
            (setf precond
               (cons (list '\~ (d-parsepattern))
                  precond)))
         (t
            (setf precond
               (cons (d-parsepattern) precond))))
      (loop
         (cond
            ((null (eq d-currentsym (dk-and)))
               (return nil)))
         (d-musthave
                                ; replaced 'and's - only
                                ; difference from
                                ; D-ParsePrecondition
             (dk-and))
         (cond
            ((eq d-currentsym '\~) (d-musthave '\~)
               (setf precond
                  (cons (list '\~ (d-parsepattern))
                     precond)))
            (t
               (setf precond
                  (cons (d-parsepattern) precond)))))
      (reverse precond)))


;;; --- Edit command functions ---

(defun edit-ecr nil
   (let
      ((name
          (get-names-for-editing "Entry CR name? "
             *ec-rules "entry CRs" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-ecr-definition
                   (list name (get name 'ecr) nil))))
            (when text
               (insert-ecr-declaration text))))))


(defun edit-mr nil
   (let
      ((name
          (get-names-for-editing
             "Multiplication rule name? "
             *multiply-rules "multiplication rules"
             nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-mr-definition
                   (list name (get name 'mr) nil))))
            (when text
               (insert-mr-declaration text))))))


(defun edit-cc nil
   (let
      ((name
          (get-names-for-editing
             "Consistency check name? " *cc-rules
             "consistency checks" nil)))
      (when name
         (let
            ((text
                (get-edited-definition
                   'print-cc-definition
                   (list name (get name 'cc) nil))))
            (when text
               (insert-cc-declaration text))))))


;;; --- Delete command functions ---

(defun delete-ecr nil
   (let
      ((pattern
          (prompt-if-necessary "Entry CR? ")))
      (when pattern
         (let
            ((ecr-names (get-items *ec-rules pattern)))
            (cond
               (ecr-names (delete-ecr-list ecr-names))
               (t (gde-cerror "entry CR not found")))))))


(defun delete-ecr-list (ecr-names)
   (dolist (ecr ecr-names)
      (progn
         (print-ecr-definition ecr (get ecr 'ecr)
            nil)
         (when
            (yes-for-question
               "Delete entry CR definition")
            (delete-ecr-invalidations ecr))
         (terpri))))


(defun delete-ecr-invalidations (ecr)
   (setf *ec-rules
      (remove-list-1 ecr *ec-rules))
   (remprop ecr 'altered) (remprop ecr 'ecr)
   (input-word-invalidations *words
      'compiled))


(defun delete-mr nil
   (let
      ((pattern
          (prompt-if-necessary
             "Multiplication rule? ")))
      (when pattern
         (let
            ((mr-names
                (get-items *multiply-rules pattern)))
            (cond
               (mr-names (delete-mr-list mr-names))
               (t
                  (gde-cerror
                     "multiplication rule not found")))))))


(defun delete-mr-list (mr-names)
   (dolist (mr mr-names)
      (progn
         (print-mr-definition mr (get mr 'mr) nil)
         (when
            (yes-for-question
               "Delete multiplication rule definition")
            (delete-mr-invalidations mr))
         (terpri))))


(defun delete-mr-invalidations (mr)
   (setf *multiply-rules
      (remove-list-1 mr *multiply-rules))
   (remprop mr 'altered) (remprop mr 'mr)
   (input-word-invalidations *words
      'compiled))


(defun delete-cc nil
   (let
      ((pattern
          (prompt-if-necessary
             "Consistency check? ")))
      (when pattern
         (let
            ((cc-names (get-items *cc-rules pattern)))
            (cond
               (cc-names (delete-cc-list cc-names))
               (t
                  (gde-cerror
                     "consistency check not found")))))))


(defun delete-cc-list (cc-names)
   (dolist (cc cc-names)
      (progn
         (print-cc-definition cc (get cc 'cc) nil)
         (when
            (yes-for-question
               "Delete consistency check definition")
            (delete-cc-invalidations cc))
         (terpri))))


(defun delete-cc-invalidations (cc)
   (setf *cc-rules
      (remove-list-1 cc *cc-rules))
   (remprop cc 'altered) (remprop cc 'cc)
   (input-word-invalidations *words
      'compiled))


;;; --- Rename command functions ---

(defun move-ecr nil
   (let
      ((name
          (get-names-for-moving "Entry CR name? "
             *ec-rules "entry CRs" nil)))
      (if name
         (let
            ((input
                (prompt-if-necessary
                   "Change name to (return for same)? ")))
            (if input
               (cond
                  ((member (car input) *ec-rules)
                     (gde-ferror
"an entry completion rule with that name already exists"))
                  (t
                     (setf (get (car input) 'ecr)
                        (get name 'ecr))
                     (remprop name 'ecr)
                     (remprop name 'altered)
                     (rplaca (member name *ec-rules)
                        (car input))
                     (setf name (car input))
                     (setf (get name 'altered) t)
                     (setf (get name 'ecr)
                        (let
                           ((structure-23
                               (copy-ec-rule (get name 'ecr))))
                           (setf (ec-rule-name structure-23)
                              name)
                           structure-23)))))
            (setf (get name 'ecr)
               (let
                  ((structure-22
                      (copy-ec-rule (get name 'ecr))))
                  (setf (ec-rule-file structure-22)
                     (get-filename-for-moving
                        (ec-rule-file structure-22)))
                  structure-22))))))


(defun move-mr nil
   (let
      ((name
          (get-names-for-moving
             "Multiplication rule name? "
             *multiply-rules "multiplication rules"
             nil)))
      (if name
         (let
            ((input
                (prompt-if-necessary
                   "Change name to (return for same)? ")))
            (if input
               (cond
                  ((member (car input) *multiply-rules)
                     (gde-ferror
"a multiplication rule with that name already exists"))
                  (t
                     (setf (get (car input) 'mr)
                        (get name 'mr))
                     (remprop name 'mr) (remprop name 'altered)
                     (rplaca (member name *multiply-rules)
                        (car input))
                     (setf name (car input))
                     (setf (get name 'altered) t)
                     (setf (get name 'mr)
                        (let
                           ((structure-25
                               (copy-multiply-rule
                                  (get name 'mr))))
                           (setf
                              (multiply-rule-name structure-25)
                              name)
                           structure-25)))))
            (setf (get name 'mr)
               (let
                  ((structure-24
                      (copy-multiply-rule (get name 'mr))))
                  (setf (multiply-rule-file structure-24)
                     (get-filename-for-moving
                        (multiply-rule-file structure-24)))
                  structure-24))))))


(defun move-cc nil
   (let
      ((name
          (get-names-for-moving
             "Consistency check name? " *cc-rules
             "consistency checks" nil)))
      (if name
         (let
            ((input
                (prompt-if-necessary
                   "Change name to (return for same)? ")))
            (if input
               (cond
                  ((member (car input) *cc-rules)
                     (gde-ferror
"a consistency check with that name already exists"))
                  (t
                     (setf (get (car input) 'cc)
                        (get name 'cc))
                     (remprop name 'cc) (remprop name 'altered)
                     (rplaca (member name *cc-rules)
                        (car input))
                     (setf name (car input))
                     (setf (get name 'altered) t)
                     (setf (get name 'cc)
                        (let
                           ((structure-27
                               (copy-cc-rule (get name 'cc))))
                           (setf (cc-rule-name structure-27)
                              name)
                           structure-27)))))
            (setf (get name 'cc)
               (let
                  ((structure-26
                      (copy-cc-rule (get name 'cc))))
                  (setf (cc-rule-file structure-26)
                     (get-filename-for-moving
                        (cc-rule-file structure-26)))
                  structure-26))))))


;;; --- Names command functions ---

(defun names-ecr nil
   (let
      ((pattern
          (prompt-if-necessary "Entry CR? ")))
      (when pattern
         (let
            ((ecr-names (get-items *ec-rules pattern)))
            (cond
               (ecr-names
                  (write-construct-names "" ecr-names))
               (t (gde-cerror "entry CR not found")))))))


(defun names-mr nil
   (let
      ((pattern
          (prompt-if-necessary
             "Multiplication rule? ")))
      (when pattern
         (let
            ((mr-names
                (get-items *multiply-rules pattern)))
            (cond
               (mr-names
                  (write-construct-names "" mr-names))
               (t
                  (gde-cerror
                     "multiplication rule not found")))))))


(defun names-cc nil
   (let
      ((pattern
          (prompt-if-necessary
             "Consistency check? ")))
      (when pattern
         (let
            ((cc-names (get-items *cc-rules pattern)))
            (cond
               (cc-names
                  (write-construct-names "" cc-names))
               (t
                  (gde-cerror
                     "consistency check not found")))))))


;;; --- View command functions ---

(defun view-ecr nil
   (let
      ((pattern
          (prompt-if-necessary "Entry CR? ")))
      (when pattern
         (let
            ((ecr-names (get-items *ec-rules pattern)))
            (cond
               (ecr-names
                  (dolist (name ecr-names)
                     (progn
                        (terpri)
                        (print-ecr-definition name
                           (get name 'ecr) nil))))
               (t (gde-cerror "entry CR not found")))))))


(defun view-mr nil
   (let
      ((pattern
          (prompt-if-necessary
             "Multiplication rule? ")))
      (when pattern
         (let
            ((mr-names
                (get-items *multiply-rules pattern)))
            (cond
               (mr-names
                  (dolist (name mr-names)
                     (progn
                        (terpri)
                        (print-mr-definition name
                           (get name 'mr) nil))))
               (t
                  (gde-cerror
                     "multiplication rule not found")))))))


(defun view-cc nil
   (let
      ((pattern
          (prompt-if-necessary
             "Consistency check? ")))
      (when pattern
         (let
            ((cc-names (get-items *cc-rules pattern)))
            (cond
               (cc-names
                  (dolist (name cc-names)
                     (progn
                        (terpri)
                        (print-cc-definition name
                           (get name 'cc) nil))))
               (t
                  (gde-cerror
                     "consistency check not found")))))))


;;; --- Printing functions ---

(defun print-ecr-definition  (ecr-name ecr-definition identify)
   (when identify (princ "ENTRYCOMPLETION "))
   (format t "~A : ~%" ecr-name)
   (if (ec-rule-comment ecr-definition)
      (print-grammar-comment1 (ec-rule-comment ecr-definition)))
   (pp-print-precondition
      (ec-rule-precond ecr-definition))
   (format t "~%   => ~%   ")
   (pp-print-precondition1
      (ec-rule-action ecr-definition))
   (when (ec-rule-features ecr-definition)
      (princ ", F in ")
      (pp-print-entry-set
         (ec-rule-features ecr-definition)))
   (format t ".~%"))


(defun print-mr-definition (mr-name mr-definition identify)
   (when identify (princ "MULTIPLICATION "))
   (format t "~A : ~%" mr-name)
   (if (multiply-rule-comment mr-definition)
      (print-grammar-comment1
         (multiply-rule-comment mr-definition)))
   (pp-print-precondition
      (multiply-rule-precond mr-definition))
   (format t "~%   =>> ~%   (")
   (dolist
      (skel
         (multiply-rule-skeletons mr-definition))
      (format t "~%    ")
      (pp-print-precondition1 skel))
   (format t ").~%"))


(defun print-cc-definition (cc-name cc-definition identify)
   (when identify (princ "CONSISTENCY "))
   (format t "~A : ~%" cc-name)
   (if (cc-rule-comment cc-definition)
      (print-grammar-comment1
         (cc-rule-comment cc-definition)))
   (pp-print-precondition
      (cc-rule-precond cc-definition))
   (format t "~%   ~A~%   " (dk-demands))
   (pp-print-precondition
      (cc-rule-postcond cc-definition))
   (format t ".~%"))


(defun pp-print-precondition (pre)
   (cond
      ((null pre))
      (t (pp-print-precondition1 (car pre))
         (cond
            ((cdr pre)
               (format t " ~A " (dk-and))
               (pp-print-precondition (cdr pre)))))))


(defun pp-print-precondition1 (pre)
   (cond
      ((atom pre) (princ pre))
      ((eq (car pre) '\~)
         (princ "~")
         (pp-print-precondition1 (cadr pre)))
      (t (princ "(")
         (pp-print-precondition1 (car pre))
         (pp-print-precondition-tail (cdr pre))
         (princ ")"))))


(defun pp-print-precondition-tail
   (pre-tail)
   (cond
      ((null pre-tail))
      (t (princ " ")
         (pp-print-precondition1
            (car pre-tail))
         (pp-print-precondition-tail
            (cdr pre-tail)))))


(defun pp-print-entry-set (features)
   (princ "{")
   (loop
      (cond
         ((null features) (return nil)))
      (princ (pop features))
      (when features (princ ", ")))
   (princ "}"))


;;; --- Ordering functions ---

(defun order-ecrs nil
   (cond
      (*ec-rules
         (let
            ((rule-ordering *ec-rules))
            (order-top-loop)
            (setf *ec-rules rule-ordering)
            (input-word-invalidations *words
               'compiled)))
      (t (gde-cerror "no entry CRs defined"))))


(defun order-mrs nil
   (cond
      (*multiply-rules
         (let
            ((rule-ordering *multiply-rules))
            (order-top-loop)
            (setf *multiply-rules rule-ordering)
            (input-word-invalidations *words
               'compiled)))
      (t
         (gde-cerror
            "no multiplication rules defined"))))


(defun order-ccs nil
   (cond
      (*cc-rules
         (let
            ((rule-ordering *cc-rules))
            (order-top-loop)
            (setf *cc-rules rule-ordering)))
      (t
         (gde-cerror
            "no consistency checks defined"))))


;;; --- Morpheme viewing ---

;;; Return morpheme definitions from the MAP lexicon.

(defun view-morpheme nil
   (cond
      (*morph-system (d-load)
         (let
            ((pattern
                (prompt-if-necessary "Morpheme? ")))
            (when pattern
               (let
                  ((morpheme-names
                      (cond
                         ((intersection '(* ?) pattern :test
                             #'equal)
                            (get-items
                               (morphemes-in-dictionary)
                               (canonise-word pattern)))
                         (t (canonise-word pattern)))))
                  (unless
                     (mapcan
                        #'(lambda (name)
                             (let
                                ((var-28
                                    (mapcar
                                       #'(lambda (def)
                                            (let
                                               ((*print-escape*
                                                   t))
                                               (progn
                                                  (write def
                                                     :pretty t)
                                                  (terpri))
                                               t))
                                       (d-morpheme name))))
                                (if var-28 (ncons var-28))))
                        morpheme-names)
                     (gde-cerror "morpheme not found"))))))
      (t
         (gde-cerror
            "morphology system flag is OFF"))))


(defun names-morpheme nil
   (cond
      (*morph-system (d-load)
         (let
            ((pattern
                (prompt-if-necessary "Morpheme? ")))
            (when pattern
               (let
                  ((morphemes
                      (get-items (morphemes-in-dictionary)
                         (canonise-word pattern))))
                  (cond
                     ((atom morphemes)
                        (gde-cerror "morpheme not found"))
                     (t
                        (write-construct-names ""
                           morphemes)))))))
      (t
         (gde-cerror
            "morphology system flag is OFF"))))


;;; Return names of all morphemes in morphology system lexicon.

(defun morphemes-in-dictionary nil
   (mapcan
      #'(lambda (lex)
           (nreverse
              (mapcan
                 #'(lambda (branch)
                      (morphemes-in-dictionary1 branch nil))
                 (cdr lex))))
      d-lexicon))


(defun morphemes-in-dictionary1
   (tree part-word)
   (cond
      ((eq (car tree) (dk-endtree))
         (ncons
            (concatl-symbol (reverse part-word))))
      (t
         (mapcan
            #'(lambda (branch)
                 (morphemes-in-dictionary1 branch
                    (cons (car tree) part-word)))
            (cdr tree)))))


;;; End of file

