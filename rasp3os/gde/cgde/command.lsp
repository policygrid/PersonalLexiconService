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

;;; GRAMMAR DEVELOPMENT ENVIRONMENT - COMMAND TABLES
;;;
;;; Author: John Carroll
;;;
;;; This file contains the command tables used by the toploop to
;;; process user commands to the GDE. A command table consists
;;; of a sequence of command-entry records, one for each command
;;; recognised.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Command table for grammar item input.

(setf *input-commands
   (list
      (make-command-entry :shortest 1 :name
         'feature :action '(input-feature))
      (make-command-entry :shortest 1 :name 'set
         :action '(input-set))
      (make-command-entry :shortest 1 :name
         'alias :action '(input-alias))
      (make-command-entry :shortest 2 :name
         'category :action '(input-category 'nil))
      (make-command-entry :shortest 2 :name
         'lcategory :action '(input-category 't))
      (make-command-entry :shortest 2 :name
         'extension :action '(input-extension))
      (make-command-entry :shortest 1 :name
         'top :action '(input-top))
      (make-command-entry :shortest 1 :name
         'idrule :action '(input-idrule))
      (make-command-entry :shortest 2 :name
         'psrule :action '(input-psrule))
      (make-command-entry :shortest 2 :name
         'metarule :action '(input-metarule))
      (make-command-entry :shortest 1 :name
         'defrule :action '(input-defrule))
      (make-command-entry :shortest 2 :name
         'proprule :action '(input-proprule))
      (make-command-entry :shortest 2 :name
         'lprule :action '(input-lprule))
      (make-command-entry :shortest 2 :name
         'entrycompletion :action '(input-ecr))
      (make-command-entry :shortest 2 :name
         'multiplication :action '(input-mr))
      (make-command-entry :shortest 3 :name
         'consistency :action '(input-cc))
      (make-command-entry :shortest 1 :name 'word
         :action '(input-word))
      (make-command-entry :shortest 3 :name
         'comment :action '(add-grammar-comment))))


;;; Command table for grammar item editing.

(setf *edit-commands
   (list
      (make-command-entry :shortest 1 :name
         'feature :action '(edit-feature))
      (make-command-entry :shortest 1 :name 'set
         :action '(edit-set))
      (make-command-entry :shortest 1 :name
         'alias :action '(edit-alias))
      (make-command-entry :shortest 2 :name
         'category :action '(edit-category 'nil))
      (make-command-entry :shortest 2 :name
         'lcategory :action '(edit-category 't))
      (make-command-entry :shortest 2 :name
         'extension :action '(edit-extension))
      (make-command-entry :shortest 1 :name
         'top :action '(edit-top))
      (make-command-entry :shortest 1 :name
         'idrule :action '(edit-idrule 'nil))
      (make-command-entry :shortest 2 :name
         'psrule :action '(edit-idrule 't))
      (make-command-entry :shortest 2 :name
         'metarule :action '(edit-metarule))
      (make-command-entry :shortest 1 :name
         'defrule :action '(edit-defrule))
      (make-command-entry :shortest 2 :name
         'proprule :action '(edit-proprule))
      (make-command-entry :shortest 2 :name
         'lprule :action '(edit-lprule))
      (make-command-entry :shortest 2 :name
         'entrycompletion :action '(edit-ecr))
      (make-command-entry :shortest 2 :name
         'multiplication :action '(edit-mr))
      (make-command-entry :shortest 3 :name
         'consistency :action '(edit-cc))
      (make-command-entry :shortest 1 :name 'word
         :action '(edit-word))))


;;; Command table for grammar item renaming.

(setf *move-commands
   (list
      (make-command-entry :shortest 1 :name
         'feature :action '(move-feature))
      (make-command-entry :shortest 1 :name 'set
         :action '(move-set))
      (make-command-entry :shortest 1 :name
         'alias :action '(move-alias))
      (make-command-entry :shortest 2 :name
         'category :action '(move-category 'nil))
      (make-command-entry :shortest 2 :name
         'lcategory :action '(move-category 't))
      (make-command-entry :shortest 2 :name
         'extension :action '(move-extension))
      (make-command-entry :shortest 1 :name
         'top :action '(move-top))
      (make-command-entry :shortest 1 :name
         'idrule :action '(move-idrule 'nil))
      (make-command-entry :shortest 2 :name
         'psrule :action '(move-idrule 't))
      (make-command-entry :shortest 2 :name
         'metarule :action '(move-metarule))
      (make-command-entry :shortest 1 :name
         'defrule :action '(move-defrule))
      (make-command-entry :shortest 2 :name
         'proprule :action '(move-proprule))
      (make-command-entry :shortest 2 :name
         'lprule :action '(move-lprule))
      (make-command-entry :shortest 2 :name
         'entrycompletion :action '(move-ecr))
      (make-command-entry :shortest 2 :name
         'multiplication :action '(move-mr))
      (make-command-entry :shortest 3 :name
         'consistency :action '(move-cc))
      (make-command-entry :shortest 1 :name 'word
         :action '(move-word))))


;;; Command tables for grammar item viewing.

(setf *view-normalised-linearised-commands
   (list
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(view-idrule 'nil 'normalised 'linearised))
      (make-command-entry :shortest 1 :name
         'psrule :action
         '(view-idrule 't 'normalised 'linearised))
      (make-command-entry :shortest 1 :name
         'fully-instantiated :action
         '(process-command-option "Construct type? "
            *view-normalised-linearised-commands))))


(setf *view-normalised-full-commands
   (list
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(view-idrule 'nil 'normalised 'full))
      (make-command-entry :shortest 1 :name
         'psrule :action
         '(view-idrule 't 'normalised 'full))
      (make-command-entry :shortest 1 :name 'word
         :action '(view-word 'normalised 'full))
      (make-command-entry :shortest 1 :name
         'linearised :action
         '(process-command-option "Construct type? "
             *view-normalised-linearised-commands))))


(setf *view-linearised-commands
   (list
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(view-idrule 'nil 'aliased 'linearised))
      (make-command-entry :shortest 1 :name
         'psrule :action
         '(view-idrule 't 'aliased 'linearised))
      (make-command-entry :shortest 1 :name
         'normalised :action
         '(process-command-option "Construct type? "
             *view-normalised-linearised-commands))
      (make-command-entry :shortest 1 :name
         'fully-instantiated :action
         '(process-command-option "Construct type? "
            *view-linearised-commands))))


(setf *view-full-commands
   (list
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(view-idrule 'nil 'aliased 'full))
      (make-command-entry :shortest 1 :name
         'psrule :action
         '(view-idrule 't 'aliased 'full))
      (make-command-entry :shortest 1 :name 'word
         :action '(view-word 'aliased 'full))
      (make-command-entry :shortest 1 :name
         'normalised :action
         '(process-command-option "Construct type? "
             *view-normalised-full-commands))
      (make-command-entry :shortest 1 :name
         'linearised :action
         '(process-command-option "Construct type? "
             *view-linearised-commands))))


(setf *view-normalised-commands
   (list
      (make-command-entry :shortest 2 :name
         'category :action
         '(view-category 'nil 'normalised))
      (make-command-entry :shortest 2 :name
         'lcategory :action
         '(view-category 't 'normalised))
      (make-command-entry :shortest 1 :name
         'alias :action '(view-alias 'normalised))
      (make-command-entry :shortest 2 :name
         'extension :action
         '(view-extension 'normalised))
      (make-command-entry :shortest 1 :name
         'top :action '(view-top 'normalised))
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(view-idrule 'nil 'normalised 'basic))
      (make-command-entry :shortest 2 :name
         'psrule :action
         '(view-idrule 't 'normalised 'basic))
      (make-command-entry :shortest 2 :name
         'metarule :action
         '(view-metarule 'normalised))
      (make-command-entry :shortest 1 :name
         'defrule :action
         '(view-defrule 'normalised))
      (make-command-entry :shortest 2 :name
         'proprule :action
         '(view-proprule 'normalised))
      (make-command-entry :shortest 2 :name
         'lprule :action '(view-lprule 'normalised))
      (make-command-entry :shortest 2 :name
         'entrycompletion :action '(view-ecr))
      (make-command-entry :shortest 2 :name
         'multiplication :action '(view-mr))
      (make-command-entry :shortest 2 :name
         'consistency :action '(view-cc))
      (make-command-entry :shortest 1 :name 'word
         :action '(view-word 'normalised 'basic))
      (make-command-entry :shortest 1 :name
         'fully-instantiated :action
         '(process-command-option "Construct type? "
             *view-normalised-full-commands))
      (make-command-entry :shortest 2 :name
         'linearised :action
         '(process-command-option "Construct type? "
             *view-normalised-linearised-commands))))


(setf *view-commands
   (list
      (make-command-entry :shortest 2 :name
         'feature :action '(view-feature))
      (make-command-entry :shortest 1 :name 'set
         :action '(view-set))
      (make-command-entry :shortest 2 :name
         'category :action
         '(view-category 'nil 'aliased))
      (make-command-entry :shortest 2 :name
         'lcategory :action
         '(view-category 't 'aliased))
      (make-command-entry :shortest 1 :name
         'alias :action '(view-alias 'aliased))
      (make-command-entry :shortest 2 :name
         'extension :action
         '(view-extension 'aliased))
      (make-command-entry :shortest 1 :name
         'top :action '(view-top 'aliased))
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(view-idrule 'nil 'aliased 'basic))
      (make-command-entry :shortest 2 :name
         'psrule :action
         '(view-idrule 't 'aliased 'basic))
      (make-command-entry :shortest 2 :name
         'metarule :action
         '(view-metarule 'aliased))
      (make-command-entry :shortest 1 :name
         'defrule :action '(view-defrule 'aliased))
      (make-command-entry :shortest 2 :name
         'proprule :action
         '(view-proprule 'aliased))
      (make-command-entry :shortest 2 :name
         'lprule :action '(view-lprule 'aliased))
      (make-command-entry :shortest 2 :name
         'entrycompletion :action '(view-ecr))
      (make-command-entry :shortest 2 :name
         'multiplication :action '(view-mr))
      (make-command-entry :shortest 3 :name
         'consistency :action '(view-cc))
      (make-command-entry :shortest 1 :name 'word
         :action '(view-word 'aliased 'basic))
      (make-command-entry :shortest 2 :name
         'morpheme :action '(view-morpheme))
      (make-command-entry :shortest 1 :name
         'normalised :action
         '(process-command-option
             "FU/LI/Construct type? " *view-normalised-commands))
      (make-command-entry :shortest 2 :name
         'fully-instantiated :action
         '(process-command-option
             "N/Construct type? " *view-full-commands))
      (make-command-entry :shortest 2 :name
         'linearised :action
         '(process-command-option
             "N/Construct type? " *view-linearised-commands))
      (make-command-entry :shortest 3 :name
         'comment :action '(print-grammar-comment))
      (make-command-entry :shortest 3 :name 'all
         :action '(view-all-constructs))))


;;; Command table for printing names of grammar items.

(setf *names-normalised-linearised-commands
   (list
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(names-idrule 'nil 'normalised 'linearised))
      (make-command-entry :shortest 1 :name
         'psrule :action
         '(names-idrule 't 'normalised 'linearised))))


(setf *names-normalised-commands
   (list
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(names-idrule 'nil 'normalised 'basic))
      (make-command-entry :shortest 1 :name
         'psrule :action
         '(names-idrule 't 'normalised 'basic))
      (make-command-entry :shortest 1 :name
         'linearised :action
         '(process-command-option "Construct type? "
             *names-normalised-linearised-commands))))


(setf *names-linearised-commands
   (list
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(names-idrule 'nil 'aliased 'linearised))
      (make-command-entry :shortest 2 :name
         'psrule :action
         '(names-idrule 't 'aliased 'linearised))
      (make-command-entry :shortest 1 :name
         'normalised :action
         '(process-command-option "Construct type? "
             *names-normalised-linearised-commands))))


(setf *names-commands
   (list
      (make-command-entry :shortest 1 :name
         'feature :action '(names-feature))
      (make-command-entry :shortest 1 :name 'set
         :action '(names-set))
      (make-command-entry :shortest 2 :name
         'category :action '(names-category 'nil))
      (make-command-entry :shortest 2 :name
         'lcategory :action '(names-category 't))
      (make-command-entry :shortest 1 :name
         'alias :action '(names-alias))
      (make-command-entry :shortest 1 :name
         'idrule :action
         '(names-idrule 'nil 'aliased 'basic))
      (make-command-entry :shortest 2 :name
         'psrule :action
         '(names-idrule 't 'aliased 'basic))
      (make-command-entry :shortest 2 :name
         'metarule :action '(names-metarule))
      (make-command-entry :shortest 1 :name
         'defrule :action '(names-defrule))
      (make-command-entry :shortest 2 :name
         'proprule :action '(names-proprule))
      (make-command-entry :shortest 1 :name
         'lprule :action '(names-lprule))
      (make-command-entry :shortest 1 :name
         'entrycompletion :action '(names-ecr))
      (make-command-entry :shortest 2 :name
         'multiplicaton :action '(names-mr))
      (make-command-entry :shortest 2 :name
         'consistency :action '(names-cc))
      (make-command-entry :shortest 1 :name 'word
         :action '(names-word))
      (make-command-entry :shortest 2 :name
         'morpheme :action '(names-morpheme))
      (make-command-entry :shortest 1 :name
         'normalised :action
         '(process-command-option
             "LI/Construct type? " *names-normalised-commands))
      (make-command-entry :shortest 2 :name
         'linearised :action
         '(process-command-option
             "N/Construct type? " *names-linearised-commands))
      (make-command-entry :shortest 3 :name 'all
         :action '(names-all-constructs))))


;;; Command table for grammar item deletion.

(setf *delete-commands
   (list
      (make-command-entry :shortest 1 :name
         'feature :action '(delete-feature))
      (make-command-entry :shortest 1 :name 'set
         :action '(delete-set))
      (make-command-entry :shortest 1 :name
         'alias :action '(delete-alias))
      (make-command-entry :shortest 2 :name
         'category :action '(delete-category))
      (make-command-entry :shortest 2 :name
         'lcategory :action '(delete-category))
      (make-command-entry :shortest 2 :name
         'extension :action '(delete-extension))
      (make-command-entry :shortest 1 :name
         'top :action '(delete-top))
      (make-command-entry :shortest 1 :name
         'idrule :action '(delete-idrule 'nil))
      (make-command-entry :shortest 2 :name
         'psrule :action '(delete-idrule 't))
      (make-command-entry :shortest 2 :name
         'metarule :action '(delete-metarule))
      (make-command-entry :shortest 1 :name
         'defrule :action '(delete-defrule))
      (make-command-entry :shortest 2 :name
         'proprule :action '(delete-proprule))
      (make-command-entry :shortest 1 :name
         'lprule :action '(delete-lprule))
      (make-command-entry :shortest 2 :name
         'entrycompletion :action '(delete-ecr))
      (make-command-entry :shortest 2 :name
         'multiplication :action '(delete-mr))
      (make-command-entry :shortest 3 :name
         'consistency :action '(delete-cc))
      (make-command-entry :shortest 1 :name 'word
         :action '(delete-word))
      (make-command-entry :shortest 3 :name
         'comment :action '(remove-grammar-comment))
      (make-command-entry :shortest 3 :name 'all
         :action '(delete-all-constructs))))


;;; Command table for flag setting.

(setf *set-commands
   (list
      (make-command-entry :shortest 2 :name 'width
         :action '(set-width-of-terminal))
      (make-command-entry :shortest 1 :name 'defining
         :action '(set-defining-messages))
      (make-command-entry :shortest 2 :name 'warning
         :action '(set-warning-messages))
      (make-command-entry :shortest 1 :name
         'propagation :action
         '(set-prop-before-default))
      (make-command-entry :shortest 1 :name
         'addition :action '(set-addition-checking))
      (make-command-entry :shortest 2 :name
         'mlinearisations :action
         '(set-multiple-linearisations))
      (make-command-entry :shortest 2 :name
         'mexpansions :action
         '(set-multiple-expansions))
      #+gde-morph
      (make-command-entry :shortest 2 :name
         'morphology :action
         '(set-morphology-system))
      #+gde-morph
      (make-command-entry :shortest 2 :name
         'wordstructure :action
         '(set-word-structure))
      #+gde-morph
      (make-command-entry :shortest 1 :name 'fast
         :action '(set-fast-morph-lookup))
      #+gde-morph
      (make-command-entry :shortest 1 :name 'ecrs
         :action '(set-ecrs-before-multiply))
      (make-command-entry :shortest 2 :name 'tagged
         :action '(set-tagged-words))
      (make-command-entry :shortest 2 :name 'term
         :action '(set-term-unification))
      (make-command-entry :shortest 1 :name
         'lr1parse :action '(set-lr1-parse))
      (make-command-entry :shortest 1 :name
         'showparses :action '(set-show-parses))))


;;; Command table for ordering grammar items.

(setf *order-commands
   (list
      (make-command-entry :shortest 1 :name
         'feature :action '(order-features))
      (make-command-entry :shortest 1 :name 'set
         :action '(order-sets))
      (make-command-entry :shortest 2 :name
         'category :action '(order-categories 'nil))
      (make-command-entry :shortest 2 :name
         'lcategory :action '(order-categories 't))
      (make-command-entry :shortest 1 :name
         'alias :action '(order-aliases))
      (make-command-entry :shortest 1 :name
         'idrule :action '(order-idrules))
      (make-command-entry :shortest 2 :name
         'psrule :action '(order-idrules))
      (make-command-entry :shortest 2 :name
         'metarule :action '(order-metarules))
      (make-command-entry :shortest 2 :name
         'proprule :action '(order-proprules))
      (make-command-entry :shortest 1 :name
         'defrule :action '(order-defrules))
      (make-command-entry :shortest 2 :name
         'lprule :action '(order-lprules))
      (make-command-entry :shortest 1 :name
         'entrycompletion :action '(order-ecrs))
      (make-command-entry :shortest 2 :name
         'multiplication :action '(order-mrs))
      (make-command-entry :shortest 2 :name
         'consistency :action '(order-ccs))
      (make-command-entry :shortest 1 :name 'word
         :action '(order-words))))


;;; Command tables for dumping grammar and words to file.

(setf *dump-grammar-commands
   (list
      (make-command-entry :shortest 1 :name
         'readable :action
         '(dump-grammar-to-file 't))
      (make-command-entry :shortest 1 :name
         'unreadable :action
         '(dump-grammar-to-file 'nil))))


(setf *dump-word-commands
   (list
      (make-command-entry :shortest 1 :name
         'readable :action
         '(dump-word-definitions-to-file 't))
      (make-command-entry :shortest 1 :name
         'unreadable :action
         '(dump-word-definitions-to-file 'nil))))


;;; Top level command table.

(setf *top-level-commands
   (list
      (make-command-entry :shortest 1 :name
         'input :action
         '(process-command-option "Construct type? "
             *input-commands))
      (make-command-entry :shortest 1 :name 'edit
         :action
         '(process-command-option "Construct type? "
             *edit-commands))
      (make-command-entry :shortest 1 :name 'view
         :action
         '(process-command-option
             "N/FU/LI/Construct type? " *view-commands))
      (make-command-entry :shortest 1 :name
         'names :action
         '(process-command-option
             "N/LI/Construct type? " *names-commands))
      (make-command-entry :shortest 1 :name
         'delete :action
         '(process-command-option "Construct type? "
             *delete-commands))
      (make-command-entry :shortest 1 :name
         'parse :action '(parse-sentences))
      (make-command-entry :shortest 1 :name
         'generate :action '(generate-sentences))
      (make-command-entry :shortest 1 :name 'set
         :action
         '(process-command-option "Flag? " *set-commands))
      (make-command-entry :shortest 1 :name
         'order :action
         '(process-command-option "Construct type? "
             *order-commands))
      (make-command-entry :shortest 2 :name
         'flags :action '(show-flags))
      (make-command-entry :shortest 2 :name
         'compile :action '(compile-world))
      (make-command-entry :shortest 2 :name
         'cdictionary :action '(compile-dictionary))
      (make-command-entry :shortest 2 :name
         'cspelling :action '(compile-spelling))
      (make-command-entry :shortest 2 :name
         'cwordgrammar :action
         '(compile-word-grammar))
      (make-command-entry :shortest 2 :name
         'clear :action '(clear-whole-grammar))
      (make-command-entry :shortest 1 :name
         'uncache :action '(clear-cached-grammar 't))
      (make-command-entry :shortest 1 :name 'help
         :action '(give-gde-help))
      (make-command-entry :shortest 1 :name '?
         :action '(give-gde-help))
      (make-command-entry :shortest 2 :name
         'files :action '(show-files))
      (make-command-entry :shortest 2 :name
         'forget :action '(forget-file))
      (make-command-entry :shortest 1 :name 'read
         :action '(read-grammar-file))
      (make-command-entry :shortest 1 :name
         'write :action '(write-grammar-file))
      (make-command-entry :shortest 1 :name 'move
         :action
         '(process-command-option "Construct type? "
             *move-commands))
      (make-command-entry :shortest 2 :name
         'fwords :action '(lookup-words-in-file))
      #+gde-morph
      (make-command-entry :shortest 3 :name 'dci
         :action '(dict-top-loop))
      (make-command-entry :shortest 2 :name 'dump
         :action
         '(process-command-option
             "Readable/Unreadable? " *dump-grammar-commands))
      (make-command-entry :shortest 2 :name
         'dwords :action
         '(process-command-option
             "Readable/Unreadable? " *dump-word-commands))
      (make-command-entry :shortest 1 :name '\!
         :action '(process-lisp-input))
      (make-command-entry :shortest 2 :name
         'shell :action '(shell))
      (make-command-entry :shortest 1 :name 'lisp
         :action '(lisp-top-loop))))


;;; End of file
