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

;;; GRAMMAR DEVELOPEMT ENVIRONMENT - DICTIONARY PATCHES
;;;
;;; Author: John Carroll
;;;
;;; To get round casing problems, find morphololgy system files
;;; in a set place, steer round problems to do with features
;;; aliases etc perhaps being declared both in morph files and
;;; GDE.

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Read in all components of the dictionary system if they are
;;; not there already. Declarations made in the GDE when the
;;; words grammar or lexicon files were compiled may since have
;;; been changed (i.e. permissable feature values). If an error
;;; occurs because of this, the files should be re-compiled.

(defun d-load nil
   (cond
      ((not (= (length d-loadedparts) 3))
         (princ "Loading ") (d-versionheading)
         (finish-output)
         (cond
            ((not (assoc 'sp d-loadedparts))
               (d-loadsprules *morph-system)))
         (cond
            ((not (assoc 'gr d-loadedparts))
               (d-loadwordgrammar *morph-system)))
         (cond
            ((not (assoc 'di d-loadedparts))
               (d-loadlexicon *morph-system)))
         (format t "Loaded~%")
         (finish-output))))


(defun load-morph-invalidations nil
   (dolist (word *cached-words)
      (progn
         (remprop word 'word)
         (remprop word 'compiled-word)))
   (setf *cached-words nil)
   (setf *generator-words nil))


(embed d-loadsprules
   (lambda (name)
       (d-loadsprules name)
       (fmakunbound 'd-buildsprules)
       (load-morph-invalidations)))


(embed d-loadwordgrammar
   (lambda (name)
       (d-loadwordgrammar name)
       (load-morph-invalidations)))


(embed d-loadlexicon
   (lambda (name)
        (d-loadlexicon name)
        (load-morph-invalidations)))


;;; Invalidate GDE cached data when adding a lexicon

(embed d-addlexicon
   (lambda (name)
       (d-addlexicon name)
       (load-morph-invalidations)))

(defun d-unload nil (d-markunload 'di)
   (d-markunload 'gr) (d-markunload 'sp))


;;; The init globals functions clobber variables holding
;;; features, variables etc, just before compilation of a MAP
;;; component. The MAP assumes the features etc are declared in
;;; the file begin compiled. Allow the GDE to give these
;;; definitions if compilation not called from the Dictionary
;;; Top Loop.

;;; Relevant variables are D-FEATURES, D-ALIASES, D-LCATEGORIES,
;;; D-WHEAD, D-WDAUGHTER, D-MORPHOLOGYONLY.

(embed d-initgramglobals
   (lambda nil
       (d-initgramglobals)
       (cond
          ((not *inside-dci)
             (setup-morph-features)))))


(embed d-initlexglobals
   (lambda nil
       (d-initlexglobals)
       (cond
          ((not *inside-dci)
             (setup-morph-features)))))


;;; Parsing of morphology system constructs within the GDE - use
;;; Consume-dict-item instead of D-NextAtom.

(embed d-nextatom
   (lambda nil
       (cond
          (gde-dict-input (consume-item))
          (t (d-nextatom)))))


;;; If a stream reading an entry file has been closed during a previous
;;; image save, re-open the stream before attempting to access any
;;; morpheme in lexicon

(embed d-lookupdict
   (lambda (word)
      (ensure-lexicon-streams-open)
      (d-lookupdict word)))


(embed d-recog
   (lambda (word)
      (ensure-lexicon-streams-open)
      (d-recog word)))


(defun ensure-lexicon-streams-open nil
   (dolist (lex d-lexicon)
      (when (stringp (car lex))
         (let
            ((entry-file (car lex)))
            (unless (probe-file entry-file)
               (gde-ferror
"a lexicon stream is no longer open and cannot be re-opened: reload lexicons"))
            (gde-warn "re-opening lexicon file " entry-file)
            (setf (car lex) (open entry-file :direction :input))))))
         

;;; End of file
