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

;;; Augment prob parser with info about phrasal verbs - prefer parse
;;; possibilities where a phrasal verb and one of its possible particles
;;; are analysed as such
;;;
;;; (read-phrasal-verbs "internal:phrasal-vle")

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

;;;

(defvar *phrasal-verbs-p* t)

(defparameter +phrasal-verb-rules+
   '(
("V/v_prt" . 2)
("V1/v_np_prt" . 3)
("V1/v_np-pro_prt" . 3)
("V1/v_np-name_prt" . 3)
("V1/v_np-tit_prt" . 3)
("V1/v_np-pl_prt" . 3)
("V1/v_np-org_prt" . 3)
("V1/v_np-refl_prt" . 3)
("V1/v_n1_prt" . 3)
("V1/v_n1-tit_prt" . 3)
("V1/v_np-ms_prt" . 3)
("V1/v_np-num_prt" . 3)
("V1/v_np_prt_np" . 3)
("V1/v_np_prt_n1" . 3)
("V1/v_n1_prt_np" . 3)
("V1/v_np-pro_prt_np" . 3)
("V1/v_np-name_prt_np" . 3)
("V1/v_np-pl_prt_np" . 3)
("V1/v_np-org_prt_np" . 3)
("V1/v_np-refl_prt_np" . 3)
("V1/v_np_np_prt" . 4)
("V1/v_np_prt_inf" . 3)
("V1/v_np_prt_ing" . 3)
("V1/v_np_prt_pp" . 3)
("V1/v_np_prt_s" . 3)
("V1/v_np_prt_ap" . 3)
("V1/be_prt" . 2)
   ))

(defparameter +n-ppart-rule-names+
   '(
"N1/n_ppart"
"N1/n-nt_ppart"
"N1/n-pl_ppart"
"N1/n-org_ppart"
"N1/n-ms_ppart"
   ))


(defun read-phrasal-verbs (file)
   (with-open-file (in file :direction :input)
      (loop
         (let ((form (read in nil t)))
            (when (eq form t) (return file))
            (do*
               ((tail form (cdr tail))
                (verbs nil))
               ((or (stringp (car tail)) (null tail))
                  (dolist (v verbs)
                     (when (get v 'phrasal-verb-particles)
                        (warn "Verb `~A' already has particle list - adding" v))
                     (dolist (p tail)
                        (pushnew p (get v 'phrasal-verb-particles) :test #'equal))))
               (push (car tail) verbs))))))

;;; Redefinition from dparse.lsp

(defun lr1-compute-word-categories (word)
   (when (eq word *sentence-end-marker*)
      (return-from lr1-compute-word-categories
         (list (list (position *sentence-end-marker* (the list *terminals*)
                        :test #'eq)))))
   (multiple-value-bind (defns probs)
       (g-defns word)
      (do*
         ((defn (pop defns) (pop defns))
            (prob (or (pop probs) 0.0)
                  (or (pop probs)
                      (if (and defn (find 'PRT (caar defn)) (search "_I" (string word)))
                         (log 0.1 10) ; disprefer PRT if tagged as preposition
                         0.0)))
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


;;; Called from lr1-parse-unpack-score when *phrasal-verbs-p* is true.
;;; When we have a verb-particle rule, if the verb is a phrasal verb
;;; then if the particle is known for this verb boost score otherwise
;;; depress it. Depress score if verb not known to be a phrasal verb

(defun lr1-parse-unpack-score-phrasal (tree rname old-score &aux entry)
   (when (member rname +n-ppart-rule-names+ :test #'equal)
      #+ignore 
      (print (list '*** rname
                   (if (atom (cadr (car (last (cdr tree)))))
                      (cadr (car (last (cdr tree))))
                      (car (last (caar (last (cdr tree))))))))
      (return-from lr1-parse-unpack-score-phrasal (+ old-score 2.0)))
   (if
      (and (atom (cadr (car (last (cdr tree))))) ; lexical 1st daughter
           (setq entry (assoc rname +phrasal-verb-rules+ :test #'equal)))
      (let*
         ((verb (string (cadr (car (last (cdr tree))))))
          (vstem (find-symbol (lr1-parse-unpack-stem verb))))
         ;; (setq cc (list vstem rname))
         (if vstem
            (let*
               ((ndaughters (length (cdr tree)))
                (prt-pos
                   (cdr entry))
                (index ; in reversed list of daughters
                   (- ndaughters prt-pos))
                (particle
                   (string (cadr (nth index (cdr tree)))))
                (pstem (lr1-parse-unpack-stem particle)))
               ;; (setq dd (list particle pstem))
               (let*
                  ((entry (get vstem 'phrasal-verb-particles))
                   (found
                      (member pstem entry :test #'string-equal)))
                  ;; (print (list verb vstem rname particle pstem entry))
                  (if found
                     (progn
                        #+ignore (print (list '+++ verb rname particle))
                        ;; (setq ee (list vstem old-score new-score))
                        (+ old-score 1.0))
                     (progn
                        #+ignore (print (list '--- verb rname particle))
                        (+ old-score -1.0)))))
            (progn
               #+ignore (print (list '--- verb rname particle))
               (+ old-score -1.0))))
      old-score))


#|
;; ignores subcat values, multiple particles

gzcat ~johnca/dict/v.le.gz | fgrep 'PRT' | gawk '\
   function printdata () {\
      printf("(|%s| . ( ",prev); \
      for (p in prts) {printf("\042%s\042 ",p); delete prts[p]}; \
      printf("))\n")} \
  {verb=substr($1,2); \
   if (verb!=prev && prev!="") {printdata()}; \
   p=$0; sub(/^.*PRT /,"",p); sub(/[)] .*$/,"",p); \
   sub(/_.*$/,"",p); p=tolower(p); \
   prts[p]=1; \
   prev=verb} \
  END{printdata()}' > ~johnca/comp/phrasal-vle

|#

;;; End of file
