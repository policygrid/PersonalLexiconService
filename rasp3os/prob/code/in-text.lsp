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

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;; Globals used from morph system

(eval-when (compile load eval)
   (proclaim '(special d-traceflag d-lookupformat)))


;;; Specials that are rebound here

(eval-when (compile load eval)
   (proclaim '(special *suppress-dict-messages *lr1-actions* *parser-commands)))


(eval-when (eval load)
   (pushnew
      (make-command-entry :shortest 1 :name
         'lr :action '(parse-and-select-definition-file))
      *parser-commands :test #'equalp))


;;;; TRAINING

;;; Entry point called by new 'lr' command

(defun parse-and-select-definition-file ()
   (let
      ((input (prompt-if-necessary "Input file? ")))
      (when input
         (let
            ((def-file
                  (canonise-grammar-file-name input)))
            (if (probe-file def-file)
               (parse-and-select-definition-file-aux def-file)
               (gde-cerror "file does not exist"))))))


(defun parse-and-select-definition-file-aux (def-file)
   (unwind-protect
      (progn
         (dribble (merge-pathnames (make-pathname :type "log") (pathname def-file)))
         (let
             ((config-file
                (merge-pathnames (make-pathname :type "config")
                   (pathname def-file))))
              (multiple-value-bind (def-pos out-0 out-1)
                 (parse-and-select-config config-file def-file)
              (when def-pos
                 (parse-and-select-definition-file1
                    config-file def-file def-pos out-0 out-1)))))
      (dribble)))
            

(defun parse-and-select-config (config-file def-file)
   (if (probe-file config-file)
      (with-open-file (str config-file :direction :input)
         (values
            (read str) (read str) (read str)))
      (with-open-file
         (str config-file :direction :output :if-does-not-exist 
            :create)
         (let
            ((files
                  (list
                     (merge-pathnames (make-pathname :type "0")
                        (pathname def-file))
                     (merge-pathnames (make-pathname :type "1")
                        (pathname def-file)))))
            (if
               (and (some #'probe-file files)
                  (not
                     (y-or-n-p
"At least one of the files ~{~A~^, ~} already exists - overwrite it?"
                        files)))
               nil
               (progn
                  (dolist (file files)
                     (with-open-file
                        (str file :direction :output :if-exists 
                           :supersede :if-does-not-exist :create)))
                  (format str "~8D~{~%~S~}" 0 files)
                  (values-list (cons 0 files))))))))


(defun update-parse-config (config-file def-pos)
   (with-open-file
      (config-str config-file :direction :output :if-exists 
         :overwrite)
      (file-position config-str 0)
      (format config-str "~8D" def-pos)))

 
(defun parse-and-select-definition-file1 (config-file def-file def-pos out-0 out-1)
   (with-open-file (def-str def-file :direction :input)
      (file-position def-str def-pos)
      (loop
         (let (sentence)
            (update-parse-config config-file (file-position def-str))
            ;; car of sentence is back-reference to origin
            (setq sentence
               (read-sentence-from-file def-str nil))
            (when (atom sentence)
               (format t "~%~%----------------~%Finished parsing file ~A~%"
                  def-file)
               (return))
            (let*
               ((*suppress-dict-messages t)
                  (*lr1-actions* (list nil)) (trees t))
               (catch-all-errors
                  #'(lambda nil
                     (catch nil
                        (setq trees (invoke-parser1 (cdr sentence))))))
               (cond
                  ((null trees)
                     (format t "~%No parses~%")
                     (write-parsed-definition sentence 0 *lr1-actions* out-0))
                  ((consp trees)
                     (format t "~%~A parses~%" (length trees))
                     (terpri) (display-parse-bracketing trees)
                     (write-parsed-definition
                        sentence (length trees) *lr1-actions* out-1))
                  (t
                     (format t "~%~%----------------~%Suspended parsing file ~A~%"
                        def-file)
                     (return))))))))

            
(defun write-parsed-definition (sentence n-parses lr1-actions out)
   (let ((*print-level* nil) (*print-length* nil) (*print-pretty* nil))
      (with-open-file
         (str out :direction :output :if-exists :append)
         (format str "(~A ~S ~S) ;_;_ ~S~%" n-parses
            (cdr sentence) lr1-actions (car sentence)))))


;;; Deal specially with grammar changes / clearing in LR training system.
;;; Only invalidate cached definitions relating to grammar compilation, and
;;; leave definitions used in LR parsing untouched.

(defun input-idrule-invalidations (rules type) 
   ;; Redefinition from cgde/invalid.lsp
   (when (and (boundp '*state-actions*) (symbol-value '*state-actions*))
      (gde-warn "LR1 parser will retain previously compiled grammar"))
   (dolist (name rules)
      (progn
         (when (member type '(normalised expanded)) 
            (remprop name 'expanded-idrules))
         (when 
            (member type 
               '(normalised expanded compiled))
            (remprop name 'compiled-idrules))))
   (setf g-gramtree nil)
   (setf g-tracerules nil)
   (setf *generator-rules nil))


;;;; CORPUS TEXT INPUT

;;; Read next sentence from file. Return pair consisting of back-
;;; reference to origin, and list of words - as symbols or symbol-tag-wth-score
;;; pairs

(defparameter +numbered-words-p+ t)
(defparameter +exit-on-blank-line-p+ nil)
(defvar *read-sentence-function*)

(defun read-sentence-from-file (stream echo &optional (log-str t))
   (let ((res
            (if (and (boundp '*read-sentence-function*)
                  (fboundp *read-sentence-function*))
               (funcall *read-sentence-function* stream)
               (read-sentence-from-file1 stream echo))))
      (if (listp res)
         (progn
            (format log-str "~%----------------~%~{~A~^ ~}~%"
               (mapcar #'(lambda (w) (if (consp w) (car w) w)) res))
            (when (consp (car res))
               ;; multiple tags per word input
               (dolist (w res)
                  (format log-str "~A ~:A~%" (car w) (mapcar #'car (cdr w)))))
            (force-output log-str)
            (cons nil res))
         res)))

(defparameter +replace-circ+ t)

(defun read-sentence-from-file1 (stream echo)
   (loop
      (let (line res)
         (loop
            (setq line (read-line stream nil 'eof))
            (cond
               ((eq line 'eof)
                  (return-from read-sentence-from-file1 'eof))
               ((and +exit-on-blank-line-p+ (equal line ""))
                  (return-from read-sentence-from-file1 'blank)))
            (if (and (> (length line) 2)
                     (eql (char line 0) #\^)
                     (member (char line 1) '(#\space #\_)))
               (return)
               ;; just echo line since it's not (start of) a tagged sentence
               (progn
		 ;; first replace the escaped &circ; with ^
		 (when +replace-circ+ (setq line (string (replace-circ line))))
		 (write-string line echo :start
			       (if (and (> (length line) 2)
					(eql (char line 0) #\^)
					(eql (char line 1) #\^)
					(eql (char line 2) #\space)) 3 0))
		 (terpri echo))))
         (setq res
            (if (and (> (length line) 1) (eql (char line 1) #\_))
               ;; sentence per line with single tag per word
               (read-single-tagged-sentence-from-file line)
               ;; word per line with tag probabilities
               (read-multi-tagged-sentence-from-file stream)))
         (return
	   (if +replace-circ+
	       (sentence-replace-circ (sentence-regularise-quotes res))
	       (sentence-regularise-quotes res)))
		 )))

(defun replace-circ (word)
  (setq word (string word))
  (do ()
      ((null (search "&raspcirc;" word)))
    (let ((start-circ-pos (search "&raspcirc;" word)))
      (setq word (format nil "~A~A~A" (subseq word 0 start-circ-pos) "^" (subseq word (+ 10 start-circ-pos))))))
  (intern word))

(defun sentence-replace-circ (words)
  ;;(format t "read in sentence:~A~%" words)
  ;; replace all &circ; with ^
  (let (witem)
    (dotimes (i (length words))
      ;;(format t "witem:~A~%" (elt words i))
      (if (consp (elt words i))
	  (progn
	    (setf witem (elt words i))
	    (setf (car witem) (replace-circ (car witem)))
	    (mapcar #'(lambda (wpair)
			(setf (car wpair) (replace-circ (car wpair))))
		    (cdr witem)))
	  (setf (elt words i) (replace-circ (elt words i))))))
  words
  )

(defun sentence-regularise-quotes (words)
   ;; suppress all quotes if sentence isn't balanced (i.e. odd number of them)
   ;; also skip dash at end of sentence?
   (let ((bal-quot-p
   	   (evenp
   	      (count-if
   	         #'(lambda (witem) (member '|"| (word-input-item-pos-tags witem)))
   	         words))))
      (mapcan
         #'(lambda (witem)
            (if (and (not bal-quot-p) (member '|"| (word-input-item-pos-tags witem)))
               nil
               (list witem)))
         words)))

(defun word-input-item-pos-tags (x)
   ;; (word-input-item-pos-tags "<w s=1>foo:1_NP</w>")
   ;; (word-input-item-pos-tags '(foo (bar_NP 0.9) (baz_NN 0.1)))
   (mapcan
      #'(lambda (wt)
         (setq wt (string wt))
         (when (and (eql (char wt 0) #\<) (eql (char wt 1) #\w))
            ;; extract PoS tag
            (let*
               ((startwtag-end (position #\> wt))
                (endwtag-start
                   (and startwtag-end
                      (position #\< wt :start startwtag-end :from-end t))))
               (when (and startwtag-end endwtag-start)
                  (setq wt
                     (subseq wt (1+ startwtag-end) endwtag-start)))))
         (let
            ((ts-pos (position #\_ wt :from-end t)))
            (if ts-pos (list (intern (subseq wt (1+ ts-pos)))) nil)))
      (if (consp x) (mapcar #'car (cdr x)) (list x))))

;;; Read sentence from input file in ^_^ word_tag ... format (single line).
;;; If line starts with < then assume it's an inter-sentence xml tag
;;; and return it as an atom
;;; E.g. (read-single-tagged-sentence-from-file "^_^ He_PPHS1 helps_VVZ ._.")

(defun read-single-tagged-sentence-from-file (line)
   (let ((nword 0))
      ;; Allow for Allegro PC and CMUCL returning two lines from read-line
      ;; if the first is empty
      (setq line (string-left-trim '(#\linefeed #\newline) line))
      (if (zerop (length line))
         nil
         (mapcar
            #'(lambda (str) (make-tagged-word-from-string str (incf nword)))
            (cdr (tokenise-words-from-string line 0 nil))))))

(defun make-tagged-word-from-string (str nword)
   (let ((underscore (position #\_ str :from-end t)))
      (make-symbol
         (cond
            ((null underscore)
               (error "Untagged token '~A' in input file" str))
            (+numbered-words-p+
               (format nil "~A:~A_~A" (subseq str 0 underscore)
                  nword (subseq str (1+ underscore))))
            (t str)))))


(defun cons-if-non-nil (x y) (if x (cons x y) y))

(defun tokenise-words-from-string (string index part-word)
   (if (>= index (length string))
      (if part-word
          (list (coerce (nreverse part-word) 'string)))
      (let ((char (char string index)))
         (cond
            ((and (eql char #\<)
                   (< (+ index 2) (length string))
                   (eql (char string (1+ index)) #\w)
                   (member (char string (+ index 2)) '(#\space #\tab #\>)))
               (push char part-word)
               (loop
                  (incf index)
                  (when (>= index (length string))
                     (error "expecting </w> but encountered end of line near '~A'"
                        (coerce (nreverse part-word) 'string)))
                  (push (char string index) part-word)
                  (when (and (eql (fourth part-word) #\<)
                             (eql (third part-word) #\/)
                             (eql (second part-word) #\w)
                             (eql (first part-word) #\>))
                     (return
                        (tokenise-words-from-string string (1+ index) part-word)))))
            ((member char '(#\space #\tab))
               (cons-if-non-nil
                  (and part-word (coerce (nreverse part-word) 'string))
                  (tokenise-words-from-string string (1+ index) nil)))
            (t
               (tokenise-words-from-string
                  string (1+ index) (cons char part-word)))))))


;;; Deal with input file in word-tags-with-scores format (option O60, not S)
;;; read until next ^

(defparameter +multiple-tag-threshold+ (log 100 10))
(defparameter +multiple-tag-certainty+ (log 0.90 10))

(defun read-multi-tagged-sentence-from-file (stream)
   ;; have just read a start of sentence anchor line ^
   (let ((words nil) (nword 0))
      (loop
         (let ((next-char (peek-char t stream nil 'eof)))
            (when (member next-char '(eof #\^))
               (return (nreverse words))))
         (multiple-value-bind (word tags)
               (read-tags-with-scores-line (read-line stream) (incf nword))
            (push (cons word tags) words)))))


(defun read-tags-with-scores-line (line nword)
   ;; extract word and tags each with score from string s of form e.g.
   ;; "feels feel+s_NN2:1.0e-2 feel+s_VVZ:0.99[*+]"
   ;; where the e.g. feel+s_ part is optional
   (let*
      ((end-word-pos
          (or (search " <w " line) (search " <w>" line) (position #\space line)
             (error "mal-formed input line ~A" line)))
       (word (subseq line 0 end-word-pos))
       (tags
          (mapcar
             #'(lambda (tok) (read-tags-with-scores-token tok nword))
             (tokenise-words-from-string line end-word-pos nil))))
      ;; now filter tags: keep a tag if it's highest probability; also if
      ;; top tag is below the threshold and this tag's probability is within
      ;; a specified factor of the top
      (setq tags (sort tags #'> :key #'cdr))
      (do*
         ((top-prob (cdar tags))
          (res (list (car tags)))
          (tail (cdr tags) (cdr tail))
          (pair (car tail) (car tail)))
         ((null tail) (return (values word (nreverse res))))
         (when
            (and (< top-prob +multiple-tag-certainty+)
               (> (cdr pair) (- top-prob +multiple-tag-threshold+)))
            (push pair res)))))


(defun read-tags-with-scores-token (s nword)
   (let* ((col (position #\: s :from-end t))
          (lnum (position-if #'digit-char-p s :from-end t))) ; start at end for e.g. ::1
      (if (and col lnum (> lnum col))
         (let
            ((bstart (position #\[ s :start (1+ lnum)))
             (bend (position #\] s :start (1+ lnum))))
            (when (and bstart bend)
               (setq s (concatenate 'string (subseq s 0 bstart) (subseq s (1+ bend)))))
            (let
               ((score-str (subseq s (1+ col) (1+ lnum))))
               (cons
                  (make-tagged-word-from-string
                     (concatenate 'string (subseq s 0 col) (subseq s (1+ lnum))) ; without score
                     nword)
                  (log
                     (max
                        (let ((*read-default-float-format* 'single-float))
                           (read-from-string score-str))
                        1.0s-20) ; not too small otherwise get -infinity
                     10))))
         (error "no score on tagged token ~A" s))))

 
;;; Extend GDE to cope with processed corpus input. Original function is in
;;; cgde/dictint.lsp
;;; (unembed get-word-definition)

(eval-when (load eval) 
   (embed get-word-definition 
      (lambda (word format mode)
         (cond
            ((symbolp word)
               (let ((*suppress-dict-messages t))
                  (get-word-definition word format mode)))
            ((consp word)
               (let ((senses nil) (words-and-probs nil))
                  (dolist (wp (cdr word))
                     (let* ((w
                              (if (find #\_ (string (car wp)))
                                 (car wp)
                                 (make-symbol
                                    (format nil "~A_~A" (car word) (car wp)))))
                            (def (get-word-definition w format mode)))
                        (when def
                           (setq senses
                              (nconc senses (copy-list (word-definition-senses def))))
                           (setq words-and-probs
                              (nconc words-and-probs
                                 (make-list (length (word-definition-senses def))
                                    :initial-element
                                    (cons w (cdr wp))))))))
                  (make-word-definition
                     :senses senses :file words-and-probs :comment nil)))
            (t
               (error "Unexpected argument type in ~A" 'get-word-definition))))))


(defun get-word-definition1 (entries-and-affix-names)
   (let ((d-traceflag nil) 
         (d-lookupformat 'd-wordstructure)
         (res (d-analyse-morphemes entries-and-affix-names)))
      (when res
         (make-word-definition :senses 
            (mapcar 
               #'(lambda (cat-and-deriv)
                  (make-word-sense :cat-bindings 
                     (process-lexical-category
                        (convert-from-morph-format (car cat-and-deriv))
                        nil nil)
                     :structure nil
                     :semantic-forms
                     (cdr cat-and-deriv)))
               res)
            :file nil :comment nil))))


;;; End of file
