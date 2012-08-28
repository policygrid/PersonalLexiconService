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

;;;

(defparameter +analysis-tree-type+ 'standard)
(defparameter +parseval-output-p+ nil)
(defparameter +analysis-tree-print-fn+ nil)

;; the default unpacking option:
(defparameter +lr1-pf-unpack-fn+ 'lr1-parse-result-analyses)

(defvar *sentence-number* 0)
(defvar *current-sentence* nil)

(eval-when (compile load eval)
   (defmacro with-output-file-or-stream ((s stream . keys) . body)
      `(let ((.stream. ,stream))
          (if (or (streamp .stream.) (eq .stream. t) (null .stream.))
             (let ((,s (cond ((streamp .stream.) .stream.)
                             ((eq .stream. t) *terminal-io*)
                             (t *standard-output*))))
                ,@body)
             (with-open-file (,s .stream. ,@keys) ,@body))))
   (defmacro with-input-file-or-stream ((s stream . keys) . body)
      `(let ((.stream. ,stream))
          (if (streamp .stream.)
             (let ((,s  .stream.)) ,@body)
             (with-open-file (,s .stream. ,@keys) ,@body)))))

;;; Add 'output' command to GDE parser top loop and fparse command

(eval-when (load eval)
   (unembed display-parse-trees2)
   (embed display-parse-trees2
      (lambda (trees mode command-options)
         (if (eq mode 'output)
            (progn
               (fresh-line *standard-output*)
               (lr1-parse-analysis-output-trees
                  *previous-sentence *current-parse-trees *current-parse-weights
                  (not *current-parse-trees) *standard-output*))
            (display-parse-trees2 trees mode command-options))))
   (pushnew
      (make-command-entry :shortest 1 :name
         'output :action '(parse-file 'output))
      *parser-fparse-commands :test #'equalp)
   (pushnew
      (make-command-entry :shortest 1 :name
         'output :action '(display-parse-trees 'output))
      *parser-view-commands :test #'equalp))


;;; Probabilistically parse sentences from input file and call output
;;; function to write results

(defparameter +max-sentence-length+ nil)
(defparameter +char-encoding+ :latin1) ;; :latin1 from epp project
(defparameter +char-encoding-string+ "iso-8859-1");; "iso-8859-1") ;; from epp project

(defun lr1-parse-analysis-trees (in-sentences out-analyses &optional (logfile t))
  (with-input-file-or-stream (in-str in-sentences 
                              :direction :input 
                              :external-format +char-encoding+
			      ;;(if +xml-output+ +xml-encoding+ :latin1)
			      )
     (with-output-file-or-stream (out-str out-analyses 
                                  :direction :output 
                                  :if-exists :supersede :if-does-not-exist :create 
                                  :external-format +char-encoding+
                                  ;;(if +xml-output+ +xml-encoding+ :latin1)
				  )
       (with-output-file-or-stream (log-str logfile
                                    :direction :output 
                                    :if-exists :overwrite :if-does-not-exist :create
                                    :external-format +char-encoding+ ;;+xml-encoding+
				    ) ; always
            (let ((*error-output* log-str)
                  (*trace-output* log-str))
          (setq *sentence-number* 0)
          (time (lr1-parse-analysis-trees1 in-str out-str log-str))
          (terpri *trace-output*)
          (finish-output *trace-output*))))))

;; print ALL!
(defparameter +print-all+ nil)
(defparameter +print-all-xml+ nil)

;; print lemma lists only [can be used without parsing]
(defparameter +print-lemma-lisp+ nil)
(defparameter +print-lemma-xml+ nil)

;; print output formats/functions:
(defvar +print-list+)

;; set all output formats to nil and set corresponding output functions:
;; sets are (type print-p xml-print-p print-fn xml-print-fn &optional: frag-print-fn frag-xml-print-fn)
(defun init-print-list ()
  (setq +print-list+ nil)
  (push (list 'a nil nil
	      nil 'print-tree-output-xml) +print-list+)
  (push (list 's nil nil
	      nil
	    'lr1-parse-analysis-upenn-print-xml) +print-list+)
  
  (push (list 'z nil nil
	      nil
	      'lr1-parse-analysis-upenn-print-xml) +print-list+)
  
  (push (list 'u nil nil
	      'lr1-parse-analysis-upenn-print
	      'lr1-parse-analysis-upenn-print-xml) +print-list+)
  
  (push (list 'g nil nil
	      'lr1-parse-analysis-grs-print
	      'lr1-parse-analysis-grs-print-xml) +print-list+)
  
  (push (list 'gw nil nil
	      'lr1-parse-analysis-grs-print-with-weights
	      'lr1-parse-analysis-grs-print-with-weights-xml) +print-list+)
  
  ;; rmrs print functions set in rasp_parse.sh in case they change
  (push (list 'r nil nil
	      nil nil) +print-list+)
  
  (push (list 't nil nil
	      nil
	      'print-tree-output-xml) +print-list+)
  
  ;; important to push last so we can test non-gio switches on!
  (push (list 'gio nil nil
	      'print-extracted-weighted-grs
	      'print-extracted-weighted-grs-xml
	      'lr1-parse-analysis-grs-print-with-weights
	      'lr1-parse-analysis-grs-print-with-weights-xml) +print-list+))

;; initialise the +print-list+
;; (init-print-list)

(defun set-rmrs-print-fn (print-fn)
  (setf (elt (assoc 'r +print-list+) 3) print-fn))

(defun set-rmrs-xml-print-fn (print-fn)
  (setf (elt (assoc 'r +print-list+) 4) print-fn))

(defun print-any-type-p (type)
  (or +print-all+ +print-all-xml+ (print-type-p type) (print-type-xml-p type)))

(defun print-type-p (type)
  (or +print-all+
      (let ((print-set (assoc type +print-list+)))
	(elt print-set 1))))

(defun print-type-xml-p (type)
  (or +print-all-xml+
      (let ((print-set (assoc type +print-list+)))
	(elt print-set 2))))

(defun print-type-fn (type frag-p)
  (let ((print-set (assoc type +print-list+)))
    (or (and frag-p (elt print-set 5))
	(elt print-set 3))))

(defun print-type-xml-fn (type frag-p)
  (let ((print-set (assoc type +print-list+)))
    (or (and frag-p (elt print-set 6))
	(elt print-set 4))))

(defun set-type-print (type)
  (let ((print-set (assoc type +print-list+)))
    (setf (elt print-set 1) t)))

(defun set-type-xml-print (type)
  (let ((print-set (assoc type +print-list+)))
    (setf (elt print-set 2) t)))

;; any print function specified?
(defun print-fn-spec-p ()
  (or +print-all+ (null (every #'(lambda (x) (null (cadr x))) +print-list+))))

(defun print-xml-fn-spec-p ()
  (or +print-all-xml+ (null (every #'(lambda (x) (null (caddr x))) +print-list+))))

;; any non-gio print function specified?
;; important that gio is pushed last for this!
(defun print-nongio-fn-spec-p ()
  (or +print-all+ +print-all-xml+ (null (every #'(lambda (x) (null (or (cadr x) (caddr x)))) (cdr +print-list+)))))

(defun print-gio-fn-spec-p ()
  (or +print-all+ +print-all-xml+ (or (elt (car +print-list+) 1)
				      (elt (car +print-list+) 2))))

(defvar +xml-output+)

(defun lr1-parse-analysis-trees1 (in-str out-str log-str)
   (when +parseval-output-p+
     (format out-str "%LB (~%%RB )~%~%"))

   (unless (boundp '+print-list+) (init-print-list))
   
   ;; if no print function specified default is grs:
   (unless (or (print-fn-spec-p) (print-xml-fn-spec-p) +print-lemma-lisp+ +print-lemma-xml+)
     (set-type-print 'g))

   ;; if xml then print the internal dtd if required:
   (let ((+xml-output+ (print-xml-fn-spec-p))
         (*print-pretty* nil) (*print-gensym* nil)
         (*print-length* nil) (*print-level* nil))
     (when (or +xml-output+ +print-lemma-lisp+ +print-lemma-xml+)
       (start-xml out-str))
     (loop
      (let ((sentence (read-sentence-from-file in-str out-str log-str)))
	(when (atom sentence) (finish-output out-str) (return))
	(incf *sentence-number*)
	(setq *current-sentence* sentence)
	;; see if need to parse to get an output format - otherwise only printing lemma list (s):
	(if (or (print-fn-spec-p) (print-xml-fn-spec-p))
	    (let ((*suppress-dict-messages t)
		  (*accepted-vertex-analyses* nil)
		  (double-unpack-p nil)
		  )
	      ;; how to unpack the parse forest:
	      ;; if one of non-gio output formats needed then unpack those first:
	      (if (print-nongio-fn-spec-p)
		  (progn
		    (when (print-gio-fn-spec-p)
		      (setq double-unpack-p t))
		    (setq +lr1-pf-unpack-fn+ 'lr1-parse-result-analyses))
		  (setq +lr1-pf-unpack-fn+ 'extract-grs))
	      (multiple-value-bind (trees n-unif n-fail weights)
		  (if (and +max-sentence-length+
			   (> +max-sentence-length+ 0)
			   (> (length (cdr sentence)) +max-sentence-length+))
		      (progn
			(format log-str "~%Input longer than ~A words~%Halted~%"
				+max-sentence-length+)
			nil)
		      (let ((*lr1-parse-timeouts* (lr1-parse-timeouts)))
			(declare (special *lr1-parse-timeouts*))
			(invoke-parser1 (cdr sentence))))
		(declare (ignore n-unif n-fail))
		(let ((frag-p
		       (and trees (lr1-fragmentary-parse-p (cdar trees)))))
		  (format log-str "~%Sentence ~A: ~A~%~%" *sentence-number*
			  (cond
			    (frag-p "1 partial parse")
			    ((null trees) "No parses")
			    ((null (cdr trees)) "1 parse")
			    (t (format nil "~A parses" (length trees)))))
		  ;; output the formats requested:
		  (lr1-parse-analysis-output-trees-start (cdr sentence) trees weights frag-p out-str)
		  (lr1-parse-analysis-output-trees-print trees weights (cdr sentence) out-str (eq +lr1-pf-unpack-fn+ 'extract-grs) frag-p)
		  ;; if now need to unpack the parse forest with gio instead
		  ;; then double unpack [may have done above instead if no other output required]
		  (when double-unpack-p
		    ;;(format t "unpacking second time...")
		    ;; set to unpack the extracted grs instead:
		    (setq +lr1-pf-unpack-fn+ 'extract-grs)
		    ;; if frag-p then just use the default output for weighted grs:
		    (if frag-p
			;; just output the gr output for the frag:
			(lr1-parse-analysis-output-trees-print trees weights (cdr sentence) out-str t t)
			;; otherwise unpack the parse forest again if there were any parses:
			(when *accepted-vertex-analyses*
			  ;; unpack the grs:
			  (multiple-value-setq (trees weights) (lr1-parse-2))
			  ;; call the output function again:
			  (lr1-parse-analysis-output-trees-print trees weights (cdr sentence) out-str t nil)
			  )))
		  
		  ;; clear space:
		  (setq *accepted-vertex-analyses* nil)
		  
		  ;; now finish the output:
		  (lr1-parse-analysis-output-trees-finish (cdr sentence) trees weights frag-p out-str)
		  (if (or +xml-output+ +print-lemma-lisp+ +print-lemma-xml+)
		      (xml-end-sentence-print out-str)
		      (terpri out-str))
		  (finish-output out-str))))
	    ;; if not parsing to get outputs then at least print the xml sentence start/end tags + lemma-lists
	    ;; if +print-lemma-lisp+ or +print-lemma-xml+
	    (progn
	      (setq *current-sentence* 
               (format nil "~:S 0 ; " 
                  (mapcar
                     #'(lambda (word) (if (consp word) (car word) word))
                     (cdr sentence))))
	      (xml-start-sentence-print out-str (cdr sentence) 0 nil)
	      (xml-end-sentence-print out-str)
	      )
	    )
	))
   (when (or +xml-output+ +print-lemma-xml+ +print-lemma-lisp+)
     (end-xml out-str))))

(defun lr1-parse-timeouts nil
   ;; given amount, plus sneak from an extra 2 secs and 100MB upwards to see if
   ;; we can rescue a partial analysis if parse forest construction times out
   (cons
      (cons +parse-timeout+
         #+(or allegro sbcl) +parse-storeout+
         #-(or allegro sbcl) nil)
      (cons
         (if (and (numberp +parse-timeout+) (> +parse-timeout+ 0))
            (max 2 (ceiling +parse-timeout+ 5))
            nil)
         #+(or allegro sbcl)
         (if (and (numberp +parse-storeout+) (> +parse-storeout+ 0))
            (max 100000000 (ceiling +parse-storeout+ 5))
            nil)
         #-(or allegro sbcl) nil)))


(defun lr1-fragmentary-parse-p (tree)
   (and (cdr tree) (consp (cadr tree))
      (member (cadar tree) '("T/frag" "T/fragx") :test #'equal)))

;;;

(defparameter +analysis-output-nprobs+ 10)

(defun lr1-parse-analysis-output-trees-start (sentence trees weights frag-p out-str)
   (let ((nparses (length trees))
         (*print-pretty* nil) (*print-gensym* nil)
         (*print-length* nil) (*print-level* nil))
      (unless +parseval-output-p+
         (let ((n (cond (frag-p 0) ((eql nparses 0) -1) (t nparses))))
            (setq *current-sentence* 
               (format nil "~:S ~S ; " 
                  (mapcar
                     #'(lambda (word) (if (consp word) (car word) word))
                     sentence)
                  n))
            (if (or +xml-output+ +print-lemma-lisp+ +print-lemma-xml+)
		;; output the xml start sentence:
		(xml-start-sentence-print out-str sentence n weights)
               (progn
                  (format out-str "~&~A" *current-sentence*)
                  (cond
                     ((> n (1+ +analysis-output-nprobs+))
                        (format out-str "(~{~,3F~^ ~} ...)~%"
                           (butlast weights (- n +analysis-output-nprobs+))))
                     ((> n 0)
                        (format out-str "(~{~,3F~^ ~})~%" weights))
                     (t (format out-str "()~%")))))))
      (when +parseval-output-p+
         (format out-str "~&~A~%" *sentence-number*))
      (when (and +parseval-output-p+ (> nparses 1) (null +analysis-tree-print-fn+))
         (format out-str "{~%"))
      (finish-output out-str)))

(defun lr1-parse-analysis-output-trees-finish (sentence trees weights frag-p out-str)
   (let ((nparses (length trees))
         (*print-pretty* nil) (*print-gensym* nil)
         (*print-length* nil) (*print-level* nil))
      (when (and +parseval-output-p+ (/= nparses 1) (null +analysis-tree-print-fn+))
         (format out-str "}~%"))
      (terpri out-str) 
      (finish-output out-str)))

(defun lr1-parse-analysis-output-trees-old (sentence trees weights frag-p out-str)
   (let ((nparses (length trees))
         (*print-pretty* nil) (*print-gensym* nil)
         (*print-length* nil) (*print-level* nil))
      (unless +parseval-output-p+
         (let ((n (cond (frag-p 0) ((eql nparses 0) -1) (t nparses))))
            (setq *current-sentence* 
               (format nil "~:S ~S ; " 
                  (mapcar
                     #'(lambda (word) (if (consp word) (car word) word))
                     sentence)
                  n))
            (if +xml-output+
               (xml-start-sentence-print out-str sentence n weights)
               (progn
                  (format out-str "~&~A" *current-sentence*)
                  (cond
                     ((> n (1+ +analysis-output-nprobs+))
                        (format out-str "(~{~,3F~^ ~} ...)~%"
                           (butlast weights (- n +analysis-output-nprobs+))))
                     ((> n 0)
                        (format out-str "(~{~,3F~^ ~})~%" weights))
                     (t (format out-str "()~%")))))))
      (when +parseval-output-p+
         (format out-str "~&~A~%" *sentence-number*))
      (when (and +parseval-output-p+ (> nparses 1) (null +analysis-tree-print-fn+))
         (format out-str "{~%"))
      (lr1-parse-analysis-output-trees-print trees weights sentence out-str)
      (when (and +parseval-output-p+ (/= nparses 1) (null +analysis-tree-print-fn+))
         (format out-str "}~%"))
      (terpri out-str) 
      (finish-output out-str)))

;; print the description or just a newline:
(defparameter +print-desc+ t)

(defun print-separator (desc tree-out-str idx xml-p)
  ;; print the separator:
  (if +print-desc+
      (if xml-p
	  (format tree-out-str "~&<~A>" desc)
	  (unless +parseval-output-p+
	    (if (or +xml-output+ +print-lemma-lisp+ +print-lemma-xml+)
		(format tree-out-str "~&<lisp-~A>" desc)
		(format tree-out-str "~&~A: ~A" desc idx)
		)))
      (terpri tree-out-str)))

(defun print-end-separator (desc tree-out-str idx xml-p)
  ;; print the end xml tag:
  (if +print-desc+
      (if xml-p
	  (format tree-out-str "~&</~A>" desc)
	  (when (or +xml-output+ +print-lemma-lisp+ +print-lemma-xml+)
	    (format tree-out-str "~&</lisp-~A>" desc)))))

(defun call-print-tree (print-fn tree tree-out-str)
  (if (fboundp print-fn)
      (funcall print-fn tree tree-out-str)
      (progn
	(format tree-out-str "~&")
	(write tree :stream tree-out-str
	       :pretty t :escape (null +parseval-output-p+))
	(terpri tree-out-str))))

(defun print-tree-out (type tree tree-out-str desc idx frag-p &optional (print-sep t))
  ;; print the non-xml version?
  (when (and (null (and (equal type 'r) (print-type-xml-p 'r))) ;; don't print rmrs twice! 
	     (print-type-p type))
    ;; print the separator:
    (when print-sep (print-separator desc tree-out-str idx nil))
    ;; print the non-xml format:
    (call-print-tree (print-type-fn type frag-p) tree tree-out-str)
    (when print-sep (print-end-separator desc tree-out-str idx nil))
    )
  ;; print the xml version?
  (when (and ;;(null (and (equal type 'r) (print-type-p 'r))) ;; don't print rmrs twice! DONE ABOVE
	     (print-type-xml-p type))
    ;; print the xml start tag:
    (when print-sep (print-separator desc tree-out-str idx t))
    ;; print the xml format:
    (call-print-tree (print-type-xml-fn type frag-p) tree tree-out-str)
    (when print-sep (print-end-separator desc tree-out-str idx t))
    ))

;; print start/end parse sets [right now only for xml output]
(defun print-parse-set-start (out-str raw-trees idx)
  (when (or +xml-output+ +print-lemma-lisp+ +print-lemma-xml+)
    (xml-print-parse-set-start out-str raw-trees idx))
  )

(defun print-parse-set-end (out-str)
  (when (or +xml-output+ +print-lemma-lisp+ +print-lemma-xml+)
    (xml-print-parse-set-end out-str))
  )

(defun lr1-parse-analysis-output-trees-print (raw-trees weights sentence out-str &optional (gio-out-p nil) (frag-p nil))
  (let ((*print-pretty* nil) (*print-gensym* nil)
	(*print-length* nil) (*print-level* nil))
    (cond
      (raw-trees
       (do* ((ttail raw-trees (cdr ttail))
	     (wtail weights (cdr wtail))
	     (raw-tree (car ttail) (car ttail))
	     (n 1 (1+ n))
	     (idx 1 (1+ idx))
	     )
	    ((null ttail))
	 (let* ((*raw-tree* (cdr raw-tree))
		(*raw-nvt* (car raw-tree))
		(*tree-weight* (car wtail))
		(*first-tree-p* (eq ttail raw-trees))
		(*last-tree-p* (null (cdr ttail))))
	   (declare (special *raw-tree* *raw-nvt* *tree-weight* *first-tree-p* *last-tree-p*))
	   ;; go through each type of switched on output function and print if switched on:
	   
	   ;; normal output or gio output?:
	   ;;(format t "output ... ~A ~A" gio-out-p frag-p)
	   (when (null gio-out-p)
	     ;; print out the parse-set number:
	     (print-parse-set-start out-str raw-trees idx)
	     
	     ;; get the tree for +analysis-tree-type+ of nil:
	     ;; print -r or -t options:
	     (when (or (print-any-type-p 't) (print-any-type-p 'r))
	       (setq +analysis-tree-type+ nil)
	       (let ((tree (lr1-parse-analysis-tree-structure *raw-tree* *raw-nvt* n)))
		 (print-tree-out 't tree out-str "tree-rasp" idx frag-p)
		 ;(print-tree-out 'r tree out-str "rmrs-rasp" idx frag-p)
		 ))
	     ;; sparkle: +analysis-tree-type+ sparkle
	     ;; print -s
	     (when (print-any-type-p 's) 
	       (setq +analysis-tree-type+ 'sparkle)
	       (let ((tree (lr1-parse-analysis-tree-structure *raw-tree* *raw-nvt* n)))
		 (print-tree-out 's tree out-str "sparkle" idx frag-p)))
	     ;; susanne: +analysis-tree-type+ susanne
	     ;; print -z
	     (when (print-any-type-p 'z) 
	       (setq +analysis-tree-type+ 'susanne)
	       (let ((tree (lr1-parse-analysis-tree-structure *raw-tree* *raw-nvt* n)))
		 (print-tree-out 'z tree out-str "susanne" idx frag-p)))
	     ;; upenn: +analysis-tree-type+ 'phrasal-parse-structure-upenn
	     (when (print-any-type-p 'u)
	       (setq +analysis-tree-type+ 'phrasal-parse-structure-upenn)
	       (let ((tree (lr1-parse-analysis-tree-structure *raw-tree* *raw-nvt* n)))
		 (print-tree-out 'u tree out-str "upenn" idx frag-p)))
	     ;; alias: +analysis-tree-type+ 'get-cat-labelling-from-parse-tree
	     (when (print-any-type-p 'a)
	       (setq +analysis-tree-type+ 'get-cat-labelling-from-parse-tree)
	       (let ((tree (lr1-parse-analysis-tree-structure *raw-tree* *raw-nvt* n)))
		 (print-tree-out 'a tree out-str "alias" idx frag-p)))
	     ;; get the gr output for +analysis-tree-type+ of lr1-parse-analysis-grs
	     ;; print -g or -gw
	     (if (or (print-any-type-p 'g) (print-any-type-p 'gw)) 
		 (progn
		   (setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
		   (let ((tree (lr1-parse-analysis-tree-structure *raw-tree* *raw-nvt* n)))
		     ;; print out grs
		     (print-tree-out 'g tree out-str "gr-list" idx frag-p)
		     ;; print end of parse-set/nbest-parses tags if in xml-output modes
		     (when (or +xml-output+ +print-lemma-lisp+ +print-lemma-xml+)
		       (print-parse-set-end out-str))
		     ;; output/collect the weighted grs:
		     (print-tree-out 'gw tree out-str "weighted" idx frag-p *last-tree-p*)
		     ))
		 ;; if not grs then print out the end of the parset:
		 (when (or +xml-output+ +print-lemma-lisp+ +print-lemma-xml+)
		   (print-parse-set-end out-str)))
	     )
	   (when gio-out-p
	     ;; else do gio output:
	     (if frag-p
		 ;; if frag then print out the frag grs:
		 (setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
		 (setq +analysis-tree-type+ 'extracted-grs))
	     (let ((tree (lr1-parse-analysis-tree-structure *raw-tree* *raw-nvt* n)))
	       (print-tree-out 'gio tree out-str "ewg-weighted" idx frag-p)))
	   )))
      (+parseval-output-p+
       (let ((*print-escape* nil))
	 (format out-str "~A"
		 (cons 'X
		       (mapcar
			#'(lambda (w)
			    (phrasal-parse-structure-word
			     (string (if (consp w) (car w) w))))
			sentence)))
	 (terpri out-str)))
      (t
       (let ((out-string (cons 'X 
			       (mapcar
				#'(lambda (w)
				    (if (consp w)
					(let ((max-item nil) (max most-negative-single-float))
					  (dolist (item (cdr w))
					    (when (> (cdr item) max)
					      (setq max (cdr item) max-item item)))
					  (car max-item))
					w))
				sentence))))
	 (if +xml-output+
	     (xml-xparse-print out-str out-string)
	     (progn
	       (pprint out-string out-str)
	       (terpri out-str))))))))
  
(defun lr1-parse-analysis-output-trees-print-old (raw-trees weights sentence out-str)
   (cond
      (raw-trees
         (do* ((ttail raw-trees (cdr ttail))
               (wtail weights (cdr wtail))
               (raw-tree (car ttail) (car ttail))
               (n 1 (1+ n)))
            ((null ttail))
            (let* ((*raw-tree* (cdr raw-tree))
                   (*raw-nvt* (car raw-tree))
                   (*tree-weight* (car wtail))
                   (*first-tree-p* (eq ttail raw-trees))
                   (*last-tree-p* (null (cdr ttail)))
                   (tree (lr1-parse-analysis-tree-structure *raw-tree* *raw-nvt* n)))
          (declare (special *raw-tree* *raw-nvt* *tree-weight* *first-tree-p* *last-tree-p*))
          ;;(format t "TREE: ~%") (pprint tree) (terpri)
          (if (fboundp +analysis-tree-print-fn+)
          (funcall +analysis-tree-print-fn+ tree out-str)
        (progn
           (terpri out-str)
           (write tree :stream out-str
              :pretty t :escape (null +parseval-output-p+))
           (terpri out-str)))
           )
       ))
      (+parseval-output-p+
         (let ((*print-escape* nil))
            (format out-str "~A"
               (cons 'X
                  (mapcar
                     #'(lambda (w)
                         (phrasal-parse-structure-word
                            (string (if (consp w) (car w) w))))
                     sentence)))
            (terpri out-str)))
      (t
       (let ((out-string (cons 'X 
                               (mapcar
                                #'(lambda (w)
                                    (if (consp w)
                                        (let ((max-item nil) (max most-negative-single-float))
                                          (dolist (item (cdr w))
                                            (when (> (cdr item) max)
                                              (setq max (cdr item) max-item item)))
                                          (car max-item))
                                      w))
                                sentence))))
         (if +xml-output+
            (xml-frag-print out-str out-string)
            (progn
               (pprint out-string out-str)
               (terpri out-str)))))))


(defun lr1-parse-analysis-tree-structure (tree nvt n)
   (let
      ((res
         (cond
            ((fboundp +analysis-tree-type+) (funcall +analysis-tree-type+ tree nvt))
            ((eq +analysis-tree-type+ 'susanne) (phrasal-parse-structure-susanne tree nvt))
            ((eq +analysis-tree-type+ 'sparkle) (phrasal-parse-structure-sparkle tree nvt))
            (t (lr1-analysis-rule-tree tree nvt)))))
      ;; !!! save space in trees beyond 5000 - under assumption that these will never
      ;; get viewed
      (when (> n 5000)
         (setf (car tree) nil)
         (setf (cdr tree) nil))
      (if (and (null (cdr res)) (not (fboundp +analysis-tree-type+)))
         (car res)
         res)))


;;; Return a rule-labelled tree from a single parse
;;; analysis, e.g. (T/NP (N2/PRE_PPMOD (P2/ADVMOD/- (P1/NP |in| ...
;;; (pprint (lr1-analysis-rule-tree (cdr (caddr *current-parse-trees))))

(defun lr1-analysis-rule-tree (tree &optional nvt)
   ;; just in case called internally when packing is still present
   (let ((tail (cdar tree)))
      (loop
         (when (or (atom tail) (atom (car tail)))
            (setq tree (cons (cons (caar tree) tail) (cdr tree))) (return))
         (setq tail (cdr tail))))
   (cond
      ((and (cdr tree) (atom (cadr tree)))
         (cadr tree))
      (t
         (cons (intern (cadar tree))
            (reverse
               (mapcan
                  #'(lambda (daughter)
                     (cond
                        ((consp daughter)
                           (let ((d (lr1-analysis-rule-tree daughter nvt)))
                              (if d (list d))))))
                  (cdr tree)))))))


;;; output all brackets unlabelled in tree scheme

(defun phrasal-parse-structure-brackets (tree &optional mother-major-cat)
  (cond
   ((and (cdr tree) (atom (cadr tree)))
    ;; strip tag if any
    (list (phrasal-parse-structure-word (string (cadr tree)))))
   (t
    ;; don't output category if it contains just a single word -
    ;; always do if it's a a co-ord structure, or V1/to_bse (infinitival
    ;; construction), NP/det_n1, NP/n2_poss, Taph/brack (textual bracket) -
    ;; under an S, don't if it's another S or a P, otherwise do -
    ;; don't if at top level and only single daughter, or if a V under
    ;; an S mother, or category is same as mother -
    ;; otherwise do...
    (let*
    ((rule (string (cadar tree))) 
               (slash (position #\/ rule))
               (rcat (if slash (subseq rule 0 slash) rule))
               (major-cat (subseq rule 0 1))
               (cat nil)
               (daughters
                  (mapcan
                     #'(lambda (d) (phrasal-parse-structure-susanne d major-cat))
                     (reverse (cdr tree)))))
      (cond
       ((and (null (cdr daughters)) (atom (car daughters)))
    (setq cat nil))
       (t (setq cat rcat)))
      (if cat
               (list (cons cat daughters))
               daughters)))))

(defun phrasal-parse-structure-susanne (tree &optional mother-major-cat)
   (cond
      ((and (cdr tree) (atom (cadr tree)))
         ;; strip tag if any
         (list (phrasal-parse-structure-word (string (cadr tree)))))
      (t
         ;; don't output category if it contains just a single word -
         ;; always do if it's a a co-ord structure, or V1/to_bse (infinitival
         ;; construction), NP/det_n1, NP/n2_poss, Taph/brack (textual bracket) -
         ;; under an S, don't if it's another S or a P, otherwise do -
         ;; don't if at top level and only single daughter, or if a V under
         ;; an S mother, or category is same as mother -
         ;; otherwise do...
         (let*
            ((rule (string (cadar tree))) 
               (slash (position #\/ rule))
               (rcat (if slash (subseq rule 0 slash) rule))
               (major-cat (subseq rule 0 1))
               (cat nil)
               (daughters
                  (mapcan
                     #'(lambda (d) (phrasal-parse-structure-susanne d major-cat))
                     (reverse (cdr tree)))))
            (cond
               ((and (null (cdr daughters)) (atom (car daughters)))
                  (setq cat nil))
               ((search "/cj_" rule)
                  (setq cat rcat))
               ((member rule '("V1/to_bse/-" "V1/to_bse/+" "V1/to_adv_bse") :test #'equal)
                  (setq cat "VP"))
               ((member rule '("NP/det_n1" "NP/n2_poss") :test #'equal)
                  (setq cat "NP"))
               ((equal rule "Taph/brack")
                  (setq cat "T"))
               ((equal major-cat "S")
                  (setq cat
                     (if (member mother-major-cat '("S" "P") :test #'equal)
                        nil rcat)))
               ((or (and (null mother-major-cat) (null (cdr daughters)))
                   (and (equal major-cat "V") (equal mother-major-cat "S"))
                   (equal major-cat mother-major-cat))
                  (setq cat nil))
               (t (setq cat rcat)))
            (if cat
               (list (cons cat daughters))
               daughters)))))

(defparameter +phrasal-parse-buffer+
   (make-array 100 :element-type 'character :adjustable t :fill-pointer 0))

(defun phrasal-parse-structure-word (word)
  ;; only escape characters (){}|\; in words and tags for parseval type output
   (flet
      ((may-escape (str)
         (if (and (notevery #'alphanumericp str) +parseval-output-p+)
            (let ((buffer +phrasal-parse-buffer+))
               (setf (fill-pointer buffer) 0)
               (do
                  ((ind 0 (1+ ind)))
                  ((eql ind (length str))
                     (copy-seq buffer))
                  (when (member (char str ind) '(#\( #\) #\{ #\} #\| #\\ #\;))
                     (vector-push-extend #\\ buffer))
                  (vector-push-extend (char str ind) buffer)))
            str)))
      (let ((underscore (position #\_ word :from-end t)))
         (values
            (may-escape (if underscore (subseq word 0 underscore) word))
            (if underscore (may-escape (subseq word (1+ underscore))) nil)))))


;;; Sparkle tree labelling scheme.

(defun phrasal-parse-structure-sparkle (tree &optional nvt mother-major-cat)
   (cond
      ((and (cdr tree) (atom (cadr tree)))
         ;; strip tag if any
         (list (phrasal-parse-structure-word (string (cadr tree)))))
      (t
         ;; don't output category if it contains just a single word -
         ;; always do if it's a a co-ord structure, or V/to_bse (infinitival
         ;; construction), NP/det_n1, NP/n2_poss, Taph/brack (textual bracket) -
         ;; under an S, don't if it's another S or a P, otherwise do -
         ;; don't if at top level and only single daughter, or if category is
         ;; same as mother -
         ;; otherwise do...
         (let*
            ((rule (string (cadar tree))) 
               (slash (position #\/ rule))
               (rcat (if slash (subseq rule 0 slash) rule))
               (major-cat (subseq rule 0 1))
               (cat nil)
               (daughters
                  (mapcan
                     #'(lambda (d)
                         (phrasal-parse-structure-sparkle d nvt
                            (if (search "-coord" rule)
                              ;; force each conjunct always to be bracketed
                              nil major-cat)))
                     (reverse (cdr tree)))))
            (cond
               ((search "End-punct" rule)
                  (setq rcat "T" major-cat "T"))
               ((equal rcat "V1") (setq rcat "VP"))
               ((equal rcat "A1") (setq rcat nil)))
            (cond
               ((search "-coord" rule)
                  (setq cat rcat))
               ((member rule '("V1/to_bse/-" "V1/to_bse/+" "V1/to_adv_bse") :test #'equal)
                  (setq cat "VP"))
               ((member rule '("NP/det_n1" "NP/n2_poss") :test #'equal)
                  (setq cat "NP"))
               ((some #'(lambda (x) (search x rule)) '("Taph/brack" "Tacl/brack"))
                  (setq cat "T"))
               ((or (and (null mother-major-cat) (null (cdr daughters)))
                   (equal major-cat mother-major-cat))
                  (setq cat nil))
               (t (setq cat rcat)))
            (if cat
               (list (cons cat daughters))
               daughters)))))


;;; UPenn Treebank format output. But note that it's only the format that is
;;; similar -- PoS and non-terminal names and bracketing structure will
;;; differ in many cases.

(defun phrasal-parse-structure-upenn (tree &optional nvt)
   (declare (ignore nvt))
   (let ((res (phrasal-parse-structure-upenn1 tree)))
      (if (cdr res) (cons "TOP" res) (car res))))


(defun phrasal-parse-structure-upenn1 (tree)
   (cond
      ((and (cdr tree) (atom (cadr tree)))
         ;; tag and word
         (let ((+parseval-output-p+ t))
            (multiple-value-bind (word tag)
                  (phrasal-parse-structure-word (string (cadr tree)))
               (list (list tag word)))))
      (t
         (let*
            ((rule (string (cadar tree))) 
             (slash (position #\/ rule))
             (cat (if slash (subseq rule 0 slash) rule))
             (daughters
                (mapcan #'phrasal-parse-structure-upenn1 (reverse (cdr tree)))))
            (cond
                 ((or (equal cat "N") (equal cat "N1")) (setq cat "NP"))
                 ((or (equal cat "Taph") (equal cat "Tacl") (equal cat "Tph")) (setq cat "T"))
                 ((equal cat "P1") (setq cat "PP"))
                 ((equal cat "V1") (setq cat "VP"))
                 ((equal cat "A1") (setq cat "AP")))
            (cond
               ((equal cat "T") daughters) ; splice out T label
               ((cdr daughters)
                  (list
                     (cons cat
                        (mapcan
                           #'(lambda (d) ; flatten any embedded NP structure
                               (if (and (equal cat "NP") (equal (car d) "NP") (consp (cadr d)))
                                  (copy-list (cdr d)) (list d)))
                           daughters))))
               (t ; unary/lexical branch, don't insert current category
                  daughters))))))


(defun lr1-parse-analysis-upenn-print (tree out-str)
   (format out-str "~&")
   (write tree :stream out-str :pretty t :escape nil)
   ;;(terpri out-str)
   )


;;; End of file
