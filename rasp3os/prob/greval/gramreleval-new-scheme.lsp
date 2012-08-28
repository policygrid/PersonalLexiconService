;;; GRAMRELEVAL.LSP
;;;
;;; Copyright John Carroll, University of Sussex, 1998-2001
;;;
;;; Evaluate parser grammatical relation output with respect to gold
;;; standard relations. Compile and load this file into a Common
;;; Lisp system that implements the ANSI standard. Tested in Macintosh
;;; CL and Franz Allegro CL.
;;;
;;; See <http://www.cogs.susx.ac.uk/lab/nlp/carroll/greval.html>.
;;; The data and this program are free for research purposes; for any
;;; proposed commercial use please contact John Carroll. 

#|
The lemmatised and numbered sentences and the gold standard should
follow the format in files suste-text and suste-gr respectively (as
linked to by <http://www.cogs.susx.ac.uk/lab/nlp/carroll/greval.html>).
The parser output is assumed to be as follows:

1
(ncsubj say jury _)
(dobj say Friday _)
(ccomp _ say produce)
(ncsubj produce investigation _)

2
(ncsubj say jury _)
(ccomp _ say believe)
(ncsubj believe it _)
...

The function gramreleval computes evaluation statistics; the functions
greval-summary, greval-relation-summary, greval-sentence-summary, and
greval-confusion-summary print out the statistics at increasing levels of
detail.

Gramreleval takes 4 arguments: the lemmatised and numbered sentence file,
the gold standard relation file, the parser output file, and the name of
a file for diagnostic output. An example call is:

  (setq stats (gramreleval "suste-text" "suste-gr" "parses" "output"))

This puts the evaluation statistics in variable stats. The statistics
can then be displayed, by e.g.

  (greval-summary t stats)

where the first argument is a Lisp output stream (the symbol t indicates
output to the terminal, as usual) and the second argument is the statistics
computed by gramreleval.

The other functions greval-relation-summary, greval-sentence-summary,
and greval-confusion-summary take the same two arguments. The output
from the latter function needs to be viewed with a long linelength since
it produces a table several columns wide.
|#

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;; (setq gr '(LOAD (compile-file "gramreleval-new-scheme.lsp")))

(defparameter *greval-weight-threshold* 0.0)

(defparameter *type 'hier) ;;'eq 'sub 'hier
(defparameter *head-dep-only nil) ;; default - nil
(defparameter *head-dep-ncsubj-only nil)
(defparameter *pre-process-ncsubj-passive t) ;; -p option removed from script

(defparameter *output-orig nil)
(defparameter *output-unl nil)

(defparameter +relation-subsume-one-table+
   ;; exactly one level of subsumption for particular relations
   '((|mod| . (|ncmod| |xmod| |cmod| |pmod|))
     (|subj| . (|ncsubj| |xsubj| |csubj|))
     ;;(|arg_mod| . (|ncmod| |iobj|))
     (|obj| . (|dobj| |obj2| |iobj|))
     (|clausal| . (|xcomp| |ccomp|))
     (|comp| . (|pcomp|))
     ))

(defparameter +relation-subsume-any-table+
   ;; >=1 levels of subsumption for all relations
   '((|dependent| .
         (|mod| |arg_mod| |arg| |ncmod| |xmod| |cmod| |pmod| |det|
          |subj| |subj_dobj| |comp|
          |ncsubj| |xsubj| |csubj| |obj| |clausal| |dobj| |obj2| |iobj|
          |xcomp| |ccomp| |pcomp| |aux| |conj| |ta| ))
	  ;;|passive| |quote|))
     ;;WRONG:(|mod| . (|ncmod| |xmod| |cmod| |pmod| |det|))
     (|mod| . (|ncmod| |xmod| |cmod| |pmod|))
     ;; WRONG: (|arg_mod| . (|ncmod| |iobj|))
     (|arg_mod| . (|mod| |ncmod| |xmod| |cmod| |pmod| |arg|
                   |subj| |subj_dobj| |comp| |ncsubj| |xsubj| |csubj| 
                   |obj| |clausal| |dobj| |obj2| |iobj|
                   |xcomp| |ccomp| |pcomp|))
     (|arg| .
         (|subj| |subj_dobj| |comp|
          |ncsubj| |xsubj| |csubj| |obj| |clausal| |dobj| |obj2| |iobj|
          |xcomp| |ccomp| |pcomp|))
     (|subj| . (|ncsubj| |xsubj| |csubj|))
     (|subj_dobj| .
         (|subj| |dobj| |ncsubj| |xsubj| |csubj|)) ; N.B. dobj, not obj as in figure
     (|comp| .
         (|obj| |clausal| |dobj| |obj2| |iobj|
          |xcomp| |ccomp| |pcomp|))
     (|obj| . (|dobj| |obj2| |iobj|))
     ;;WRONG:(|clausal| . (|xcomp| |ccomp| |pcomp|))
     (|clausal| . (|xcomp| |ccomp|))
     ))

(defparameter +relation-depth-table+
   ;; depth of each relation in hierarchy
   '((|dependent| . 0)
     (|arg_mod| . 1)
     (|mod| . 2)
     (|ncmod| . 3)
     (|xmod| . 3)
     (|cmod| . 3)
     (|pmod| . 3)
     (|arg| . 2)
     (|subj| . 3)
     (|ncsubj| . 4)
     (|xsubj| . 4)
     (|csubj| . 4)
     (|subj_dobj| . 3)
     (|comp| . 3)
     (|obj| . 4)
     (|dobj| . 5)
     (|obj2| . 5)
     (|iobj| . 5)
     (|clausal| . 4)
     (|xcomp| . 5)
     (|ccomp| . 5)
     (|pcomp| . 4)
     (|ta| . 1)
     (|det| . 1)
     (|aux| . 1)
     (|conj| . 1)
     (|passive| . 1)
     ;; don't need:(|quote| . 1)
     ))

(defparameter +relation-slot-table+
   ;; argument slots for each relation
   '((|dependent| . (type head dependent))
     (|mod| . (type head dependent))
     (|ncmod| . (type head dependent))
     (|xmod| . (type head dependent))
     (|cmod| . (type head dependent))
     (|pmod| . (head dependent))
     (|det| . (head dependent))
     (|arg_mod| . (type head dependent))
     (|arg| . (type head dependent))
     (|subj| . (head dependent initial_gr))
     (|ncsubj| . (head dependent initial_gr))
     (|xsubj| . (head dependent initial_gr))
     (|csubj| . (head dependent initial_gr))
     (|subj_dobj| . (head dependent))
     (|comp| . (head dependent))
     (|obj| . (head dependent))
     (|dobj| . (head dependent))
     (|obj2| . (head dependent))
     (|iobj| . (head dependent))
     (|clausal| . (head dependent)) ; N.B. type slot added then deleted again
     (|xcomp| . (type head dependent))
     (|ccomp| . (type head dependent))
     (|pcomp| . (head dependent))
     (|aux| . (head dependent))
     (|conj| . (head dependent))
     (|ta| . (type head dependent))
     (|passive| . (head))
     ;;don't need: (|quote| . (head))
     ))

(defparameter +type-underspecified-relations+
   ;; relations for which type field may be unfilled in standard or test GR
   '(|mod| |ncmod| |xmod| |cmod| |pmod| |arg| |xcomp| |ccomp| |ta|))

(defstruct
  (gr-state
     (:print-function
         (lambda (x stream level)
            (declare (ignore level))
            (format stream "<GR-STATE ~A>" (gr-state-nsents x)))))
   nsents std-total tst-total agree states confusion)

;;; Entry point

(defun gramreleval (text std tst &optional (out t))
   (let ((readtable (copy-readtable nil))) 
      (setf (readtable-case readtable) :preserve) 
      (set-syntax-from-char #\' #\a readtable)
      (set-syntax-from-char #\, #\a readtable)
      (with-open-file (text-str text :direction :input)
         (with-open-file (std-str std :direction :input)
            (with-open-file (tst-str tst :direction :input)
               (cond
                  ((null out)
                     (gramreleval1
                        text-str std-str tst-str (make-broadcast-stream) readtable))
                  ((eq out t)
                     (gramreleval1
                        text-str std-str tst-str *standard-output* readtable))
                  (t
                     (with-open-file (out-str out :direction :output :if-exists :supersede
                            :if-does-not-exist :create)
                        (gramreleval1 text-str std-str tst-str out-str readtable))
                     )))))))

(defvar *gr-state-orig)

;; new measures:
(defvar *gr-state-new)

(defun gramreleval1 (text-str std-str tst-str out-str readtable)
   (greval-skip-to-next std-str) (peek-char #\1 std-str) ; skip comments, bracket specs etc
   (greval-skip-to-next tst-str) (peek-char #\1 tst-str)
   (let ((*gr-state-orig (make-gr-state :nsents 0)) 
         (*gr-state-new (make-gr-state :nsents 0))
         (*gr-state-unlabelled (make-gr-state :nsents 0))
         (errorp nil) (count-sent 0))
     (loop
       (let ((next-text (greval-next-text text-str readtable))
             (next-std (greval-next-gr std-str readtable))
             (next-tst (greval-next-gr tst-str readtable)))
         ;;(unless (equal 0 (length (cdr next-tst)))
	   (incf count-sent)
	   ;;)
	 ;;(format out-str "~%sentence: ~A~%" count-sent)
	 (cond
          ((and (null next-text) (null next-std) (null next-tst))
           (when *output-orig
	     (format out-str "~%ORIGINAL SENTENCE PERFORMANCE:~%")
	     (greval-sentence-summary out-str *gr-state-orig))
	   (format out-str "~%NEW SCHEME SENTENCE PERFORMANCE:~%")
	   (greval-sentence-summary out-str *gr-state-new)
	   (when *output-unl
	     (format out-str "~%UNLABELLED SENTENCE PERFORMANCE:~%")
	     (greval-sentence-summary out-str *gr-state-unlabelled))	   
	   (when *output-orig
	     (format out-str "~%ORIGINAL PERFORMANCE:~%")
	     (greval-relation-summary out-str *gr-state-orig))
	   (format out-str "~%NEW SCHEME PERFORMANCE:~%")
           (greval-relation-summary out-str *gr-state-new)
	   (format t "~%NEW SCHEME PERFORMANCE:~%")
           (greval-relation-summary t *gr-state-new)
           (when *output-unl
	     (format out-str "~%UNLABELLED DEPENDENCY PERFORMANCE:~%")
	     (greval-summary out-str *gr-state-unlabelled))
           (return (if errorp nil 
                     (list *gr-state-orig *gr-state-new *gr-state-unlabelled))))
          ((or (null next-text) (null next-std) (null next-tst))
           (error "Reached end of file at different times"))
          ((= (car next-text) (car next-std) (car next-tst))
           (setq errorp
             (greval-check-words (car next-text) (cdr next-text) 
                                 (cdr next-std) (cdr next-tst) errorp))
           
	   ;;(setq errorp nil)
	   (unless errorp ;;(or errorp (equal 0 (length (cdr next-tst)))) ;;only do if found grs!
             ;;(format t "--------------------- SENTENCE: ~A-------------------~%"
	;;	     count-sent)
	 ;;    (format t "text:~%")
	   ;;  (pprint next-text)
	     ;;(format t "std:~%")
	     ;;(pprint next-std)
	     ;;(format t "tst:~%")
	     ;;(pprint next-tst)
	     (greval-sentence
              (cdr next-text) (cdr next-std) (cdr next-tst) out-str
              (car next-text) *gr-state-orig)
             (greval-sentence-new-scheme
              (cdr next-text) (cdr next-std) (cdr next-tst) out-str
              (car next-text) *gr-state-new)
             (greval-sentence-new-unlabelled 
              (cdr next-text) (cdr next-std) (cdr next-tst) out-str
              (car next-text) *gr-state-unlabelled)
             ;;(when (>= count-sent 2)
	       ;;(return))
             ))
          (t
           (error "Mismatch in sentence numbers: ~A (text), ~A (standard), ~
                         ~A (test)"
                  (car next-text) (car next-std) (car next-tst))))))))

(defun greval-next-text (str readtable)
   (let ((*readtable* readtable))
      (let ((n (read str nil 'eof)))
         (if (eq n 'eof) nil (cons n (read-line str))))))

(defun greval-next-gr (str readtable)
   (let ((*readtable* readtable))
      (let ((n (read str nil 'eof)))
         (if (eq n 'eof)
            nil
            (cons n
               (let ((res nil)
                     (bracesp nil)
                     (next (greval-skip-to-next str)))
                  (when (eql next #\{) (read-char str) (setq bracesp t))
                  (loop
                     (let ((next (greval-skip-to-next str)))
                        (if bracesp
                           (cond
                              ((eql next #\})
                                 (read-char str)
                                 (return (nreverse (remove-if #'greval-gr-ignore-p res))))
                              ((eq next 'eof)
                                 (error "Unexpected end of file, stream ~A" str))
                              ((digit-char-p next)
                                 (let ((w (read str nil 'eof)))
                                    (unless (numberp w)
                                       (error "Expecting number at byte ~A, stream ~A"
                                          (file-position str) str))
                                    (when (> w 1.0)
                                       ;; (warn "Found weight ~A>1 at byte ~A, stream ~A - setting to 1"
                                       ;;    w (file-position str) str)
                                       (setq w 1.0))
                                    (push (cons w (greval-read-gr str)) res)))
                              (t (push (cons 1.0 (greval-read-gr str)) res)))
                           (if (or (eq next 'eof) (digit-char-p next))
                              (return (nreverse (remove-if #'greval-gr-ignore-p res)))
                              (push (cons 1.0 (greval-read-gr str)) res)))))))))))

(defun greval-skip-to-next (str)
   (let (next)
      (loop
         (setq next (peek-char t str nil 'eof))
         (unless (eql next #\;) (return next))
         (peek-char #\newline str))))

(defun greval-gr-ignore-p (wgr)
   (cond
      ((member nil (cdr wgr)) ; e.g. (conj _ add NIL)
         ;; (warn "Unexpected slot value NIL in ~S - ignoring" (cdr wgr))
         t)
      ((< (car wgr) *greval-weight-threshold*))
      (t nil)))


(defun greval-read-gr (str)
   (peek-char t str nil 'eof)
   (unless (eql (read-char str) #\()
      (error "Expecting left parenthesis at byte ~A, stream ~A" (file-position str)
         str))
   (let ((res nil) (pcount 0) c)
      (loop
         (peek-char t str)
         (setq c (read-char str))
         (let ((tok nil))
            (loop
               (cond
                  ((eql c #\\) (setq c (read-char str)))
                  ((and (eql c #\)) (> pcount 0)) (decf pcount))
                  ((member c '(#\space #\tab #\newline #\)))
                     (push (intern (coerce (nreverse tok) 'string)
                                   (find-package :COMMON-LISP-USER))
                        res)
                     (if (eql c #\))
                        (return-from greval-read-gr (nreverse res))
                        (return))))
               (when (eql c #\() (incf pcount))
               (push c tok)
               (setq c (read-char str)))))))


(defun greval-check-words (sent-no text std-list tst-list errorp)
   (let ((text (concatenate 'string text " ")))
      (labels
         ((slot-filler-member-p (filler text)
            (or (member filler '(|ellip| |pro|))
                (search (concatenate 'string (string filler) " ") text)
                (search (concatenate 'string (string filler) ",") text)
                (search (concatenate 'string (string filler) ")") text)
                (search (concatenate 'string (string filler) "'") text) ; !!! messy
                (search
                   (concatenate 'string (substitute #\space #\_ (string filler)) " ")
                   text)))
          (check-grs (wgrs type)
            (dolist (wgr wgrs)
               (mapc
                  #'(lambda (gr-slot slot-name)
                      (when (or (eq slot-name 'dependent)
                                (eql (search "HEAD" (string slot-name)) 0))
                         (unless (slot-filler-member-p gr-slot text)
                            (warn "Slot filler ~A in ~A relation ~A in sentence ~A not in text"
                                  gr-slot type (cdr wgr) sent-no)
                            (unless errorp
                               (warn "Only checking, not scoring from now on"))
                            (setq errorp nil)))) ; *** t
                  (rest (cdr wgr)) (gr-slot-list (cdr wgr))))))
         (check-grs std-list "standard")
         (check-grs tst-list "test")
         errorp)))

(defun greval-remove-passive-duplicates (gr-lst)
  (let ((ncsubj-lst nil) 
	(passive-lst nil)
	(gr-lst-new nil)) 
    (dolist (gr gr-lst)
      (when (eq '|ncsubj| (cadr gr))
	(push gr ncsubj-lst))
      (if (eq '|passive| (cadr gr))
	  (push gr passive-lst)
	(push gr gr-lst-new)))
    (when (and ncsubj-lst passive-lst)
      (dolist (ncsubj ncsubj-lst)
	(dolist (passive passive-lst)
	  (let ((ncsubj-vb (caddr ncsubj))
		(ncsubj-igr (car (cddddr ncsubj)))
		(passive-vb (caddr passive)))
	    (when (eq passive-vb ncsubj-vb)
	      (setq gr-lst (remove passive gr-lst :test #'eq))
	      (when (eq ncsubj-igr '_)
		(setf (car (cddddr ncsubj)) '|obj|)
		;;(format t "replacing obj...~%")
		))))))
    gr-lst-new))

;; calculate the unlabelled dependencies score
(defun greval-sentence-new-unlabelled 
    (text std-list tst-list out-str sentn global-state)
  ;; there may legally be duplicate GRs in either standard or test
  ;; (setq std-list (remove-duplicates std-list :test #'equal))
  ;; (setq tst-list (remove-duplicates tst-list :test #'equal))
  (incf (gr-state-nsents global-state))
  (let ((state (make-gr-state :nsents sentn))
        (common nil)
        (missing std-list)
        (extra nil))
    (dolist (tst tst-list)
      (multiple-value-bind (std-match matchp) (gr-find-unlabelled tst std-list)
        (if matchp
            (progn
              (push (list std-match tst) common)
              ;; remove gold-gr from missing/gold list - thus only
              ;; enables gold-grs to be matched once!!!
              (setq missing (remove std-match missing :test #'eq))
              (setq std-list (remove std-match std-list :test #'eq)))
          (push tst extra))))
    ;; print out info to file
    (when *output-unl
      (format out-str "~%------------~%")
      (format out-str "UNLABELLED DEPENDENCY MATCHES: ~A~%~A~%~%" sentn text)
      (format out-str "In both:~%~:{   ~A : ~A~%~}~%"
	      (reverse (mapcar #'(lambda (x) (list (cdr (car x)) 
						   (cdr (cadr x)))) common)))
      (format out-str "Standard only:~%~{   ~A~%~}~%" (mapcar #'cdr missing))
      (format out-str "Test only:~%~{   ~A~%~}~%" 
	      (reverse (mapcar #'cdr extra))))
    ;; for gold list only
    (dolist (std std-list)
      (when (not (eq (first (cdr std)) '|passive|))
	(incf (getf (gr-state-std-total state) '|dependent| 0) (car std))
	(incf (getf (gr-state-std-total global-state) '|dependent| 0) 
	      (car std))))
    ;; add extra grs to total counts for tst only
    (dolist (tst extra)
      (when (not (eq (first (cdr tst)) '|passive|))
	(incf (getf (gr-state-tst-total state) '|dependent| 0) (car tst))
	(incf (getf (gr-state-tst-total global-state) '|dependent| 0) 
	      (car tst))))
    ;; add common grs to total counts for both
    (dolist (pair common)
      (when (not (eq (first (cdr (car pair))) '|passive|))
	(incf (getf (gr-state-std-total state) '|dependent| 0) 
	      (car (car pair)))
	(incf (getf (gr-state-std-total global-state) '|dependent| 0) 
	      (car (car pair)))
	(incf (getf (gr-state-tst-total state) '|dependent| 0) 
	      (car (cadr pair)))
	(incf (getf (gr-state-tst-total global-state) '|dependent| 0) 
	      (car (cadr pair)))
	;; add to agreement - between gold std GR + test GR
	(incf (getf (gr-state-agree state) '|dependent| 0) 
              (car (cadr pair)))
	(incf (getf (gr-state-agree global-state) '|dependent| 0) 
	      (car (cadr pair)))))
    (push state (gr-state-states global-state))))

(defun gr-find-unlabelled (tst std-lst)
   ;; fuzzy equivalent of (find tst std-list :test #'equal) with second value
   ;; indicating whether relation found had a compatible name (t), or a
   ;; non-compatible one (nil)
  (let ((best-relations-match nil)
        (best-std-match nil))
    (dolist (std std-lst)
      (let*
	  ((relations-match-p ;; must match perfectly!!
	    (gr-subsumes-both (first (cdr tst)) (first (cdr std))))
	   (tst-slots (gr-slot-list (cdr tst)))
	   (std-slots (gr-slot-list (cdr std)))
	   (slots-match-p 
	    (cond (*head-dep-only
		   (head-dep-slots-match tst tst-slots std std-slots))
		  (*head-dep-ncsubj-only
		   (head-dep-ncsubj-slots-match tst tst-slots std std-slots))
		  (t
		   (all-slots-match tst tst-slots std std-slots)))))
	(when slots-match-p
	  (when (or (null best-relations-match)
		    (gr-eq-or-subsumes-new best-relations-match 
					   relations-match-p))
	    (setq best-relations-match relations-match-p)
	    (setq best-std-match std)))))
    (when (and best-relations-match best-std-match)
      (return-from gr-find-unlabelled 
	(values best-std-match best-relations-match)))
    nil))

(defparameter +number-match+ 0)

(defun greval-sentence-new-scheme
    (text std-list tst-list out-str sentn global-state)
   ;; there may legally be duplicate GRs in either standard or test
   ;; (setq std-list (remove-duplicates std-list :test #'equal))
   ;; (setq tst-list (remove-duplicates tst-list :test #'equal))
  
  (when *pre-process-ncsubj-passive
    (setq std-list (greval-remove-passive-duplicates std-list))
    (setq tst-list (greval-remove-passive-duplicates tst-list)))
  
  (incf (gr-state-nsents global-state))
  (let ((state (make-gr-state :nsents sentn))
        (common nil)
        (missing std-list)
        (extra nil))
    ;; go through each test GR
    (dolist (std std-list)
      (multiple-value-bind (tst-match matchp) (gr-find-new std tst-list)
	(when tst-match
	  ;; correct heads but possibly wrong relation
	  ;; store in the confusion matrix!
          (push (first (cdr tst-match))
                (getf (gr-state-confusion global-state) 
                      (first (cdr std)))))
	;; when matching all - the matchp lets you know 
	;; which level to compare on
	
	(if matchp
	  (progn
	    ;;(incf +number-match+)
	    ;; push gold-gr and test on common
	    (push (list std tst-match) common)
	    ;; remove gold-gr from missing/gold list - thus only
	    ;; enables gold-grs to be matched once!!!
	    (setq missing (remove std missing :test #'eq))
	    (setq tst-list (remove tst-match tst-list :test #'eq))
	    ;; pass up the common count all the way to the top!
	    (when (eq *type 'hier)
	      (dolist (rel-pair +relation-depth-table+)
		;; count the std 
		(when (gr-eq-or-subsumes-new (car rel-pair)
					     (cadr std))
		  (incf (getf (gr-state-std-total state) 
			      (car rel-pair) 0))
		  (incf (getf (gr-state-std-total global-state) 
			      (car rel-pair) 0))
		  )
		;; consider if this relation is the 
		;; test relation or subsumes it
		(when (gr-eq-or-subsumes-new (car rel-pair)
					     (cadr tst-match))
		  (incf (getf (gr-state-tst-total state) 
			      (car rel-pair) 0))
		  (incf (getf (gr-state-tst-total global-state) 
			      (car rel-pair) 0)))
		;; relation that subsumes/equal to the current test GR:
		;; if exact match between GRs -> count upwards for ALL!
		;; if relation subsumes/equal to common gr level
		(when (gr-eq-or-subsumes-new (car rel-pair) matchp)
		  (incf (getf (gr-state-agree state) 
			      (car rel-pair) 0))
		  (incf (getf (gr-state-agree global-state) 
			      (car rel-pair) 0))
		  )))
	    )
	  (dolist (rel-pair +relation-depth-table+)
	    (when (gr-eq-or-subsumes-new (car rel-pair) (cadr std))
	      (incf (getf (gr-state-std-total state) 
			  (car rel-pair) 0) (car std))
	      (incf (getf (gr-state-std-total global-state) 
			  (car rel-pair) 0) 
		    (car std))))))
	  
	  )

    ;; go through each test GR
    (dolist (tst tst-list)
      (push tst extra)
      ;; pass up the extra count all the way to the top (slots incorrect)
      (when (eq *type 'hier)
	(dolist (rel-pair +relation-depth-table+)
	  (when (or (eq (cadr tst) (car rel-pair))
		    (member (cadr tst) 
			    (cdr (assoc (car rel-pair) 
					+relation-subsume-any-table+))))
	    (incf (getf (gr-state-tst-total state) 
			(car rel-pair) 0) 
		  (car tst))
	    (incf (getf (gr-state-tst-total global-state) 
			(car rel-pair) 0) 
		  (car tst)))))
      )
    
    ;; print out info to file
    (format out-str "~%------------~%")
    (format out-str "NEW SCHEME MATCHES:~A~%~A~%~%" sentn text)
    (format out-str "In both:~%~:{   ~A : ~A~%~}~%"
            (reverse (mapcar #'(lambda (x) (list (cdr (car x)) (cdr (cadr x)))) common)))
    (format out-str "Standard only:~%~{   ~A~%~}~%" (mapcar #'cdr missing))
    (format out-str "Test only:~%~{   ~A~%~}~%" (reverse (mapcar #'cdr extra)))
    
    (when (not (eq *type 'hier))
      ;; add missing gold grs to total counts of GR types
      ;; for gold list only
      (dolist (std std-list)
        (incf (getf (gr-state-std-total state) (first (cdr std)) 0) (car std))
        (incf (getf (gr-state-std-total global-state) (first (cdr std)) 0) (car std)))
      ;; add extra grs to total counts for tst only
      (dolist (tst extra)
        (incf (getf (gr-state-tst-total state) (first (cdr tst)) 0) (car tst))
        (incf (getf (gr-state-tst-total global-state) (first (cdr tst)) 0) (car tst)))
      ;; add common grs to total counts for both
      (dolist (pair common)
        (incf (getf (gr-state-std-total state) (first (cdr (car pair))) 0) 
              (car (car pair)))
        (incf (getf (gr-state-std-total global-state) (first (cdr (car pair))) 0) 
              (car (car pair)))
        (incf (getf (gr-state-tst-total state) (first (cdr (cadr pair))) 0) 
              (car (cadr pair)))
        (incf (getf (gr-state-tst-total global-state) (first (cdr (cadr pair))) 0) 
              (car (cadr pair)))
        ;; add to agreement - between gold std GR + test GR
        (incf (getf (gr-state-agree state) (first (cdr (cadr pair))) 0) 
              (car (cadr pair)))
        (incf (getf (gr-state-agree global-state) (first (cdr (cadr pair))) 0) 
              (car (cadr pair)))))
    (push state (gr-state-states global-state))))

;; need to match the most likely candidate for type all - otherwise first one
;; that matches head/dep will be returned!!
(defun gr-find-new (std tst-lst)
   ;; fuzzy equivalent of (find tst std-list :test #'equal) with second value
   ;; indicating whether relation found had a compatible name (t), or a
   ;; non-compatible one (nil)
  (let ((tst-slots-matching nil)
        (best-relations-match nil)
        (best-tst-match nil)
	(matched-p))
    (dolist (tst tst-lst)
      (let*
          ((relations-match-p ;; must match perfectly!!
            (cond ((eq *type 'EQ)
                   ;; equality of relations:
                   (eq (first (cdr std)) (first (cdr tst))))
                  ((eq *type 'SUB)
		   ;; subsumption of relations (tst is less specific):
                   (gr-eq-or-subsumes-new (first (cdr std)) 
                                          (first (cdr tst))))
                  ((eq *type 'HIER)
                   ;; relation subsumes both:
                   (gr-subsumes-both (first (cdr std)) (first (cdr tst))))))
           ;; check if slots match:
           (tst-slots (gr-slot-list (cdr tst)))
           (std-slots (gr-slot-list (cdr std)))
           (slots-match-p 
	    (cond (*head-dep-only
		   (head-dep-slots-match tst tst-slots std std-slots))
		  (*head-dep-ncsubj-only
		   (head-dep-ncsubj-slots-match tst tst-slots std std-slots))
		  (t
		   (all-slots-match tst tst-slots std std-slots)))))
	(when (and (not matched-p) slots-match-p)
	  (setq matched-p t)
	  (incf +number-match+))
	(cond
         ((and relations-match-p slots-match-p)
	  (if (eq *type 'HIER)
              (when (or (null best-relations-match)
                        (gr-eq-or-subsumes-new best-relations-match 
					       relations-match-p))
                (setq best-relations-match relations-match-p)
                (setq best-tst-match tst))
            (return-from gr-find-new (values tst relations-match-p))))
         (slots-match-p
          (push tst tst-slots-matching)))))
    (when (and best-relations-match best-tst-match)
      (return-from gr-find-new (values best-tst-match best-relations-match)))
    (if tst-slots-matching
        (values (car tst-slots-matching) nil)
      nil)))      

;; if tst-rel subsumes or eq to std-rel
(defun gr-eq-or-subsumes-new (tst-rel std-rel)
  (or (eq tst-rel std-rel)
      (some
       #'(lambda (entry)
           ;; test relation is less specific than standard to any level?
           (and (eq tst-rel (car entry)) 
		(member std-rel (cdr entry) :test #'eq)))
       +relation-subsume-any-table+)))

;; find the lowest subsuming relation over tst-rel and std-rel
(defun gr-subsumes-both (tst-rel std-rel)
  (dolist (rel-pair (sort (copy-seq +relation-depth-table+) #'> :key #'cdr))
    (let ((rel-sub (assoc (car rel-pair) +relation-subsume-any-table+)))
      (when (and
             (or (eq std-rel (car rel-pair)) 
                 (member std-rel (cdr rel-sub) :test #'eq))
             (or (eq tst-rel (car rel-pair))
                 (member tst-rel (cdr rel-sub) :test #'eq)))
        (return (car rel-pair))
        ))))

(defun all-slots-match (tst tst-slots std std-slots)
  (and
   ;;(eql (length (intersection std-slots '(HEAD DEPENDENT)))
   ;;     (length (intersection tst-slots '(HEAD DEPENDENT))))
   (every
    #'(lambda (std-slot std-arg)
        (let ((tst-pos (position std-slot tst-slots :test #'eq)))
          (if tst-pos
              ;; slot with same name in both std and tst so check values
              (let ((tst-arg (nth tst-pos (rest (cdr tst)))))
                (or (gr-arg-equalp tst-arg std-arg)
                    (and (eq std-slot 'TYPE)
                         (member (first (cdr tst))
                                 +type-underspecified-relations+ :test #'eq)
                         (or (eq std-arg '_) (eq tst-arg '_)))))
            ;; not a shared slot, but we know both have same main slots
            t)))
    std-slots (rest (cdr std)))))

(defun head-dep-slots-match (tst tst-slots std std-slots)
  (every
   #'(lambda (std-slot std-arg)
       (let ((tst-pos (position std-slot tst-slots :test #'eq)))
         (if (and tst-pos
                  (or (eq std-slot 'HEAD)
                      (eq std-slot 'DEPENDENT)))
             (gr-arg-equalp 
              (nth tst-pos (rest (cdr tst))) std-arg)
           t)))
   std-slots (rest (cdr std))))

;; for ncsubj the only important subtype is the initial_gr = obj
;; because this means passive
(defun head-dep-ncsubj-slots-match (tst tst-slots std std-slots)
  (every
   #'(lambda (std-slot std-arg)
       (let ((tst-pos (position std-slot tst-slots :test #'eq)))
         (if (and tst-pos
                  (or (eq std-slot 'HEAD)
                      (eq std-slot 'DEPENDENT)
		      (and (eq (cadr std) '|ncsubj|) 
			   (eq std-slot 'INITIAL_GR)
			   (or (eq std-arg 'obj)
			       (eq (rest (cdr tst)) 'obj))
			       )))
             (gr-arg-equalp 
              (nth tst-pos (rest (cdr tst))) std-arg)
           t)))
   std-slots (rest (cdr std))))

;; global state is orig one

(defun greval-sentence (text std-list tst-list out-str sentn global-state)
   ;; there may legally be duplicate GRs in either standard or test
   ;; (setq std-list (remove-duplicates std-list :test #'equal))
   ;; (setq tst-list (remove-duplicates tst-list :test #'equal))
   (incf (gr-state-nsents global-state))
   (let ((state (make-gr-state :nsents sentn))
         (common nil)
         (missing std-list)
         (extra nil))
      (dolist (tst tst-list)
         (multiple-value-bind (std-match matchp) (gr-find tst std-list)
            (if matchp
               (progn
                  (push (list std-match tst) common)
                  (setq missing (remove std-match missing :test #'eq))
                  (setq std-list (remove std-match std-list :test #'eq)))
               (push tst extra))
            (when std-match
               ;; correct heads but possibly wrong relation
               (push (first (cdr tst))
                     (getf (gr-state-confusion global-state) 
                           (first (cdr std-match)))))))
      (when *output-orig
	(format out-str "~%------------~%")
	(format out-str "ORIGINAL MATCHES: ~A~%~A~%~%" sentn text)
	(format out-str "In both:~%~:{   ~A : ~A~%~}~%"
		(reverse (mapcar #'(lambda (x) 
				     (list (cdr (car x)) (cdr (cadr x)))) 
				 common)))
	(format out-str "Standard only:~%~{   ~A~%~}~%" (mapcar #'cdr missing))
	(format out-str "Test only:~%~{   ~A~%~}~%" 
		(reverse (mapcar #'cdr extra))))
      (dolist (std std-list)
        ;; add to counts for std
        ;;(when (eq '|ncmod| (cadr std))
	  ;;(format t "~&missing ncmod~%"))
	(incf (getf (gr-state-std-total state) (first (cdr std)) 0) (car std))
        (incf (getf (gr-state-std-total global-state) (first (cdr std)) 0) (car std))
        )
      (dolist (tst extra)
         (incf (getf (gr-state-tst-total state) (first (cdr tst)) 0) (car tst))
         (incf (getf (gr-state-tst-total global-state) (first (cdr tst)) 0) (car tst)))
      (dolist (pair common)
         (incf (getf (gr-state-std-total state) (first (cdr (car pair))) 0) 
	       (car (car pair)))
         (incf (getf (gr-state-std-total global-state) (first (cdr (car pair))) 0) 
	       (car (car pair)))
         (incf (getf (gr-state-tst-total state) (first (cdr (cadr pair))) 0) 
	       (car (cadr pair)))
         (incf (getf (gr-state-tst-total global-state) (first (cdr (cadr pair))) 0) 
	       (car (cadr pair)))
         (incf (getf (gr-state-agree state) (first (cdr (cadr pair))) 0) 
	       (car (cadr pair)))
         (incf (getf (gr-state-agree global-state) (first (cdr (cadr pair))) 0) 
	       (car (cadr pair))))
      (push state (gr-state-states global-state))))

;;; GR matching
;;;
;;; (gr-find '(1.0 |clausal| |add| |be|) '((1.0 |ncsubj| |keep| |there| _) (1.0 |ccomp| |that| |add| |be|)))
;;; (gr-find '(1.0 |ccomp| _ |add| |be|) '((1.0 |comp| |add| |be|) (1.0 |ccomp| |that| |add| |be|)))

(defun gr-find (tst std-lst)
   ;; fuzzy equivalent of (find tst std-list :test #'equal) with second value
   ;; indicating whether relation found had a compatible name (t), or a
   ;; non-compatible one (nil)
   (let ((std-slots-matching nil))
      (dolist (std std-lst)
         (let*
            ((relations-match-p
                (gr-subsumes-or-eq (first (cdr tst)) (first (cdr std))))
             (tst-slots (gr-slot-list (cdr tst)))
             (std-slots (gr-slot-list (cdr std)))
             (slots-match-p
                (and
                   (eql (length (intersection std-slots '(HEAD DEPENDENT)))
                        (length (intersection tst-slots '(HEAD DEPENDENT))))
                   (every
		    #'(lambda (std-slot std-arg)
			(let ((tst-pos 
			       (position std-slot tst-slots :test #'eq)))
			  (if tst-pos
			      ;; slot with same name in both std 
			      ;;and tst so check values
			      (let ((tst-arg (nth tst-pos (rest (cdr tst)))))
				(or (gr-arg-equalp tst-arg std-arg)
				    (and (eq std-slot 'TYPE)
                                         (member 
					  (first (cdr tst))
					  +type-underspecified-relations+ 
					  :test #'eq)
                                         (or (eq std-arg '_) (eq tst-arg '_)))))
			    ;; not a shared slot, but we know both 
			    ;;have same main slots
			    t)))
		    std-slots (rest (cdr std))))))
	   (cond
	    ((and relations-match-p slots-match-p)
	     (return-from gr-find (values std t)))
	    (slots-match-p
	     (push std std-slots-matching)))))
      (if std-slots-matching
	  (values (car std-slots-matching) nil)
	nil)))

(defun gr-subsumes-or-eq (tst-rel std-rel)
   (or (eq tst-rel std-rel)
       (some
          #'(lambda (entry)
              ;; test relation is less specific than standard by one level?
              (and (eq tst-rel (car entry)) (member std-rel (cdr entry) :test #'eq)))
          +relation-subsume-one-table+)
       (some
          #'(lambda (entry)
              ;; test relation is more specific than standard to any level?
              (and (eq std-rel (car entry)) (member tst-rel (cdr entry) :test #'eq)))
          +relation-subsume-any-table+)))


(defun gr-arg-equalp (arg1 arg2)
   (cond
      ((eql arg1 arg2))
      ((or (eq arg1 '_) (eq arg2 '_))
         nil)
      ;; ellip
      ((or (eq arg1 '|ellip|) (eq arg2 '|ellip|))
         t)
      ;; _ inside values separates parts of named entity
      ((find #\_ (string arg1))
         (search (string arg2) (string arg1)))
      ((find #\_ (string arg2))
         (search (string arg1) (string arg2)))))
         

(defun gr-slot-list (gr)
   (let ((slots (cdr (assoc (first gr) +relation-slot-table+ :test #'eq))))
      (unless slots
         (error "Unknown relation name ~A ~A" (first gr) gr))
      (cond
         ((not (listp (cdr (last slots)))) ; e.g. (type . heads)
            (let ((initial (copy-tree slots)))
               (setf (cdr (last initial)) nil)
               (unless (>= (length (cdr gr)) (length initial))
                  (error "Not enough arguments to relation in ~A - expecting ~
                          ~A" gr slots))
               (append initial
                  (let ((res nil))
                     (dotimes (n (- (length (cdr gr)) (length initial))
                                 (nreverse res))
                        (push (intern (format nil "~A~A" (cdr (last slots)) n))
                           res))))))
         ((not (eql (length (cdr gr)) (length slots)))
            (error "Wrong number of slots in relation ~A - expecting ~
                    ~{~A~^ ~}" gr slots))
         (t slots))))


;;; (greval-summary t aa)

(defun greval-summary (out-str gr-state)
   (let ((std (greval-plist-+ (gr-state-std-total gr-state)))
         (tst (greval-plist-+ (gr-state-tst-total gr-state)))
         (agree (greval-plist-+ (gr-state-agree gr-state))))
      (let
         ((p (if (> tst 0) (* 100 (/ agree tst)) (- agree)))
          (r (if (> std 0) (* 100 (/ agree std)) (- agree))))
         (format out-str "~%Micro-averaged~%Precision      ~7,2F   Recall~7,2F   ~
                          F-score~7,2F   tst GRs~7,2F std GRs~7,2F~%"
            p r (greval-f-score p r) (/ tst (gr-state-nsents gr-state))
	    (/ std (gr-state-nsents gr-state))
	    ))
      (let ((p 0) (r 0) (nscored 0))
         (dolist (pair +relation-slot-table+)
            (let*
               ((rel (car pair))
                (std (getf (gr-state-std-total gr-state) rel 0))
                (tst (getf (gr-state-tst-total gr-state) rel 0))
                (agree (getf (gr-state-agree gr-state) rel 0)))
               (when (> tst 0) (incf p (/ agree tst)))
               (when (> std 0) (incf r (/ agree std)) (incf nscored))))
         (setq p (* 100 (/ p nscored)))
         (setq r (* 100 (/ r nscored)))
         (format out-str "~%Macro-averaged~%Precision      ~7,2F   Recall~7,2F   ~
                          F-score~7,2F~%"
            p r (greval-f-score p r)))))

(defun greval-sentence-summary (out-str gr-state)
   (format out-str "~%------------~%Summary~%")
   (format out-str "~%Sentence     Precision          Recall          ~
                    F-score          tst GRs~%")
   (dolist (s
              (sort (copy-list (gr-state-states gr-state)) #'<
                 :key #'gr-state-nsents))
      (let ((std (greval-plist-+ (gr-state-std-total s)))
            (tst (greval-plist-+ (gr-state-tst-total s)))
            (agree (greval-plist-+ (gr-state-agree s))))
         (let
            ((p (if (> tst 0) (* 100 (/ agree tst)) (- agree)))
             (r (if (> std 0) (* 100 (/ agree std)) (- agree))))
            (format out-str "~4D           ~7,2F         ~7,2F          ~
                             ~7,2F        ~9,2F~%"
               (gr-state-nsents s) p r (greval-f-score p r) tst))))
   (greval-summary out-str gr-state))


(defun greval-relation-summary (out-str gr-state)
   (format out-str "~%------------~%Summary~%")
   (format out-str "~%Relation                Precision    Recall    F-score   tst GRs    std GRs   agree GRs~%")
   (dolist (pair +relation-slot-table+)
     (when (not (eq (car pair) '|passive|))
       (let*
	   ((rel (car pair))
	    (std (getf (gr-state-std-total gr-state) rel 0))
	    (std-i std)
	    (tst (getf (gr-state-tst-total gr-state) rel 0))
	    (tst-i tst)
	    (agree (getf (gr-state-agree gr-state) rel 0))
	    (agree-i agree)
	    (p (if (> tst 0) (* 100 (/ agree tst)) (- agree)))
	    (r (if (> std 0) (* 100 (/ agree std)) (- agree))))
	 (dolist (desc-rel
		     (cdr (assoc rel +relation-subsume-any-table+ :test #'eq)))
	   (incf std (getf (gr-state-std-total gr-state) desc-rel 0))
	   (incf tst (getf (gr-state-tst-total gr-state) desc-rel 0))
	   (incf agree (getf (gr-state-agree gr-state) desc-rel 0)))
         (let
	     ((p-all (if (> tst 0) (* 100 (/ agree tst)) (- agree)))
	      (r-all (if (> std 0) (* 100 (/ agree std)) (- agree))))
	   ;; print out sum of all sub-cat too
           (when (not (eq *type 'hier))
             (format 
	      out-str 
	      "~20A ~10,2F ~10,2F ~10,2F ~10,2F ~10,2F ~10,2F~%"
	      (format nil "~VA~A"
		      (1+ (cdr (assoc rel +relation-depth-table+))) " " rel)
	      p-all r-all (greval-f-score p-all r-all) tst std agree))
	   ;; print out exact match for particular relation too! (if any)
           (when (or (eq *type 'hier)
                     (and (assoc rel +relation-subsume-any-table+)
                          (> tst-i 0)))
             (format 
	      out-str 
	      ":~20A ~10,2F ~10,2F ~10,2F ~10,2F ~10,2F ~10,2F~%"
	      (if (eq *type 'hier)
		  (format nil "~VA~A"
			  (1+ (cdr (assoc rel +relation-depth-table+))) 
			  " " rel)
		"") 
	      p r (greval-f-score p r) tst-i std-i agree-i))
	   ))))
     (greval-summary out-str gr-state))

(defun greval-f-score (p r)
   (if (< (+ p r) least-positive-short-float)
      0
      (/ (* 2 p r) (+ p r))))

(defun greval-plist-+ (plist)
   (do ((tail plist (cddr tail))
        (res 0))
       ((null tail) res)
      (incf res (cadr tail))))


;;; (greval-confusion-summary t aa)

(defun greval-confusion-summary (out-str gr-state)
   (let*
      ((matrix (gr-state-confusion gr-state))
       (rel-list
          (mapcan
             #'(lambda (pair)
                 (let ((rel (car pair)))
                    (dolist (x matrix)
                       (when
                          (or (eq x rel) (and (consp x) (member rel x :test #'eq)))
                          (return (list rel))))))
             +relation-slot-table+))
        (total-rels 0)
        (rel-totals
          (make-list (length rel-list) :initial-element 0)))
      (format out-str
"~%------------~%Confusion matrix~%")
      (format out-str
         "~%Standard   Test relation returned~%         ~{~7@A ~}~7@A~%~%"
         rel-list "Totals")
      (dolist (rel rel-list)
         (let ((confused (make-list (length rel-list) :initial-element 0))
               (confused-total 0))
            (dolist (item (getf matrix rel))
               (incf (nth (position item rel-list) confused))
               (incf confused-total)
               (incf (nth (position item rel-list) rel-totals))
               (incf total-rels))
            (format out-str "~9A~{~7@A ~}~7@A~%" rel confused confused-total)))
      (format out-str "~9A~{~7@A ~}~7@A~%" "Totals" rel-totals total-rels)
      (format out-str
         "~%Overall precision ~7,2F, recall ~7,2F~%"
         (* 100 (/ total-rels (greval-plist-+ (gr-state-tst-total gr-state))))
         (* 100 (/ total-rels (greval-plist-+ (gr-state-std-total gr-state)))))
      (format out-str
"Matches between all slots with same name, but relation names may be unrelated~%"
      )))


;;; End of file




