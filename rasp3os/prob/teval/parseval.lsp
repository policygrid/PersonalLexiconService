
;;#+(or cltl2 x3j13 ansi-cl) (make-package "PARSEVAL" :use '("COMMON-LISP"))

;;(in-package "PARSEVAL")
(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

;; (setq ps '(LOAD (compile-file "parseval.lsp")))

#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|

  This file contains the entire code for the Parseval program.
  The program was developed by Steven Abney, Philip Harrison, 
  and Ezra Black with input from other participants of the grammar
  evaluation workshops held at the University of Pennsylvania
  in 1992 and 1993.  Please report problems and suggestions to
  Philip Harrison (phil@grace.rt.cs.boeing.com).

  The toplevel call is:

    (parseval txt-file std-file tst-file &optional out-file report-fnc)

  If you are working in a directory with files names "text", "std-trees",
  "tst-trees" and wish the output file to be named "output1" then
  the call is

   (parseval "suste-text" "suste-trees-susanne" "suste-trees-susanne" "output1")

  The value returned is the summary statistics.  It is a list of form:

    (table sum-s sum-t sum-s&t sum-c sum-p)

  Where the sums are all weighted by p (except for sum-p, of course), and
  the table is of form:

    ((sent-no (s t s&t c p) (s t s&t c p) ...)
     (sent-no ...)
     ...)

  If the report-fnc is supplied, it is called on the summary statistics, with
  *standard-output* directed to the out-file.


|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|

  Reduce Trees

  Reduce-trees is the main call to the code that takes the standard and test 
  trees in list form, and outputs reduced trees.  A reduced tree is a list of 
  pairs (<start> <end>), each pair representing a node.

|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defvar node1)		;root node structure for the standard tree
(defvar node2)		;root node structure for the client tree
(defvar pairs1)		;list of ordered pairs representing the standard tree
(defvar pairs2)		;list of ordered pairs representing the client tree
(defvar left-brackets1)		;an array giving left bracket positions for
                        	;the modified standard
(defvar right-brackets1)	;an array giving right bracket positions for
				;the modified standard
(defvar left-brackets2)		;similar to 1, for client
(defvar right-brackets2)	;similar to 1, for client
(defvar brkt-info1)
(defvar brkt-info2)
(defvar *aux-setup-done* nil)
(defvar *nodepool*)
(defvar *availablenodes*)
(defvar *usednodes*)
(defvar *nodepoolcleared* nil)
(defvar *token-failure* nil)
(defvar *next-char*)
(defvar *reduced-text*)
(defvar *BLANK-INDICES*)
(defvar *CHAR-MAP*)
(defvar LAST-INDEX)
(defvar CHAR-INDEX)
(defvar *DELETED-CHARACTERS*)

(defmacro while-parseval (test &body body)
  `(loop (unless ,test (return nil)) ,@body))

(defstruct (node (:print-function print-node-structure))
              nname category subnodes pname parent start-char end-char
              del-start-char del-end-char)

(defun print-node-structure (node output level)
  (declare (ignore level))
  (format output "~%<NODE: ~a  category: ~a  pname: ~a  parent ~a~
                  ~%      subnodes: ~a~
                  ~%      start-char: ~a  end-char: ~a~
                  ~%      del-start-char: ~a  del-end-char: ~a > ~%"
          (node-nname node)
          (node-category node)
          (node-pname node)
          (cond ((node-parent node)(node-nname (node-parent node))))
          (mapcar #'(lambda (x) (node-nname x))(node-subnodes node))
          (node-start-char node)
          (node-end-char node)
          (node-del-start-char node)
          (node-del-end-char node)))

(defun tokenization-reduce-trees (text brktng1 nulls1 brktng2 nulls2)
   (cond ((null *aux-setup-done*)(set-up-auxiliaries)))
   (reduce-text text)
   (setq node1 (convert-bracketing brktng1 nulls1))
   (cond ((or (null node1)(null (check-final-character node1)))
          (terpri)(princ "Tokenization error in the standard")(terpri)))
   (setq node2 (convert-bracketing brktng2 nulls2))
   (cond ((or (null node2)(null (check-final-character node2)))
          (terpri)
          (princ "Tokenization error in the client")(terpri)))
 )

(defun reduce-trees (text brktng1 nulls1 brktng2 nulls2)
" The driver produces the ordered pairs corresponding to the two bracketings
  based on the input text.  There is a call to '(clear-nodes)' after processing 
  each sentence so that the node structures are garbage collected. 

  Inputs: text - an upper case string, no leading or trailing blanks,
                 the raw text to be parsed;
          brktng1 - the first bracketing, assumed to be the standard brktng;
          nulls1  - a list containing pairs like '(NP T)' containing
                    a category and a special symbol indicating a null element,
                    OR symbols such as '<>' which are themselves to be
                    indicative of a null element.  This list encodes
                    the conventions used by the standard.
          brktng2 - the client bracketing.
          nulls2  - same format as nulls1 - null conventions used by the client.


  Output - a list consisting of two lists of sorted ordered pairs, representing
           first the standard tree, and second the client tree.
  Side effects: some globals (see below) are set so that intermediate results 
                can be examined.  Also, the reduced bracketings are printed
                to standard output for verification.
  Globals of interest:
    node1 - root node structure for the standard tree;
    node2 - root node structure for the client tree;
    pairs1 - list of ordered pairs representing the standard tree;
    pairs2 - list of ordered pairs representing the client tree;
    left-brackets1 - an array giving left bracket positions for
                     the modified standard;
    right-brackets1 - an array giving right bracket positions for
                      the modified standard;
    left-brackets2 - similar to 1, for client;
    right-bracket2 - similar to 1, for client;
" 
   (cond ((null *aux-setup-done*)(set-up-auxiliaries)))
   (reduce-text text)
   (setq node1 (convert-bracketing brktng1 nulls1))
   (cond ((or (null node1)(null (check-final-character node1)))
          (error "Tokenization error in the standard")))
   (setq node2 (convert-bracketing brktng2 nulls2))
   (cond ((or (null node2)(null (check-final-character node2)))
          (error "Tokenization error in the client")))

   ;perform the character deletion for the standard tree
   (delete-characters node1)

   ;reassign blanks following deleted characters to the first non-blank
   ;to the left of the deleted character
   (modify-blank-indices)

   ;use the global array *deleted-characters* to modify the start and end
   ;  positions of the standard tree and the client tree
   (alter-nodes node1)
   (alter-nodes node2)

   (setq pairs1 (compute-ordered-pairs node1))
   (setq pairs2 (compute-ordered-pairs node2))

   ;these two calls are used only for setting up the arrays for
   ;  printing the reduced bracketings.  They are not necessary if
   ;  there is no desire to print the reduced bracketings
   (setq brkt-info1 (compute-bracket-positions pairs1))
   (setq brkt-info2 (compute-bracket-positions pairs2))

   (setq left-brackets1 (car brkt-info1))
   (setq right-brackets1 (cadr brkt-info1))
   (setq left-brackets2 (car brkt-info2))
   (setq right-brackets2 (cadr brkt-info2))
   (princ "THE MODIFIED STANDARD BRACKETING IS:")(terpri)
   (print-reduced-bracketing left-brackets1 right-brackets1)
   (terpri)(terpri)
   (princ "THE MODIFIED CLIENT BRACKETING IS:")(terpri)
   (print-reduced-bracketing left-brackets2 right-brackets2)
   (clear-nodes)
   (list pairs1 pairs2)
)



;utility function
(defun explodec (symb)
   ;;Takes a symbol as input, returns a list of chars
   (let* ((stri (format nil "~a" symb))
          (lstri (length stri))
          (result nil))
     (do ((x 0 (1+ x)))
         ((= x lstri)(reverse result))
       (setq result (cons (elt stri x) result)))))


;utility function
(defun mygensym (sym)(intern
                       (string (gensym (if (or (stringp sym)
                                                 (integerp sym))
                                           sym
                                           (string sym))))))
;utility function
(defun setplist (sym proplist)
           (do ((e1 (symbol-plist sym)(cddr e1)))
               ((null e1))
               (remprop sym (car e1)))
           (do ((e1 proplist (cddr e1)))
               ((null e1))
               (setf (get sym (car e1))(cadr e1))))


;utility function
(defun initnodepool (&optional (node-number 600))
 (setq *nodepool* (do ((x 0 (1+ x))(result)(n1))
                    ((= x node-number) result)
                    (setq n1 (mygensym 'n))
                    (set n1 (make-node :nname n1))
                    (setq result (nconc result (list n1)))))
 (setq *availablenodes* *nodepool*)
 (setq *usednodes* nil)
 )
 
 
;utility function
(defun getnode ()
 (cond ((or (null (boundp '*nodepool*))(null *nodepool*))(initnodepool)))
 (let (node)
  (loop
    (cond (*availablenodes* (setq node (car *availablenodes*))
                          (setq *availablenodes* (cdr *availablenodes*))
                          (setq *usednodes* (cons node *usednodes*))
                          (cond ((null (get node 'save-this-node))
                                 (return-from getnode node))))
          (t (setq node (do ((x 0 (1+ x))(result)(n1))
                            ((= x 100) result)
                            (setq n1 (mygensym 'n))
                            (set n1
                              (make-node :nname n1))
                            (setq result (nconc result (list n1)))))
             (setq *nodepool* (nconc *nodepool* node))
             (setq *availablenodes* (cdr node))
             (setq *usednodes* (cons (car node) *usednodes*))
             (return-from getnode (car node)))))
  ))
 

;utility function
(defun clear-nodes ()
"This function reclaims the nodes that have been used and restores the
 variable *availablenodes* to point to the initial node"
      (setq *nodepoolcleared* t)
      (setq *availablenodes* *nodepool*)
      (do ((n1 *usednodes* (cdr n1))(node))
           ((null n1))
           (setq node (car n1))
           (initialize-node-struct (symbol-value node))
           (setplist node nil))
       (setq *usednodes* nil))


;utility function
(defun initialize-node-struct (node)
 (setf (node-category node) nil)
 (setf (node-subnodes node) nil)
 (setf (node-pname node) nil)
 (setf (node-parent node) nil)
 (setf (node-start-char node) nil)
 (setf (node-end-char node) nil)
 (setf (node-del-start-char node) nil)
 (setf (node-del-end-char node) nil)
 )


;utility function for displaying vector contents
(defun print-vector (v)
  (do ((i 0 (1+ i)))
      ((= i (length v)))
      (princ i)(princ " ")(princ (aref v i))(terpri)))


(defun convert-bracketing (brkt null-indicators &optional (top-level t))
 "Converts labelled bracketing into a tree built from node structures.  
  input: brkt - labelled bracketing
         null-indicators  -  a list of pairs such as  '(NP T)' or
                             individual symbols such as '<>' which are
                             used by the bracketing to indicate null elements.
                             If this is not provided and there really are null
                             elements in the bracketing, a tokenization error
                             will occur.
         top-level  -  used for internal purposes only.
  output: a single root node structure or nil (if there is a token error)
  required global: *reduced-text* - string of text without blanks used
                   by check-token
  globals set by this procedure: *token-failure* - becomes 't' if token error
                       *next-char* - index to next char of *reduced-text*
                                     used by this computation, but otherwise
                                     not significant.
 "
  (cond (top-level (setq *token-failure* nil)(setq *next-char* 0)))
  (cond (*token-failure* (return-from convert-bracketing nil)))
  (cond ((or (and brkt (symbolp brkt))
	     (numberp brkt)
	     (stringp brkt))
         (cond ((member brkt null-indicators) 	;check for null indicator
                (return-from convert-bracketing nil)))
         (let ((node (symbol-value (getnode))))
            (setf (node-pname node) brkt)
            (setf (node-category node) 'expl)
            (setf (node-start-char node) *next-char*)
            (check-token brkt)
            (setf (node-end-char node) (1- *next-char*))
            (cond (*token-failure* nil)
                  (t node))))
        ((= (length brkt) 1) nil)
        ((and (= (length brkt) 2)
              (let ((val (cadr brkt)))
                 (or (symbolp val)(numberp val) (stringp val))))
         (cond ((or (member brkt null-indicators :test #'equal)
                    (member (cadr brkt) null-indicators :test #'equal))
                 nil)
               (t (let ((node (eval (getnode)))(sym (cadr brkt)))
                     (setf (node-category node) (car brkt)) 
                     (setf (node-pname node) sym)
                     (setf (node-start-char node) *next-char*)
                     (check-token sym)
                     (setf (node-end-char node) (1- *next-char*))
                     (cond (*token-failure* nil)
                           (t node))))))
        (t (let ((subnodes (mapcar #'convert-bracketing (cdr brkt) 
                                               (make-list (length (cdr brkt))
                                                  :initial-element
                                                   null-indicators)
                                               (make-list (length (cdr brkt)) 
                                                  :initial-element nil))))
              (setq subnodes (remove nil subnodes))
              (cond ((or (null subnodes) *token-failure*) nil)
                    (t (let ((node (symbol-value (getnode))))
                          (setf (node-category node) (car brkt))
                          (setf (node-subnodes node) subnodes)
                          (setf (node-start-char node)
                                (node-start-char (car (node-subnodes node))))
                          (setf (node-end-char node)
                                (node-end-char 
                                    (car (last (node-subnodes node)))))
                          (dolist (s subnodes)
                              (setf (node-parent s) node))
                          node)))))))


(defun check-final-character (root-node)
"This is called after convert-bracketing in order to make sure that the
 last input character is covered by the root node.  This would not be
 the case if the last character of input is missing from the bracketing.
 We could relax this to allow final punctuation to be missing from
 the bracketing.
"
  (cond ((= (node-end-char root-node)(1- (length *reduced-text*))))
        (t nil)))


(defun check-token (sym)
 "Symbols in the leaf nodes of the labelled bracketing are compared with
  the character sequence of *reduced-text*.  Any discrepancy is an error.
  This function updates the global *next-char* which is an index into
  *reduced-text* giving the next character position.
 "
 (let ((string (string sym)))
   (dotimes (i (length string))
     (cond ((>= *next-char* (length *reduced-text*))
            (terpri)
            (princ "Too many characters")(terpri)
            (setq *token-failure* t)
            (return-from check-token nil)))
     (cond ((not (char-equal (aref string i)
			     (aref *reduced-text* *next-char*)))
            (setq *token-failure* t)
            (terpri)(princ "Error found at character ")(princ *next-char*)
            (terpri)
            (return-from check-token nil))
           (t (setq *next-char* (1+ *next-char*)))))
   t))


(defun reduce-text (txt)
 "Input: original raw text with blanks, leading and trailing blanks trimmed.
  Output: three-element list whose elements are also stored as globals. 
       1. reduced text string (orig. text w/o spaces); 
          global: *reduced-text*
       2. vector with 't' for those positions of the reduced text
          that are followed by blanks in the original text, nil otherwise;
          global: *blank-indices*
       3. vector of indices giving position in the original text
          for each character of the reduced text.
          global: *char-map*
  In the current version of reduce-trees the returned value is ignored
  and futher computation is done using the globals.  Note that *char-map*
  is not used anywhere and appears unnecessary.
  "
 (let (reduced-text blank-indices char-map (char-index 0) last-index)
  (do ((i 0 (1+ i))(result nil)(blanks nil)(char-indices nil))
      ((= i (length txt))
       (setq reduced-text (coerce (reverse result) 'string))
       (setq blank-indices (make-array (length reduced-text)
                                          :initial-element nil))
       (dolist (b blanks)
            (setf (aref blank-indices b) t))
       (setq char-map (make-array (length reduced-text)
                                     :initial-element nil))
       (do ((i 0 (1+ i))(indices (reverse char-indices)(cdr indices))(index nil))
           ((= i (length reduced-text)))
           (setq index (car indices))
           (setf (aref char-map i) index))
       (setq *reduced-text* reduced-text)
       (setq *blank-indices* blank-indices)
       (setq *char-map* char-map)
       (list reduced-text blank-indices char-map))
    (cond ((null (char= (aref txt i) #\Space))
           (push i char-indices)
           (push (aref txt i) result))
          (t (cond ((> (length result) 0)(push (1- (length result)) blanks))))))
 ))



(defun delete-characters (node &optional (top-level t))
"Compute the array *deleted-characters* so it contains 't' at a position 
 if the character at that position has been deleted.
 Input - the root node of the standard tree

 Side effect - global array *deleted-characters* is computed.  The array
               is the same length as *reduced-text* and has a 't' if the
               character at the corresponding position is deleted and 'nil'
               otherwise.

               The deletion decisions are based on the UPenn bracketing
               standards and are subject to revision.

               After deletion is completed, the vector *blank-indices* is
               modified so that any blank following a deleted character
               is moved left to the first undeleted character (if any).
               If the first undeleted character already has a blank, no
               problem arises.
"

  (cond (top-level (setq *deleted-characters* 
                          (make-array (length *reduced-text*) 
                                              :initial-element nil))))

  (let ((subnodes (node-subnodes node)))
    (cond ((test-for-deletion node)
           (do ((i (node-start-char node)(1+ i)))
                ((> i (node-end-char node)))
                (setf (aref *deleted-characters* i) t)))
 
          (subnodes
           (mapc #'delete-characters subnodes (make-list (length subnodes)
                                                     :initial-element nil)))))
  )


(defun set-up-auxiliaries ()
"This routine marks all aux forms with property 'AUXIL' for easy recognition
"
    (setq *aux-setup-done* t)
    (mapc #'(lambda (x) (setf (get x 'auxil) t)) 
                                '(HAS HAVE HAVING HAD |'VE| |'D|
                                  |HAVEN'T| |HASN'T| BE BEING
                                  IS AM |'M| ARE |'RE| BEEN WAS WERE |'S|
                                  |ISN'T| |AREN'T| |WEREN'T|
                                  CAN COULD SHOULD WOULD MIGHT WILL |'LL|
                                  MAY MUST SHALL
                                  |CAN'T| |COULDN'T| |SHOULDN'T| |WON'T|
                                  |MIGHTN'T| |MUSTN'T| |SHAN'T|
                                  CA CANNOT DOES DID DO)))
                                                ;WHAT ABOUT FORMS OF 'DO'??


(defun test-for-deletion (node)
"This is subject to revision according to the UPenn bracketing standard"

  (let ((pname (node-pname node))(parent (node-parent node)))

     (cond
           ;;delete 'not', "n't" or free-standing punctuation
           ((member pname '(NOT |,| |;| |.| |:| ? ! |"| -- |N'T| |(| |)|)))

           ;;delete anything labelled `AUX'.  This includes modals, first auxiliary
           ;;verbs, pre-infinitival `to'.
           ((eq (node-category node) 'AUX))

           ;;delete possessive endings |'S| and |'| if the parent category is NP
           ((and parent (or (eq pname '|'S|)(eq pname '|'|))
            (eq (node-category parent) 'NP)))

           ;;delete any remaining auxiliary forms with category VP
           ;; or parent category VP.  This includes all forms of
           ;;  main verb `have' and `be'.
           ((and parent
                 (get pname 'AUXIL)
               (or (eq (node-category node) 'VP)(eq (node-category parent) 'VP))))
          )))

(defun modify-blank-indices ()
"Reassign any blanks associated with deleted characters to the first non-deleted
 character to the left"

 (dotimes (i (length *deleted-characters*))
    (cond ((and (aref *deleted-characters* i)
                (> i 0)
                (aref *blank-indices* i)) 
           (setf (aref *blank-indices* i) nil)
           (do ((i1 (1- i) (1- i1)))
               ((= i1 -1))
               (cond ((null (aref *deleted-characters* i1))
                      (setf (aref *blank-indices* i1) t)
                      (return))))))))

(defun alter-nodes (node)
"Use the array *deleted-characters* to determine the slots del-start-char
 and del-end-char for the tree rooted at 'node' by proceeding recursively 
 from the root.
 Input - the root node of the tree.  This function needs to be called for
         both the standard tree and the client tree
"
  (do ((i (node-start-char node) (1+ i)))
      ((> i (node-end-char node))(return-from alter-nodes))
      (cond ((null (aref *deleted-characters* i))
             (setf (node-del-start-char node) i)
             (return))))
  (do ((i (node-end-char node) (1- i)))
      ((< i (node-start-char node))(return-from alter-nodes))
      (cond ((null (aref *deleted-characters* i))
             (setf (node-del-end-char node) i)
             (return))))
  (mapc #'alter-nodes (node-subnodes node)))

(defmacro insert-pair (pair place)
  `(setf ,place (insert-pair-1 ,pair ,place)))

(defun insert-pair-1 (x s)
  (cond ((null s)
	 (list x))
	((equal x (car s))
	 s)
	((pair< x (car s))
	 (cons x s))
	(t
	 (let ((rest (cdr s))
	       (prev s))
	   (loop
	    (cond ((null rest)
		   (setf (cdr prev) (list x))
		   (return s))
		  ((equal x (car rest))
		   (return s))
		  ((pair< x (car rest))
		   (setf (cdr prev) (cons x rest))
		   (return s))
		  (t
		   (setq prev rest
			 rest (cdr rest)))))))))

(defun pair< (pair1 pair2)
"This is used in sorting a list of ordered pairs.
"
  (cond ((< (car pair1)(car pair2)))
        ((and (= (car pair1)(car pair2))
              (> (cadr pair1)(cadr pair2))))))
 

(defun compute-ordered-pairs (node)
"Check first that 'node' has not been deleted (it must have an entry for
 del-start-char) and that node covers multiple subnodes.  Then collect 
 subnode pairs and add the pair for node.
"
  (let (pairs (todo (list node)))
    (while-parseval todo
      (setq node (pop todo))
      (when (node-del-start-char node)
	(when (and (node-has-multiple-subnodes node)
                  ;(node-parent node) ;rule out root node - commented out for now
                  ;(not (left-recursive-subnode node))   ;commented out for now
                 )
	  (insert-pair (list (node-del-start-char node)
			     (node-del-end-char node))
		       pairs))
	(dolist (sub (node-subnodes node)) (push sub todo))))
    pairs))


(defun left-recursive-subnode (node)
  (let ((parent (node-parent node)))
     (and (eq node (car (node-subnodes parent)))
          (eq (node-category node)(node-category parent)))))


(defun node-has-multiple-subnodes (node)
"Returns 't' if the node has more than one undeleted subnode, otherwise 
 'nil' is returned.
"
 (let (one)
   (dolist (sub (node-subnodes node))
     (when (node-del-start-char sub)
       (when one (return t))
       (setq one t)))))


(defun compute-bracket-positions (pairs)
"This procedure records the positions of left and right brackets so
 that the reduced bracketing can be easily printed.
 Input: a list of ordered pairs representing the tree
 Output: a list of two vectors:
           left-brackets: each entry contains the number of left
                          brackets preceding the i-th position
           right-brackets: each entry contains the number of right
                          brackets following the i-th position"
 (let ((left-brackets (make-array (length *reduced-text*)
                                   :initial-element 0))
       (right-brackets (make-array (length *reduced-text*)
                                     :initial-element 0)))
    (dolist (pair pairs)
       (setf (aref left-brackets (car pair))
             (1+ (aref left-brackets (car pair))))

       (setf (aref right-brackets (cadr pair))
             (1+ (aref right-brackets (cadr pair)))))
    (list left-brackets right-brackets))
 )


(defun print-reduced-bracketing (left-brackets right-brackets)
  (dotimes (i (length *reduced-text*))

    ;;print the left brackets, if any at position i
    (dotimes (b (aref left-brackets i))
      (princ "["))

    ;;print the i-th character, if not deleted
    (cond ((null (aref *deleted-characters* i))
	   (princ (aref *reduced-text* i))))

    ;;if there are right brackets after char i, print them, followed by a blank
    (dotimes (b (aref right-brackets i))
      (princ "]"))

    (if (or (> (aref right-brackets i) 0)
            (aref *blank-indices* i)
	  ; (and (aref *blank-indices* i)   ;unnec. condition removed
	;	 (or (= i 0)
	;	     (not (aref *deleted-characters* (1- i)))))
            )
	(princ #\space))))



#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|

  Score

  Score takes the output of reduce-trees, and produces recall, precision, and
  crossing nodes scores.

  The output of reduce-trees is two lists: pairs in the reduced standard tree,
  pairs in the reduce test tree.  We go through both lists, classifying pairs
  as belonging to std-only, tst-only, or both.

  We have crossing nodes iff that constraint is violated:

    ai < bi <= aj < bj

|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#
  
(defvar *sent-no* nil)

(defvar *S*)			; Number of pairs in the std tree
(defvar *T*)			; Number of pairs in the test tree
(defvar *S&T*)			; Number of pairs in the intersection
(defvar *s-only*)		; Pairs in std only
(defvar *t-only*)		; Pairs in tst only
(defvar *both*)			; Pairs in both
(defvar *C*)			; Number of tst pairs that cross one or more
				;   std pairs
(defvar *crossing-pairs*)	; A-list associating tst pairs with the list of
				;   std pairs they cross
(defvar *p*)			; Probability, to weight scores.  For case
				;   where there are multiple test parses.

(defvar *sum-s*)
(defvar *sum-t*)
(defvar *sum-s&t*)
(defvar *sum-c*)
(defvar *sum-p*)

(defvar *summary-stats*)

(defun init-scoring ()
  (setq *sum-s* 0
	*sum-t* 0
	*sum-s&t* 0
	*sum-c* 0
	*sum-p* 0
	*summary-stats* nil))

(defun score (reduced-trees &optional (probability 1))
  (let ((spairs (car reduced-trees))
	(tpairs (cadr reduced-trees)))
    (setq *S* (length spairs)
	  *T* (length tpairs)
	  *p* probability)
    (count-s&t spairs tpairs)
    ;; Pairs in *S&T* cannot cross with any other pairs, so we can ignore them.
    (count-crossing-pairs *s-only* *t-only*)
    (incf *sum-p* *P*)
    (incf *sum-s* (* *s* *P*))
    (incf *sum-t* (* *t* *P*))
    (incf *sum-s&t* (* *s&t* *P*))
    (incf *sum-c* (* *c* *P*))
    (add-summary-stats-entry)))

(defun add-summary-stats-entry ()
  (let ((entry (car *summary-stats*)))
    (unless (and entry
		 (eq (car entry) *sent-no*))
      (setq entry (list *sent-no*))
      (push entry *summary-stats*))
    (push (list *s* *t* *s&t* *c* *p*) (cdr entry))))


;;; Remember, the lists are sorted by pair<

(defun count-s&t (spairs tpairs)
  (setq *S&T* 0
	*s-only* nil
	*t-only* nil
	*both* nil)
  (while-parseval (and spairs tpairs)
    (cond ((pair< (car spairs) (car tpairs))
	   (push (pop spairs) *s-only*))
	  ((pair< (car tpairs) (car spairs))
	   (push (pop tpairs) *t-only*))
	  (t
	   (incf *S&T*)
	   (push (pop spairs) *both*)
	   (pop tpairs))))
  (setq *s-only* (nconc (nreverse *s-only*) spairs)
	*t-only* (nconc (nreverse *t-only*) tpairs)
	*both* (nreverse *both*))
  *S&T*)

(defun crossing-pair-1 (a b)
  (and (< (car a) (car b))
       (<= (car b) (cadr a))
       (< (cadr a) (cadr b))))

(defun crossing-pair-p (a b)
  (or (crossing-pair-1 a b) (crossing-pair-1 b a)))

(defun count-crossing-pairs (spairs tpairs)
  (setq *C* 0
	*crossing-pairs* nil)
  (let (entry table)
    (dolist (tp tpairs)
      (setq entry nil)
      (dolist (sp spairs)
	(when (crossing-pair-p sp tp)
	  (push sp entry)))
      (when entry
	(incf *C*)
	(push (cons tp (nreverse entry)) table)))
    (setq *crossing-pairs* (nreverse table)))
  *C*)

(defun print-pair (pair &optional (stream *standard-output*))
  (let (i (len (- (1+ (cadr pair)) (car pair))))
    (write-char #\[ stream)
    (dotimes (k len)
      (setq i (+ (car pair) k))
      
      ;;print the i-th character, if not deleted
      (cond ((null (aref *deleted-characters* i))
	     (princ (aref *reduced-text* i) stream)))

      (if (and (aref *blank-indices* i)
	       (< k (1- len))
	    ;  (or (= i 0)                ;condition removed
	    ;      (not (aref *deleted-characters* (1- i))))
               )
	  (write-char #\space stream)))
    (write-char #\] stream)))

(defun print-report (&optional (stream *standard-output*))
  (when *both*
    (format stream "~%~%Both:")
    (dolist (p *both*)
      (format stream "~%  ")
      (print-pair p)))
  (when *s-only*
    (format stream "~%~%Standard only:")
    (dolist (sp *s-only*)
      (format stream "~%  ")
      (print-pair sp)))
  (when *t-only*
    (format stream "~%~%Test only:")
    (dolist (tp *t-only*)
      (format stream "~%  ")
      (print-pair tp)))
  (when *crossing-pairs*
    (format stream "~%~%Crossing pairs (test: std*):")
    (dolist (entry *crossing-pairs*)
      (format stream "~%  ")
      (print-pair (car entry))
      (write-char #\: stream)
      (dolist (sp (cdr entry))
	(write-char #\space stream)
	(print-pair sp))))
  (format stream
	  "~%~:[~%Probability (weight) = ~A~;~*~]~
	   ~%Recall (both/std) = ~A/~A = ~A%~
	   ~%Precision (both/tst) = ~A/~A = ~A%~
	   ~%Crossing = ~A"
	  (= *p* 1) *P*
	  *S&T* *S* (if (zerop *S&T*) 0 (* 100.0 (/ *S&T* *S*))) ; *** jac avoid 0/0
	  *S&T* *T* (if (zerop *S&T*) 0 (* 100.0 (/ *S&T* *T*)))
	  *c*))

(defun finish-scoring ()
  (let ((stats *summary-stats*))
    (setq *summary-stats* nil)
    (dolist (entry stats)
      (setf (cdr entry) (nreverse (cdr entry)))
      (push entry *summary-stats*))
    (setq *summary-stats*
	  (list *summary-stats*
		*sum-s* *sum-t* *sum-s&t* *sum-c* *sum-p*))))

(defmacro recall ()
  `(if (zerop *s&T*) 0 (* 100.0 (/ *s&T* *S*)))) ; *** jac avoid 0/0

(defmacro precision ()
  `(if (zerop *s&T*) 0 (* 100.0 (/ *s&T* *T*)))) ; *** jac avoid 0/0

(defmacro using-parse-entry (entry &body body)
  (let ((e (gensym)))
    `(let* ((,e ,entry)
	    (*s* (pop ,e))
	    (*t* (pop ,e))
	    (*s&t* (pop ,e))
	    (*c* (pop ,e))
	    (*p* (pop ,e)))
       ,@body)))

(defmacro do-parse-entries (parse-entries &body body)
  (let ((e (gensym)))
    `(dolist (,e ,parse-entries)
       (using-parse-entry ,e
	 ,@body))))

(defun print-final-report (stats)
  (let ((table (car stats))
	(sents-with-crossing 0))
    (format t "~%~%~,,79,'=A~
               ~%~%~7A  ~7@A  ~7@A  ~7@A  ~7@A  ~7@A  ~7@A  ~7@A~%~,,70,'-A"
	    "" "Sent" "minC" "C" "maxR" "R" "maxP" "P" "Prob" "")
    (dolist (entry table)
      (cond

       ;; Multiple parses
       ((cddr entry)
	(let (minc maxr maxp r p)
	  (do-parse-entries (cdr entry)
	    (setq r (recall)
		  p (precision))
	    (if (or (null minc) (> minc *C*))
		(setq minc *C*))
	    (if (or (null maxr) (< maxr r))
		(setq maxr r))
	    (if (or (null maxp) (< maxp p))
		(setq maxp p)))
	  (format t "~%~7A  ~7@A  ~7@A  ~7,2F  ~7@A  ~7,2F  ~7@A  ~7@A"
		  (car entry) minc "" maxr "" maxp "" "")
	  (if (> minc 0) (incf sents-with-crossing))
	  (let ((i 0))
	    (do-parse-entries (cdr entry)
	      (format t "~%  .~4A  ~7@A  ~7@A  ~7@A  ~7,2F  ~7@A  ~7,2F  ~7,2F"
		      (incf i) "" *c* "" (recall) "" (precision) *P*)))))

       ;; Single parse
       (t
	(using-parse-entry (cadr entry)
	  (if (> *C* 0) (incf sents-with-crossing))
	  (format t "~%~7A  ~7@A  ~7@A  ~7,2F  ~7,2F  ~7,2F  ~7,2F  ~7@A"
		  (car entry) *C* *C* (recall) (recall) (precision) (precision) "")))))

    (let* ((sums (cdr stats))
	   (*sum-s* (pop sums))
	   (*sum-t* (pop sums))
	   (*sum-s&t* (pop sums))
	   (*sum-c* (pop sums))
	   (*sum-p* (pop sums)))
      (format t "~%~,,70,'-A~%~8A ~7@A  ~7,2F  ~7@A  ~7,2F  ~7@A  ~7,2F  ~7@A~%~%~
                 (MinC summary is # of sents with minC > 0; ~
                 ~% C, R, and P are means weighted by Prob)~%~%"
	      "" "Summary"
	      sents-with-crossing (* 1.0 (/ *sum-c* *sum-p*))
	      "" (* 100.0 (/ *sum-s&t* *sum-s*))
	      "" (* 100.0 (/ *sum-s&t* *sum-t*))
	      ""))))


#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|

  File Format

  At the top of the file, the user can specify what characters they are using
  for brackets, escapes, and nulls.  Any line at the top of the file beginning 
  with '%' is taken to be a character specification.  It should contain a
  specifier followed by whitespace followed by a single character.  The 
  specifiers are:

                                          default:
    Left-Bracket, LB                         [
    Right-Bracket, RB                        ]
    Start-List, SL                           {
    End-List, EL                             }
    Start-Escape, SE                         |
    End-Escape, EE                           |
    Single-Escape, Escape, E                 \
    Comment, C                               ;
    Null, t                                 none

  For example:

    % Left-bracket	(
    % Right-bracket	)
    % Comment		/
    % Null		(NP t)

  Trees are preceded by the sentence number.  If there are multiple trees,
  they should be surrounded by <Start-List> and <End-List> characters.
  Within the list, each tree may be preceded by a score representing its
  likelihood of being the right parse.  Omitted scores default to 1.  Scores
  should be proportional to the probability that the given parse is correct.
  (The scoring program normalizes them and treats them as probability 
  estimates.)

    125
    {
     25 (S (NP This)
           (VP is (NP a test))
           .)
      2 (S (ADV This)
	   (NP t)
	   (VP is (NP a test))
	   .)
    }

|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defstruct style
  (left-bracket #\[)
  (right-bracket #\])
  (start-list #\{)
  (end-list #\})
  (start-escape #\|)
  (end-escape #\|)
  (single-escape #\\)
  (comment #\;)
  (nulls nil))

(defvar *default-style* (make-style))
(defvar *style* *default-style*)

(defvar *whitespace* '(#\space #\tab #\newline))


;;; Generally useful stuff

(defmacro make-adjustable-string (&optional (size '20))
  `(make-array ,size :element-type 'character :adjustable t :fill-pointer 0)) ; *** jac was string-char (not ANSI)

(defun string-nconc (string1 string2)
  (dotimes (i (length string2))
    (vector-push-extend (aref string2 i) string1))
  string1)


;;; Simple reading functions

(defun read-ws (stream)
  (let (prev-char char)
    (while-parseval (and (setq char (read-char stream nil nil))
		(member char *whitespace*))
      (setq prev-char char))
    (if char (unread-char char stream))
    prev-char))

(defun read-comment (stream)
  (let (char)
    (while-parseval (and (setq char (read-char stream nil nil))
		(not (char= char #\newline))))
    char))

(defun read-ws-or-comment (stream style)
  (let (char)
    (loop
     (read-ws stream)
     (unless (and (setq char (peek-char nil stream nil nil))
		  (char= char (style-comment style)))
       (return nil))
     (read-comment stream))))

(defun read-token (stream)
  (when (peek-char nil stream nil nil)
    (let ((word (make-adjustable-string)) char)
      (while-parseval (and (setq char (read-char stream nil nil))
		  (not (member char *whitespace*)))
	(vector-push-extend char word))
      (when char (unread-char char stream))
      word)))


;;;-------------------------------------------------------------------------------
;;;  Headers
;;;

(defun read-style (stream)
  (let ((style (make-style)) char line attrib value)
    (while-parseval (and (member (read-ws stream) '(nil #\newline))
                (setq char (peek-char nil stream nil nil))
                (char= char #\%))
      (read-char stream nil nil)
      (read-ws stream)
      (setq line (read-line stream nil nil))
      (with-input-from-string (stream line)
        (setq attrib (intern (string-upcase (read-token stream))))
        (if (eq attrib 't) (setq attrib 'null))
        (read-ws stream)
        (setq value (if (eq attrib 'null)
                        (read-tree stream style)
                      (read-char stream nil nil)))
        (read-ws stream)
        (when (read-char stream nil nil)
          (error "Bad header: ~S" line))
        (eval-header attrib value style)))
    (when (char= (style-left-bracket style) (style-start-list style))
      (error "Left bracket and start list are the same.  Bad news."))
    style))

 
(defun eval-header (attrib value style)
  (case attrib
    ((left-bracket left_bracket lb)
     (setf (style-left-bracket style) value))
    ((right-bracket right_bracket rb)
     (setf (style-right-bracket style) value))
    ((start-list start_list sl)
     (setf (style-start-list style) value))
    ((end-list end_list el)
     (setf (style-end-list style) value))
    ((start-escape start_escape se)
     (setf (style-start-escape style) value))
    ((end-escape end_escape ee)
     (setf (style-end-escape style) value))
    ((single-escape single_escape escape e)
     (setf (style-single-escape style) value))
    (null
     (push value (style-nulls style)))
    (t (error "Unknown specifier: ~A" attrib))))

;;;-------------------------------------------------------------------------------
;;;  Read Sentence ID
;;;

(defun read-sentence-id (stream &optional style)
  (if style
      (read-ws-or-comment stream style)
    (read-ws stream))
  (read-word stream (or style *default-style*)))


;;;-------------------------------------------------------------------------------
;;;  Read Number
;;;

(defun read-number (stream &optional style)
  (if style
      (read-ws-or-comment stream style)
    (read-ws stream))
  (let ((word (make-adjustable-string)) char)
    (while-parseval (and (setq char (read-char stream nil nil))
		(char>= char #\0)
		(char<= char #\9))
      (vector-push-extend char word))
    (when char (unread-char char stream))
    (when (> (length word) 0)
      (parse-integer word))))


;;;-------------------------------------------------------------------------------
;;;  Read Tree
;;;

(defmacro create-node (category)
  `(list ,category))
  
(defmacro add-child (child parent)
  `(setf (cdr (last ,parent)) (list ,child)))

(defun read-word (stream style)
  (read-ws-or-comment stream style)
  (when (peek-char nil stream nil nil)
    (let ((word (make-adjustable-string)) char escaping)
      (while-parseval (setq char (read-char stream nil nil))
	(cond

	 ((and escaping (char= char (style-end-escape style)))
	  (setq escaping nil))

	 (escaping
	  (vector-push-extend char word))
	 
	 ((char= char (style-start-escape style))
	  (setq escaping t))

	 ((char= char (style-single-escape style))
	  (setq char (read-char stream nil nil))
	  (vector-push-extend (or char (style-single-escape style)) word))

	 ((member char *whitespace*)
	  (return nil))

	 ((or (char= char (style-left-bracket style))
	      (char= char (style-right-bracket style))
	      (char= char (style-comment style)))
	  (unread-char char stream)
	  (return nil))

	 (t
	  (vector-push-extend char word))))

      (and (> (length word) 0) (intern (string-upcase word))))))


;;; A non-terminal is a list whose car is the category and whose cdr are
;;; its children.

(defun read-tree (stream &optional (style *style*))
  (let (char node stack)
    (while-parseval (progn (read-ws-or-comment stream style)
		  (setq char (read-char stream nil nil)))
     (cond

       ((char= char (style-left-bracket style))
	(setq node (create-node (read-word stream style)))
	(if stack
	    (add-child node (car stack)))
	(push node stack))

       ((char= char (style-right-bracket style))
	(when (null stack)
	  (warn "Too many right brackets"))
	(when (null (cdr stack)) (return nil))
	(pop stack))

       ((char= char (style-comment style))
	(while-parseval (and (setq char (read-char nil stream nil nil))
		    (not (char= char #\newline)))))

       (T
	(unread-char char stream)
	(setq node (read-word stream style))
	(when (null stack)
	  (push node stack)
	  (return nil))
	(add-child node (car stack)))))

    (when (cdr stack) (warn "Premature end of expression"))
    (car (last stack))))


;;;----------------------------------------------------------------------------
;;;  Read sentence
;;;

;;;  In the sentence file, there are no escapes.  Sentences are separated by
;;;  empty lines.

(defvar *sentence* (make-adjustable-string 1000))

(defun read-ws-eos-p (stream)
  (let ((nls 0) char)
    (while-parseval (and (setq char (peek-char nil stream nil nil))
		(member char *whitespace*))
      (read-char stream)
      (if (char= char #\newline) (incf nls)))
    (or (null char)
	(> nls 1))))

(defun read-sentence (stream)
  (setf (fill-pointer *sentence*) 0)
  (read-ws stream)
  (let (token)
    (while-parseval (setq token (read-token stream))
      (string-nconc *sentence* token)
      (if (read-ws-eos-p stream) (return nil))
      (vector-push-extend #\space *sentence*)))
  (and (> (length *sentence*) 0) *sentence*))



#|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|

  (Parseval text-file std-file tst-file &optional output-file report-fnc)

  This is the toplevel call.  All the arguments are pathnames.

|;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;|#

(defvar *std-style*)
(defvar *tst-style*)
(defvar *txt*)
(defvar *std-tree*)
(defvar *tst-trees*)
(defvar *tst-tree*)
(defvar *tst-probs*)
(defvar *prob-sum*)

(defun parseval (txt-path std-path tst-path
			  &optional out-path (report-fnc 'print-final-report))
  (if out-path
      (with-open-file (*standard-output* out-path
					 :direction :output
					 :if-does-not-exist :create
					 :if-exists :overwrite)
	(parseval-1 txt-path std-path tst-path report-fnc))
    (parseval-1 txt-path std-path tst-path report-fnc)))

(defun parseval-1 (txt-path std-path tst-path report-fnc)
  (let (p i)
    (with-open-file (txt-file txt-path)
      (with-open-file (std-file std-path)
        (with-open-file (tst-file tst-path)
	  (setq *std-style* (read-style std-file)
		*tst-style* (read-style tst-file))
	  (init-scoring)
	  (while-parseval (peek-char t txt-file nil nil)
	    (read-sentence-and-trees txt-file std-file tst-file)
	    (format t "~%~%~,,79,'-@A~%~D~%~A" "" *sent-no* *txt*)
	    (setq i 0)
	    (while-parseval *tst-trees*
	      (incf i)
	      (setq p (pop *tst-probs*)
		    *tst-tree* (pop *tst-trees*))
	      (format t "~%~%~,,40,'-@A~%~A.~D~%~%" "" *sent-no* i)
	      (score (reduce-trees *txt*
				   *std-tree* (style-nulls *std-style*)
				   *tst-tree* (style-nulls *tst-style*))
		     (/ p *prob-sum*))
	      (print-report)))
	  (finish-scoring)
	  (funcall report-fnc *summary-stats*)))))
  *summary-stats*)

(defun check-tokenization (txt-path std-path tst-path &optional out-path)
  (if out-path
      (with-open-file (*standard-output* out-path
                                         :direction :output
                                         :if-does-not-exist :create
                                         :if-exists :overwrite)
        (check-tokenization-1 txt-path std-path tst-path))

    (check-tokenization-1 txt-path std-path tst-path)))

(defun check-tokenization-1 (txt-path std-path tst-path)
  (let (p i)
    (with-open-file (txt-file txt-path)
      (with-open-file (std-file std-path)
        (with-open-file (tst-file tst-path)
          (setq *std-style* (read-style std-file)
                *tst-style* (read-style tst-file))
          (while-parseval (peek-char t txt-file nil nil)
            (read-sentence-and-trees txt-file std-file tst-file)
            (format t "~%~%~,,79,'-@A~%~D~%~A" "" *sent-no* *txt*)
            (setq i 0)
            (while-parseval *tst-trees*
              (incf i)
              (setq p (pop *tst-probs*)
                    *tst-tree* (pop *tst-trees*))
              (tokenization-reduce-trees *txt*
                                   *std-tree* (style-nulls *std-style*)
                                   *tst-tree* (style-nulls *tst-style*))
              ))
          )))))

(defun read-sentence-and-trees (txt-file std-file tst-file)
  (let (id char)
    (unless (setq id (read-sentence-id txt-file))
      (error "Number missing ~:[at beginning of text file~*~;~
                                in text file after ~A~]"
	     *sent-no* *sent-no*))
    (setq *sent-no* id)
    (unless (setq *txt* (read-sentence txt-file))
      (error "Number without text"))
    (unless (setq id (read-sentence-id std-file *std-style*))
      (error "Standard tree ~A missing" *sent-no*))
    (unless (eq id *sent-no*)
      (error "Synch error: text ~A, std ~A" *sent-no* id))
    (unless (setq *std-tree* (read-tree std-file *std-style*))
      (error "Standard tree ~A missing" *sent-no*))
    (unless (setq id (read-sentence-id tst-file *tst-style*))
      (error "Test tree ~A missing" *sent-no*))
    (unless (eq id *sent-no*)
      (error "Synch error: text ~A, tst ~A" *sent-no* id))
    (setq *tst-trees* nil
	  *tst-probs* nil
	  *prob-sum* 0)
    (read-ws-or-comment tst-file *tst-style*)
    (cond
     ;; Multiple test trees
     ((char= (peek-char nil tst-file nil nil)
	     (style-start-list *tst-style*))
      (read-char tst-file)
      (while-parseval (progn (read-ws-or-comment tst-file *tst-style*)
		    (not (and (setq char (peek-char nil tst-file nil nil))
			      (char= char (style-end-list *tst-style*)))))
	(read-test-tree tst-file))
      (when char (read-char tst-file)))
     ;; Single test tree
     (t (read-test-tree tst-file)))
    (unless *tst-trees* (error "Test tree ~A missing" *sent-no*))
    (setq *tst-trees* (nreverse *tst-trees*)
	  *tst-probs* (nreverse *tst-probs*))))

(defun read-test-tree (tst-file)
  (let ((i (or (read-number tst-file *tst-style*) 1))
	(tst-tree (read-tree tst-file *tst-style*)))
    (push i *tst-probs*)
    (push tst-tree *tst-trees*)
    (incf *prob-sum* i)))


