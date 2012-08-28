;;; CHECK-PARC.LSP
;;;
;;; Compare file of merged sets of PARC and RASP grammatical relations, identifying
;;; ones that match completely, partial matches, and ones that are only in either of
;;; the two sets.
;;;
;;; E.g.
;;;
;;; (compare-parc-rasp-rels "parc700.txt" "out.txt")
;;; (compare-parc-rasp-rels "gold700files.rasp" "out.txt")


;; words where inflected form does not have same first 3 letters as lemma, taking
;; account of idiosyncracies of PARC lemmatisation. Only includes words in the
;; 700 sentences. 

(defparameter +fuzzy-lexical-items+
  '(
    ("10000" . ("10,000"))
    ("5000" . ("5,000"))
    ("2000" . ("2,000"))
    ("September" . ("Sept."))
    ("a" . ("an"))
    ("be" . ("am" "are" "is" "was" "were" "'m" "'s" "'re" "being" "been"))
    ("break" . ("broke"))
    ("bring" . ("brought"))
    ("buy" . ("bought"))
    ("can" . ("ca"))
    ("catch" . ("caught"))
    ("come" . ("came"))
    ("do" . ("does" "did" "doing" "done"))
    ("fall" . ("fell"))
    ("feed" . ("fed"))
    ("find" . ("found"))
    ("five" . ("fifth"))
    ("get" . ("got"))
    ("give" . ("gave"))
    ("go" . ("goes" "going" "gone" "went"))
    ("grow" . ("grew"))
    ("have" . ("has" "had" "'ve"))
    ("he" . ("him" "his" "himself"))
    ("hold" . ("held"))
    ("I" . ("me" "my" "mine" "myself"))
    ("it" . ("its" "itself"))
    ("keep" . ("kept"))
    ("know" . ("knew"))
    ("lay" . ("laid"))
    ("lead" . ("led"))
    ("make" . ("made"))
    ("meet" . ("met"))
    ("not" . ("n't"))
    ("one" . ("first"))
    ("pay" . ("paid"))
    ("percent" . ("%"))
    ("rise" . ("rose"))
    ("say" . ("said"))
    ("see" . ("saw"))
    ("sell" . ("sold"))
    ("she" . ("her" "hers" "herself"))
    ("shoot" . ("shot"))
    ("sing" . ("sang" "sung"))
    ("sit" . ("sat"))
    ("stick" . ("stuck"))
    ("take" . ("took"))
    ("teach" . ("taught"))
    ("tell" . ("told"))
    ("that" . ("those"))
    ("they" . ("them" "their" "theirs" "themselves"))
    ("this" . ("these"))
    ("think" . ("thought"))
    ("three" . ("third"))
    ("try" . ("tried"))
    ("two" . ("second"))
    ("use" . ("using"))
    ("we" . ("us" "our" "ourselves"))
    ("will" . ("wo" "'ll"))
    ("win" . ("won"))
    ("would" . ("'d"))
    ("you" . ("your" "yours" "yourself"))
   ))


;; PARC rels that are superfluous

; NB must first substitute value of coord_form and pron_form into all rels

(defparameter +rels-to-ignore+
  '(|adegree|
    |coord_form|
    |focus_int|
    |num|
    |number_type|
    |perf|
    |pron_form|
    |pron_int|
    |pron_rel|
    |prog|
    |proper|
    |stmt_type|
    |tense|
    |topic_rel|))


;; correspondances between sets of PARC and RASP relations

(defparameter +rel-translations+
  '(

;  adjunct(situation~7, encourage~12)
;  subj(encourage~12, that)
;(cmod that situation encouraged)
;(ncsubj encouraged situation _)
;  adjunct($~9, in~11)
;(ncmod _ $ in)
;  adjunct(be~0, say~10)
;(xmod _ 's said)
;  adjunct(through~7, but~22)
;(conj but through)
;  adjunct(become~0, with~6)
;(cmod _ become with)
;  adjunct(laptop~2, pro~5)
;(ta bal Laptops anything)
;  adjunct(funding~13, such~45)
;(det funding such)
;  adjunct(make~18, pro~23)
;(arg_mod _ make where)
;  adjunct(hold~0, as~7)
;(pmod held as)

  (((|adjunct| 1 2) (|subj| 2 3)) . ((|cmod| 3 1 2) (|ncsubj| 2 1 "_")))
  (((|adjunct| 1 2)) . ((|ncmod| "_" 1 2)))
  (((|adjunct| 1 2)) . ((|xmod| "_" 1 2)))
  (((|adjunct| 1 2)) . ((|conj| 2 1)))
  (((|adjunct| 1 2)) . ((|cmod| "_" 1 2)))
  (((|adjunct| 1 2)) . ((|ta| "_" 1 2)))
  (((|adjunct| 1 2)) . ((|det| 1 2)))
  (((|adjunct| 1 2)) . ((|arg_mod| "_" 1 2)))
  (((|adjunct| 1 2)) . ((|pmod| 1 2)))

;  aquant(part~7, most~10)
;(ncmod _ part most)

  (((|aquant| 1 2)) . ((|ncmod| "_" 1 2)))

;  comp(tell~14, be~15)
;(ccomp _ told be)
;  comp(announce~0, give~52)
;  subord_form(give~52, that)
;(ccomp that announced given)
;  comp(say~9, be~0)
;(comp said is)

  (((|comp| 1 2)) . ((|ccomp| "_" 1 2)))
  (((|comp| 1 2) (|subord_form| 2 3)) . ((|ccomp| 3 1 2)))
  (((|comp| 1 2)) . ((|comp| 1 2)))

;  conj(and, be~11)
;(conj and was)

  (((|conj| 1 2)) . ((|conj| 1 2)))

;  det_form(premium~3, a)
;(det premium a)

  (((|det_form| 1 2)) . ((|det| 1 2)))

;  mod(debate~14, budget~19)
;(ncmod _ debate budget)

  (((|mod| 1 2)) . ((|ncmod| "_" 1 2)))

;  number($~24, million~4)
;  adjunct(million~4, 30.5~28)
;(ncmod _ $ 30.5)
;(ncmod num 30.5 million)
;  number(pound~11, 15~14)
;(ncmod _ pounds 15)
;  number(manufacturer~4, one~27)
;(det manufacturer One))

  (((|number| 1 2) (|adjunct| 2 3)) .
   ((|ncmod| "_" 1 3) (|ncmod| "num" 3 2)))
  (((|number| 1 2)) . ((|ncmod| "_" 1 2)))
  (((|number| 1 2)) . ((|det| 1 2)))

;  obj(depend~2, build~5)
;  pcase(build~5, on)
;(xcomp _ depends on)
;(xcomp _ on building)
;  obj(assume~7, $~9)
;(dobj assume $)
;  obj(of~59, take~60)
;(xcomp _ of taking)
;  obj(although~5, soar~13)
;(ccomp _ Although soars)
;  obj(when~52, come~6)
;(arg_mod _ comes When)
;  obj(comply~0, request~3)
;  pcase(request~3, with)
;(iobj complied with)
;(dobj with request)

  (((|obj| 1 2) (|pcase| 2 3)) . ((|xcomp| "_" 1 3) (|xcomp| "_" 3 2)))
  (((|obj| 1 2)) . ((|dobj| 1 2)))
  (((|obj| 1 2)) . ((|xcomp| "_" 1 2)))
  (((|obj| 1 2)) . ((|ccomp| "_" 1 2)))
  (((|obj| 1 2)) . ((|arg_mod| "_" 2 1)))
  (((|obj| 1 3) (|pcase| 3 2)) . ((|iobj| 1 2) (|dobj| 2 3)))

;  obj_theta(give~0, image~3)
;(obj2 give image)

  (((|obj_theta| 1 2)) . ((|obj2| 1 2)))

;  obl(receive~10, from~26)
;(iobj receive from)
;  obl(one~2, portfolio~14)
;(ncmod _ one of)
;(dobj of portfolios)
;  obl(know~0, of~2)
;(ccomp _ know of)

  (((|obl| 1 2)) . ((|iobj| 1 2)))
  (((|obl| 1 2)) . ((|ncmod| "_" 1 "of") (|dobj| "of" 2)))
  (((|obl| 1 2)) . ((|ccomp| "_" 1 2)))

;  obl_compar(less~16, than~17)
;(ncmod _ less than)

  (((|obl_compar| 1 2)) . ((|ncmod| "_" 1 2)))

;  obl_ag(pass~19, house~20)
;  subj(pass~19, pro~21)
;  pcase(house~20, by)
;(ncmod _ passed by)
;(dobj by House)
;  obl_ag(obsolete~0, machine~1)
;  subj(obsolete~0, guy~2)
;  pcase(machine~1, by)
;(ncsubj obsoleted guys _)
;(ncmod _ obsoleted by)
;(dobj by machines)

  (((|obl_ag| 1 2) (|subj| 1 "pro") (|pcase| 2 "by")) .
   ((|ncmod| "_" 1 "by") (|dobj| "by" 2)))
  (((|obl_ag| 1 2) (|subj| 1 3) (|pcase| 2 "by")) .
   ((|ncmod| "_" 1 "by") (|dobj| "by" 2) (|ncsubj| 1 3 "_")))

;  passive(find~10, +)
;(passive found)

  (((|passive| 1 "+")) . ((|passive| 1)))

;  pcase(build~5, on)
;(xcomp _ on building)

  (((|pcase| 1 2)) . ((|xcomp| "_" 2 1)))
  
;  poss(reaction~12, company~4)
;(ncmod poss reaction company)
;  poss(audience~2, pro~37)
;(det audience their)

  (((|poss| 1 2)) . ((|ncmod| "poss" 1 2)))
  (((|poss| 1 2)) . ((|det| 1 2)))

;  precoord_form(coord~20, either)
;???

  (((|precoord_form| 1 2)) . ((|ncmod| "_" 1 2)))

;  prt_form(tiptoe~0, back)
;(ncmod prt tiptoed back)

  (((|prt_form| 1 2)) . ((|ncmod| "prt" 1 2)))

;  quant(program~41, more~57)
;(ncmod _ programs more)
;  quant(right~11, any~39)
;(det right any)

  (((|quant| 1 2)) . ((|ncmod| "_" 1 2)))
  (((|quant| 1 2)) . ((|det| 1 2)))

;  subj(would~50, pro~2)
;  subj(say~1, pro~2)
;  xcomp(would~50, say~1)
;(ncsubj say I _)
;(aux say would)
;  subj(pay~0, Meridian~5)
;(ncsubj pay Meridian _)
;  subj(be~2, sell~7)
;(xsubj is selling _)

  (((|subj| 1 3) (|subj| 2 3) (|xcomp| 1 2)) . ((|ncsubj| 2 3 "_") (|aux| 2 1)))
  (((|subj| 1 2)) . ((|ncsubj| 1 2 "_")))
  (((|subj| 1 2)) . ((|xsubj| 1 2 "_")))

;  xcomp(be~11, behind~13)
;(pcomp was behind)
;  xcomp(be~15, problem~28)
;(xcomp _ be problem)
;  xcomp(have~0, act~1)
;(ncmod prt have to)
;(aux have act)

  (((|xcomp| 1 2)) . ((|pcomp| 1 2)))
  (((|xcomp| 1 2)) . ((|xcomp| "_" 1 2)))
  (((|xcomp| 1 2)) . ((|ncmod| "prt" 1 "to") (|aux| 1 2)))

))


;;; (compare-parc-rasp-rels "parc700.txt" "out.txt")

#|
E.g. of file format:

sentence(
  id(wsj_2308.8, parc_23.250)
  date(2002.6.12)
  validators(T.H. King, J.-P. Marcotte)
sentence_form(Nevertheless\, the company's reaction underscores the domino effect that a huge manufacturer such as Boeing can have on other parts of the economy.)
structure(  adjunct(underscore~0, nevertheless~8)
  ...
  num(domino~46, sg))
rasp(
(ncmod _ underscores Nevertheless)
(ncsubj underscores reaction _)
...
(det company the))
)

|#

(defun compare-parc-rasp-rels (merged-file out-file &aux id (n 0))
   (with-open-file (in merged-file :direction :input)
      (with-open-file (out out-file :direction :output
                       :if-exists :supersede :if-does-not-exist :create)
         (loop
            (let ((line (read-line in nil t)))
               (unless (stringp line) (return))
               (cond
                  ((eql (search "sentence(" line) 0))
                  ((eql (search "  id(" line) 0) (setq id (subseq line 2)))
                  ((eql (search "  date(" line) 0))
                  ((eql (search "  validators(" line) 0))
                  ((eql (search "sentence_form(" line) 0)
                     (format out "~%~%------~%~A~%~A~%~%" id line)
                     (setq id nil n (1+ n))
                     (let* ((sent
                              (string-right-trim ")" (subseq line 14)))
                            (parc (read-parc-rels in))
                            (rasp (read-rasp-rels in)))
                        (check-rel-connectivity parc "PARC" out)
                        (when rasp
                           (check-rel-connectivity rasp "RASP" out)
                           (check-word-coverage rasp sent out)
                           (multiple-value-bind (l-rels r-rels matched partial)
                                 (translate-and-match-rels parc rasp)
                              (format out
"Matched:~%~{  ~{~{~A~^ ~} : ~{~A~^ ~}~}~^~%~}~%Partial:~%~{  ~{~{~A~^ ~} : ~{~A~^ ~}~}~^~%~}
Remaining PARC:~%~{  ~A~^~%~}~%Remaining RASP:~%~{  ~A~^~%~}~%"
                                 matched partial l-rels r-rels)))))
                  ((equal line ")"))
                  ((every #'(lambda (c) (eql c #\space)) line))
                  (t (error "unrecognised line '~A'" line)))))))
   n)
                   

;;; Read PARC format and RASP format sets of rels

(defun read-parc-rels (str)
   (let ((rels nil) (finishing nil))
      (loop
         (let ((line (read-line str nil t)))
            (unless (stringp line) (return))
            (when (equal (subseq line 0 10) "structure(")
               (setq line (subseq line 10)))
            (when (equal (subseq line (- (length line) 2) (length line)) "))")
               (setq line (subseq line 0 (- (length line) 1)))
               (setq finishing t))
            (setq line (string-trim " " line))
            (let* ((p (position #\( line))
                   (rel (list (intern (subseq line 0 p))))
                   (x nil))
               (loop
                  (incf p)
                  (let ((c (schar line p)))
                     (cond
                        ((member c '(#\) #\,))
                           (push (concatenate 'string (nreverse x)) rel)
                           (setq x nil)
                           (when (eql c #\))
                              (push (nreverse rel) rels)
                              (return)))
                        ((eql c #\\) (incf p) (setq c (schar line p)) (push c x))
                        ((eql c #\space) (when x (push c x)))
                        (t (push c x)))))))
         (when finishing (return)))
      (setq rels (nreverse rels))
      (dolist (rel rels)
         (when (member (first rel) '(|pron_form| |coord_form|))
            (setq rels
               (subst (third rel) (second rel) (copy-tree rels) :test #'equal))))
      (mapcan
         #'(lambda (rel)
            (unless (member (first rel) +rels-to-ignore+)
               (list
                  (mapcar
                     #'(lambda (x)
                         (if (stringp x)
                            (let ((tilde (position #\~ x :from-end t)))
                               (if tilde (subseq x 0 tilde) x))
                            x))
                      rel))))
         rels)))


(defun read-rasp-rels (str)
   (let ((rels nil))
      (loop
         (let ((line (read-line str nil t)))
            (when (or (not (stringp line)) (equal line "") (equal line ")")) (return))
            (setq line (string-trim " " line))
            (unless (equal line "rasp(")
               (when (equal (subseq line (- (length line) 2) (length line)) "))")
                  (setq line (subseq line 0 (- (length line) 1))))
               (let* ((p (position #\space line))
                      (rel (list (intern (subseq line 1 p))))
                      (x nil))
                  (loop
                     (incf p)
                     (let ((c (schar line p)))
                        (cond
                           ((member c '(#\) #\space))
                              (when x (push (concatenate 'string (nreverse x)) rel))
                              (setq x nil)
                              (when (eql c #\))
                                 (push (nreverse rel) rels)
                                 (return)))
                           (t (push c x)))))))))
      (nreverse rels)))


;;;

(defparameter +ignore-marker-tokens+
   '("_" "bal" "colon" "comma" "ellip" "inv" "num" "obj" "poss" "prt" "pl" "pro"
     "sg" "ta" "+" "-"))

(defun check-rel-connectivity (rels type out)
   (format out "~A:~%" type)
   (let ((pairs nil))
      (dolist (rel rels)
         (case (length rel)
            (2)
            (3 (unless (member (third rel) +ignore-marker-tokens+ :test #'equal)
                  (setq pairs (add-connectivity (second rel) (third rel) pairs))))
            (4 (setq pairs 
            	  (cond
            	     ((member (second rel) +ignore-marker-tokens+ :test #'equal)
                        (add-connectivity (third rel) (fourth rel) pairs))
                     ((member (fourth rel) +ignore-marker-tokens+ :test #'equal)
                        (add-connectivity (second rel) (third rel) pairs))
                     (t
                        (add-connectivity (third rel) (fourth rel)
                           (add-connectivity (third rel) (second rel) pairs))))))
            (t (format out "Ignoring n-ary rel: ~A~%"
                  type rel))))
      (let ((hs nil) (ds nil) (tops nil))
         (dolist (p pairs)
            (pushnew (car p) hs :test #'equal)
            (pushnew (cdr p) ds :test #'equal))
         (setq tops (set-difference hs ds :test #'equal))
         (cond
            ((null tops)
               (format out "No unique top node - cannot check further~%")
               (return-from check-rel-connectivity nil))
            ((cdr tops)
               (format out "Multiple top nodes: ~{~A~^, ~}~%" tops)))
         (let ((links (make-hash-table :test #'equal)))
            (dolist (p pairs)
               (unless (gethash (car p) links)
                  (setf (gethash (car p) links) (list (car p))))
               (unless (gethash (cdr p) links)
                  (setf (gethash (cdr p) links) (list (cdr p))))
               (setf (cdr (gethash (car p) links))
                  (merge 'list (list (gethash (cdr p) links))
                     (cdr (gethash (car p) links)) #'string-lessp :key #'car)))
            (dolist (top tops)
               (check-for-rel-circularities
                  (gethash top links) links (make-hash-table :test #'eq) out))
            (write
               (mapcar #'(lambda (top) (gethash top links)) tops)
               :stream out :circle t :pretty t :escape nil)
            (terpri out)))))


(defun add-connectivity (h d pairs)
   (cons (cons h d) pairs))


(defun check-for-rel-circularities (n links visited out)
   (cond
      ((eq (gethash n visited) :inside)
         (format out "Circularity found: ~A~%" (car n)))
      ((gethash n visited))
      (t
         (setf (gethash n visited) :inside)
         (dolist (d (cdr n))
            (check-for-rel-circularities d links visited out))
         (setf (gethash n visited) t))))


;;; (check-word-coverage nil "For instance: ``Haole'' \(white\) is not the ultimate insult; ``Mainland haole'' is." t)

(defun check-word-coverage (rels str out &aux words partial)
   (labels
      ((tokenise (n)
          (if (>= n (length str))
             (when partial
                (push (coerce (reverse partial) 'string) words))
             (let ((c (schar str n)))
                (case c
                   ((#\\ #\`))
                   ((#\n)
                      (if (and (> (length str) (+ n 2))
                                 (eql (schar str (1+ n)) #\')
                                 (eql (schar str (+ n 2)) #\t))
                         (when partial
                            (push (coerce (reverse partial) 'string) words)
                            (setq partial '(#\t #\' #\n))
                            (incf n 2))
                         (push c partial)))
                   ((#\')
                      (when partial
                         (push (coerce (reverse partial) 'string) words))
                      (setq partial (list c))
                      (when (and (> (length str) (1+ n))
                                 (eql (schar str (1+ n)) #\'))
                         (push c partial)
                         (incf n)))
                   ((#\$ #\% #\; #\( #\) #\{ #\} #\space)
                      (when partial
                         (push (coerce (reverse partial) 'string) words))
                      (setq partial nil)
                      (unless (eql c #\space)
                         (push (coerce (list c) 'string) words)))
                   ((#\, #\:)
                      (if (and partial
                               (> (length str) (1+ n))
                               (digit-char-p (schar str (1+ n))))
                         (push c partial)      
                         (progn
                            (when partial
                               (push (coerce (reverse partial) 'string) words))
                            (setq partial (list c)))))
                   (t (push c partial)))
                (tokenise (1+ n))))))
      (tokenise 0)
      (let ((rels-words nil)
            (sent-words nil))
         (do* ((wtail words (cdr wtail))
               (w (car wtail) (car wtail)))
              ((null wtail))
              (cond
                 ((member w '("''" "," ";" ":" "--" "(" ")" "{" "}") :test #'equal))
                 ((and (null sent-words)
                     (member (schar w (1- (length w))) '(#\. #\: #\? #\!)))
                    (when (> (length w) 1)
                       (push (subseq w 0 (1- (length w))) sent-words)))
                 (t (push w sent-words))))
         (dolist (rel rels)
            (loop for w in (cdr rel) and n from 2
               do
               (cond
                  ((equal w "poss")
                     (cond
                        ((member "'s" sent-words :test #'equal) (pushnew "'s" rels-words :test #'equal))
                        ((member "'" sent-words :test #'equal) (pushnew "'" rels-words :test #'equal))))
                  ((member w +ignore-marker-tokens+ :test #'equal))
                  ((and (member (car rel) '(|ta| |ncmod|)) (eql n 2)))
                  (t (pushnew w rels-words :test #'equal)))))
         (let ((missing (set-difference sent-words rels-words :test #'equal))
               (extra (set-difference rels-words sent-words :test #'equal)))
            (when missing
               (format out "RASP words missing: ~A~%" missing))
            (when extra
               (format out "RASP words extra: ~A~%" extra))))))
      

;;;

(defun translate-and-match-rels (l-rels r-rels)
   (let ((rels-matched nil) (rels-partially-matched nil))
      ;; complete match according to translation rules
      (dolist (tran +rel-translations+)
         (dolist (l-matched-and-bindings
                    (rel-translation-matches (car tran) l-rels (list nil)))
            (let ((r-matched 
               	    (find-matching-rels (cdr tran) r-rels (cdr l-matched-and-bindings))))
               (when r-matched
               	  (setq l-rels
               	     (set-difference l-rels (car l-matched-and-bindings) :test #'equal))
               	  (setq r-rels
               	     (set-difference r-rels r-matched :test #'equal))
               	  (push (list (car l-matched-and-bindings) r-matched) rels-matched)))))
      ;; partial match such that a pair of rels contains the same words
      (dolist (l-rel (copy-list l-rels))
         (block found
            (dolist (r-rel (copy-list r-rels))
               (dolist (w (cdr l-rel)
                          (progn
                             (push (list (list l-rel) (list r-rel)) rels-partially-matched)
                             (setq l-rels (remove l-rel l-rels :test #'eq))
                             (setq r-rels (remove r-rel r-rels :test #'eq))
                             (return-from found)))
                  (unless (some #'(lambda (w2) (fuzzy-lexical-match w w2)) (cdr r-rel))
                     (return nil))))))
      (values l-rels r-rels rels-matched rels-partially-matched)))


#|
(translate-and-match-rels
'((|passive| "found" "+") (|subj| "found" "pro") (|obl_ag| "found" "courts"))
'((|passive| "found") (|ncmod| "_" "found" "by") (|dobj| "by" "court"))
)
|#


(defun rel-translation-matches (patt rels matched-and-bindings)
   ;; find all ways of matching given pattern against set of rels
   (if patt
      (mapcan
         #'(lambda (m-b)
            (mapcan
               #'(lambda (r)
                  (multiple-value-bind (matchp newb)
                        (rel-translation-match-p (car patt) r (cdr m-b))
                     (when matchp
                        (rel-translation-matches
                           (cdr patt) (remove r rels :test #'eq)
                           (list (cons (cons r (car m-b)) newb))))))
               rels))
         matched-and-bindings)
      matched-and-bindings))

(defun rel-translation-match-p (x y bindings)
   (and (eql (length x) (length y))
      (loop for xi in x and yi in y
         do
         (cond
            ((or (symbolp xi) (symbolp yi))
               (unless (eq xi yi) (return nil)))
            ((equal xi "_"))
            ((numberp xi)
               (if (assoc xi bindings)
                  (unless (fuzzy-lexical-match (cdr (assoc xi bindings)) yi) (return nil))
                  (push (cons xi yi) bindings)))
            ((fuzzy-lexical-match xi yi))
            (t (return nil)))
         finally (return (values t bindings)))))


#|
(rel-translation-matches
'((|obl_ag| 1 2) (|subj| 1 "pro"))
'((|obl_ag| "pass" "house") (|subj| "pass" "pro"))
(list nil))
|#


(defun find-matching-rels (action rels bindings)
   ;; look for rels in action in set of rels consistent with bindings
   (let ((matched nil))
      (dolist (rel rels)
         (dolist (a action)
            (when (rel-translation-match-p a rel bindings)
               (push rel matched)
               (setq action (remove a action :test #'eq))
               (if action (return) (return-from find-matching-rels matched)))))
      nil))

#|
(find-matching-rels
'((|passive| 1) (|ncmod| "_" 1 "by") (|dobj| "by" 2))
'((|passive| "found") (|ncmod| "_" "found" "by") (|dobj| "by" "court"))
'((2 . "court") (1 . "found")))
|#


(defun fuzzy-lexical-match (x y)
   ;; e.g. (fuzzy-lexical-match "have" "Having")
   (or
      (string-equal x y)
      (dolist (alt +fuzzy-lexical-items+ nil)
         (when (and (string-equal x (car alt)) (member y (cdr alt) :test #'string-equal))
            (return t)))
      (and (find #\space x) (search y x))
      (and (>= (length x) 3) (>= (length y) 3)
         (string-equal (subseq x 0 3) (subseq y 0 3)))))


;;; End of file
