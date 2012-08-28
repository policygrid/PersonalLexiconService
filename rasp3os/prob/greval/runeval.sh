# runeval.sh

# run RASP on PARC 700 sentences (execute in Bourne Shell): sentence boundaries
# already marked, no morph, assign tag NP to NEs already marked in input 

rasp_sentence=cat rasp_morph=cat rasp_ner=./pn_ner.sh \
  ~/Desktop/rasp3/scripts/rasp.sh -p"-c0 -t40 -otg" -n < sents-ne-tst.txt > sents-ne-tst.out


# sentences for evaluation:

gawk 'BEGIN{sent=0} \
  /[0-9] ; [(]/ { \
    sent++; printf("\n%s\n", sent); \
    sub(/^[(]/,""); gsub(/[|\\]/,""); \
    gsub(/:[0-9]*_[^ ][^ <)]*/,""); gsub(/[)] [0-9] ; .*$/,""); \
    gsub(/<[/]?w>/,""); \
    print}' sents-ne-tst.out > sents-ne-tst.grtext

# GR gold standard: only include the "head" of things that are marked as
# "proper" in the PARC annotation

gawk 'BEGIN{hld=" 1 6 11 16 21 26 31 36 41 46 51 56 61 66 71 76 81 86 91 96 101 106 111 116 121 126 131 136 141 146 151 156 161 166 171 176 181 186 191 196 201 206 211 216 221 226 231 236 241 246 251 256 261 266 271 276 281 286 291 296 301 306 311 316 321 326 331 336 341 346 351 356 361 366 371 376 381 386 391 396 401 406 411 416 421 426 431 436 441 446 451 456 461 466 471 476 481 486 491 496 501 506 511 516 521 526 531 536 541 546 551 556 561 566 571 576 581 586 591 596 601 606 611 616 621 626 631 636 641 646 651 656 661 666 671 676 681 686 691 696 "; \
            sent=0; senth=0; sentt=0; rasp=0} \
  function output_gr() {\
    nmatches=0; \
    for (v in proper) {if (index($0,v)) {nmatches++}}; \
    if (nmatches<2) {print > outfile}} \
  {sub(/[ ]*;;;.*$/, "")} \
  /^sentence_form/ {delete proper}; \
  /  proper[(][^ ]* .*~/ {sub(/^.*  proper[(]/,""); sub(/~.*$/,""); \
    n=split($0,pr); \
    for (x in pr) \
      {sub(/\047s/,"",pr[x]); \
       if (proper[pr[x]]!=2) {proper[pr[x]]=1}; \
       proper[pr[n]]=2}} \
  /^rasp[(]/ { \
    sent++; 
    if (hld ~ " " sent " ") \
      {senth++; outfile="sents-ne-hld.gr"; printf("\n%s\n", senth) > outfile} \
      else {sentt++; outfile="sents-ne-tst.gr"; printf("\n%s\n", sentt) > outfile}; \
    rasp=1; next} \
  /^[)]$/ {rasp=0} \
  rasp==0 {next} \
  /^[(][^)]*[)][)][)]$/ {sub(/[)][)]$/, ""); output_gr(); rasp=0; next} \
  /^[(][^)]*[)][)]$/ {sub(/[)]$/, ""); output_gr(); rasp=0; next} \
  {output_gr()}' gold700files.rasp

# parser output to be evaluated (from rasp_parse.sh -otg option): extract
# GRs (moving any ncsubj GRs to end of each set), remove quote GRs, and map
# pairs of passive and ncsubj GRs to ncsubj...obj

cat sents-ne-tst.out  | \
gawk 'BEGIN{printf("%LB (\n%RB )\n"); sent=0} \
  /[0-9] ; [(]/ {sent++; printf("\n%s\n\n", sent); next} \
  /^[(][|]T[/]/ {next} \
  /^[(]X/ {next} \
  /^$/ {for (r in ncsubj) {print r}; delete ncsubj} \
  /^[(]/ { \
    gsub(/[|\\]/,""); gsub(/:[0-9]*_[^ ][^ <)]*/,""); \
    do {prev=$0; $0=gensub(/ ([^< ]*<[/]w>)/,"_\\1",1)} while (prev!=$0); \
    gsub(/<[/]?w>/,""); \
    if ($0 ~ /^[(]ncsubj /) {ncsubj[$0]=1; next}; \
    print}' | \
gawk '/^[(]quote / {next} \
  /^$/ {delete pass} \
  /^[(]passive / {p=$2; sub(/[)]$/,"",p); pass[p]=1} \
  /^[(]ncsubj / {v=$2; if (v in pass) {if ($4=="_)") {$4="obj)"}}} \
  {print}' > sents-ne-tst.parses


# run evaluation

openmcl
(load "gramreleval-new.lsp")

(setq stats
   (gramreleval "sents-ne-tst.grtext" "sents-ne-tst.gr" "sents-ne-tst.parses"
      "output"))

(greval-relation-summary t stats)

;;; results reported in ACL submission

Relation     Precision          Recall          F-score          tst GRs
 dependent       73.45           70.21            71.79          9878.00
 mod             73.65           69.47            71.50          4288.00
  ncmod          71.00           65.89            68.35          2859.00
  xmod           42.77           41.57            42.17           173.00
  cmod           50.00           33.73            40.28           114.00
  pmod           30.00           25.00            27.27            10.00
  det            87.81           89.87            88.83          1132.00
 arg_mod         70.38           66.30            68.28          3407.00
 arg             73.00           71.08            72.03          4252.00
  subj           74.85           64.68            69.40          1177.00
   ncsubj        75.91           64.97            70.01          1158.00
   xsubj         20.00           14.29            16.67             5.00
   csubj          7.14           50.00            12.50            14.00
  subj_or_dobj   78.44           71.61            74.87          2843.00
  comp           72.46           73.98            73.21          3068.00
   obj           76.53           74.71            75.61          2254.00
    dobj         80.97           77.00            78.94          1666.00
    obj2         23.53           38.10            29.09            34.00
    iobj         68.40           68.66            68.53           538.00
   clausal       61.18           71.55            65.96           814.00
    xcomp        75.46           75.07            75.26           379.00
    ccomp        48.29           68.04            56.49           410.00
    pcomp        56.00           58.33            57.14            25.00
 aux             92.25           86.25            89.15           374.00
 conj            72.99           70.17            71.55           522.00
 ta              43.49           46.99            45.17           269.00
 passive         92.02           65.79            76.73           163.00
 quote            0.00            0.00             0.00             0.00

Precision        73.45   Recall  70.21   F-score  71.79   tst GRs  17.64


(greval-confusion-summary t stats)


;;; Load in following to get
;;; Close matches as well

(defun greval-sentence (text std-list tst-list out-str sentn global-state)
   (let ((common nil)
         (approx nil)
         (missing std-list)
         (extra nil))
      (dolist (tst tst-list)
         (multiple-value-bind (std-match matchp) (gr-find tst std-list)
            (if matchp
               (progn
                  (push (list std-match tst) common)
                  (setq missing (remove std-match missing :test #'eq))
                  (setq std-list (remove std-match std-list :test #'eq)))
               (push tst extra))))
      (setq tst-list extra)
      (setq std-list missing)
      (setq extra nil)
      (dolist (tst tst-list)
         (let ((std-match (gr-approx-find tst std-list)))
            (if std-match
               (progn
                  (push (list std-match tst) approx)
                  (setq missing (remove std-match missing :test #'eq))
                  (setq std-list (remove std-match std-list :test #'eq)))
               (push tst extra))))
      (format out-str "~%------------~%~A~%~A~%~%" sentn text)
      (format out-str "In both:~%~:{   ~A : ~A~%~}~%"
         (reverse (mapcar #'(lambda (x) (list (cdr (car x)) (cdr (cadr x)))) common)))
      (format out-str "Close match:~%~:{   ~A : ~A~%~}~%"
         (reverse (mapcar #'(lambda (x) (list (cdr (car x)) (cdr (cadr x)))) approx)))
      (format out-str "Standard only:~%~{   ~A~%~}~%" (mapcar #'cdr missing))
      (format out-str "Test only:~%~{   ~A~%~}~%" (reverse (mapcar #'cdr extra)))))


(defun gr-approx-find (tst std-lst)
   (let ()
      (dolist (std std-lst)
         (let*
            ((tst-slots (gr-slot-list (cdr tst)))
             (std-slots (gr-slot-list (cdr std))))
            (when
               (and
                  (eql (length (intersection std-slots '(HEAD DEPENDENT)))
                       (length (intersection tst-slots '(HEAD DEPENDENT))))
                  (every
                     #'(lambda (std-slot std-arg)
                         (if (member std-slot '(HEAD DEPENDENT))
                            (some
                               #'(lambda (tst-slot tst-arg)
                                   (if (member tst-slot '(HEAD DEPENDENT))
                                      (progn #+ignore (print (list std-slot std-arg tst-slot tst-arg
                                                 (gr-arg-equalp tst-arg std-arg)))
                                         (gr-arg-equalp tst-arg std-arg))
                                      nil))
                               tst-slots (rest (cdr tst)))
                            t))
                     std-slots (rest (cdr std))))
               (return-from gr-approx-find std))))))
