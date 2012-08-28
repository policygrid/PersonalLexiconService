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

;;; Augment prob parser with acquired word vsubcat value freqs - works only
;;; with dparse.lsp

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;;

(defvar *subcat-probs-p* t)

(defstruct epattern target subcat classes reliability freqscore relfreq freqcnt
   dict tltl sltl olt1l olt2l olt3l lrl)

(defun read-subcat-scores (file)
   (with-open-file (in file :direction :input)
      (loop
         (let ((form (read in nil t)))
            (when (eq form t) (return file))
            (when form
               (let ((word (epattern-target (car form))))
		 ;;(format t "word ~A~%" word)
		 (setf (get word 'freqs) nil)
                  (dolist (patt form)
                     (let ((freq (epattern-relfreq patt))
                           (already
                             (assoc (cadr (epattern-subcat patt)) (get word 'freqs))))
                        (if already
                           (incf (cdr already) freq)
                           (push
                              (cons (cadr (epattern-subcat patt)) freq)
                              (get (epattern-target patt) 'freqs)))))
                  (dolist (pair (get word 'freqs))
                     (setf (cdr pair)
                        (log (if (zerop (cdr pair)) least-positive-single-float (cdr pair)) 10)))
                  (push (log least-positive-single-float 10) (get word 'freqs))))))))


;;; FEATURE VSUBCAT{SCOMP, SINF, SING, SING_PP, VPINF, VPING,
;;; VPING_PP, VPPRT, VPBSE, NP, NP_NP, NP_PP, NP_WHPP, NP_PPOF, NP_PP_PP,
;;; NP_AP, NP_SCOMP, NP_NP_SCOMP, PP, WHPP, PP_PP, PP_WHPP, PP_SCOMP,
;;; PP_WHS, PP_WHVP, PP_VPINF, PP_AP, AP, NONE}
;;;
;;; can't deal with T/lmta_v V/cj_beg V/cj_int V/cj_end V/0_p

(defparameter +rule-name-vsubcats+ '(
("V1/v_ppart" VPPRT . nil)
("V1/v_s" SCOMP . NP_SCOMP)
("V1/v_tcl" SCOMP . NP_SCOMP)
("V1/v_s-subjgap-r" SCOMP . NP_SCOMP)
("V1/v_np_inf" SINF . nil)
("V1/v_np_prt_inf" SINF . nil)
("V1/v_np_ing" SING . nil)
("V1/v_poss_ing" SING . nil)
("V1/v_np_prt_ing" SING . nil)
("V1/v_np_ing_pp" SING_PP . nil)
("V1/v_poss_ing_pp" SING_PP . nil)
("V1/v_inf" VPINF . SINF)
("V1/v_ing" VPING . SING)
("V1/v_ing_pp" VPING_PP . nil) ; SING_PP
("V1/v_pp_ing-hs-r" VPING_PP . nil) ; SING_PP
("V1/v_bse" VPBSE . SCOMP)
("V1/v_n_of_np" NP . NP_NP)
("V1/v_np" NP . NP_NP)
("V1/v_np-pro" NP . NP_NP)
("V1/v_np-name" NP . NP_NP)
("V1/v_np-tit" NP . NP_NP)
("V1/v_np-pl" NP . NP_NP)
("V1/v_np-org" NP . NP_NP)
("V1/v_np-refl" NP . NP_NP)
("V1/v_np-dir" NP . NP_NP)
("V1/v_n1" NP . NP_NP)
("V1/v_n1-tit" NP . NP_NP)
("V1/v_np-ms" NP . NP_NP)
("V1/v_np-num" NP . NP_NP)
("V1/v_np_prt" NP . NP_NP)
("V1/v_np-pro_prt" NP . NP_NP)
("V1/v_np-name_prt" NP . NP_NP)
("V1/v_np-tit_prt" NP . NP_NP)
("V1/v_np-pl_prt" NP . NP_NP)
("V1/v_np-org_prt" NP . NP_NP)
("V1/v_np-refl_prt" NP . NP_NP)
("V1/v_n1_prt" NP . NP_NP)
("V1/v_n1-tit_prt" NP . NP_NP)
("V1/v_np-ms_prt" NP . NP_NP)
("V1/v_np-num_prt" NP . NP_NP)
("V1/v_np_np" NP_NP . nil)
("V1/v_np_np-pl" NP_NP . nil)
("V1/v_np_np-org" NP_NP . nil)
("V1/v_np_np-name" NP_NP . nil)
("V1/v_np_np-tit" NP_NP . nil)
("V1/v_np_np-ms" NP_NP . nil)
("V1/v_np_np-num" NP_NP . nil)
("V1/v_n1_np" NP_NP . nil)
("V1/v_np-pro_np" NP_NP . nil)
("V1/v_np-pro_np-pl" NP_NP . nil)
("V1/v_np-pro_np-org" NP_NP . nil)
("V1/v_np-pro_np-name" NP_NP . nil)
("V1/v_np-pro_np-tit" NP_NP . nil)
("V1/v_np-pro_np-pro" NP_NP . nil)
("V1/v_np-pro_np-ms" NP_NP . nil)
("V1/v_np-pro_np-num" NP_NP . nil)
("V1/v_np-name_np" NP_NP . nil)
("V1/v_np-name_np-pl" NP_NP . nil)
("V1/v_np-name_np-org" NP_NP . nil)
("V1/v_np-name_np-name" NP_NP . nil)
("V1/v_np-name_np-tit" NP_NP . nil)
("V1/v_np-name_np-ms" NP_NP . nil)
("V1/v_np-name_np-num" NP_NP . nil)
("V1/v_np-pl_np" NP_NP . nil)
("V1/v_np-org_np" NP_NP . nil)
("V1/v_np-pl_np-pl-r" NP_NP . nil)
("V1/v_np-org_np-org-r" NP_NP . nil)
("V1/v_np-org_np-pl" NP_NP . nil)
("V1/v_np-pl_np-org" NP_NP . nil)
("V1/v_np-pl_np-name" NP_NP . nil)
("V1/v_np-org_np-name" NP_NP . nil)
("V1/v_np-pl_np-tit" NP_NP . nil)
("V1/v_np-org_np-tit" NP_NP . nil)
("V1/v_np-pl_np-ms" NP_NP . nil)
("V1/v_np-org_np-ms" NP_NP . nil)
("V1/v_np-pl_np-num" NP_NP . nil)
("V1/v_np-org_np-num" NP_NP . nil)
("V1/v_np-refl_np" NP_NP . nil)
("V1/v_np-refl_np-pl" NP_NP . nil)
("V1/v_np-refl_np-name" NP_NP . nil)
("V1/v_np-refl_np-tit" NP_NP . nil)
("V1/v_np-refl_np-ms" NP_NP . nil)
("V1/v_np-refl_np-num" NP_NP . nil)
("V1/v_np_prt_np" NP_NP . nil)
("V1/v_np_prt_n1" NP_NP . nil)
("V1/v_n1_prt_np" NP_NP . nil)
("V1/v_np-pro_prt_np" NP_NP . nil)
("V1/v_np-name_prt_np" NP_NP . nil)
("V1/v_np-pl_prt_np" NP_NP . nil)
("V1/v_np-org_prt_np" NP_NP . nil)
("V1/v_np-refl_prt_np" NP_NP . nil)
("V1/v_np_np_prt" NP_NP . nil)
("V1/v_np_np-wh" NP_NP . nil)
("V1/v_np_np-ms_inf" NP_NP_VPINF . nil)
("V1/v_np_np-nt_inf" NP_NP_VPINF . nil)
("V1/v_np_np-ms_pp" NP_NP_PP . nil)
("V1/v_np_np-nt_pp" NP_NP_PP . nil)
("V1/v_np_pp" NP_PP . nil)
("V1/v_np_prt_pp" NP_PP . nil)
("V1/v_n1_pp" NP_PP . nil)
("V1/v_pp_np-hs-r" NP_PP . nil)
("V1/v_np_pp-wh" NP_WHPP . nil)
("V1/v_np_of-np" NP_PPOF . nil)
("V1/v_np_of-ing" NP_PPOF . nil)
("V1/v_np_pp_pp" NP_PP_PP . nil)
("V1/v_np_ap" NP_AP . nil)
("V1/v_ap_np-hs-r" NP_AP . nil)
("V1/v_np_prt_ap" NP_AP . nil)
("V1/v_np_s" NP_SCOMP . NP_NP_SCOMP)
("V1/v_np_prt_s" NP_SCOMP . NP_NP_SCOMP)
("V1/v_np_np_s" NP_NP_SCOMP . nil)
("V1/v_pp" PP . nil) ; NP_PP !!! stop PPby becoming a complement in passives
("V1/v_pp-of" PPOF . NP_PP)
("V1/v_pp-wh" WHPP . NP_WHPP)
("V1/v_pp_pp" PP_PP . nil) ; NP_PP_PP
("V1/v_pp_pp-wh" PP_WHPP . nil)
("V1/v_pp_s" PP_SCOMP . nil)
("V1/v_pp_s-wh" PP_WHS . nil)
("V1/v_pp_np-wh" PP_WHVP . nil)
("V1/v_pp_inf" PP_VPINF . nil)
("V1/v_pp_ap" PP_AP . nil)
("V1/v_ap" AP NP_AP)
("V1/v" NONE . NP)
))


;;; Called from lr1-parse-unpack-score when *subcat-probs-p* is true

(defun lr1-parse-unpack-score-comp (tree rname old-score)
  (if
      (and (atom (cadr (car (last (cdr tree))))) ; lexical 1st daughter
           (lr1-parse-unpack-v0-cat-p
              (node-category (car (car (last (cdr tree)))))))
      
      (let* ((word
                (string (cadr (car (last (cdr tree))))))
	     (stem
                (find-symbol (lr1-parse-unpack-stem word)))
	     (tag
	      (intern
	       (subseq word (1+ (position #\_ word :from-end t))))))
	(let*
             ((entry (assoc rname +rule-name-vsubcats+ :test #'equal))
              (val
                 (cond ((null entry) nil)
                       ((eq tag 'VVN) ; !!! crude test for passive context
                          (cddr entry))
                       (t (cadr entry)))))
             #+ignore (print (list '*** word stem tag rname val))
             (if (and val stem)
                (let
                   ((new-score
                       (or (cdr (assoc val (cdr (get stem 'freqs))))
                          (car (get stem 'freqs)))))
                   (if new-score
                      (progn
                         #+ignore (print (list '&&& stem old-score new-score (if new-score (expt 10 (+ old-score new-score)))))
                         (+ new-score old-score))
                      ;; no freq info for verb
                      old-score))
                old-score)))
       old-score))


(defun lr1-parse-unpack-v0-cat-p (cat)
   (let* ((feats (svref *index-category-table (svref cat 0)))
          (n (position 'N feats :test #'eq))
          (v (position 'V feats :test #'eq))
          (bar (position 'BAR feats :test #'eq)))
      (and n v bar
         (eq (svref cat (1+ n)) '-)
         (eq (svref cat (1+ v)) '+)
         (eq (svref cat (1+ bar)) '\0))))


#|
(read-subcat-scores "internal:sub:lobsustev.eval-00-05")

(read-subcat-scores "~/comp/lobsustev.eval-00-05")
(lr1-parse-analysis-trees "~/teval/xx-mult.data" "/tmp/xx-mult.parses")


gawk '/VSUBCAT/ {gsub(/^.*VSUBCAT /,""); gsub(/[)].*$/,""); print}' A1-short | sort -u
|#

;;; End of file

