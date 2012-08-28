#|----------------------------------------------------------------------------|
 | Copyright 2006, 2011 John Carroll, Ted Briscoe, Rebecca Watson             |
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

;;; adapted from: PROB CODE - OUT-ANALYSIS.LSP
;;;
;;; stores in a config file where we are up to in parsing a FILE (not *std-in* compatible!)
;;; continues where it left off if the parser crashes.

;;; config file in (in).config
(defun lr1-parse-analysis-trees-config (in-sentences out-analyses &optional (logfile t) (config-file nil))
  (let ((file-pos 0)
	(sentence-number 0)
	(config-file (or config-file (format nil "~A.config" in-sentences))))
    (if (probe-file config-file)
	(with-open-file (str config-file :direction :input)
	  (setq file-pos (read str))
	  (setq sentence-number (read str))))
    (lr1-parse-analysis-trees-config1 in-sentences out-analyses logfile config-file file-pos sentence-number)
    ))

(defun lr1-parse-analysis-trees-config1 (in-sentences out-analyses logfile config-file file-pos sent-num)
  (with-input-file-or-stream (in-str in-sentences 
				     :direction :input 
				     :external-format  
				     (if +xml-output+ +xml-encoding+ :latin1))
     (with-output-file-or-stream (out-str out-analyses 
					  :direction :output 
					  :if-exists (if (= file-pos 0) :supersede :append)
					  :if-does-not-exist :create 
					  :external-format 
					  (if +xml-output+ +xml-encoding+ :latin1))
       (with-output-file-or-stream (log-str logfile :direction :output 
					    :if-exists (if (= file-pos 0) :supersede :append)
					    :if-does-not-exist :create)
            (let ((*error-output* log-str)
                  (*trace-output* log-str))
	      (setq *sentence-number* sent-num)
	      (file-position in-str file-pos)
	      (time (lr1-parse-analysis-trees1-config in-str out-str log-str config-file))
	      (terpri *trace-output*)
	      (finish-output *trace-output*))))))

(defun lr1-parse-analysis-trees1-config (in-str out-str log-str &optional (config-file nil))
   (when +parseval-output-p+
     (format out-str "%LB (~%%RB )~%~%"))
   (when +xml-output+
     (start-xml out-str))
   (loop
      (let ((sentence (read-sentence-from-file in-str out-str log-str)))
	(when (or (atom sentence))
	  (update-config-finished config-file (file-position in-str))
	  (return))
         (incf *sentence-number*)
         (setq *current-sentence* sentence)
         (let ((*suppress-dict-messages t)
	       (numbered-p +numbered-words-p+)
	       (*accepted-vertex-analyses-copy* nil))
	   (when +output-trees-and-ewg+
	     (setq +analysis-tree-type+ nil)
	     (setq +analysis-tree-print-fn+ nil))
	   (multiple-value-bind (trees n-unif n-fail weights)
	       (if (and +max-sentence-length+
			(> +max-sentence-length+ 0)
			(> (length (cdr sentence)) +max-sentence-length+))
		   (progn
		     (format log-str "~%Input longer than ~A words~%Halted~%"
			     +max-sentence-length+)
		     nil)
		 (let ((*lr1-parse-timeouts*
			(cons +parse-timeout+
			      (if (> +parse-timeout+ 0)
				  (max 2 (ceiling (* +parse-timeout+ 0.2)))
				0))))
		   ;; given amount, plus sneak a little extra to see if we
		   ;; can rescue a partial analysis if initially times out
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
	       ;; if outputing trees and weighted grs and it's a frag parse 
	       ;; then print out frag tree + corresponding grs.
	       (when (and +output-trees-and-ewg+ frag-p)
		 (setq +analysis-tree-type+ 'lr1-parse-analysis-grs)
		 (setq +analysis-tree-print-fn+ 'lr1-parse-analysis-tree-and-grs-print)
		 )
	       (lr1-parse-analysis-output-trees
		(cdr sentence) trees weights frag-p out-str)
	       ;; now if printing trees and weighted grs and >= 1 parses 
	       ;; we want to print extract-grs for copy 
	       ;; - *accepted-vertex-analyses-copy*
	       (when (and +output-trees-and-ewg+ (null frag-p))
		 (setq +analysis-tree-type+ 'extracted-grs)
		 (setq +analysis-tree-print-fn+ 'print-extracted-weighted-grs)
		 (setq +numbered-words-p+ t)
		 (setq *accepted-vertex-analyses* *accepted-vertex-analyses-copy*)
		 (when trees
		   (let* ((analyses (lr1-parse-result-analyses))
			  (tree (analysis-nvt-tree (car analyses))))
		     (print-extracted-weighted-grs (extracted-grs (cdr tree) (car tree)) out-str))
		   (terpri out-str))
		 (setq +numbered-words-p+ numbered-p)
		 (setq *accepted-vertex-analyses* nil)
		 (setq *accepted-vertex-analyses-copy* nil)
		 )
	       (if +xml-output+
		   (xml-end-sentence-print out-str)
		 (terpri out-str))
	       (finish-output out-str)
	       )))
	 (when config-file
	   (format log-str "~%up to sentence, position: ~A ~A" 
		   *sentence-number*
		   (file-position in-str))
	   (update-config config-file (file-position in-str)))
	 ))
   (when +xml-output+
     (end-xml out-str))
   )

(defun update-config (config-file file-pos)
  (format t "updating config:~%")
  (with-open-file
   (config-str config-file :direction :output :if-exists :supersede :if-does-not-exist :create)
   (file-position config-str 0)
   (format config-str "~8D ~8D~%" file-pos *sentence-number*)))

(defun update-config-finished (config-file file-pos)
  (with-open-file
   (config-str config-file :direction :output :if-exists :append :if-does-not-exist :create)
   (format config-str "FINISHED ~8D ~8D~%" file-pos *sentence-number*)))
