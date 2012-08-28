#!/bin/sh

RASP=$1

arch=`arch | sed "s/i.86/ix86/"`_`uname -s | tr "[:upper:]" "[:lower:]"`

case $arch in
  ppc_darwin) gde="openmcl -I $RASP/gde/ppc_darwin/gde.image";;
  *) gde="$RASP/gde/${arch}/gde -backtrace-on-error -batch";;
esac

# map the output to .parses ready for transformation

cat $2.out  | \
gawk 'BEGIN{printf("%LB (\n%RB )\n"); sent=0} \
  /[0-9] ; [(]/ {sent++; printf("\n%s\n\n", sent); next} \
  /^[(][|]T[/]/ {next} \
  /^[(]X/ {next} \
  /^$/ {for (r in ncsubj) {print r}; delete ncsubj} \
  /^[(]/ { \
    gsub(/[|\\]/,""); 
    do {prev=$0; $0=gensub(/ ([^< ]*<[/]w>)/,"_\\1",1)} while (prev!=$0); \
    gsub(/<[/]?w>/,""); \
    if ($0 ~ /^[(]ncsubj /) {ncsubj[$0]=1; next}; \
    print}' | \
gawk '/^[(]quote / {next} \
  /^$/ {delete pass} \
  /^[(]passive / {p=$2; sub(/[)]$/,"",p); pass[p]=1} \
  /^[(]ncsubj / {v=$2; if (v in pass) {if ($4=="_)") {$4="obj)"}}} \
  {print}' > $2.parses

fsh="/tmp/`whoami`-`basename $0`-sh$$"

cat > "$fsh" <<EOF
(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

(load (format nil "$RASP/extra/bec/eval/gramreleval-new-scheme.lsp"))

(defun transform-grs (data-file forms-file out-files)
  (flet ((lr1-parse-unpack-word (fm &optional (numberwords t))
	   (let* ((form (string fm)))
	     (when (and (> (length form) 1)
			(eql (char form 0) #\<) (eql (char form 1) #\w))
	       (let*
		   ((wtag-end (position #\> form :start 2))
		    (endwtag-start
		     (and wtag-end (position #\< form :start (1+ wtag-end)))))
		 (when (and wtag-end endwtag-start)
		   (setq form
		     (subseq form (1+ wtag-end) endwtag-start)))))
	     (let
		 ((epos (or (and numberwords
                                 (position #\: form :from-end t))
			    (position #\_ form :from-end t))))
	       (when epos (setq form (subseq form 0 epos))))
	     form))
	 (convert-to-w (gr-item)
	   (let ((w-item "")
		 (end (position #\_ gr-item :from-end t)))
	     (if (not end)
		 gr-item
	       (progn
		 (dotimes (n (length gr-item)) 
		   (let ((ch (char gr-item n)))
		     (if (and (< n end) (equal ch #\_)) 
			 (setq w-item (format nil "~A " w-item)) 
		       (setq w-item (format nil "~A~A" w-item ch)))))
		 (format nil "<w>~A</w>" w-item)
		 )))
	   )
	 (convert-to-group (gr-item)
	   (let ((w-item ""))
	     (dotimes (n (length gr-item)) 
	       (let ((ch (char gr-item n)))
		 (if (and (equal ch #\space)) 
		     (setq w-item (format nil "~A_" w-item)) 
		   (setq w-item (format nil "~A~A" w-item ch)))))
	     w-item))
	 
	 )
    (let ((readtable (copy-readtable nil))
	  (count-sent 0)) 
      (setf (readtable-case readtable) :preserve) 
      (set-syntax-from-char #\' #\a readtable)
      (set-syntax-from-char #\, #\a readtable)
      (with-open-file (data-str data-file :direction :input)
	(with-open-file (forms-str forms-file :direction :input)
	  (with-open-file (grs-in-str 
			   (format nil "~A.parses" out-files)
			   :direction :input)
	    (with-open-file (grs-out-str 
			     (format nil "~A.trans.parses" out-files)
			     :direction :output 
			     :if-exists :supersede 
			     :if-does-not-exist :create)
	      (with-open-file (grtext-out-str 
			       (format nil "~A.trans.grtext" out-files)
			       :direction :output 
			       :if-exists :supersede 
			       :if-does-not-exist :create)
		(greval-skip-to-next grs-in-str) (peek-char #\1 grs-in-str)
		(format grs-out-str "%LB (~%%RB )~%")
		(loop
		  (let ((next-forms (read-sentence-from-file forms-str t t))
			(next-data (read-sentence-from-file data-str t t))
			(next-grs (greval-next-gr grs-in-str readtable)))
		    (incf count-sent)
		    (cond
		     ((or nil ;;(> count-sent 4)
			  (and (null next-grs) (atom next-forms) 
			       (atom next-data)))
		      (format t "Finished transforming successfully~%")
		      (return))
		     ((or (null next-grs) (atom next-forms) 
			  (atom next-data))
		      (error "Reached end of file at different times"))
		     (t
                      (if (equal '$3 'TRUE)
			  (progn 
			    (setq next-forms (cdr next-forms))
			    (setq next-data  (cdr next-data)))
			  (progn
			    (setq next-forms (mapcar #'lr1-parse-unpack-word (cdr next-forms)))
			    (setq next-data  (mapcar #'lr1-parse-unpack-word (cdr next-data)))))
		      ;;(format t "~%SENTENCE: ~A~%" count-sent)
		      ;;(format t "forms:~%~S~%" next-forms)
		      ;;(format t "data:~%~S~%" next-data)
		      ;;(format t "grs: ~%~S~%" next-grs)
		      ;; go through each gr and swap over word-for-word data for form input!
		      (when (not (equal (car next-grs) count-sent))
			(format t "MISMATCHED SENTENCES!! ~A ~A~%" 
				(car next-grs) count-sent))
		      (format grs-out-str "~%~A~%~%" count-sent)
		      (format grtext-out-str "~%~A~%~{~A ~}~%" count-sent 
			      (mapcar #'(lambda (x) (lr1-parse-unpack-word 
						     x (equal '$3 'TRUE)))
				      next-forms))
		      (dolist (gr-pair (cdr next-grs))
			(let* ((gr (cdr gr-pair)))
			  (format grs-out-str "(~A" (car gr))
			  (dolist (gr-item (cdr gr))
			    (let ((map-pos 
				   (or
				    (position (if (equal '$3 'TRUE) gr-item 
						(lr1-parse-unpack-word (string gr-item) t))
					      next-data :test #'string=)
				    (position (convert-to-w (string gr-item)) 
					      next-data :test #'string=))))
			      ;;(setq temp-var gr-item)
			      (cond ((equal '_ gr-item)
				     (format grs-out-str " ~A" gr-item))
				    (map-pos
				     (format 
				      grs-out-str " ~A" 
				      (convert-to-group
				       (lr1-parse-unpack-word (elt next-forms map-pos) (equal '$3 'TRUE)))))
				    (t
				     (format grs-out-str " ~A" 
					     (lr1-parse-unpack-word gr-item))))
			      )
			    )
			  (format grs-out-str ")~%")
			  ))
		      
		      
		      ))))
		(finish-output grtext-out-str)
		(finish-output grs-out-str)
		))))))))

(transform-grs 
   "$4.data"
   "$4"
   "$2")
EOF

${gde} -e \(progn\
\(load\ \"$fsh\"\ :verbose\ nil\)\
#+allegro\ \(\exit\ 0\ :quiet\ t\)\ #-allegro\ \(quit\)\) 

rm -rf "$fsh"
