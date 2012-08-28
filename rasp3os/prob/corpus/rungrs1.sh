#!/bin/sh
#
# Take disambiguated training trees and output set of GRs for each tree.
# Insert appropriate value for the shell variable RASP. Example invocation:
#
# ~/Documents/rasp/extra/rungrs.sh tsg-grs corpus/names.trees1
#
#
#foreach file (corpus/*.trees1)
#echo $file
#~/Documents/rasp/extra/rungrs.sh tsg-grs $file > grs/`basename $file .trees1`.grs
#end


RASP=/local/scratch/ejb/rasp

arch=`arch | sed "s/i.86/ix86/"`_`uname -s | tr "[:upper:]" "[:lower:]"`

case $arch in
  ppc_darwin) gde="openmcl -I $RASP/gde/${arch}/gde.image";;
  *) gde="$RASP/gde/${arch}/gde -backtrace-on-error -batch";;
esac

if [ ! -r "$1" ]; then
  printf "%s: grammar file '%s' cannot be read or does not exist\n" "$0" "$1" > /dev/stderr
  exit 1
fi

logfile="/tmp/`whoami`-`basename $0`-log"
fsh="/tmp/`whoami`-`basename $0`-sh$$"

cat > "$fsh" <<EOF
!(lisp-top-loop)

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)


;;;

(defparameter +lr1train-pathname+
   (make-pathname
      :directory (append (pathname-directory (truename "$RASP/")) '("extra" "lr1train"))
      :name nil :type "lsp"
      :defaults (truename "$RASP/")))
(defparameter +grammar-pathname+
   (truename "$1"))
(defparameter +trees1-pathname+
   (truename "$2"))


(defun yes-for-question (&rest x) (declare (ignore x)) t)
(clear-whole-grammar)


#+openmcl
(setq *default-external-format* :inferred)


(load "/home/ejb/projects/anlt/code/gr-patch")

(read-grammar +grammar-pathname+)

(progn (make-parse-rule-tree nil) nil)
(setq *lr1-parse nil)


(defun lr1-trees-to-grs (tree-file)
   (format t "~%Starting...~%")
   (with-open-file (in tree-file :direction :input)
      (loop
         (let ((*suppress-dict-messages t)
               (sentence (read in nil t)))
            (when (atom sentence) (return))
            (format t "~&~{~A~^ ~}~%" sentence)
            (let* ((tree (read in))
                   (candidates
                      (execute-with-timeout (progn (format t "~%Timed out~%") nil)
                         (invoke-parser1 sentence))))
               (if candidates
                  (dolist (bindings-and-tree candidates
                             (format t "*** Warning: no matching parse~%~%"))
                     (when (equal
                              (get-rule-labelling-from-parse-tree (cdr bindings-and-tree))
                              tree)
                        (display-parse-semantics (list bindings-and-tree) nil)
                        (return)))
                  (format t "*** Warning: no parses~%~%"))))))
  (format t "Finishing...~%"))


(lr1-trees-to-grs +trees1-pathname+)


#+allegro (exit)
#-allegro (quit)
EOF

${gde} < "$fsh" | awk '/^Finishing\.\.\./ {on=0} on {print} /^Starting\.\.\./ {on=1}'

# remove intermediate files
rm -rf "$fsh"
