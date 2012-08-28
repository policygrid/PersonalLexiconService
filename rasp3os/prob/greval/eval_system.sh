#!/bin/sh

# eval_system.sh

RASP=/local/scratch/`whoami`/rasp3

SBCL=0 # if 0 acl if 1 sbcl

arch=`arch | sed "s/i.86/ix86/"`_`uname -s | tr "[:upper:]" "[:lower:]"`


if [ $SBCL -lt 1 ]; then 

case $arch in
  ppc_darwin) gde="openmcl -I $RASP/gde/${arch}/gde.image";;
  ix86_linux) gde="/usr/opt/acl80/alisp -I $RASP/gde_80.dxl -backtrace-on-error -batch";;
  x86_64_linux) gde="/usr/opt/acl80.64/alisp -I $RASP/gde_80.64.dxl -backtrace-on-error -batch";;
esac

else

case $arch in
  ppc_darwin) gde="$RASP/gde/${arch}/gde --noinform --dynamic-space-size 2000";;
  ix86_linux) gde="$RASP/gde/${arch}/gde --noinform --dynamic-space-size 1648";;
  x86_64_linux) gde="$RASP/gde/${arch}/gde --noinform";;  
esac

fi

testfile=""
stdfile=""
slotmatch=a
grmatch=h
outputorig=nil
outputunl=nil
matrix=nil
while getopts t:c:s:g:oumz: opt
do
     case $opt in
     t) testfile="$OPTARG";;
     c) stdfile="$OPTARG";;
     s) slotmatch="$OPTARG";;
     g) grmatch="$OPTARG";;
     o) outputorig=t;;
     u) outputunl=t;;
     m) matrix=t;;
     z) RASP="$OPTARG";;
     ?) echo "Usage: $0 [-t<testfile>] [-c<stdfile>] [-s<slot-match>] 
              [-g<gr-type-match>] [-o] [-u] [-m]";
        exit 1;;
     esac
done

if [ "$testfile" = "" ]; then
  printf "ERROR: a test file must be specified using -t option" > /dev/stderr
  exit 1
fi

if [ "$stdfile" = "" ]; then
  printf "ERROR: a standard file must be specified using -c option" > /dev/stderr
  exit 1
fi

fsh="/tmp/`whoami`-`basename $0`-sh$$"
logfile="/tmp/`whoami`-`basename $0`-log$$"

cat > "$fsh" <<EOF

#|
Options: $debug $parseval $initforms $logfile $memlimit $nparses $outformat $subcat $timeout $unnumbered $wordlimit $xphrasal
|#

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

(load (compile-file "$RASP/prob/greval/gramreleval-new-scheme.lsp"))

(cond ((eq '$grmatch 'e)
       (setq *type 'EQ))
     ((eq '$grmatch 's)
       (setq *type 'SUB))
     ((eq '$grmatch 'h)
       (setq *type 'HIER)))

(cond ((eq '$slotmatch 'h)
      (setq *head-dep-only t))
     ((eq '$slotmatch 'n)
      (setq *head-dep-ncsubj-only t)))

(setq *output-orig $outputorig)
(setq *output-unl $outputunl)

;; nb: stats is a list of GR-STATES: orig, new, and unlabelled dependency stats
(setq stats
(gramreleval "$testfile.grtext" "$stdfile" "$testfile.parses" "$testfile.output"))

;; can call confusion matrix on stats here too:

(when $matrix
;; output the confusion matrix for new scheme:
(greval-confusion-summary T (CADR STATS))
)

EOF

if [ ! -r "$fsh" ]; then
  printf "%s: could not create temporary file '%s'\n" "$0" "$fsh" > /dev/stderr
  exit 1
fi

${gde} -e \(progn\
\(load\ \"$fsh\"\ :verbose\ nil\)\
#+allegro\ \(\exit\ 0\ :quiet\ t\)\ #-allegro\ \(quit\)\)
 > "$logfile"


