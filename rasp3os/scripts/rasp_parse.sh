#!/bin/sh

 ##############################################################################
 # Copyright 2002, 2006, 2011 John Carroll, Oeistein Andersen                 #
 #                                                                            #
 # This file is part of RASP.                                                 #
 #                                                                            #
 # RASP is free software: you can redistribute it and/or modify it            #
 # under the terms of the GNU Lesser General Public License as published      #
 # by the Free Software Foundation, either version 3 of the License, or       #
 # (at your option) any later version.                                        #
 #                                                                            #
 # RASP is distributed in the hope that it will be useful,                    #
 # but WITHOUT ANY WARRANTY; without even the implied warranty of             #
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
 # GNU Lesser General Public License for more details.                        #
 #                                                                            #
 # You should have received a copy of the GNU Lesser General Public License   #
 # along with RASP.  If not, see <http://www.gnu.org/licenses/>.              #
 ##############################################################################

#
# Run the probabilistic parser on text that has been tokenised, tagged etc.
# Insert appropriate value for the shell variable RASP right below this comment.
# Nothing else in the file should be changed.
#
# Example invocation:
#
# ./rasp_parse.sh < ../prob/greval/parc700/test.not-ne.stag.data | more 
#

RASP=/home/`whoami`/rasp3os

arch=`uname -m | sed "s/i.86/ix86/"`_`uname -s | tr "[:upper:]" "[:lower:]"`
if [ $arch = ix86_darwin ] && [ `sysctl -n hw.optional.x86_64` = 1 ]; 
then
    arch=x86_64_darwin;
fi


# Maximum contiguous address space limit in 32 bits: 2GB for Linux, and around
# 2.25GB for Mac OSX. Dynamic space parameter needs to be set a fair bit lower
# than this

case $arch in
  x86_64_*) gde="$RASP/gde/${arch}/gde --noinform --dynamic-space-size 8196";;
  *) gde="$RASP/gde/${arch}/gde --noinform --dynamic-space-size 1600";;
esac

if [ ! -d "$RASP" ]; then
  printf "$0: could not read RASP directory '%s'\n" "$RASP" > /dev/stderr
  exit 1;
fi

# Process any command line options

debug="false"
parseval=nil
initforms=nil
logfile="/tmp/`whoami`-`basename $0`-log"
nparses=1
# set of non-XML output formats:
outformat=""
subcat=nil
timeout=10
unnumbered=nil
wordlimit=0
xphrasal=nil

while getopts dei:l:m:n:o:p:st:uw:xy opt
do
  case $opt in
    d) debug="true";;
    e) parseval=t;;
    i) initforms="$OPTARG";;
    l) logfile="$OPTARG";;
    n) nparses="$OPTARG";;
    o) outformat="$OPTARG";;
    s) subcat=t;;
    t) timeout="$OPTARG";;
    u) unnumbered=t;;
    w) wordlimit="$OPTARG";;
    x) xphrasal=t;;
    ?) echo "Usage: $0 [-e(val)] [-i <initforms>] [-l <logfile>] [-n <nparses>] [-o <outformats>] [-s(ubcat)] [-t <timeout>] [-u(nnumbered)] [-w <wordlimit>] [-x(phrasal)]"  > /dev/stderr;
       exit 1;;
  esac
done

if [ $# -ge $OPTIND ]; then
  printf "$0: superfluous command line arguments\n" > /dev/stderr
  exit 1
fi

cat /dev/null > "$logfile"
if [ ! -w "$logfile" ]; then
  printf "$0: logfile '%s' cannot be written to\n" "$logfile" > /dev/stderr
  exit 1
fi

if [ `expr "$nparses" : '[0-9]*$'` -eq 0 ]; then
  printf "$0: number of parses '%s' should be an integer >= 0\n" "$nparses" > /dev/stderr
  exit 1
fi

outa=`echo "$outformat" | grep -c "a"` # alias
outg=`echo "$outformat" | grep -c "g"` # grs
outw=`echo "$outformat" | grep -c "w"` # weighted grs
outi=`echo "$outformat" | grep -c "i"` # gio (ewg alg)    
outs=`echo "$outformat" | grep -c "s"` # sparkle          
outt=`echo "$outformat" | grep -c "t"` # tree             
outu=`echo "$outformat" | grep -c "u"` # upenn            
outz=`echo "$outformat" | grep -c "z"` # susanne        
outl=`echo "$outformat" | grep -c "l"` # the lemma-list
outp=`echo "$outformat" | grep -c "p"` # print everything!

# have to use numbered for gio (i) output format:
if [ $unnumbered = t -a $outi -eq 1 ]; then
  echo "$0: -u and -oi options are incompatible -- ignoring -u" > /dev/stderr
  unnumbered=nil
fi

if [ `expr "$timeout" : '[0-9]*$'` -eq 0 ]; then
  printf "$0: timeout '%s' should be an integer >= 0\n" "$timeout" > /dev/stderr
  exit 1
fi

if [ `expr "$wordlimit" : '[0-9]*$'` -eq 0 ]; then
  printf "$0: wordlimit '%s' should be an integer >= 0\n" "$wordlimit" > /dev/stderr
  exit 1
fi

# Defensively set a memory limit on each parse to avoid (sbcl-based) lisp
# system running out of memory and failing dramatically if user has disabled
# the timeout or has specified a large timeout value e.g. 120 sec or more.
# The values below are sensible, generic defaults; the value could be
# increased, but not beyond the amount of real memory otherwise the overall
# operation of the machine may suffer due to swapping.

case $arch in
  *_darwin) memlimit=${memlimit:-1200000000};; # 1.2GB
  x86_64_*) memlimit=${memlimit:-2400000000};; # 2.4GB
  *) memlimit=${memlimit:-1000000000};; # 1.0GB
esac

if [ `expr "$memlimit" : '[1-9][0-9]\{8,\}$'` -eq 0 ]; then
  printf "$0: memory limit '%s' should be an integer > 100000000\n" "$memlimit" > /dev/stderr
  exit 1
fi

# Ensure sbcl/gde opens standard input (and output) so that input/output is
# "faithful", passing bytes through without interpretation - because we don't
# know the encoding of the input stream. Ideally we'd just set the locale to
# ISO-8859-1 and that would be it. But this locale might not have been installed
# and/or sbcl might not honour it. So do it directly from inside the gde, below.

#export LC_CTYPE=en_GB.iso88591


fsh="/tmp/`whoami`-`basename $0`-sh$$"

cat > "$fsh" <<EOF

#|
Options: $debug $parseval $initforms $logfile $memlimit $nparses $outformat $subcat $timeout $unnumbered $wordlimit $xphrasal
|#

(in-package #+(or cltl2 x3j13 ansi-cl) common-lisp-user #-(or cltl2 x3j13 ansi-cl) 'user)

;; Use double-float precision

(setq *read-default-float-format* 'double-float)

;;; Disable rules which deal with elliptical dialogue-like text as
;;; they tend to overapply elsewhere

(defparameter +disabled-rules+
    '(|V1/do_gap-r| |V1/have_gap-r| |V1/be_gap-r| |V1/mod_gap-r|
      |P1/prt-of| |P1/prt-r|
      ))

;;; Use probabilistic LR parser vs. non-probabilistic chart

(setq *lr1-parse t)

;;; Subcat probabilities, phrasal verb list

(setq *subcat-probs-p* $subcat)
(setq *phrasal-verbs-p* (not $xphrasal))

;;(setq +analysis-tree-type+ '$tt)
;;(setq +analysis-tree-print-fn+ '$tp)

;; initialise the +print-list+
(init-print-list)

;; set the print functions:
(when (eq 1 '$outa) (set-type-print 'a))
(when (eq 1 '$outg) (set-type-print 'g))
(when (eq 1 '$outw) (set-type-print 'gw))
(when (eq 1 '$outi) (set-type-print 'gio))
(when (eq 1 '$outs) (set-type-print 's))
(when (eq 1 '$outt) (set-type-print 't))
(when (eq 1 '$outu) (set-type-print 'u))
(when (eq 1 '$outz) (set-type-print 'z))
(when (eq 1 '$outp) (setq +print-all+ t))

;; set the format options for output:

;; print the lemma-list for non-xml output format or if require well formed
(setq +print-lemma-lisp+ (eq 1 '$outl))

(setq +parseval-output-p+ $parseval)

(setq +numbered-words-p+ (not $unnumbered))

;;; Tag filtering, numbers of parses and timeout
;;; relevant only if multiple tags per word are input
;;; +multiple-tag-certainty+ - if the top ranked tag is more probable
;;; than this value (in this case 0.9) then only this tag is considered
;;; +multiple-tag-threshold+ - only consider a tag if not less than this many
;;; times as probable. e.g. in this case we won't consider tags which are
;;; less than 1/50 times as probable as the top ranked tag.

(setq +multiple-tag-threshold+ (log 50 10))
(setq +multiple-tag-certainty+ (log 0.90 10))

(setq +n-best-retained+ $nparses)

(defparameter +parse-timeout+ $timeout) ; seconds cpu time
#+sbcl (defparameter +parse-storeout+ $memlimit) ; bytes allocated

(setq +max-sentence-length+ $wordlimit)

;;; Patches (if any, loaded in lexicographical order) and initialisation

(mapc #'(lambda (f) (load f :verbose nil))
   (sort
      (directory
         (merge-pathnames
            (make-pathname :name "patch*" :type "lsp")
            (parse-namestring "$RASP/prob/")))
      #'string< :key #'namestring))
         
;;; Generational GC not useful because there are no long-lived objects

#+(and sbcl gencgc (not x86-64))
(setf (extern-alien "gencgc_oldest_gen_to_gc" unsigned) 1)
#+(and sbcl gencgc x86-64)
(let ((conses-last-gc (get-bytes-consed)))
   (embed read-sentence-from-file
      (lambda (in-str out-str log-str)
         (when (> (- (get-bytes-consed) conses-last-gc) +parse-storeout+)
            (gc :full t)
            (setq conses-last-gc (get-bytes-consed)))
         (read-sentence-from-file in-str out-str log-str))))

;; Ensure graceful recovery from errors and interrupt

#+sbcl
(embed lr1-parse-analysis-trees1
   (lambda (in out log-str)
      (handler-case (lr1-parse-analysis-trees1 in out log-str)
         ((or sb-int:stream-encoding-error sb-int:stream-decoding-error) (condition)
            (format *error-output* "~%~A~%" condition)
            (finish-output *error-output*)
            (sb-ext:quit :unix-status 1))
         (stream-error () ; output piped through head / less and then q / etc
            (sb-ext:quit :recklessly-p t :unix-status 1))))) ; too risky to allow any further output
#+sbcl
(setq sb-ext:*invoke-debugger-hook*
   #'(lambda (cond hook) (declare (ignore hook))
      (let ((cond-str (princ-to-string cond)))
         (format *error-output* "~%~A~%" cond-str)
         (unless (or (search "interrupt" cond-str) ; user did ^C
                     (search "breakpoint/trap" cond-str)
                     (search "Broken pipe" cond-str))
            (sb-debug:backtrace 200 *error-output*))
         (finish-output *error-output*)
         (sb-ext:quit :unix-status 1))))

(progn $initforms)
EOF

if [ ! -r "$fsh" ]; then
  printf "$0: could not create temporary file '%s'\n" "$fsh" > /dev/stderr
  exit 1
fi

${gde} -e "(progn
(load \"$fsh\" :verbose nil)
(lr1-parse-analysis-trees
 (sb-sys:make-fd-stream (sb-sys:fd-stream-fd sb-sys:*stdin*) :external-format +char-encoding+ :input t :output nil)
 (sb-sys:make-fd-stream (sb-sys:fd-stream-fd sb-sys:*stdout*) :external-format +char-encoding+ :input nil :output t)
 \"$logfile\")
(quit))"

parse_status=$?


# Remove temporary file

if [ $debug = "false" ]; then
  rm -rf "$fsh"
fi

exit $parse_status

