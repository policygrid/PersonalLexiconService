
#!/bin/sh
#
# Create LR1 version of grammar and train probabilistic parser. Insert
# appropriate value for the shell variable RASP. Example invocation:
#
# ./runtrain.sh /home/j/jo/johnca/MacHome/Desktop/rasp3/prob/data/tsg15
#

RASP=/local/scratch/`whoami`/rasp3

arch=`arch | sed "s/i.86/ix86/"`_`uname -s | tr "[:upper:]" "[:lower:]"`

case $arch in
  sun4_sunos) ACL_HOME=/local/acl70;;
  ix86_linux) ACL_HOME=/usr/opt/acl70;; #/eli #/nfs/repl/acl/acl62;;
  x86_64_linux) ACL_HOME=/usr/opt/acl70.64;;
esac

LISP=$ACL_HOME/alisp

case $arch in
  ppc_darwin) gde="openmcl -I $RASP/gde/${arch}/gde.image";;
  ix86_linux) gde="$LISP -I $RASP/gde_70.dxl -backtrace-on-error -batch";;
  x86_64_linux) gde="$LISP -I $RASP/gde_70.64.dxl -backtrace-on-error -batch";;
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
(defparameter +grammar-data-pathname+
   (make-pathname
      :directory (pathname-directory +grammar-pathname+)
      :name (pathname-name +grammar-pathname+)
      :type nil
      :defaults +grammar-pathname+))


(defun yes-for-question (&rest x) (declare (ignore x)) t)
(clear-whole-grammar)


#+openmcl
(setq *default-external-format* :inferred)

(defun load-compiled-file (p)
   (unless (probe-file p) (error "Source file ~A does not exist" p))
   #+(or procyon mcl poplog cormanlisp) (load p)
   #-(or procyon mcl poplog cormanlisp)
   (let ((cp (compile-file-pathname p)))
      (when (or (not (probe-file cp))
                (< (file-write-date cp) (file-write-date p)))
         (compile-file p))
      (unless (probe-file cp)
         (error "Compilation of ~A did not yield a compiled file named ~A" p cp))
      (load cp)))

(dolist (x '("make-backbone" "lr0-compile" "lookahead" "machine"))
   (load-compiled-file (merge-pathnames x +lr1train-pathname+)))


#+lucid
(change-memory-management :growth-limit 1500 ; 96MB
   :growth-rate 24 ; 1.5MB
)

#+allegro
(eval-when (load eval) (setq *global-gc-behavior* :auto))

#+lispworks (extend-current-stack 300)

#+allegro
(progn
   (excl:gc t)
   (setf (sys:gsgc-parameter :free-percent-new) 10)
   (setf (sys:gsgc-parameter :expansion-free-percent-new) 15)
   (setf (sys:gsgc-parameter :expansion-free-percent-old) 20)
   (setf (sys:gsgc-switch :print) t))


;;; Read grammar, compute and write out LR1 backbone and machine 

(read-grammar +grammar-pathname+)


(defparameter *instantiate-features* nil) ; '(VSUBCAT)

;;; return true if cat unifies with any lexical item, i.e. with tags
;;; it's a closed set so we are safe
;;; - cheaper: the following are both terminal and non-terminal
;;; [MINOR CONJ, CJTYPE @, TA @]
;;; [MINOR COMP, TA @]
;;; [MINOR DET, PLU @, POSS @, WH @]
;;; [MINOR NOT]
;;; BAR 0 categories
;;; [N +, V -, BAR 2, PLU @7, POSS @8, NTYPE @9, WH @, MOD @, CONJ @, SCOLON +, COLON @, DASH @, TA @, BAL @, BRACK @, COMMA @, TXTCAT UNIT, TXT PH]

(defun ps-category-expands-terminal (cat)
   (let ((features (svref *index-category-table (svref cat 0))))
      (or
         ;; MINOR CONJ/COMP/NOT
         (let
            ((minor-index (position 'MINOR features :test #'eq)))
            (and minor-index
               (member (svref cat (1+ minor-index)) '(CONJ COMP DET NOT) :test #'eq)))
         ;; BAR 0 categories
         (let
            ((bar-index (position 'BAR features :test #'eq)))
            (and bar-index
               (eq (svref cat (1+ bar-index)) '|0|)))
         ;; N2
         (let
            ((n-index (position 'N features :test #'eq))
             (v-index (position 'V features :test #'eq))
             (bar-index (position 'BAR features :test #'eq)))
            (and n-index
               (eq (svref cat (1+ n-index)) '+)
               v-index
               (eq (svref cat (1+ v-index)) '-)
               bar-index
               (eq (svref cat (1+ bar-index)) '|2|))))))


(compute-ps-backbone
   (merge-pathnames (make-pathname :type "backbone") +grammar-data-pathname+)
   nil)

#+mcl
(let ((previous-done-p nil))
   (process-run-function '(:name "LR item-sets/lookahead" :stack-size 512000)
      #'(lambda ()
          (construct-item-sets)
          (compute-items-lookahead)
          (setq previous-done-p t)))
   (process-wait "LR computation" #'(lambda () previous-done-p)))

#-mcl
(progn (construct-item-sets) (compute-items-lookahead))

(construct-lalr-machine)

(clear-lalr-intermediate-results)
(progn
   (dump-machine-to-file
      (merge-pathnames (make-pathname :type "machine") +grammar-data-pathname+))
   t)


#|
PS backbone contains 1147 productions, 60 distinct categories
(28 terminals, and 32 non-terminals including 0 null)
LR(0) parser contains 920 states, 40273 canonical items
Total of 872136 canonical and non-canonical items
LALR(1) parse table contains 21736 goto entries (665 distinct)
14174 shift actions (254 distinct), 79617 reduce actions
|#


;;; Read grammar, LR1 backbone and machine, and then map old trees to new

(dolist (x '("nparse" "auto-trees"))
   (load-compiled-file (merge-pathnames x +lr1train-pathname+)))

(when (probe-file (merge-pathnames "rule-mapping.lsp" +lr1train-pathname+))
   (load (merge-pathnames "rule-mapping.lsp" +lr1train-pathname+)))


#|
(read-grammar +grammar-pathname+)
|#

(progn
   (install-unification-grammar nil)
   (install-grammar-from-file
      (merge-pathnames (make-pathname :type "backbone") +grammar-data-pathname+))
   (install-machine-from-file
      (merge-pathnames (make-pathname :type "machine") +grammar-data-pathname+)))

(setq *lr1-parse t)

(defparameter +parse-timeout+ 50)
#+allegro (defparameter +parse-storeout+ 800000000) ; 800 Mbytes

(setq +max-sentence-length+ nil)

(setq *print-pretty* nil)


#+allegro
(progn
   (gc :tenure)
   (setf (sys:gsgc-switch :auto-step) nil)
   (embed read-sentence-and-tree-from-file
      (lambda (def-str trees1-p)
         (gc)
         (sys:gsgc-step-generation)
         (read-sentence-and-tree-from-file def-str trees1-p))))

#|
(dolist
   (file
      (list
         (merge-pathnames (make-pathname :type "config") +grammar-data-pathname+)
	(merge-pathnames (make-pathname :type "trees0") +grammar-data-pathname+)
	(merge-pathnames (make-pathname :type "trees1") +grammar-data-pathname+)))
   (if (probe-file file) (delete-file file)))

(let ((trees-file (merge-pathnames "sustr-mod15.trees1" +grammar-data-pathname+))
      (data-file (merge-pathnames (make-pathname :type "data") +grammar-data-pathname+)))
   (unless (probe-file data-file)
      #+(and mcl darwin)
      (run-program "cp" (list (namestring trees-file) (namestring data-file)) :wait t)
      #+(and mcl (not darwin))
      (copy-file trees-file data-file)
      #-mcl
      (run-shell-command (format nil "cp ~a ~a" trees-file data-file) :wait t)))

(parse-and-select-definition-file-aux
   (merge-pathnames (make-pathname :type "data") +grammar-data-pathname+))
;; -> tsgx.trees1
|#

#+allegro
(setf (sys:gsgc-switch :auto-step) t)


;;; Compute prob table from the new trees

(dolist (x '("tparse" "ttrain"))
   (load-compiled-file (merge-pathnames x +lr1train-pathname+)))


(setq +titech-normalise-p+ t)
(setq +old-normalise-p+ nil)
(setq +add-1-normalise-p+ t)
(setq +history-probs-stats-freq+ 100)
(finish-output)


(lr1-tree-train (merge-pathnames (make-pathname :type "trees1") +grammar-data-pathname+)
   (merge-pathnames (make-pathname :type "trans") +grammar-data-pathname+))


(defun lr1-history-prob-action (action)
   (let* ((state (svref *state-action-probs* (svref action 0)))
          (la (svref action 2))
          (la-pair (assoc la (state-prob-transitions state))))
      (unless la-pair
         (push (setq la-pair (list* la 0 nil nil))
            ;; la, total smoothed freq of this la in this state, nested
            ;; reduce alist keyed on to-state then rule, then optionally
            ;; shift freq
            (state-prob-transitions state)))
      (if (eql (length action) 3)
         ;; shift
         (let
            ((n (if (and (null (cdddr la-pair)) +add-1-normalise-p+) 2 1)))
            (incf (second la-pair) n)
            (if (cdddr la-pair)
               (incf (cdddr la-pair) n) (setf (cdddr la-pair) n)))
         ;; reduce
         (let* ((to-state (if +titech-normalise-p+ nil (svref action 1)))
                (to-state-pair (assoc to-state (third la-pair)))
                (rule (svref action 3))
                rule-pair)
            (unless nil ; *** (rare-rule-p (string (cfrule-name (svref *productions* rule))))
               ;; ignore reduce transitions that use a rare rule
               (unless to-state-pair
                  (push (setq to-state-pair (cons to-state nil))
                     (third la-pair)))
               (setq rule-pair (assoc rule (cdr to-state-pair)))
               (unless rule-pair
                  (push (setq rule-pair (cons rule 0)) (cdr to-state-pair)))
               (let
                  ((n (if (and (zerop (cdr rule-pair)) +add-1-normalise-p+) 2 1)))
                  (incf (second la-pair) n)
                  (incf (cdr rule-pair) n)))))))

(lr1-history-probs (merge-pathnames (make-pathname :type "trans") +grammar-data-pathname+)
   (merge-pathnames (make-pathname :type "probs-ti") +grammar-data-pathname+)) ; -2 -rla ***


#+allegro (exit)
#-allegro (quit)
EOF

${gde} < "$fsh" > "$logfile"

# remove intermediate files
rm -rf "$fsh"
