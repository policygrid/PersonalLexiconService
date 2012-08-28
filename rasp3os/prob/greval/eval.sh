#!/bin/sh

# eval.sh

# change to point to your copy of rasp:
RASP=/local/scratch/`whoami`/rasp3

#
deb="false"
ner="false" 
runrasp="false"
evalopts=""
testfile=""
translate="true"
numberwords="true"
parc="560" # which data set? parc560 parc140 or parc700?
while getopts dnre:t:xwhaz: opt
do
     case $opt in
     a) parc="700";;
     h) parc="140";;
     d) deb="true";;
     n) ner="true";;
     r) runrasp="true";;
     e) evalopts="$OPTARG";;
     t) testfile="$OPTARG";;
     x) translate="false";;
     w) numberwords="false";;
     z) RASP="$OPTARG";;
     ?) echo "Usage: $0 [-d] [-n] [-r] [-e<evalopts>] [-t<testfile>]";
        exit 1;;
     esac
done
#
echo "RASP DIRECTORY: " $RASP #
#

if [ ! -d "$RASP" ]; then
  printf "%s: could not read RASP directory '%s'\n" "$0" "$RASP" > /dev/stderr
  exit 1;
fi

if [ $testfile = "" ]; then
  printf "ERROR: a test file must be specified using -t option" > /dev/stderr
  exit 1
fi

if [ $runrasp = "true" -a $translate = "false" ]; then
  echo "$0: -r and -x options are incompatible -- ignoring -x" > /dev/stderr
  translate="true"
fi

# note the user can change the parser options:
# use -p parser option if weighted GR output is desired!
if [ $runrasp = "true" ]; then
if [ $ner = "true" ]; then

#NE:
if [ $parc = "560" ]; then
rasp_sentence=cat $RASP/scripts/rasp.sh -p"-t40 -og -m 0 -t 0" < $RASP/prob/greval/DepBank/parc560/test.ne > $testfile.out
elif [ $parc = "140" ]; then
rasp_sentence=cat $RASP/scripts/rasp.sh -p"-t40 -og -m 0 -t 0" < $RASP/prob/greval/DepBank/parc140/heldout.ne > $testfile.out
else
rasp_sentence=cat $RASP/scripts/rasp.sh -p"-t40 -og -m 0 -t 0" < $RASP/prob/greval/DepBank/parc700/all.ne > $testfile.out
fi

else 
# NON NE :
if [ $parc = "560" ]; then
rasp_sentence=cat $RASP/scripts/rasp.sh -p"-t40 -og -m 0 -t 0" < $RASP/prob/greval/DepBank/parc560/test.not-ne > $testfile.out
elif [ $parc = "140" ]; then
rasp_sentence=cat $RASP/scripts/rasp.sh -p"-t40 -og -m 0 -t 0" < $RASP/prob/greval/DepBank/parc140/heldout.not-ne > $testfile.out
else
rasp_sentence=cat $RASP/scripts/rasp.sh -p"-t40 -og -m 0 -t 0" < $RASP/prob/greval/DepBank/parc700/all.not-ne > $testfile.out
fi

fi
fi

# sentences for evaluation in $1.out - transform from .data to .forms output:
# creates $testfile.trans.parses $testfile.trans.grtext

if [ $translate = "false" ]; then
   $RASP/prob/greval/eval-split.sh $testfile
else   


if [ $ner = "true" ]; then

#NE:
if [ $parc = "560" ]; then
   $RASP/prob/greval/eval_transform.sh $RASP $testfile $numberwords $RASP/prob/greval/DepBank/parc560/test.ne.stag
elif [ $parc = "140" ]; then
$RASP/prob/greval/eval_transform.sh $RASP $testfile $numberwords $RASP/prob/greval/DepBank/parc140/heldout.ne.stag
else
$RASP/prob/greval/eval_transform.sh $RASP $testfile $numberwords $RASP/prob/greval/DepBank/parc700/all.ne.stag
fi
else

#NON NE:
if [ $parc = "560" ]; then
   $RASP/prob/greval/eval_transform.sh $RASP $testfile $numberwords $RASP/prob/greval/DepBank/parc560/test.not-ne.stag
elif [ $parc = "140" ]; then
$RASP/prob/greval/eval_transform.sh $RASP $testfile $numberwords $RASP/prob/greval/DepBank/parc140/heldout.not-ne.stag
else
$RASP/prob/greval/eval_transform.sh $RASP $testfile $numberwords $RASP/prob/greval/DepBank/parc700/all.not-ne.stag
fi

fi

fi

# GR gold standard: only include the "head" of things that are marked as
# "proper" in the PARC annotation

if [ $ner = "true" ]; then

if [ $parc = "700" ]; then

gawk -v RASP=$RASP 'BEGIN{hld="0"; \
            sent=0; senth=0; sentt=0; rasp=0} \
  function output_gr() {\
    nmatches=0; \
    for (v in proper) {if (index($0,v)) {nmatches++}}; \
    if (nmatches<2) {print > outfile}} \
  {sub(/[ ]*;;;.*$/, "")} \
  /^rasp[(]/ { \
    sent++; 
    outfile=RASP "/prob/greval/DepBank/parc700/sents-ne-all.gr"; 
    printf("\n%s\n", sent) > outfile ;
    rasp=1; next} \
  /^[)]$/ {rasp=0} \
  rasp==0 {next} \
  /^[(][^)]*[)][)][)]$/ {sub(/[)][)]$/, ""); output_gr(); rasp=0; next} \
  /^[(][^)]*[)][)]$/ {sub(/[)]$/, ""); output_gr(); rasp=0; next} \
  {output_gr()}' $RASP/prob/greval/DepBank/parc700/gold700ne.rasp

else

gawk -v RASP=$RASP  'BEGIN{hld=" 1 6 11 16 21 26 31 36 41 46 51 56 61 66 71 76 81 86 91 96 101 106 111 116 121 126 131 136 141 146 151 156 161 166 171 176 181 186 191 196 201 206 211 216 221 226 231 236 241 246 251 256 261 266 271 276 281 286 291 296 301 306 311 316 321 326 331 336 341 346 351 356 361 366 371 376 381 386 391 396 401 406 411 416 421 426 431 436 441 446 451 456 461 466 471 476 481 486 491 496 501 506 511 516 521 526 531 536 541 546 551 556 561 566 571 576 581 586 591 596 601 606 611 616 621 626 631 636 641 646 651 656 661 666 671 676 681 686 691 696 "; \
            sent=0; senth=0; sentt=0; rasp=0} \
  function output_gr() {\
    nmatches=0; \
    for (v in proper) {if (index($0,v)) {nmatches++}}; \
    if (nmatches<2) {print > outfile}} \
  {sub(/[ ]*;;;.*$/, "")} \
  /^rasp[(]/ { \
    sent++; 
    if (hld ~ " " sent " ") \
      {senth++; outfile=RASP "/prob/greval/DepBank/parc140/sents-ne-hld.gr"; printf("\n%s\n", senth) > outfile; } \
      else {sentt++; outfile=RASP "/prob/greval/DepBank/parc560/sents-ne-tst.gr"; printf("\n%s\n", sentt) > outfile}; \
    rasp=1; next} \
  /^[)]$/ {rasp=0} \
  rasp==0 {next} \
  /^[(][^)]*[)][)][)]$/ {sub(/[)][)]$/, ""); output_gr(); rasp=0; next} \
  /^[(][^)]*[)][)]$/ {sub(/[)]$/, ""); output_gr(); rasp=0; next} \
  {output_gr()}' $RASP/prob/greval/DepBank/parc700/gold700ne.rasp

grep -v "()" $RASP/prob/greval/DepBank/parc560/sents-ne-tst.gr > $RASP/prob/greval/DepBank/parc560/sents-ne-tst.gr2
mv $RASP/prob/greval/DepBank/parc560/sents-ne-tst.gr2 $RASP/prob/greval/DepBank/parc560/sents-ne-tst.gr

grep -v "()" $RASP/prob/greval/DepBank/parc140/sents-ne-hld.gr > $RASP/prob/greval/DepBank/parc140/sents-ne-hld.gr2
mv $RASP/prob/greval/DepBank/parc140/sents-ne-hld.gr2 $RASP/prob/greval/DepBank/parc140/sents-ne-hld.gr

fi

else


if [ $parc = "700" ]; then

gawk -v RASP=$RASP 'BEGIN{hld="0"; \
            sent=0; senth=0; sentt=0; rasp=0} \
  function output_gr() {\
    nmatches=0; \
    for (v in proper) {if (index($0,v)) {nmatches++}}; \
    if (nmatches<2) {print > outfile}} \
  {sub(/[ ]*;;;.*$/, "")} \
  /^rasp[(]/ { \
    sent++; 
    outfile=RASP "/prob/greval/DepBank/parc700/sents-all.gr"; 
    printf("\n%s\n", sent) > outfile ;
    rasp=1; next} \
  /^[)]$/ {rasp=0} \
  rasp==0 {next} \
  /^[(][^)]*[)][)][)]$/ {sub(/[)][)]$/, ""); output_gr(); rasp=0; next} \
  /^[(][^)]*[)][)]$/ {sub(/[)]$/, ""); output_gr(); rasp=0; next} \
  {output_gr()}' $RASP/prob/greval/DepBank/parc700/gold700.rasp

else

echo "getting the sents-tst.gr file" 

gawk -v RASP=$RASP  'BEGIN{hld=" 1 6 11 16 21 26 31 36 41 46 51 56 61 66 71 76 81 86 91 96 101 106 111 116 121 126 131 136 141 146 151 156 161 166 171 176 181 186 191 196 201 206 211 216 221 226 231 236 241 246 251 256 261 266 271 276 281 286 291 296 301 306 311 316 321 326 331 336 341 346 351 356 361 366 371 376 381 386 391 396 401 406 411 416 421 426 431 436 441 446 451 456 461 466 471 476 481 486 491 496 501 506 511 516 521 526 531 536 541 546 551 556 561 566 571 576 581 586 591 596 601 606 611 616 621 626 631 636 641 646 651 656 661 666 671 676 681 686 691 696 "; \
            sent=0; senth=0; sentt=0; rasp=0} \
  function output_gr() {\
    nmatches=0; \
    for (v in proper) {if (index($0,v)) {nmatches++}}; \
    if (nmatches<2) {print > outfile}} \
  {sub(/[ ]*;;;.*$/, "")} \
   /^rasp[(]/ { \
    sent++; 
    if (hld ~ " " sent " ") \
      {senth++; outfile=RASP "/prob/greval/DepBank/parc140/sents-hld.gr"; printf("\n%s\n", senth) > outfile; } \
      else {sentt++; outfile=RASP "/prob/greval/DepBank/parc560/sents-tst.gr"; printf("\n%s\n", sentt) > outfile}; \
    rasp=1; next} \
  /^[)]$/ {rasp=0} \
  rasp==0 {next} \
  /^[(][^)]*[)][)][)]$/ {sub(/[)][)]$/, ""); output_gr(); rasp=0; next} \
  /^[(][^)]*[)][)]$/ {sub(/[)]$/, ""); output_gr(); rasp=0; next} \
  {output_gr()}' $RASP/prob/greval/DepBank/parc700/gold700.rasp

fi

fi

# parser output to be evaluated (from rasp_parse.sh -otg option): extract
# GRs (moving any ncsubj GRs to end of each set), remove quote GRs, and map
# pairs of passive and ncsubj GRs to ncsubj...obj

# run evaluation
if [ $ner = "true" ]; then

# NE :
if [ $parc = "560" ]; then
std=$RASP/prob/greval/DepBank/parc560/sents-ne-tst.gr
elif [ $parc = "140" ]; then
std=$RASP/prob/greval/DepBank/parc140/sents-ne-hld.gr
else
std=$RASP/prob/greval/DepBank/parc700/sents-ne-all.gr
fi

else

if [ $parc = "560" ]; then
std=$RASP/prob/greval/DepBank/parc560/sents-tst.gr
elif [ $parc = "140" ]; then
std=$RASP/prob/greval/DepBank/parc140/sents-hld.gr
else
std=$RASP/prob/greval/DepBank/parc700/sents-all.gr
fi
fi

echo "DOING EVALUATION...testfile: $testfile"
echo "DOING EVALUATION...std: $std"

if [ $translate = "false" ]; then
   $RASP/prob/greval/eval_system.sh -c $std -t $testfile $evalopts -z $RASP
else    
   $RASP/prob/greval/eval_system.sh -c $std -t $testfile.trans $evalopts -z $RASP
fi

if [ $deb = "false" ]; then
  rm -rf $testfile.grtext $testfile.parses
fi
