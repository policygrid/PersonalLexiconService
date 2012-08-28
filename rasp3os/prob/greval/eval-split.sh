#!/bin/sh

gawk 'BEGIN{sent=0} \
  /[0-9] ; [(]/ { \
    sent++; printf("\n%s\n", sent); \
    sub(/^[(]/,""); gsub(/[|\\]/,""); \
    gsub(/:[0-9]*_[^ ][^ <)]*/,""); gsub(/[)] [0-9] ; .*$/,""); \
    gsub(/<[/]?w>/,""); \
    print}' $1.out > $1.grtext

# parser output to be evaluated (from rasp_parse.sh -otg option): extract
# GRs (moving any ncsubj GRs to end of each set), remove quote GRs, and map
# pairs of passive and ncsubj GRs to ncsubj...obj

cat $1.out  | \
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
  {print}' > $1.parses
