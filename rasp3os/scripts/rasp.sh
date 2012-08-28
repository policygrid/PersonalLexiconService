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
# Run the complete RASP toolkit: sentence boundary detector - tokeniser -
# ... - parser. Insert the appropriate value for the shell variable RASP
# right below this comment. Nothing else in the file should be changed.
#
# Example invocation:
#
# ./rasp.sh -m < README > README.parses
#

RASP=/home/`whoami`/rasp3os

arch=`uname -m | sed "s/i.86/ix86/"`_`uname -s | tr "[:upper:]" "[:lower:]"`
if [ $arch = ix86_darwin ] && [ `sysctl -n hw.optional.x86_64` = 1 ]; then
    arch=x86_64_darwin;
fi

tempf=/tmp/`whoami`-`basename $0`-$$


if [ ! -d "$RASP" ]; then
  printf "%s: could not read RASP directory '%s'\n" "$0" "$RASP" > /dev/stderr
  exit 1
fi

# Point tagger binary at our version of libdb.so

export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$RASP/tag/database/${arch} # linux
export DYLD_LIBRARY_PATH=$DYLD_LIBRARY_PATH:$RASP/tag/database/${arch} # mac

# Process any command line options

debug="false"
tagout="O36"
parseopts=
while getopts dmp:t: opt
do
     case $opt in
     d) debug="true";;
     m) tagout="O60";;
     p) parseopts="$OPTARG";;
     t) tempf="$OPTARG";;
     ?) echo "Usage: $0 [-d] [-m] [-p<parseopts>]";
        exit 1;;
     esac
done

if [ $debug = "true" ]; then
   parseopts=`echo $parseopts "-d"`
fi

# Find sentence boundaries adding sentence-initial anchors ^, then tokenise. 

${rasp_sentence:-$RASP/sentence/sentence.${arch}} | \
${rasp_tokenise:-$RASP/tokenise/token.${arch}} | tee $tempf.token |

# Tag, either with single tag per word, or with multiple tags for increased
# robustness if -m command line option specified. 
#
#  W tag turns on uncapatilised look up as captialised

eval ${rasp_tag:-"$RASP/tag/${arch}/label - B1 b C1 N W \
  t $RASP/tag/auxiliary_files/slb.trn \
  d $RASP/tag/auxiliary_files/seclarge.lex \
  j $RASP/tag/auxiliary_files/unkstats-seclarge \
  m $RASP/tag/auxiliary_files/tags.map \
  $tagout"} | sed "s/&raspsquo;/'/g;" | tee $tempf.tag |


# If multiple tags per word add wordforms to tag alternatives, else put
# sentences one to a line. Append part-of-speech tags to wordforms so morph
# can pick them up.

if [ $tagout = "O60" ]; then
  awk 'BEGIN{out=1}
    /^\^ \^/ {out=0}
    out      {print; next}
    {gsub(/\[\*\+\]/,"")
       {ws=$1; w=$1; we=""; sub(/^[^ ]* /,"",$0)}
     for (i=1; i<=NF; i++) \
       {colon=match($i,/:[-0-9e.]*$/)
        tag=substr($i,1,colon-1)
        score=substr($i,colon+1)
        alt[ws "_" tag we]=score}
     printf "%s ",w
     for (x in alt) {printf "%s &rasp_colon;%s ",x,alt[x]; delete alt[x]}
     printf "\n"}'
else
  awk 'BEGIN{out=1}
    /^\^ \^/ {if (!out) {printf "\n"}; out=0}
    out      {print; next}
       {printf "%s_%s ",$1,$2}
    END{if (!$out) {printf "\n"}}'
fi | tee $tempf.forms | 

# Morph analyse

eval ${rasp_morph:-"$RASP/morph/morpha.${arch} -actf $RASP/morph/verbstem.list"} | \
sed 's/ &rasp_colon;/:/g; s/&rasp_underscore;/_/g' | tee $tempf.data |

# Run parser

eval ${rasp_parse:-"$RASP/scripts/rasp_parse.sh $parseopts"}

rasp_status=$?


# Remove intermediate files

if [ $debug = "false" ]; then
  rm -rf $tempf.token $tempf.tag $tempf.forms $tempf.data
fi

exit $rasp_status
