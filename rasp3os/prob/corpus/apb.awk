#!/bin/sh
# adapted from counts2analysis file
#
# compute percentage coverage and APB for set of parsed sentences
# Print PB (literal interpretation of Black), APB (geom mean over sentences
# of the nwords'th root of nparses)

gawk '{suc=$NF
       #printf $(NF)
       if (suc > 0) # successful parse
         {count=$(NF)
          len=NF-1; nwords+=len
          nsuc+=1
          #logpbtotal+=log(count) - log(len)
          logapbtotal+=log(count) / len}
       else {nwords+=NF-1}}
      END{"date" | getline date; printf "%s on %s\n",FILENAME,date
          msl=nwords/NR
          #printf "Count" count
          #printf "Length" len
          #printf "Logpb" logpbtotal
          #printf "Nsuc" nsuc
          printf "Mean sentence length = %s tokens\n",msl
          printf "Coverage = %s%% (%s/%s)\n",100*nsuc/NR,nsuc,NR
          #pb=exp(logpbtotal / nsuc)
      	  apb=exp(logapbtotal / nsuc)
          #printf "PB = %s\n",pb
          printf "APB = %s\n",apb
          #printf "PB*%s = %s\n",msl,pb*msl
          printf "APB^%s = %s\n",msl,exp(log(apb) * msl)
          }' $1
