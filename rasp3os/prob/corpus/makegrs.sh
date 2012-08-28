#!/bin/csh
#run from /tsgram

foreach file (corpus/*.trees1)
echo $file
corpus/grs/rungrs1.sh tsg-grs $file > corpus/grs/`basename $file .trees1`.grs
end
