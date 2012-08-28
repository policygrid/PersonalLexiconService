SHELL=/bin/sh
CC=gcc

# Note that FLEX must point to the 2.5.4a version of flex recompiled with
# larger internal limits -- see the README file

FLEX=/home/`whoami`/flex-2.5.4/flex
#FLEX=/usr/groups/nltools/tools/flex-2.5.4/flex

#
# Flex options -Cfe -8 give good speed.
# But for use via pipes inside other programs don't give flex any options
# and use gcc option -Dinteractive.
#
#FLEXOPT = -Cfe -8
#CCOPT = -O
FLEXOPT =
CCOPT = -O -Dinteractive

morpha.yy.c: morpha.lex
	$(FLEX) -i $(FLEXOPT) -o$@ ../enc/charclasses.flex $< 

%.clean:
	/bin/rm -f $*.yy.c

%: %.yy.c
	$(CC) $(CCOPT) -o $@ $< 
	strip $@
	mv $< $<.saved

morph.tar.gz:
	( cd ..; tar czf morph/morph.tar.gz morph/Makefile morph/README morph/doc.txt morph/gpost morph/gpre morph/invert.sh morph/minnen.pdf morph/morpha.*_* morph/morpha.lex morph/verbstem.list )
