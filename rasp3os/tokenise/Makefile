SHELL=/bin/sh
CC=gcc
FLEX=/home/`whoami`/flex-2.5.4/flex

CCOPT = -O
FLEXOPT =

%.yy.c: %.flex
	$(FLEX) $(FLEXOPT) -o$@ ../enc/charclasses.flex $< 

%.clean:
	/bin/rm -f $*.yy.c

%: %.yy.c
	$(CC) $(CCOPT) -o $@ $<
	strip $@
	mv $< $<.saved
