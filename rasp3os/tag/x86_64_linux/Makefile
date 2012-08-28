# Makefile for labeller (standard version)
# Does not include some of the minor utilities.

CC = gcc
CCOPT = -g -ansi -W -Wall -DNo_Phrase -c
LINK = gcc
LINKOPT = -g
LINKLIB = 
LIB = ar
LIBOPT = rc
DOLINK = $(LINK) $(LINKOPT) -o $@
LABELLIB = labellib.a
VPATH = ../src

# This is the only Linux specific bit at the moment

LIBDB = ../database/x86_64_linux/libdb-4.2.so

# This is a suffix rule 
.c.o: 
	$(CC) $(CCOPT) $<

# Main target
all:  	label

# Library
labellib.a: common.o unkcommon.o diction.o unkdiction.o io.o list.o low.o map.o trans.o
	$(LIB) $(LIBOPT) $@ $?

# Targets
label: analyse.o mainl.o label.o stack.o  $(LABELLIB)
	$(DOLINK) analyse.o mainl.o label.o stack.o  $(LABELLIB) $(LINKLIB) $(LIBDB) -lm

# For cleanup purposes
clean:	rm_objects rm_executables

rm_objects:
	rm -f analyse.o common.o unkcommon.o diction.o unkdiction.o io.o list.o low.o map.o trans.o label.o mainl.o stack.o

# For cleanup purposes
rm_executables:
	rm -f label

# c -> o
analyse.o: analyse.c common.h map.h diction.h label.h analyse.h
common.o: common.c common.h
unkcommon.o: unkcommon.c common.h diction.h unkcommon.h unkdiction.h \
 map.h
diction.o: diction.c common.h diction.h map.h
unkdiction.o: unkdiction.c common.h diction.h unkcommon.h unkdiction.h \
 map.h
io.o: io.c common.h diction.h low.h map.h label.h stack.h unkcommon.h \
 unkdiction.h analyse.h
label.o: label.c common.h diction.h trans.h label.h map.h stack.h \
 analyse.h unkcommon.h
list.o: list.c common.h list.h
low.o: low.c common.h low.h map.h
mainl.o: mainl.c common.h diction.h trans.h label.h map.h low.h unkcommon.h \
 unkdiction.h analyse.h
map.o: map.c common.h list.h map.h
stack.o: stack.c common.h map.h diction.h label.h stack.h unkcommon.h \
 unkdiction.h list.h
trans.o: trans.c common.h trans.h map.h
