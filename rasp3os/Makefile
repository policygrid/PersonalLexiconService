# The RASP system is ready-built for a number of unix architectures
#
# To re-build the system in the current directory, execute
#
#   make all

arch=$(shell uname -m | sed "s/i.86/ix86/")_$(shell uname -s | tr "[:upper:]" "[:lower:]")

all: sentence_binary tokenise_binary tag_binary morph_binary parse_binary

sentence_binary:
	( \
		cd sentence; \
		make sentence; \
		mv sentence sentence.$(arch); \
	)

tokenise_binary:
	( \
		cd tokenise; \
		make token; \
		mv token token.$(arch); \
	)

tag_binary:
	( \
		cd tag/$(arch); \
		make label; \
	)

morph_binary:
	( \
		cd morph; \
		make morpha; \
		mv morpha morpha.$(arch); \
	)

parse_binary:
	( \
		RASP=$(CURDIR) ./scripts/deliver.sh; \
	)

clean:
	find . -name "*.fasl" -exec rm {} \;
	find . -name "*.o" -exec rm {} \;	
