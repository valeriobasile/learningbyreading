
PROJECT = weights6
BACKUP = *.cc *.h Makefile generate.prl functions
OBJECTS = $(WEIGHTS) weights.o
PROGRAMS = libweights.a

include /home/james/system/nlp/Makefile.incl

include Makefile.gen

libweights.a: $(OBJECTS) Makefile
	ar cr libweights.a $(OBJECTS)
	ranlib libweights.a

include Makefile.deps
