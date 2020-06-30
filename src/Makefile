C = gcc
OPTS = -c -fPIC
LOADER = gcc

SOURCES_C = ks.c init.c distance.c
DEPENDS = $(SOURCES_C:.c=.d)
OBJECTS = $(SOURCES_C:.c=.o) 

BoutrosLab.plotting.general: $(OBJECTS)
	$(R_HOME)/bin/R CMD SHLIB -o BoutrosLab.plotting.general.so $(OBJECTS)

clean:
	rm -f *.o *.so
