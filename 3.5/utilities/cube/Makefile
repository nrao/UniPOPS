# @(#)Makefile	5.4 03/01/99
#
FCMP = f77
CCMP = cc
FC = $(FCMP)
CC = $(CCMP)
FDEBOROPT = -g
CDEBOROPT = -g
FLOATOPT =
FILLIB = -libmil
CILLIB = -xlibmil
FFLOAT =  $(FDEBOROPT) $(FLOATOPT) $(FILLIB)
CFLOAT =  $(CDEBOROPT) $(FLOATOPT) $(CILLIB)
FFLAGS = -u $(FFLOAT)
CFLAGS = $(CFLOAT)
LFLAGS = $(FFLOAT)
LLIBS  = -lm
# Macros which define compilation and linking parameters
# FDEBOROPT (debug or optimize flag) = 'g' as a default for using the debugger 
# 	(reset for optimization using command line substitution).
#

CUBEOBJ = adjustclass.o cube.o cvrt.o ddtoddmmss.o fitsdates.o headline.o \
	  init.o lchkdate.o position.o readindex.o readscan.o setuphead.o \
	  toilet2.o vask.o velocity.o xask.o yask.o

CUBEINC = adjustclass.o cube.o headline.o init.o position.o setuphead.o toilet2.o \
	  velocity.o 

DFORMINC = adjustclass.o cube.o position.o setuphead.o velocity.o

all: cube.exe

clean:
	rm -f cube.exe
	rm -f *.o
	-sccs clean

cube.exe: $(CUBEOBJ) $(CUBEINC)
	$(FC) $(LFLAGS) -o cube.exe $(CUBEOBJ) $(LLIBS)

fitsdates.o: $(popsdir)/source/conline/fitsdates.c
	rm -f fitsdates.c
	\cp $(popsdir)source/conline/fitsdates.c fitsdates.c
	$(CC) -c $(CFLAGS) fitsdates.c

$(CUBEINC): params.inc cube.inc

$(DFORMINC): dform.inc

params.inc: $(popsdir)includes/params.inc
	rm -f params.inc
	\cp $(popsdir)includes/params.inc params.inc
