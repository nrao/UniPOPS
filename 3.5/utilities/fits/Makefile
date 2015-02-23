# @(#)Makefile	5.2 11/02/94
#
CCMP = cc
CC = $(CCMP)
FDEBOROPT = -g
CDEBOROPT = -g
FLOATOPT =
FILLIB = -libmil
CILLIB = -libmil
FFLOAT =  $(FDEBOROPT) $(FLOATOPT) $(FILLIB)
CFLOAT =  $(CDEBOROPT) $(FLOATOPT) $(CILLIB)
CFLAGS = $(CFLOAT)
FFLAGS = $(FFLOAT)
LFLAGS = $(FFLOAT)
LLIBS  = -lm
# Macros which define compilation and linking parameters
# [FC]DEBOROPT (debug or optimize flag for fortran or c) = 'g' as a default 
# for using the debugger (reset for optimization using command line 
# substitution).
#
#---------------------------------------------------------------

U2FOBJ = u2f.o uinit.o udata.o utrans.o fitsubs.o
F2UOBJ = f2u.o fitsubs.o ureadhdr.o umake.o uinit.o utrans.o
#
#---------------------------------------------------------------
#
U2FGLOBH = u2f.o uinit.o udata.o f2u.o umake.o utrans.o
F2UH = utrans.o ureadhdr.o umake.o f2u.o
UNIPOPSH = u2f.o f2u.o udata.o  utrans.o  umake.o 
SETKWH = f2u.o 
#
#---------------------------------------------------------------
#

BINLIST = u2f f2u

all: $(BINLIST)

u2f : $(U2FOBJ)
	$(CC) $(CFLAGS) -o u2f $(U2FOBJ) $(LLIBS)

f2u : $(F2UOBJ)
	$(CC) $(CFLAGS) -o f2u $(F2UOBJ) $(LLIBS)

clean: 
	rm -f f2u u2f
	rm -f *.o
	-sccs clean

#
#---------------------------------------------------------------
#
$(U2FGLOBH): u2fglob.h

$(F2UH): f2u.h

$(UNIPOPSH): unipops.h

$(SETKWH): setkw.h

