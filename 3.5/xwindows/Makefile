#
# @(#)Makefile	5.5 03/07/95
#
CCMP = cc
FCMP = f77
FC = $(FCMP)
CC = $(CCMP)
FDEBOROPT = -g
CDEBOROPT = -g
FLOATOPT = 
F77INLINE = -libmil
CINLINE = -libmil
XLIBS = -L$(OPENWINHOME)/lib -lX11
XINCS = $(OPENWINHOME)/include
FFLOAT =  $(FDEBOROPT) $(FLOATOPT) $(F77INLINE)
CFLOAT =  $(CDEBOROPT) $(FLOATOPT) $(CINLINE)
FFLAGS = -i2 -u $(FFLOAT)
CFLAGS = -I. -I$(XINCS) $(CFLOAT) 
CLLIBS  = -lm
FLLIBS  = -lm 
SOLARISXLIBS = -Bstatic $(XLIBS) -Bdynamic -lsocket -lw -ldl -lnsl
#
# Macros which define compilation and linking parameters
# [FC]DEBOROPT (debug or optimize flag for fortran or c) = 'g' as a default 
# for using the debugger (reset for optimization using command line 
# substitution).
#

BINLIST = xgraphics.exe 

all:	$(BINLIST)

clean:
	rm -f xgraphics.exe
	rm -f *.o
	rm -f shm_graph.c
	-sccs clean

install: xgraphics.exe
	\cp -p xgraphics.exe ${popsdir}sunbin

xgraphics.exe: xgraphics.o shm_graph.o xroutines.o zignal.o
#		Solaris doesn't automatically put /usr/openwin in
#		search path for dynamic libraries at run time
#		The following is an attempt to deal with that.
#		This will likely need further enhancement.
	@if [ -r /usr/lib/libsocket.a ];\
	then \
	   ( echo $(FC) xgraphics.o shm_graph.o xroutines.o zignal.o \
		  $(FFLAGS) -o xgraphics.exe $(SOLARISXLIBS) $(FILLIBS); \
		$(FC) xgraphics.o shm_graph.o  xroutines.o zignal.o \
		$(FFLAGS) -o xgraphics.exe $(SOLARISXLIBS) $(FLLIBS))\
	else \
	   ( echo $(FC) xgraphics.o shm_graph.o  xroutines.o zignal.o \
		  $(FFLAGS) -o xgraphics.exe $(XLIBS) $(FLLIBS); \
		$(FC) xgraphics.o shm_graph.o  xroutines.o zignal.o \
		$(FFLAGS) -o xgraphics.exe $(XLIBS) $(FLLIBS))\
	fi

shm_graph.o: shm_graph.c
	$(CC) -c shm_graph.c $(CFLAGS) 

shm_graph.c: $(popsdir)source/conline/shm_graph.c
	rm -f shm_graph.c
	cp -p $(popsdir)source/conline/shm_graph.c .
	chmod +w shm_graph.c

xgraphics.o: xgraphics.f
	$(FC) -c xgraphics.f $(FFLAGS)

xroutines.o: xroutines.c pops.xbm
	$(CC) -c xroutines.c $(CFLAGS)

zignal.o: zignal.c
	$(CC) -c zignal.c $(CFLAGS)

zignal.c: $(popsdir)source/conline/zignal.c
	rm -f zignal.c
	cp -p $(popsdir)source/conline/zignal.c .
	chmod +w zignal.c
