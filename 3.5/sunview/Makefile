# @(#)Makefile	5.2 11/02/94
#
FCMP = f77
CCMP = cc
FC = $(FCMP)
CC = cc
FDEBOROPT = -g
CDEBOROPT = -g
FLOATOPT = 
F77INLINE = -libmil
CINLINE = -libmil
LIBCORE = /usr/old/libcore77.a /usr/old/libcore.a
FFLOAT =  $(FDEBOROPT) $(FLOATOPT) $(F77INLINE)
CFLOAT =  $(CDEBOROPT) $(FLOATOPT) $(CINLINE)
FFLAGS = -i2 -u $(FFLOAT)
CFLAGS = -I. $(CFLOAT) 
CLLIBS  = -lsuntool -lsunwindow -lpixrect -lm
FLLIBS  = -lsunwindow -lpixrect -lm 
#
# Macros which define compilation and linking parameters
# [FC]DEBOROPT (debug or optimize flag, for fortan or c) = 'g' as a default 
# for using the debugger (reset for optimization using command line 
# substitution).
#

BINLIST = popstool.exe scgraphics.exe 

all:	$(BINLIST)

clean:
	rm -f popstool.exe scgraphics.exe
	rm -f *.o
	rm -f shm_graph.c
	-sccs clean
	rm -f shm_graph.c

install: popstool.exe scgraphics.exe
	\cp -p scgraphics.exe popstool.exe ${popsdir}sunbin

popstool.exe: popstool.c pops.icon
	$(CC) popstool.c $(CFLAGS) -o popstool.exe $(CLLIBS)

scgraphics.exe: scgraphics.o shm_graph.o keystroke.o
	$(FC) scgraphics.o shm_graph.o  keystroke.o $(FFLAGS) -o scgraphics.exe $(LIBCORE) -lsunwindow -lpixrect $(FLLIBS)

shm_graph.o: shm_graph.c
	$(CC) -c shm_graph.c $(CFLAGS) 

keystroke.o: keystroke.c
	$(CC) -c keystroke.c $(CFLAGS) 

shm_graph.c: $(popsdir)source/conline/shm_graph.c
	rm -f shm_graph.c
	cp -p $(popsdir)source/conline/shm_graph.c .
	chmod +w shm_graph.c

scgraphics.o: scgraphics.f
	$(FC) -c scgraphics.f $(FFLAGS)
