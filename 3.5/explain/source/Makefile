# @(#)Makefile	5.3 11/02/94
#
FCMP= f77
FC = $(FCMP)
FDEBOROPT = -g
FLOATOPT = 
FILLIB = -libmil
FFLOAT =  $(FDEBOROPT) $(FLOATOPT) $(FILLIB)
FFLAGS = -u -i2 $(FFLOAT)
LFLAGS = $(FFLOAT)
LLIBS  = -lm
AR = /usr/5bin/ar
#
# Macros which define compilation and linking parameters
# FDEBOROPT (debug or optimize flag) = 'g' as a default for using the debugger 
# 	(reset for optimization using command line substitution).
#

BINLIST = explain initexplain mkexp manpages manonepage manrange explainmap \
	  filecheck

OTHER = README addpage

all:	$(BINLIST) $(OTHER)

clean:
	rm -f $(BINLIST)
	-sccs clean
	rm -f *.o *.a

explain: explain.o explainlib.a 
	$(FC) $(LFLAGS) explain.o -o explain explainlib.a  $(LLIBS)

explainmap: explainmap.o explainlib.a
	$(FC) $(LFLAGS) explainmap.o -o explainmap explainlib.a $(LLIBS)

filecheck: filecheck.o explainlib.a
	$(FC) $(LFLAGS) filecheck.o -o filecheck explainlib.a $(LLIBS)

initexplain: initexplain.o explainlib.a
	$(FC) $(LFLAGS) initexplain.o -o initexplain explainlib.a  $(LLIBS)

mkexp: mkexp.o explainlib.a
	$(FC) $(LFLAGS) mkexp.o -o mkexp explainlib.a  $(LLIBS)

manpages: manpages.o explainlib.a
	$(FC) $(LFLAGS) manpages.o -o manpages explainlib.a  $(LLIBS)

manrange: manrange.o explainlib.a
	$(FC) $(LFLAGS) manrange.o -o manrange explainlib.a  $(LLIBS)

manonepage: manonepage.o explainlib.a
	$(FC) $(LFLAGS) manonepage.o -o manonepage explainlib.a  $(LLIBS)

explainlib.a: explainlib.o
	rm -f explainlib.a
	$(AR) r explainlib.a explainlib.o

explainlib.o: explain.inc explainlib.f
	$(FC) $(FFLAGS) -c explainlib.f

explain.o: explain.inc

explainmap.o: explain.inc explainmap.inc

filecheck.o: explain.inc

initexplain.o: explain.inc

mkexp.o: explain.inc

manpages.o: explain.inc

manrange.o: explain.inc

manonepage.o: explain.inc
