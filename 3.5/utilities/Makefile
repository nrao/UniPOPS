# @(#)Makefile	5.8 09/10/98
#
ARCH = `uname -m`
CCMP = cc
FCMP = f77
CC = @cd $(@D); echo $@; $(CCMP)
FC = @cd $(@D); $(FCMP)
FDEBOROPT = -g
CDEBOROPT = -g
FLOATOPT = 
FILLIB = /usr/lang/SC1.0/libm.il
CILLIB = /usr/lib/libm.il
FFLOAT =  $(FDEBOROPT) $(FLOATOPT) $(FILLIB)
CFLOAT =  $(CDEBOROPT) $(FLOATOPT) $(CILLIB)
FFLAGS = -Nn1500 -i2 -DI2ON -u $(FFLOAT) -D$(ARCH)
#	remove the -DI2ON if your remove the -i2 flag
CFLAGS = $(CFLOAT) -I$(popsdir)includes/tuc/ -I$(popsdir)includes/  -D$(ARCH) 
LFLAGS = $(FFLOAT)
LLIBS  = -lm
AR = /usr/5bin/ar
# Macros which define compilation and linking parameters
# Note redefinition of FC and CC.  
# [FC]DEBOROPT (debug or optimize flag for fortran or c) = 'g' as a default 
# for using the debugger (reset for optimization using command line 
# substitution).
#
#---------------------------------------------------------------

DIRECT = $(popsdir)utilities/
POINT = $(DIRECT)point/
VLBCL = $(DIRECT)vlbcl/
VLBFO = $(DIRECT)vlbfo/
CONV = $(DIRECT)conversion/
DISK = $(DIRECT)diskfiles/
CUBE = $(DIRECT)cube/
FITS = $(DIRECT)fits/
CONVLIB = $(CONV)library.a
ANAL = $(popsdir)source/conline/gb/
INCS = $(popsdir)includes/
TUCINCS = $(popsdir)includes/tuc/
MAC2 =
# Macros which define directories containing source code 
# popsdir = external shell script variable and gives the home directory
#	of the source code directory
#

ANALOBJO = $(MAC2)access_2.o $(MAC2)heapsort.o \
	   $(MAC2)modcomp.o $(MAC2)cvtmoddbl.o $(MAC2)cvtmodflt.o \
	   $(MAC2)lconvert.o $(MAC2)unpadraz.o $(MAC2)gbrecio.o \
	   $(MAC2)jdtodate.o $(MAC2)inrange.o
ANALOBJR = $(ANAL)data_client.o 
POINTOBJ = $(POINT)apch.o $(POINT)apfs.o $(POINT)baseline.o $(POINT)cnps.o \
	   $(POINT)convolve.o $(POINT)cptgauss.o $(POINT)cptlsq.o \
	   $(POINT)cptprep.o $(POINT)cptshape.o $(POINT)cptuinit.o \
	   $(POINT)gel.o $(POINT)mfs.o $(POINT)mis.o $(POINT)point.o \
	   $(POINT)rns.o $(POINT)uppercase.o $(POINT)lastblnk.o
VLBCLOBJ = $(VLBCL)vlbcl.o $(VLBCL)vpt.o $(POINT)rns.o $(POINT)uppercase.o \
	   $(POINT)lastblnk.o
VLBFOOBJ = $(VLBFO)vlbfo.o $(VLBFO)opt.o $(POINT)rns.o $(POINT)uppercase.o \
	   $(POINT)lastblnk.o
CONVLIBOBJ = $(CONV)average.o $(CONV)checkdate.o $(CONV)dayofyear.o \
	  $(CONV)cvtieeedbl.o $(CONV)cvtieeeflt.o \
	  $(CONV)cvtieeemod.o $(CONV)lconvert.o $(CONV)cvtmoddbl.o \
	  $(CONV)cvtmodflt.o $(CONV)cvtmodieee.o $(CONV)cvtpcieee.o \
	  $(CONV)cvtsddieee.o $(CONV)jd.o $(CONV)jdtodate.o \
	  $(CONV)rearrange.o $(CONV)unpadraz.o $(CONV)rwbuff.o \
	  $(CONV)rwbuffer.o $(CONV)tuc2uni.o $(CONV)tucalign.o \
	  $(CONV)tucconvert.o $(CONV)tucraz.o $(CONV)vax2sun.o \
	  $(CONV)uppercase.o $(CONV)lastblnk.o $(CONV)getsprec.o \
	  $(CONV)conversion2.o
# Macros which list the object files in the various subdirectories 
#
#----------------------------------------------------------------

POINTINC = $(POINT)baseline.o $(POINT)convolve.o $(POINT)cptgauss.o \
	   $(POINT)cptlsq.o $(POINT)cptprep.o $(POINT)cptuinit.o \
	   $(POINT)point.o
CVTINC = $(CONV)average.o $(CONV)rearrange.o $(CONV)tele2ieee.o
I140H = $(CONV)lconvert.o $(CONV)cvtsddieee.o
SDDH = $(CONV)lconvert.o $(CONV)cvtpdflsdd.o $(CONV)cvtsddieee.o \
	  $(CONV)unpadraz.o $(CONV)tuc2uni.o \
	  $(CONV)tucalign.o $(CONV)tucconvert.o \
	  $(CONV)tucraz.o $(DISK)makeindex.o
SPHDR = $(CONV)lconvert.o
PDFLH = $(CONV)cvtpdflsdd.o
TUCONLINESDDH = $(CONV)cvtpdflsdd.o $(CONV)tuc2uni.o $(CONV)tucalign.o \
	        $(CONV)tucconvert.o
MODESH = $(DISK)modefn.o
# Macros which list the 'Include' file dependencies
#
#---------------------------------------------------------------

BINLIST = checkfile.exe makeempty.exe makeindex.exe point.rmt.exe  \
	  vlbcl.rmt.exe vlbfo.rmt.exe ldump.rmt.exe cdump.rmt.exe ieee2ascii.exe \
	  ascii2ieee.exe ieee2keep.exe keep2ieee.exe tele2ieee.exe pcieee.exe \
	  ieee2sdd.exe sdd2ieee.exe cvtpdflsdd.exe ieeesummary.exe  \
	  ieee2pcpops.exe  makerecindex.exe tele2recs.exe makeoldindex.exe \
	  makeoldrecindex.exe
OLLIST = point.exe vlbcl.exe vlbfo.exe ldump.exe cdump.exe 
EXECS = cvt.disk-disk cvt.disk-tape cvt.tape-disk fits2uni uni2fits readfits \
	vsst.sourcetemp vsst.systemp pointing cdump ldump cvt.tele-recs \
	expandsdd mergesdd pdfl2sdd new2old bootstrap
# Macro which lists executables 
#
#---------------------------------------------------------------
# End of Macros
#
# Target and dependencies:
#
# General targets first
#

all:	EXECUTABLES $(CONVLIB) $(BINLIST)
	(cd $(CUBE);\
		 make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" LLIBS="$(LLIBS)" \
			CCMP="$(CCMP)" FCMP="$(FCMP)" all; \
		 cp -p $(CUBE)cube.exe $(DIRECT))
	(cd $(FITS);\
		 make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" LLIBS="$(LLIBS)" \
			CCMP="$(CCMP)" FCMP="$(FCMP)" all; \
		 cp -p $(FITS)u2f $(FITS)f2u $(DIRECT))

gbonline: $(OLLIST)

clean:
	rm -f $(BINLIST)
	rm -f conversion/*.o
	rm -f conversion/*.a
	rm -f diskfiles/*.o
	rm -f point/*.o
	rm -f vlbcl/*.o
	(cd $(CUBE); make clean)
	(cd $(FITS); make clean)
	rm -f vlbfo/*.o
	-sccs clean
	rm -f *.trace
	rm -f core
	-(cd $(POINT); sccs clean; rm -f apch.f apfs.f cmps.f uppercase.f \
		lastblnk.f)
	-(cd $(VLBCL); sccs clean)
	-(cd $(VLBFO); sccs clean)
	-(cd $(CONV); sccs clean; rm -f raz.c tucraz.c jdtodate.f \
		uppercase.f lastblnk.f)
	-(cd $(DISK); sccs clean; rm -f modefn.c)

# Executable targets next
#

checkfile.exe: $(DISK)checkfile.o $(DISK)modefn.o
	$(FC) $(LFLAGS) -o checkfile.exe $(DISK)checkfile.o \
		$(DISK)modefn.o $(LLIBS)

makeempty.exe: $(DISK)makeempty.o
	$(FC) $(LFLAGS) -o makeempty.exe $(DISK)makeempty.o  $(LLIBS)

ieeesummary.exe: $(DISK)summary.o
	$(CC) $(CFLAGS) -o ieeesummary.exe $(DISK)summary.o  $(LLIBS)

makeindex.exe: $(DISK)makeindex.o $(DISK)modefn.o
	$(CC) $(CFLAGS) -o makeindex.exe $(DISK)makeindex.o \
		$(DISK)modefn.o $(LLIBS)

makerecindex.exe: makeindex.exe
	\rm -f makerecindex.exe
	\ln -s makeindex.exe makerecindex.exe

makeoldindex.exe: makeindex.exe
	\rm -f makeoldindex.exe
	\ln -s makeindex.exe makeoldindex.exe

makeoldrecindex.exe: makeindex.exe
	\rm -f makeoldrecindex.exe
	\ln -s makeindex.exe makeoldrecindex.exe

vlbfo.exe: $(VLBFOOBJ) $(ANALOBJO) 
	$(FC) $(LFLAGS) -o vlbfo.exe $(VLBFOOBJ) $(ANALOBJO) $(LLIBS)

vlbfo.rmt.exe: $(VLBFOOBJ) $(ANALOBJR) 
	$(FC) $(LFLAGS) -o vlbfo.rmt.exe $(VLBFOOBJ) $(ANALOBJR) $(LLIBS)

vlbcl.exe: $(VLBCLOBJ) $(ANALOBJO) 
	$(FC) $(LFLAGS) -o vlbcl.exe $(VLBCLOBJ) $(ANALOBJO) $(LLIBS)

vlbcl.rmt.exe: $(VLBCLOBJ) $(ANALOBJR) 
	$(FC) $(LFLAGS) -o vlbcl.rmt.exe $(VLBCLOBJ) $(ANALOBJR) $(LLIBS)

point.exe: $(POINTOBJ) $(ANALOBJO) 
	$(FC) $(LFLAGS) -o point.exe $(POINTOBJ) $(ANALOBJO) $(LLIBS)

point.rmt.exe: $(POINTOBJ) $(ANALOBJR) 
	$(FC) $(LFLAGS) -o point.rmt.exe $(POINTOBJ) $(ANALOBJR) $(LLIBS)

ldump.exe: $(CONV)ldump.o $(CONVLIB) $(ANALOBJO)
	$(FC) $(LFLAGS) -o ldump.exe $(CONV)ldump.o $(CONVLIB) $(ANALOBJO) $(LLIBS)

ldump.rmt.exe: $(CONV)ldump.o $(CONVLIB) $(ANALOBJR) 
	$(FC) $(LFLAGS) -o ldump.rmt.exe $(CONV)ldump.o $(CONVLIB) $(ANALOBJR) $(LLIBS)

cdump.exe: $(CONV)cdump.o $(CONVLIB) $(ANALOBJO) 
	$(FC) $(LFLAGS) -o cdump.exe $(CONV)cdump.o $(CONVLIB) $(ANALOBJO) $(LLIBS)

cdump.rmt.exe: $(CONV)cdump.o $(CONVLIB) $(ANALOBJR) 
	$(FC) $(LFLAGS) -o cdump.rmt.exe $(CONV)cdump.o $(CONVLIB) $(ANALOBJR) $(LLIBS)

ieee2ascii.exe: $(CONV)ieee2ascii.o $(CONVLIB)
	$(CC) $(CFLAGS) -o ieee2ascii.exe $(CONV)ieee2ascii.o $(CONVLIB) $(LLIBS)

ascii2ieee.exe: $(CONV)ascii2ieee.o $(CONVLIB)
	$(FC) $(LFLAGS) -o ascii2ieee.exe $(CONV)ascii2ieee.o $(CONVLIB) $(LLIBS)

ieee2keep.exe: $(CONV)ieee2keep.o $(CONVLIB)
	$(FC) $(LFLAGS) -o ieee2keep.exe $(CONV)ieee2keep.o $(CONVLIB) $(LLIBS)

keep2ieee.exe: $(CONV)keep2ieee.o $(CONVLIB)
	$(FC) $(LFLAGS) -o keep2ieee.exe $(CONV)keep2ieee.o $(CONVLIB) $(LLIBS)

tele2ieee.exe: $(CONV)tele2ieee.o $(CONVLIB)
	@echo tele2ieee
	$(FC) $(LFLAGS) -o tele2ieee.exe $(CONV)tele2ieee.o $(CONVLIB) $(LLIBS)

tele2recs.exe: $(CONV)tele2recs.o $(CONVLIB)
	@echo tele2recs
	$(FC) $(LFLAGS) -o tele2recs.exe $(CONV)tele2recs.o $(CONVLIB) $(LLIBS)

pcieee.exe: $(CONV)pcieee.o $(CONVLIB)
	$(FC) $(LFLAGS) -o pcieee.exe $(CONV)pcieee.o $(CONVLIB) $(LLIBS)

ieee2pcpops.exe: $(CONV)ieee2pcpops.o $(CONVLIB)
	$(FC) $(LFLAGS) -o ieee2pcpops.exe $(CONV)ieee2pcpops.o $(CONVLIB) $(LLIBS)

ieee2sdd.exe: $(CONV)ieee2sdd.o $(CONVLIB) $(ANALOBJR) $(CONV)params.inc
	$(FC) $(LFLAGS) -o ieee2sdd.exe $(CONV)ieee2sdd.o $(CONVLIB) \
		$(ANALOBJR) $(LLIBS)

sdd2ieee.exe: $(CONV)sdd2ieee.o $(CONVLIB)
	$(FC) $(LFLAGS) -o sdd2ieee.exe $(CONV)sdd2ieee.o $(CONVLIB) $(LLIBS)

cvtpdflsdd.exe: $(CONV)cvtpdflsdd.o $(CONVLIB)
	$(CC) $(CFLAGS) -o cvtpdflsdd.exe $(CONV)cvtpdflsdd.o $(CONVLIB) $(LLIBS)
#
# Start of Miscelaneous dependencies
#

EXECUTABLES: $(EXECS) keep.label head.mask U2f_trans

$(CONVLIB): $(CONVLIBOBJ)
	rm -f $(CONVLIB)
	$(AR) r $(CONVLIB) $(CONVLIBOBJ)

$(DISK)makeindex.o: $(DISK)makeindex.c
	$(CC) -c -u $(CFLAGS) $(DISK)makeindex.c

$(CONV)unpadraz.o: $(popsdir)source/conline/line/raz.c
	rm -f $(CONV)unpadraz.c
	\cp $(popsdir)source/conline/line/raz.c $(CONV)unpadraz.c
	$(CC) -c $(CFLAGS) $(CONV)unpadraz.c

$(CONV)tucraz.o: $(popsdir)source/conline/line/raz.c
	rm -f $(CONV)tucraz.c
	rm -f $(CONV)tucraz.o
	\cp $(popsdir)source/conline/line/raz.c $(CONV)tucraz.c
	$(CC) -c -DUSETUC $(CFLAGS) $(CONV)tucraz.c

$(CONV)jdtodate.o: $(popsdir)source/conline/line/jdtodate.f
	rm -f $(CONV)jdtodate.f
	\cp $(popsdir)source/conline/line/jdtodate.f $(CONV)jdtodate.f
	$(FC) -c $(FFLAGS) $(CONV)jdtodate.f

$(CONV)uppercase.o: $(popsdir)source/conline/uppercase.f
	rm -f $(CONV)uppercase.f
	\cp $(popsdir)source/conline/uppercase.f $(CONV)uppercase.f
	$(FC) -c $(FFLAGS) $(CONV)uppercase.f

$(CONV)lastblnk.o: $(popsdir)source/lastblnk.f
	rm -f $(CONV)lastblnk.f
	\cp $(popsdir)source/lastblnk.f $(CONV)lastblnk.f
	$(FC) -c $(FFLAGS) $(CONV)lastblnk.f

$(CONV)conversion2.o: $(popsdir)source/conline/line/conversion2.f $(CONV)cform.inc $(CONV)params.inc
	rm -f $(CONV)conversion2.f
	\cp $(popsdir)source/conline/line/conversion2.f $(CONV)conversion2.f
	$(FC) -c $(FFLAGS) $(CONV)conversion2.f

$(POINT)apch.o: $(popsdir)source/conline/apch.f
	rm -f $(POINT)apch.f
	\cp $(popsdir)source/conline/apch.f $(POINT)apch.f
	$(FC) -c $(FFLAGS) $(POINT)apch.f

$(POINT)apfs.o: $(popsdir)source/conline/apfs.f
	rm -f $(POINT)apfs.f
	\cp $(popsdir)source/conline/apfs.f $(POINT)apfs.f
	$(FC) -c $(FFLAGS) $(POINT)apfs.f

$(POINT)cnps.o: $(popsdir)source/conline/cnps.f
	rm -f $(POINT)cnps.f
	\cp $(popsdir)source/conline/cnps.f $(POINT)cnps.f
	$(FC) -c $(FFLAGS) $(POINT)cnps.f

$(POINT)uppercase.o: $(popsdir)source/conline/uppercase.f
	rm -f $(POINT)uppercase.f
	\cp $(popsdir)source/conline/uppercase.f $(POINT)uppercase.f
	$(FC) -c $(FFLAGS) $(POINT)uppercase.f

$(POINT)lastblnk.o: $(popsdir)source/lastblnk.f
	rm -f $(POINT)lastblnk.f
	\cp $(popsdir)source/lastblnk.f $(POINT)lastblnk.f
	$(FC) -c $(FFLAGS) $(POINT)lastblnk.f

$(DISK)modefn.o: $(popsdir)source/conline/modefn.c
	rm -f $(DISK)modefn.c
	\cp $(popsdir)source/conline/modefn.c $(DISK)modefn.c
	$(CC) -c $(CFLAGS) $(DISK)modefn.c
#
# Start of include file targets
#

$(POINTINC): $(POINT)concfmt.inc $(POINT)condappl.inc $(POINT)coneappl.inc

$(CVTINC): $(CONV)cvttele.inc

$(I140H): $(INCS)140.h

$(SDDH): $(INCS)sdd.h

$(SPHDR): $(INCS)sp_hdr.h

$(PDFLH): $(TUCINCS)pdfl.h

$(TUCONLINESDDH): $(TUCINCS)tuc_online_sdd.h

$(CONV)cform.inc: $(INCS)cform.inc
	rm -f $(CONV)cform.inc
	\cp $(INCS)cform.inc $(CONV)cform.inc

$(CONV)params.inc: $(INCS)params.inc
	rm -f $(CONV)params.inc
	\cp $(INCS)params.inc $(CONV)params.inc

$(MODESH): $(INCS)modes.h

$(ANALOBJO):
	(cd $(MAC2); make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" LLIBS="$(LLIBS)" \
		CCMP="$(CCMP)" FCMP="$(FCMP)" gbonline)

$(ANALOBJR):
	(cd $(ANAL); make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" LLIBS="$(LLIBS)" \
		CCMP="$(CCMP)" FCMP="$(FCMP)" all)

