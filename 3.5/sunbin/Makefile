# Makefile: For creating line, condar, or genmemory
# @(#)Makefile	5.11 03/24/00
#
# Instructions:
# To add a new routine, add name of .o file to the apllicable macros 
# For example, if the routine, 'au5.f' is in the 'direct' directory and
# uses the 'junk.inc' include file, then add '$(DIRECT)au5.o' to the
# DIRECTOBJ and JUNKINC macros.  If no include files are used, do not
# add anything to the 'include' macros.
#
# To remove a routine, delete all references to the routines .o file
# For example, if you have removed 'au5.f', delete all entries in all
# macros which refer to 'au5.o'
#
# To rename a routine, change all references to the .o files to the files
# new name. (For example, if oldname.f is changed to newname.f, change
# all 'oldname.o' to 'newname.o' in this file.
#
# To move a routine to a new directory, first follow the above instructions
# for removing a routine and then follow the steps for adding a new routine.
#
# To add a new directory, create a macro whose name is that of the directory
# and whose 'value' is the path to the directory (use, if applicable, 
# $(popsdir) as the root directory for all of the pops source code).  Add
# another macro whose name is that directory with OBJ added at the end and
# whose valus is a list of .o files which are to be in that directory.  Add
# any neccessary dependencies to the line.exe, condar.exe, etc. target lines.
# Also add the appropriate line to the 'clean' target line.
# For example, if the directory is named 'direct', is a subdirectory of
# the $(direct9) directory, contains the files au51.f, au52.f, and is needed 
# by the program 'test.exe', then the two macros would be: 
# DIRECT = $(direct9)direct
# DIRECTOBJ = $(DIRECT)au51.o $(DIRECT)au52.o
# and the target dependency line should look like:
# test.exe: $(DIRECTOBJ) .... plus any other directories
# You would then add <tab> -rm -r $(DIRECTOBJ) to the 'clean' target.
# Of course, you may need to do more work if au51.f or au52.f use
# include files.
#
# To remove a directory, delete the two appropriate macros and the references
# to these macros on the target/dependency files throughout the file
#
# To rename a directory, change all references to the old name throughtout the 
# file to the new names.
#
# To add a new include file, add a macro whose name is that of the include
# file and whose value is that of the .o files which depend upon that include
# file.  Then add a new target whose name is that of the macro and whose
# dependency is the name of the include file.  (For example, if the include
# file is called 'ddd.inc' and is located in the $(INCLUDES) directory and the
# routines junk1.c and junk2.c, located in the 'direct1' directory, depend 
# upon that include file, then the macro would look like:
# DDDINC = $(DIRECT1)junk1.o $(DIRECT1)junk2.o
# and the target/dependency line would be:
# $(DDINC): $(INCLUDES)ddd.inc
#
# To remove an include file, delete its macro and its target/dependency line
# in the makefile.  Make sure that all source modules know about the changes
# in the include file.
#
# To change the name of an include file, make sure all source modules know
# about the change, and then change the name of the macro for the include
# file and change the target/dependency line throughout this file.
#
# If a routine no longer needs an include file, remove the reference to 
# the routine's .o file in the include file's macro.
#
# If a routine needs an include file, either because of changes you have
# made to the routine or because the routine is a new one, add the 
# routine's .o file to the include's macro.
#
# If you want to add a new execuatble to be created by make, add the name
# of the executable to the BINLIST macro, and add a new target/dependency
# using the rest of the target/dependency lines as examples.
#
# If you want to remove an executable, delete its entry in the BINLIST macro
# and remove its target/dependency lines
#
# If you want to rename an executable, change all references to the executable
# throughout the file to its new name.
#
#--------------------------------------------------------------
# Macro definitions:
# 
#--------------------------------------------------------------
CCMP = cc
FCMP = f77
CC = @cd $(@D); echo $@; $(CCMP)
FC = @cd $(@D); $(FCMP)
F77VS = VS1_4
ARCH = `uname -m`
SUNOS = 
VDATE = `date '+%y%m%d'`
#	for non-test versions after version 3.0: VERSION = version/1000.
#       where version is the version number, i.e. 3.1
#       for example: version 3.1 will have VERSION = 0.0031
VERSION = $(VDATE).
#	Control which, if any, window related stuff is compiled or cleaned
USEX = TRUE
USESUNVIEW = TRUE
CDEBOROPT = -g
FDEBOROPT = -g
FLOATOPT = 
LIBCORE = /usr/old/libcore77.a /usr/old/libcore.a
FILLIB = /usr/lang/SC1.0/libm.il
CILLIB = /usr/lib/libm.il
FFLOAT =  $(FDEBOROPT) $(FLOATOPT) $(FILLIB) -D$(F77VS)
CFLOAT =  $(CDEBOROPT) $(FLOATOPT) $(CILLIB) -D$(F77VS) $(SUNOS)
FFLAGS = -Nn1500 -i2 -DI2ON -u $(FFLOAT) -D$(ARCH)
#	remove the -DI2ON if your remove the -i2 flag
CFLAGS = $(CFLOAT) -I$(INCLUDES) -I$(TUCANINCS) -D$(ARCH) -DUSEPAD
USEHACK =
LFLAGS = $(FFLOAT)
LLIBS  = -lm
FLLIBS  = $(LLIBS)
XHOME = $OPENWINHOME
EXTRAXLIBS = -lXext
XLIBS = -L${XHOME}/lib  -lX11 ${EXTRAXLIBS}
XINCS = ${XHOME}/include
AR = /usr/5bin/ar
CPP = /lib/cpp
# Macros which define compilation and linking parameters
# Note redefinition of FC and CC.  
# FDEBOROPT (debug or optimize flag for fortran) = 'g' as a default for 
# using the debugger (reset for optimization using command line substitution).
# CDEBOROPT (debug or optimize flag for c) = 'g' as a default for 
# using the debugger (reset for optimization using command line substitution).
LOCALMAILLIST =
# used in constructing comments.exe
# this is best set in makelocal rather than here.
#
#---------------------------------------------------------------
INCLUDES = $(popsdir)includes/
SOURCE = $(popsdir)source/
CONLINE = $(SOURCE)conline/
LINE = $(CONLINE)line/
CONDAR = $(CONLINE)condar/
DISK = $(CONLINE)disk/
GENMEMORY = $(SOURCE)genmemory/
ANAL = $(popsdir)source/conline/gb/
TUCANAL = $(popsdir)source/conline/tuc/
CVANAL = $(popsdir)source/conline/cv/
TUCANINCS = $(INCLUDES)tuc/
GBANINCS = $(INCLUDES)gb/
MAC2 =
# Macros which define directories containing source code 
# popsdir = external shell script variable and gives the home directory
#	of the source code directory
#
#-----------------------------------------------------------------
ANALOBJO = $(MAC2)access_2.o $(MAC2)heapsort.o \
	   $(MAC2)modcomp.o $(MAC2)cvtmoddbl.o \
	   $(MAC2)cvtmodflt.o $(MAC2)lconvert.o $(MAC2)unpadraz.o
ANALOBJR = $(ANAL)data_client.o
TUCANALOBJ = $(TUCANAL)access.o $(TUCANAL)getincbs.o $(TUCANAL)getinit.o \
	     $(TUCANAL)incrindex.o $(TUCANAL)initfile.o \
	     $(TUCANAL)olfiles.o $(TUCANAL)verslist.o
CVANALOBJ = $(CVANAL)access.o
SOURCEOBJ = $(SOURCE)amathparser.o $(SOURCE)bclean.o $(SOURCE)closms.o \
	    $(SOURCE)compil.o $(SOURCE)copy.o $(SOURCE)copy2.o $(SOURCE)cread.o \
	    $(SOURCE)cwrite.o $(SOURCE)editor.o $(SOURCE)fill.o \
	    $(SOURCE)finish.o $(SOURCE)fshort.o $(SOURCE)getfld.o \
	    $(SOURCE)getnum.o $(SOURCE)gtbyte.o $(SOURCE)hunt.o \
	    $(SOURCE)iinitp.o $(SOURCE)inquirefile.o $(SOURCE)iounits.o \
	    $(SOURCE)irealp.o $(SOURCE)iwpc.o $(SOURCE)iwpr.o \
	    $(SOURCE)kdump.o $(SOURCE)kpack.o $(SOURCE)lastblnk.o \
	    $(SOURCE)llocat.o $(SOURCE)ltstor.o $(SOURCE)memusage.o \
	    $(SOURCE)mhunt.o $(SOURCE)oerror2.o $(SOURCE)opened.o \
	    $(SOURCE)openms.o $(SOURCE)polish.o $(SOURCE)pop.o \
	    $(SOURCE)prompt.o $(SOURCE)pseudo.o $(SOURCE)ptbyte.o \
	    $(SOURCE)push.o $(SOURCE)pushcmp.o $(SOURCE)readms.o \
	    $(SOURCE)relink.o $(SOURCE)settyp.o $(SOURCE)symbol.o \
	    $(SOURCE)unpack.o $(SOURCE)writms.o $(SOURCE)ieeestuff.o 
CNLOBJ = $(CONLINE)apch.o $(CONLINE)apfs.o $(CONLINE)assgn.o \
	 $(CONLINE)batch.o $(CONLINE)box.o $(CONLINE)bshape.o \
	 $(CONLINE)changeproj.o $(CONLINE)charsize.o \
	 $(CONLINE)checkfperror.o $(CONLINE)chngfile.o \
	 $(CONLINE)cleanup.o $(CONLINE)clrpage.o $(CONLINE)cmprsstrng.o \
	 $(CONLINE)cnps.o $(CONLINE)color.o $(CONLINE)compar.o \
	 $(CONLINE)compress.o $(CONLINE)corebox.o \
	 $(CONLINE)coreclose.o $(CONLINE)coreclrpage.o \
	 $(CONLINE)corecolor.o \
	 $(CONLINE)corecursor.o $(CONLINE)coredither.o \
	 $(CONLINE)coreflush.o $(CONLINE)coreint.o \
	 $(CONLINE)coreplace.o $(CONLINE)corevctr.o \
	 $(CONLINE)ctrlcintr.o $(CONLINE)cursor.o $(CONLINE)dither.o \
	 $(CONLINE)donemenu.o $(CONLINE)exitpops.o $(CONLINE)files.o \
	 $(CONLINE)fcioget.o $(CONLINE)filecomp.o $(CONLINE)fitsdates.o \
	 $(CONLINE)fphandler.o $(CONLINE)gcopy.o \
	 $(CONLINE)genop.o $(CONLINE)getformat.o $(CONLINE)getprel.o \
	 $(CONLINE)gpclose.o $(CONLINE)gpflush.o $(CONLINE)gphint.o \
	 $(CONLINE)gtline.o $(CONLINE)helps.o \
	 $(CONLINE)hphardplace.o \
	 $(CONLINE)hphardtcopy.o $(CONLINE)ilct.o $(CONLINE)initcio.o \
	 $(CONLINE)initcomm.o $(CONLINE)initfiles.o \
	 $(CONLINE)initfperror.o $(CONLINE)initlock.o \
	 $(CONLINE)initlog.o $(CONLINE)initmess.o \
	 $(CONLINE)initonline.o $(CONLINE)initsetup.o \
	 $(CONLINE)inrange.o $(CONLINE)interp.o $(CONLINE)main.o \
	 $(CONLINE)massgn.o $(CONLINE)menus.o \
	 $(CONLINE)modefn.o $(CONLINE)move2.o \
	 $(CONLINE)nextfield.o $(CONLINE)noghard.o $(CONLINE)nogint.o \
	 $(CONLINE)oerror.o $(CONLINE)otherbox.o $(CONLINE)packer.o \
	 $(CONLINE)pchar.o $(CONLINE)pfixchar.o $(CONLINE)place.o \
	 $(CONLINE)posthplace.o $(CONLINE)posthtcopy.o \
	 $(CONLINE)posthvctr.o $(CONLINE)pread.o $(CONLINE)pread2.o \
	 $(CONLINE)putchar.o $(CONLINE)pwrite.o $(CONLINE)qcomp.o \
	 $(CONLINE)qmshardplace.o $(CONLINE)qmshardtcopy.o \
	 $(CONLINE)qmshardvctr.o  $(CONLINE)posthcolor.o $(CONLINE)posthbox.o \
	 $(CONLINE)quichardplace.o $(CONLINE)quichardtcopy.o \
	 $(CONLINE)quichardvctr.o $(CONLINE)quick.o $(CONLINE)rdrm.o \
	 $(CONLINE)recover.o $(CONLINE)rn1.o $(CONLINE)rns.o $(CONLINE)rns8.o \
	 $(CONLINE)runtag.o $(CONLINE)scal.o $(CONLINE)setdefaults.o \
	 $(CONLINE)setflags.o $(CONLINE)shm_graph.o \
	 $(CONLINE)smooth.o $(CONLINE)stores.o $(CONLINE)subs.o \
	 $(CONLINE)tcopy.o $(CONLINE)tekclrpage.o \
	 $(CONLINE)tekcursor.o $(CONLINE)tekchar.o \
	 $(CONLINE)tekint.o $(CONLINE)tekplace.o $(CONLINE)tekvctr.o \
	 $(CONLINE)tickmrk.o $(CONLINE)udat.o $(CONLINE)undo.o \
	 $(CONLINE)unused.o $(CONLINE)upack.o $(CONLINE)updatehist.o \
	 $(CONLINE)updateline.o $(CONLINE)uppercase.o \
	 $(CONLINE)v102clrpage.o $(CONLINE)funclev.o \
	 $(CONLINE)v102cursor.o \
	 $(CONLINE)v102int.o $(CONLINE)v102place.o \
	 $(CONLINE)v102vctr.o $(CONLINE)vctr.o \
	 $(CONLINE)verbs.o $(CONLINE)yscale.o $(CONLINE)zwait.o \
	 $(CONLINE)sread.o $(CONLINE)fptfmt.o  $(CONLINE)history.o \
	 $(CONLINE)lfit.o $(CONLINE)str4cur.o $(CONLINE)autolev.o \
	 $(CONLINE)execpol.o $(CONLINE)execcomp.o $(CONLINE)zignal.o
DISKOBJ = $(DISK)getascan.o $(DISK)getbs.o \
	  $(DISK)getnindex.o $(DISK)getsddbs.o $(DISK)getslist.o \
	  $(DISK)filefns.o $(DISK)makesindex.o $(DISK)saveascan.o \
	  $(DISK)scanloc.o $(DISK)sddclose.o \
	  $(DISK)sddopen.o $(DISK)getarec.o $(DISK)getsddscan.o \
	  $(DISK)getnsddindex.o $(DISK)getsddscanlist.o
LINEOBJ = $(LINE)accums.o $(LINE)au1.o $(LINE)au2.o $(LINE)au3.o \
	  $(LINE)au4.o $(LINE)au5.o $(LINE)au6.o $(LINE)au7.o \
	  $(LINE)au8.o $(LINE)au9.o $(LINE)aua.o \
	  $(LINE)chantovel.o $(LINE)cheader.o $(LINE)checkbyloc.o \
	  $(LINE)chgres.o $(LINE)chngol.o $(LINE)chngver.o \
          $(LINE)conversion2.o $(LINE)conversion3.o \
	  $(LINE)craz.o $(LINE)cube.o $(LINE)dft.o \
	  $(LINE)drawcntr.o $(LINE)fft.o $(LINE)funct.o \
	  $(LINE)funfunct.o $(LINE)getbyloc.o $(LINE)getbynum.o \
	  $(LINE)getcurrent.o $(LINE)getlist.o $(LINE)getscan.o \
	  $(LINE)gget.o $(LINE)gmeasure.o $(LINE)gnextindex.o \
	  $(LINE)gprep.o $(LINE)header.o $(LINE)hlftne.o \
	  $(LINE)initappl.o $(LINE)itfits.o $(LINE)jdtodate.o \
	  $(LINE)laub.o $(LINE)lchkdate.o $(LINE)gbrecio.o \
	  $(LINE)linetype.o \
	  $(LINE)mfill.o $(LINE)mheader.o $(LINE)minit.o \
	  $(LINE)mlabel.o $(LINE)mraz.o $(LINE)mread.o $(LINE)msmooth.o \
	  $(LINE)msmoth2.o $(LINE)mtable.o $(LINE)mwrite.o \
	  $(LINE)lpdoc.o $(LINE)plot.o $(LINE)pltdots.o \
	  $(LINE)posstring.o $(LINE)prep.o $(LINE)progname.o \
	  $(LINE)raster.o $(LINE)raz.o $(LINE)readcube.o \
	  $(LINE)rotate.o $(LINE)rshape2.o $(LINE)savebyloc.o \
	  $(LINE)savebynum.o $(LINE)sddcopy.o $(LINE)select.o \
	  $(LINE)setunits.o $(LINE)show.o $(LINE)slice.o \
	  $(LINE)splinecf.o $(LINE)summary.o $(LINE)table.o \
	  $(LINE)tellout.o $(LINE)tells.o $(LINE)tracer.o \
	  $(LINE)veltochan.o $(LINE)zerror.o \
	  $(LINE)zlimit.o $(LINE)xtochan.o $(LINE)chantox.o $(LINE)fgauss.o \
	  $(LINE)mrqmin.o $(LINE)virfft.o $(LINE)window.o $(LINE)fitfoc.o \
	  $(LINE)tellstat.o $(LINE)getir.o $(LINE)getbyrec.o $(LINE)getotf.o \
	  $(LINE)getpolz.o
CONOBJ = $(CONDAR)accums.o $(CONDAR)au1.o $(CONDAR)au2.o \
	 $(CONDAR)au3.o $(CONDAR)au4.o $(CONDAR)au5.o $(CONDAR)au6.o \
	 $(CONDAR)au7.o $(CONDAR)au8.o $(CONDAR)au9.o $(CONDAR)aua.o \
	 $(CONDAR)caub.o $(CONDAR)cheader.o \
	 $(CONDAR)checkbyloc.o $(CONDAR)chgres.o $(CONDAR)chngol.o \
         $(CONDAR)chngver.o $(CONDAR)conversion2.o \
	 $(CONDAR)conversion3.o $(CONDAR)craz.o \
	 $(CONDAR)cube.o $(CONDAR)curfit.o $(CONDAR)dft.o \
	 $(CONDAR)drawcntr.o $(CONDAR)fchisq.o $(CONDAR)fderiv.o \
	 $(CONDAR)fft.o $(CONDAR)funct.o \
	 $(CONDAR)functn.o $(CONDAR)funfunct.o $(CONDAR)gbmean.o \
	 $(CONDAR)getbyloc.o $(CONDAR)getbynum.o $(CONDAR)getcurrent.o \
	 $(CONDAR)getlist.o $(CONDAR)getscan.o $(CONDAR)gget.o \
	 $(CONDAR)gnextindex.o $(CONDAR)gprep.o $(CONDAR)header.o \
	 $(CONDAR)hlftne.o $(CONDAR)initappl.o $(CONDAR)itfits.o \
	 $(CONDAR)jdtodate.o $(CONDAR)lchkdate.o \
	 $(CONDAR)linetype.o $(CONDAR)matinv.o \
	 $(CONDAR)mean.o $(CONDAR)mfill.o $(CONDAR)mheader.o \
	 $(CONDAR)minit.o $(CONDAR)mlabel.o $(CONDAR)mraz.o \
	 $(CONDAR)mread.o $(CONDAR)msmooth.o $(CONDAR)msmoth2.o \
	 $(CONDAR)mtable.o $(CONDAR)mwrite.o $(CONDAR)cpdoc.o \
	 $(CONDAR)plot.o $(CONDAR)pltdots.o $(CONDAR)posstring.o \
	 $(CONDAR)prep.o $(CONDAR)progname.o $(CONDAR)raster.o \
	 $(CONDAR)raz.o $(CONDAR)readcube.o $(CONDAR)rotate.o \
	 $(CONDAR)rshape2.o $(CONDAR)rt.o $(CONDAR)savebyloc.o \
	 $(CONDAR)savebynum.o $(CONDAR)sddcopy.o $(CONDAR)select.o \
	 $(CONDAR)setunits.o $(CONDAR)show.o $(CONDAR)slice.o \
	 $(CONDAR)solve.o $(CONDAR)splinecf.o $(CONDAR)summary.o \
	 $(CONDAR)table.o $(CONDAR)tellout.o $(CONDAR)tells.o \
	 $(CONDAR)tipslv.o $(CONDAR)tracer.o \
	 $(CONDAR)vsntip.o $(CONDAR)zerror.o $(CONDAR)zlimit.o \
	 $(CONDAR)xtochan.o $(CONDAR)chantox.o $(CONDAR)fgauss.o \
	 $(CONDAR)mrqmin.o $(CONDAR)virfft.o $(CONDAR)window.o \
	 $(CONDAR)fitfoc.o $(CONDAR)tellstat.o $(CONDAR)gbrecio.o
GENOBJ = $(GENMEMORY)arainit.o $(GENMEMORY)cre8ms.o \
	 $(GENMEMORY)gendummy.o $(GENMEMORY)generrors.o \
	 $(GENMEMORY)genmain.o $(GENMEMORY)genpread.o \
	 $(GENMEMORY)genpwrite.o $(GENMEMORY)genstores.o
#
# Macros which list the object files in the various subdirectories 
#
#----------------------------------------------------------------
FILESH = $(DISK)getascan.o $(DISK)getbs.o \
	 $(DISK)getnindex.o $(DISK)getsddbs.o $(DISK)getslist.o \
	 $(DISK)filefns.o $(DISK)makesindex.o $(DISK)saveascan.o \
	 $(DISK)scanloc.o $(DISK)sddclose.o \
	 $(DISK)sddopen.o $(DISK)getsddscan.o $(DISK)getsddscanlist.o \
	 $(DISK)getnsddindex.o $(TUCANAL)getincbs.o $(TUCANAL)incrindex.o \
	 $(TUCANAL)initfile.o
MODESH = $(CONLINE)modefn.o
SDDH = $(CONDAR)gnextindex.o $(CONDAR)raz.o \
	$(DISK)getbs.o $(DISK)makesindex.o $(DISK)saveascan.o \
	$(LINE)gnextindex.o $(LINE)gbrecio.o $(LINE)raz.o \
	$(DISK)getnindex.o $(DISK)getnsddindex.o $(TUCANAL)getincbs.o \
	$(TUCANAL)incrindex.o $(CONDAR)gbrecio.o
SPHDR = $(LINE)gbrecio.o $(CONDAR)gbrecio.o
TUCACCESSH = $(TUCANAL)access.o $(TUCANAL)getinit.o \
	     $(TUCANAL)olfiles.o $(TUCANAL)verslist.o
AMBIGINC = $(CONLINE)oerror.o $(SOURCE)mhunt.o
APPLINC = $(CONDAR)accums.o $(CONDAR)au1.o $(CONDAR)au2.o \
	  $(CONDAR)au3.o $(CONDAR)au4.o $(CONDAR)au5.o $(CONDAR)au6.o \
	  $(CONDAR)au7.o $(CONDAR)au8.o $(CONDAR)au9.o $(CONDAR)aua.o \
	  $(CONDAR)caub.o $(CONDAR)drawcntr.o $(CONDAR)funct.o \
	  $(CONDAR)funfunct.o $(CONDAR)getcurrent.o $(CONDAR)getscan.o \
	  $(CONDAR)gget.o $(CONDAR)gprep.o $(CONDAR)header.o \
	  $(CONDAR)hlftne.o $(CONDAR)initappl.o \
	  $(CONDAR)mlabel.o $(CONDAR)cpdoc.o $(CONDAR)plot.o \
	  $(CONDAR)pltdots.o $(CONDAR)prep.o $(CONDAR)raster.o \
	  $(CONDAR)rshape2.o $(CONDAR)select.o $(CONDAR)show.o \
	  $(CONDAR)table.o $(CONDAR)tells.o $(CONDAR)tracer.o \
	  $(CONDAR)zerror.o $(CONLINE)bshape.o $(CONLINE)exitpops.o \
	  $(CONLINE)genop.o $(CONLINE)interp.o $(CONLINE)menus.o \
	  $(CONLINE)oerror.o $(CONLINE)pwrite.o $(CONLINE)recover.o \
	  $(CONLINE)setflags.o $(CONLINE)undo.o $(LINE)accums.o \
	  $(LINE)au1.o $(LINE)au2.o $(LINE)au3.o $(LINE)au4.o \
	  $(LINE)au5.o $(LINE)au6.o $(LINE)au7.o $(LINE)au8.o \
	  $(LINE)au9.o $(LINE)aua.o \
	  $(LINE)drawcntr.o $(LINE)funct.o $(LINE)funfunct.o \
	  $(LINE)getcurrent.o $(LINE)getscan.o $(LINE)gget.o \
	  $(LINE)gmeasure.o $(LINE)gprep.o $(LINE)header.o \
	  $(LINE)hlftne.o $(LINE)initappl.o $(LINE)laub.o \
	  $(LINE)mlabel.o $(LINE)lpdoc.o $(LINE)plot.o \
	  $(LINE)pltdots.o $(LINE)prep.o $(LINE)raster.o \
	  $(LINE)rshape2.o $(LINE)select.o $(LINE)show.o \
	  $(LINE)table.o $(LINE)tells.o $(LINE)tracer.o  $(LINE)getir.o \
	  $(LINE)veltochan.o $(LINE)zerror.o $(LINE)virfft.o $(CONDAR)virfft.o \
	  $(CONDAR)fitfoc.o $(LINE)fitfoc.o $(CONLINE)ctrlcintr.o \
          $(CONLINE)str4cur.o $(LINE)getotf.o $(CONLINE)funclev.o \
	  $(CONLINE)color.o $(LINE)getpolz.o
CFORMINC = $(CONDAR)accums.o $(CONDAR)au1.o $(CONDAR)au2.o \
	   $(CONDAR)au3.o $(CONDAR)au4.o $(CONDAR)au6.o $(CONDAR)au7.o \
	   $(CONDAR)au8.o $(CONDAR)au9.o $(CONDAR)aua.o \
	   $(CONDAR)caub.o $(CONDAR)funct.o $(CONDAR)funfunct.o \
	   $(CONDAR)gprep.o $(CONDAR)header.o \
	   $(CONDAR)cpdoc.o $(CONDAR)prep.o $(CONDAR)rshape2.o \
	   $(CONDAR)savebyloc.o $(CONDAR)savebynum.o $(CONDAR)show.o \
	   $(CONDAR)summary.o $(CONDAR)table.o $(CONDAR)tells.o \
	   $(CONLINE)genop.o $(LINE)accums.o $(LINE)au1.o \
	   $(LINE)au2.o $(LINE)au3.o $(LINE)au4.o $(LINE)au6.o \
	   $(LINE)au7.o $(LINE)au8.o $(LINE)au9.o $(LINE)aua.o \
	   $(LINE)chantovel.o $(LINE)funct.o $(LINE)funfunct.o \
	   $(LINE)gmeasure.o $(LINE)gprep.o $(LINE)header.o \
	   $(LINE)laub.o $(LINE)lpdoc.o $(LINE)prep.o \
	   $(LINE)rshape2.o $(LINE)savebyloc.o $(LINE)savebynum.o \
	   $(LINE)show.o $(LINE)summary.o $(LINE)table.o \
	   $(LINE)tells.o $(LINE)veltochan.o \
	   $(CONDAR)xtochan.o $(CONDAR)chantox.o \
	   $(LINE)xtochan.o $(LINE)chantox.o \
	   $(CONDAR)fitfoc.o $(LINE)fitfoc.o $(LINE)getir.o $(LINE)getbyrec.o \
	   $(LINE)getotf.o $(LINE)au5.o $(CONDAR)au5.o $(LINE)getpolz.o
CIOINC = $(CONDAR)au1.o $(CONDAR)au3.o $(CONDAR)au4.o $(CONDAR)au5.o \
	 $(CONDAR)au6.o $(CONDAR)au7.o $(CONDAR)au9.o \
	 $(CONDAR)cheader.o $(CONDAR)chgres.o $(CONDAR)cube.o \
	 $(CONDAR)dft.o $(CONDAR)fft.o $(CONDAR)getcurrent.o \
	 $(CONDAR)getscan.o $(CONDAR)gget.o $(CONDAR)minit.o \
	 $(CONDAR)mread.o $(CONDAR)mwrite.o $(CONDAR)cpdoc.o \
	 $(CONDAR)prep.o $(CONDAR)readcube.o \
	 $(CONDAR)select.o $(CONDAR)show.o $(CONDAR)summary.o \
	 $(CONDAR)tellout.o $(CONDAR)tells.o $(CONDAR)tellstat.o \
	 $(CONLINE)assgn.o $(CONLINE)batch.o $(CONLINE)box.o \
	 $(CONLINE)changeproj.o \
	 $(CONLINE)checkfperror.o $(CONLINE)chngfile.o \
	 $(CONLINE)clrpage.o $(CONLINE)color.o $(CONLINE)compress.o \
	 $(CONLINE)coreclose.o \
	 $(CONLINE)coreclrpage.o $(CONLINE)corecursor.o \
	 $(CONLINE)coreflush.o $(CONLINE)coreint.o \
	 $(CONLINE)coreplace.o $(CONLINE)corevctr.o \
	 $(CONLINE)ctrlcintr.o $(CONLINE)cursor.o $(CONLINE)dither.o \
	 $(CONLINE)donemenu.o $(CONLINE)exitpops.o $(CONLINE)files.o \
	 $(CONLINE)fcioget.o $(CONLINE)filecomp.o \
	 $(CONLINE)gcopy.o $(CONLINE)genop.o $(CONLINE)gpclose.o \
	 $(CONLINE)gpflush.o $(CONLINE)gphint.o $(CONLINE)gtline.o \
	 $(CONLINE)helps.o \
	 $(CONLINE)hphardplace.o \
	 $(CONLINE)hphardtcopy.o $(CONLINE)initcomm.o \
	 $(CONLINE)initfiles.o $(CONLINE)initlock.o \
	 $(CONLINE)initlog.o $(CONLINE)initmess.o \
	 $(CONLINE)initonline.o $(CONLINE)initsetup.o \
	 $(CONLINE)interp.o $(CONLINE)massgn.o $(CONLINE)menus.o \
	 $(CONLINE)nextfield.o $(CONLINE)noghard.o $(CONLINE)oerror.o \
	 $(CONLINE)otherbox.o $(CONLINE)pchar.o $(CONLINE)place.o \
	 $(CONLINE)posthplace.o $(CONLINE)posthtcopy.o \
	 $(CONLINE)posthvctr.o $(CONLINE)pread.o $(CONLINE)pread2.o \
	 $(CONLINE)pwrite.o $(CONLINE)posthcolor.o $(CONLINE)posthbox.o \
	 $(CONLINE)qmshardplace.o \
	 $(CONLINE)qmshardtcopy.o $(CONLINE)qmshardvctr.o \
	 $(CONLINE)quichardplace.o \
	 $(CONLINE)quichardtcopy.o $(CONLINE)quichardvctr.o \
	 $(CONLINE)quick.o $(CONLINE)recover.o $(CONLINE)setflags.o \
	 $(CONLINE)stores.o $(CONLINE)subs.o $(CONLINE)tcopy.o \
	 $(CONLINE)tekclrpage.o $(CONLINE)tekchar.o \
	 $(CONLINE)tekcursor.o \
	 $(CONLINE)tekplace.o $(CONLINE)tekvctr.o $(CONLINE)undo.o \
	 $(CONLINE)updatehist.o $(CONLINE)updateline.o $(CONLINE)vctr.o \
	 $(GENMEMORY)cre8ms.o $(GENMEMORY)genmain.o \
	 $(GENMEMORY)genpread.o $(GENMEMORY)genpwrite.o \
	 $(GENMEMORY)genstores.o $(LINE)au1.o $(LINE)au3.o $(LINE)au4.o \
	 $(LINE)au5.o $(LINE)au6.o $(LINE)au7.o $(LINE)au9.o \
	 $(LINE)cheader.o $(LINE)chgres.o $(LINE)cube.o $(LINE)dft.o \
	 $(LINE)fft.o $(LINE)getcurrent.o $(LINE)getscan.o \
	 $(LINE)gget.o $(LINE)minit.o $(LINE)mread.o $(LINE)mwrite.o \
	 $(LINE)lpdoc.o $(LINE)prep.o $(LINE)readcube.o \
	 $(LINE)select.o $(LINE)show.o $(LINE)summary.o \
	 $(LINE)tellout.o $(LINE)tells.o $(LINE)tellstat.o \
	 $(LINE)getir.o $(LINE)getotf.o $(LINE)getpolz.o \
	 $(SOURCE)amathparser.o \
	 $(SOURCE)closms.o $(SOURCE)editor.o $(SOURCE)getfld.o \
	 $(SOURCE)memusage.o $(SOURCE)oerror2.o $(SOURCE)openms.o \
	 $(SOURCE)pseudo.o $(SOURCE)readms.o $(SOURCE)symbol.o \
	 $(SOURCE)writms.o $(LINE)funct.o $(CONDAR)funct.o $(CONLINE)sread.o \
	 $(CONLINE)history.o $(LINE)chngol.o $(CONDAR)chngol.o \
	 $(LINE)chngver.o $(CONDAR)chngver.o $(CONLINE)execcomp.o
COREINC = $(CONDAR)accums.o $(CONDAR)au1.o $(CONDAR)au2.o \
	  $(CONDAR)au3.o $(CONDAR)au4.o $(CONDAR)au5.o $(CONDAR)au6.o \
	  $(CONDAR)au7.o $(CONDAR)au8.o $(CONDAR)au9.o $(CONDAR)aua.o \
	  $(CONDAR)caub.o $(CONDAR)drawcntr.o $(CONDAR)funct.o \
	  $(CONDAR)funfunct.o $(CONDAR)getscan.o $(CONDAR)gget.o \
	  $(CONDAR)gprep.o $(CONDAR)header.o $(CONDAR)itfits.o \
	  $(CONDAR)mlabel.o $(CONDAR)plot.o \
	  $(CONDAR)prep.o $(CONDAR)rshape2.o $(CONDAR)select.o \
	  $(CONDAR)show.o $(CONDAR)summary.o $(CONDAR)table.o \
	  $(CONDAR)tells.o $(CONDAR)tracer.o $(CONDAR)zerror.o \
	  $(CONLINE)assgn.o $(CONLINE)bshape.o $(CONLINE)compress.o \
	  $(CONLINE)exitpops.o $(CONLINE)genop.o $(CONLINE)gtline.o \
	  $(CONLINE)helps.o $(CONLINE)initcomm.o $(CONLINE)interp.o \
	  $(CONLINE)massgn.o $(CONLINE)menus.o $(CONLINE)oerror.o \
	  $(CONLINE)pread.o $(CONLINE)quick.o $(CONLINE)recover.o \
	  $(CONLINE)runtag.o $(CONLINE)setflags.o $(CONLINE)stores.o \
	  $(CONLINE)subs.o $(CONLINE)undo.o $(GENMEMORY)arainit.o \
	  $(GENMEMORY)genmain.o $(GENMEMORY)genstores.o \
	  $(LINE)accums.o $(LINE)au1.o $(LINE)au2.o $(LINE)au3.o \
	  $(LINE)au4.o $(LINE)au5.o $(LINE)au6.o $(LINE)au7.o \
	  $(LINE)au8.o $(LINE)au9.o $(LINE)aua.o $(LINE)drawcntr.o \
	  $(LINE)funct.o $(LINE)funfunct.o $(LINE)getscan.o \
	  $(LINE)gget.o $(LINE)gmeasure.o $(LINE)gprep.o \
	  $(LINE)header.o $(LINE)itfits.o $(LINE)laub.o \
	  $(LINE)mlabel.o $(LINE)plot.o $(LINE)prep.o $(LINE)rshape2.o \
	  $(LINE)select.o $(LINE)show.o $(LINE)summary.o \
	  $(LINE)table.o $(LINE)tells.o $(LINE)tracer.o $(LINE)zerror.o \
	  $(SOURCE)compil.o $(SOURCE)editor.o $(SOURCE)ltstor.o \
	  $(SOURCE)memusage.o $(SOURCE)pseudo.o $(SOURCE)relink.o \
	  $(SOURCE)symbol.o $(CONLINE)sread.o \
	  $(CONLINE)changeproj.o $(LINE)chngol.o $(CONDAR)chngol.o \
	  $(LINE)chngver.o $(CONDAR)chngver.o  $(LINE)getir.o  \
	  $(CONLINE)str4cur.o $(LINE)getotf.o $(LINE)getpolz.o \
	  $(CONLINE)execcomp.o
DEFAULTINC = $(CONLINE)setdefaults.o
ERRORSINC = $(CONLINE)checkfperror.o $(CONLINE)fphandler.o \
	    $(CONLINE)genop.o $(CONLINE)initfperror.o \
	    $(CONLINE)oerror.o $(CONLINE)quick.o $(CONLINE)setflags.o
FOXPLOTINC = $(CONLINE)charsize.o $(CONLINE)pchar.o $(CONLINE)place.o \
	     $(CONLINE)vctr.o
LSFINC = $(CONLINE)compress.o $(CONLINE)exitpops.o $(CONLINE)recover.o \
	 $(CONLINE)stores.o $(GENMEMORY)genmain.o \
	 $(GENMEMORY)genstores.o $(SOURCE)memusage.o
MAPPLINC = $(CONDAR)au4.o $(CONDAR)au5.o $(CONDAR)cheader.o \
	   $(CONDAR)craz.o $(CONDAR)cube.o $(CONDAR)drawcntr.o \
	   $(CONDAR)funct.o $(CONDAR)funfunct.o $(CONDAR)hlftne.o \
	   $(CONDAR)initappl.o $(CONDAR)mfill.o $(CONDAR)mheader.o \
	   $(CONDAR)minit.o $(CONDAR)mlabel.o $(CONDAR)mraz.o \
	   $(CONDAR)mread.o $(CONDAR)msmooth.o $(CONDAR)msmoth2.o \
	   $(CONDAR)mtable.o $(CONDAR)mwrite.o $(CONDAR)pltdots.o \
	   $(CONDAR)raster.o $(CONDAR)readcube.o $(CONDAR)rotate.o \
	   $(CONDAR)slice.o $(CONDAR)tracer.o $(CONDAR)zerror.o \
	   $(CONDAR)zlimit.o $(CONLINE)genop.o $(CONLINE)ilct.o \
	   $(LINE)au4.o $(LINE)au5.o $(LINE)cheader.o $(LINE)craz.o \
	   $(LINE)cube.o $(LINE)drawcntr.o $(LINE)funct.o \
	   $(LINE)funfunct.o $(LINE)hlftne.o $(LINE)initappl.o \
	   $(LINE)mfill.o $(LINE)mheader.o $(LINE)minit.o \
	   $(LINE)mlabel.o $(LINE)mraz.o $(LINE)mread.o \
	   $(LINE)msmooth.o $(LINE)msmoth2.o $(LINE)mtable.o \
	   $(LINE)mwrite.o $(LINE)pltdots.o $(LINE)raster.o \
	   $(LINE)readcube.o $(LINE)rotate.o $(LINE)slice.o \
	   $(LINE)tracer.o $(LINE)zerror.o $(LINE)zlimit.o $(LINE)virfft.o \
	   $(CONDAR)virfft.o $(LINE)window.o $(CONDAR)window.o \
	   $(CONDAR)fitfoc.o $(LINE)fitfoc.o  $(CONLINE)str4cur.o \
	   $(CONLINE)funclev.o
MESAGEINC = $(CONLINE)oerror.o
MFORMINC = $(CONDAR)au4.o $(CONDAR)au5.o $(CONDAR)cheader.o \
	   $(CONDAR)craz.o $(CONDAR)cube.o $(CONDAR)funct.o \
	   $(CONDAR)funfunct.o $(CONDAR)initappl.o $(CONDAR)mheader.o \
	   $(CONDAR)minit.o $(CONDAR)mlabel.o $(CONDAR)mraz.o \
	   $(CONDAR)mread.o $(CONDAR)mwrite.o $(CONDAR)readcube.o \
	   $(CONDAR)slice.o $(CONLINE)genop.o $(CONLINE)ilct.o \
	   $(LINE)au4.o $(LINE)au5.o $(LINE)cheader.o $(LINE)craz.o \
	   $(LINE)cube.o $(LINE)funct.o $(LINE)funfunct.o \
	   $(LINE)initappl.o $(LINE)mheader.o $(LINE)minit.o \
	   $(LINE)mlabel.o $(LINE)mraz.o $(LINE)mread.o $(LINE)mwrite.o \
	   $(LINE)readcube.o $(LINE)slice.o \
	   $(CONDAR)xtochan.o $(CONDAR)chantox.o \
	   $(LINE)xtochan.o $(LINE)chantox.o LINE$(LINE)virfft.o \
	   $(CONDAR)virfft.o $(LINE)window.o $(CONDAR)window.o \
	   $(CONDAR)fitfoc.o $(LINE)fitfoc.o  $(CONLINE)funclev.o
PARAMSINC = $(CONDAR)accums.o $(CONDAR)au1.o $(CONDAR)au2.o \
	  $(CONDAR)au3.o $(CONDAR)au4.o $(CONDAR)au5.o $(CONDAR)au6.o \
	  $(CONDAR)au7.o $(CONDAR)au8.o $(CONDAR)au9.o $(CONDAR)aua.o \
	  $(CONDAR)caub.o $(CONDAR)drawcntr.o $(CONDAR)funct.o \
	  $(CONDAR)funfunct.o $(CONDAR)getcurrent.o $(CONDAR)getbyloc.o \
	  $(CONDAR)getbynum.o $(CONDAR)getbyrec.o $(CONDAR)getscan.o \
	  $(CONDAR)gget.o $(CONDAR)gprep.o $(CONDAR)header.o \
	  $(CONDAR)hlftne.o $(CONDAR)initappl.o \
	  $(CONDAR)mlabel.o $(CONDAR)cpdoc.o $(CONDAR)plot.o \
	  $(CONDAR)pltdots.o $(CONDAR)prep.o $(CONDAR)raster.o \
	  $(CONDAR)rshape2.o $(CONDAR)select.o $(CONDAR)show.o \
	  $(CONDAR)summary.o \
	  $(CONDAR)table.o $(CONDAR)tells.o $(CONDAR)tracer.o \
	  $(CONDAR)zerror.o $(CONLINE)bshape.o $(CONLINE)exitpops.o \
	  $(CONLINE)genop.o $(CONLINE)interp.o $(CONLINE)menus.o \
	  $(CONLINE)oerror.o $(CONLINE)pwrite.o $(CONLINE)recover.o \
	  $(CONLINE)setflags.o $(CONLINE)undo.o $(LINE)accums.o \
	  $(LINE)au1.o $(LINE)au2.o $(LINE)au3.o $(LINE)au4.o \
	  $(LINE)au5.o $(LINE)au6.o $(LINE)au7.o $(LINE)au8.o \
	  $(LINE)au9.o $(LINE)aua.o \
	  $(LINE)drawcntr.o $(LINE)funct.o $(LINE)funfunct.o \
	  $(LINE)getbyloc.o $(LINE)getbynum.o $(LINE)getbyrec.o \
	  $(LINE)getcurrent.o $(LINE)getscan.o $(LINE)gget.o \
	  $(LINE)gmeasure.o $(LINE)gprep.o $(LINE)header.o \
	  $(LINE)hlftne.o $(LINE)initappl.o $(LINE)laub.o \
	  $(LINE)mlabel.o $(LINE)lpdoc.o $(LINE)plot.o \
	  $(LINE)pltdots.o $(LINE)prep.o $(LINE)raster.o \
	  $(LINE)rshape2.o $(LINE)select.o $(LINE)show.o $(LINE)summary.o \
	  $(LINE)table.o $(LINE)tells.o $(LINE)tracer.o  $(LINE)getir.o \
	  $(LINE)veltochan.o $(LINE)zerror.o $(LINE)virfft.o $(CONDAR)virfft.o \
	  $(CONDAR)fitfoc.o $(LINE)fitfoc.o $(CONLINE)ctrlcintr.o \
          $(CONLINE)str4cur.o $(LINE)getotf.o $(CONLINE)funclev.o \
	  $(CONLINE)color.o $(LINE)getpolz.o $(CONLINE)genop.f \
	  $(CONDAR)chgres.o $(LINE)chgres.o $(CONDAR)conversion2.o \
	  $(CONDAR)conversion3.o $(CONDAR)dft.o $(CONDAR)fft.o \
	  $(LINE)conversion2.o $(LINE)conversion3.o $(LINE)dft.o \
	  $(LINE)fft.o
RIPINC = $(LINE)au6.o $(LINE)rshape2.o $(CONDAR)rshape2.o \
	 $(LINE)fgauss.o $(CONDAR)au6.o $(CONDAR)fgauss.o 
SMSTUFINC = $(CONLINE)assgn.o $(CONLINE)gtline.o $(CONLINE)helps.o \
	    $(CONLINE)initcomm.o $(CONLINE)interp.o $(CONLINE)massgn.o \
	    $(CONLINE)nextfield.o $(CONLINE)oerror.o $(CONLINE)quick.o \
	    $(CONLINE)stores.o $(CONLINE)subs.o $(GENMEMORY)generrors.o \
	    $(GENMEMORY)genmain.o $(GENMEMORY)genstores.o \
	    $(SOURCE)amathparser.o $(SOURCE)bclean.o $(SOURCE)compil.o \
	    $(SOURCE)editor.o $(SOURCE)getfld.o $(SOURCE)ltstor.o \
	    $(SOURCE)polish.o $(SOURCE)pseudo.o $(SOURCE)symbol.o  \
	    $(CONLINE)execpol.o $(CONLINE)execcomp.o
STKINC = $(CONDAR)au1.o $(CONDAR)au2.o $(CONDAR)au4.o $(CONDAR)au5.o \
	 $(CONDAR)au8.o $(CONDAR)aua.o $(CONDAR)caub.o $(CONDAR)funct.o \
	 $(CONDAR)funfunct.o $(CONDAR)select.o $(CONDAR)show.o \
	 $(CONDAR)summary.o $(CONDAR)tells.o $(CONLINE)assgn.o \
	 $(CONLINE)compress.o $(CONLINE)genop.o $(CONLINE)gtline.o \
	 $(CONLINE)helps.o $(CONLINE)initcomm.o $(CONLINE)interp.o \
	 $(CONLINE)massgn.o $(CONLINE)oerror.o $(CONLINE)quick.o \
	 $(CONLINE)setflags.o $(CONLINE)stores.o $(CONLINE)subs.o \
	 $(GENMEMORY)genmain.o $(GENMEMORY)genstores.o $(LINE)au1.o \
	 $(LINE)au2.o $(LINE)au4.o $(LINE)au5.o $(LINE)au8.o \
	 $(LINE)aua.o $(LINE)funct.o $(LINE)funfunct.o $(LINE)laub.o \
	 $(LINE)select.o $(LINE)show.o $(LINE)summary.o $(LINE)tells.o \
	 $(SOURCE)bclean.o $(SOURCE)compil.o $(SOURCE)editor.o \
	 $(SOURCE)getfld.o $(SOURCE)ltstor.o $(SOURCE)polish.o \
	 $(SOURCE)pseudo.o $(SOURCE)symbol.o $(CONLINE)sread.o \
	 $(CONLINE)pread.o $(CONLINE)execpol.o $(CONLINE)execcomp.o 
TAGSINC = $(CONLINE)helps.o $(CONLINE)interp.o $(CONLINE)quick.o \
	  $(CONLINE)stores.o $(CONLINE)unused.o $(CONLINE)verbs.o \
	  $(SOURCE)compil.o $(SOURCE)editor.o $(SOURCE)finish.o \
	  $(SOURCE)polish.o $(SOURCE)pseudo.o $(SOURCE)relink.o \
	  $(CONLINE)oerror.o $(CONLINE)execpol.o

# Macros which list the 'Include' file dependencies
#
#---------------------------------------------------------------
BINLIST = genmemory.exe defaults.exe rundaemon
GBBINLIST = gbline.rmt.exe gbcondar.rmt.exe
GBOLLIST = gbline.exe gbcondar.exe 
TUCBINLIST = tucline.exe tuccondar.exe
CVBINLIST = cvline.exe cvcondar.exe
# Macro which lists executables 
#
POPSDATLIST = LMEMORY CMEMORY
#
#---------------------------------------------------------------
EXECS = checkup.exe clearlock.exe comments.exe gbline gbcondar \
	edit.exe explain2.exe help.exe identify.exe \
	ls.exe makefile.exe makegraph.exe makemenu.exe man.exe \
	mess.exe messages.exe more.exe news.exe prcdir.exe \
	prcdir2.exe print.exe edit2.exe hpprint quicprint postprint \
	qmsprint nogprint makegraph2.exe killgraph.exe tucline \
	tuccondar cvline cvcondar line.exe line.rmt.exe condar.exe \
	condar.rmt.exe mkfiles.unipops filecomp.exe infantcide printit \
	isSolaris
PROCS = getbits.prc testarray.prc testerr.prc testif.prc testrtrn.prc \
	testwhile.prc
OTHERS = hpstart hpterm poststart postterm qmsstart qmsterm quicstart \
	 quicterm explain.icon menu.icon comments.icon help.icon \
	 ihstorgb twodim.mask prcdir.icon ls.icon man.icon more.icon \
	 defaults.f  Makeman tuc.nrao.edu.mkfiles.unipops \
	 kp.nrao.edu.mkfiles.unipops gb.nrao.mkfiles.unipops \
	 cv.mkfiles.unipops print.awk printers.example
#
# Lists of things whicj occasionally should be SCCS gotten
#---------------------------------------------------------------
INITCIO = $(INCLUDES)appl.inc $(INCLUDES)cio.inc $(INCLUDES)core.inc \
	  $(INCLUDES)lsf.inc $(CONLINE)initcio.F $(INCLUDES)params.inc \
	  POPSDAT.GEN POPSDAT.ADVERBS  POPSDAT.MAIN  POPSDAT.VERBS  \
	  POPSDAT.FFUNC
# initcio dependencies for a new version number for program
#---------------------------------------------------------------
#
# End of Macros
#
# Target and dependencies:
#
# General targets first
#

all:  most $(GBBINLIST) $(TUCBINLIST) $(CVBINLIST)

most: MISCELANEOUS EXECUTABLES $(POPSDATLIST) $(BINLIST)

tucall: most $(TUCBINLIST)

gball: most $(GBBINLIST)

gbonline: GBOLMISCELANEOUS $(GBOLLIST)

cvall: most $(CVBINLIST)

clean:
	-rm -f $(SOURCEOBJ)
	-rm -f $(CNLOBJ)
	-rm -f $(DISKOBJ)
	-rm -f $(LINEOBJ)
	-rm -f $(CONOBJ)
	-rm -f $(GENOBJ)
	-rm -f $(POPSDATLIST)
	-rm -f $(BINLIST)
	@if [ $(USESUNVIEW) = TRUE ]; \
	 then (cd $(popsdir)sunview; make clean)\
	 fi
	@if [ $(USEX) = TRUE ]; \
	 then (cd $(popsdir)xwindows; make clean)\
	 fi
	(cd $(popsdir)explain/source; make clean)
	(cd $(popsdir)source/conline/gb; make clean)
	(cd $(popsdir)source/conline/tuc; make clean)
	(cd $(popsdir)source/conline/cv; rm -f *.o)
	(cd $(popsdir)utilities; make clean)
	(cd $(popsdir)fonts; make clean)
	-sccs clean
	(cd $(popsdir)procedures; make clean) 
	-rm -f graphics.exe popstool.exe
	(cd $(popsdir)sys.unipops; make clean)
	-(cd $(INCLUDES); sccs clean; sccs get SCCS/s.*)
	(cd $(SOURCE); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)
	(cd $(CONLINE); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)
	(cd $(GENMEMORY); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)
	(cd $(LINE); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)
	(cd $(CONDAR); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)

links:
	-(cd $(INCLUDES); sccs clean; sccs get SCCS/s.*)
	(cd $(SOURCE); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)
	(cd $(CONLINE); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)
	(cd $(GENMEMORY); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)
	(cd $(LINE); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)
	(cd $(CONDAR); rm -f *.inc *.h; ln -s $(INCLUDES)*.[i]* .)

oclean:
	-rm -f $(SOURCEOBJ)
	-rm -f $(CNLOBJ)
	-rm -f $(DISKOBJ)
	-rm -f $(LINEOBJ)
	-rm -f $(CONOBJ)
	-rm -f $(GENOBJ)
 
# Executable targets next
#

gbline.exe: $(SOURCEOBJ) $(CNLOBJ) $(LINEOBJ) $(DISKOBJ) $(ANALOBJO) 
	@echo "Linking gbline.exe.  Version:" $(VERSION)
	@$(FC) $(LFLAGS) -o gbline.exe $(ANALOBJO) $(SOURCEOBJ) $(CNLOBJ) \
	      $(LINEOBJ) $(DISKOBJ) $(LLIBS) 

tucline.exe: $(SOURCEOBJ) $(CNLOBJ) $(LINEOBJ) $(DISKOBJ) $(TUCANALOBJ)
	@echo "Linking tucline.exe.  Version:" $(VERSION)
	@$(FC) $(LFLAGS) -o tucline.exe $(SOURCEOBJ) $(CNLOBJ) \
	      $(LINEOBJ) $(DISKOBJ) $(TUCANALOBJ) $(LLIBS)

cvline.exe: $(SOURCEOBJ) $(CNLOBJ) $(LINEOBJ) $(DISKOBJ) $(CVANALOBJ)
	@echo "Linking cvline.exe.  Version:" $(VERSION)
	@$(FC) $(LFLAGS) -o cvline.exe $(SOURCEOBJ) $(CNLOBJ) \
	      $(LINEOBJ) $(DISKOBJ) $(CVANALOBJ) $(LLIBS)

gbline.rmt.exe: $(SOURCEOBJ) $(CNLOBJ) $(LINEOBJ) $(DISKOBJ) $(ANALOBJR)
	@echo "Linking gbline.rmt.exe.  Version:" $(VERSION)
	@$(FC) $(LFLAGS) -o gbline.rmt.exe $(ANALOBJR) $(SOURCEOBJ) $(CNLOBJ) \
		$(LINEOBJ) $(DISKOBJ) $(LLIBS)

gbcondar.exe: $(SOURCEOBJ) $(CNLOBJ) $(CONOBJ) $(DISKOBJ) $(ANALOBJO) 
	@echo "Linking gbcondar.exe.  Version:" $(VERSION)
	@$(FC) $(LFLAGS) -o gbcondar.exe $(ANALOBJO) $(SOURCEOBJ) $(CNLOBJ) \
	      $(CONOBJ) $(DISKOBJ) $(LLIBS)

gbcondar.rmt.exe: $(SOURCEOBJ) $(CNLOBJ) $(DISKOBJ) $(CONOBJ) $(ANALOBJR)
	@echo "Linking gbcondar.rmt.exe.  Version:" $(VERSION)
	@$(FC) $(LFLAGS) -o gbcondar.rmt.exe $(ANALOBJR) $(SOURCEOBJ) \
	       $(CNLOBJ) $(CONOBJ) $(DISKOBJ) $(LLIBS)

tuccondar.exe: $(SOURCEOBJ) $(CNLOBJ) $(CONOBJ) $(DISKOBJ) $(TUCANALOBJ)
	@echo "Linking tuccondar.exe.  Version:" $(VERSION)
	@$(FC) $(LFLAGS) -o tuccondar.exe $(TUCANALOBJ) $(SOURCEOBJ) $(CNLOBJ) \
	      $(CONOBJ) $(DISKOBJ) $(LLIBS)

cvcondar.exe: $(SOURCEOBJ) $(CNLOBJ) $(CONOBJ) $(DISKOBJ) $(CVANALOBJ)
	@echo "Linking cvcondar.exe.  Version:" $(VERSION)
	@$(FC) $(LFLAGS) -o cvcondar.exe $(CVANALOBJ) $(SOURCEOBJ) $(CNLOBJ) \
	      $(CONOBJ) $(DISKOBJ) $(LLIBS)

genmemory.exe: $(SOURCEOBJ) $(GENOBJ)
	@echo "Linking genmemory.exe"
	@$(FC) $(LFLAGS) -o genmemory.exe $(SOURCEOBJ) $(GENOBJ) $(LLIBS)

comments.exe: comments.exe.base
	@sed s/LOCALMAILLIST/"$(LOCALMAILLIST)"/ < comments.exe.base > comments.exe
	@chmod +x comments.exe

defaults.exe: defaults.f
	@$(FC) $(FFLAGS) defaults.f -o defaults.exe

rundaemon: rundaemon.c
	@$(CC) $(CFLAGS) rundaemon.c -o rundaemon

line.rmt.exe: line.exe
	rm -f line.rmt.exe
	ln -s line.exe line.rmt.exe

condar.rmt.exe: condar.exe
	rm -f condar.rmt.exe
	ln -s condar.exe condar.rmt.exe

# End of executable target/dependencies
#
# Start of Memory file dependencies
#

$(CONLINE)initcio.o LMEMORY CMEMORY: genmemory.exe $(INITCIO)
	$(CPP) -P -C -DVERSION=$(VERSION) POPSDAT.MAIN LPOPSDAT;
	rm -f LMEMORY
	genmemory.exe L
	rm -f CMEMORY
	$(CPP) -P -C -DVERSION=$(VERSION) -DCONTINUUM POPSDAT.MAIN CPOPSDAT;\
	genmemory.exe C;\
	cd $(CONLINE); $(CPP) -DVERSION=$(VERSION) initcio.F initcio.f
	cd $(CONLINE); $(FCMP) -c $(FFLAGS) initcio.f
	@rm -f $(CONLINE)initcio.f

# End of Memory file dependencies
#
# Start of Miscelaneous routines

GBOLMISCELANEOUS: $(ANALOBJO)
	(cd $(popsdir)utilities; make CCMP="$(CCMP)" \
		FCMP="$(FCMP)" FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" \
		MAC2="$(MAC2)" gbonline)

MISCELANEOUS:
	@if [ $(USESUNVIEW) = TRUE ]; \
	then \
	(cd $(popsdir)sunview; \
		make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" FLLIBS="$(FLLIBS)" \
		   LIBCORE="$(LIBCORE)" CCMP="$(CCMP)" FCMP="$(FCMP)" install) \
	fi
	@if [ $(USEX) = TRUE ]; \
	then \
	(cd $(popsdir)xwindows; \
		make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" FLLIBS="$(FLLIBS)" \
		   XLIBS="$(XLIBS)" XINCS="$(XINCS)"  CCMP="$(CCMP)" \
		   FCMP="$(FCMP)" install) \
	fi
	(cd $(popsdir)utilities; \
		make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" LLIBS="$(LLIBS)" \
		   CCMP="$(CCMP)" FCMP="$(FCMP)" AR="$(AR)" all)
	(cd $(popsdir)fonts; make all)
	(cd $(popsdir)explain/source; \
		make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" LLIBS="$(LLIBS)" \
		   CCMP="$(CCMP)" FCMP="$(FCMP)" AR="$(AR)" all)

# End of Miscelaneous routines
#
EXECUTABLES: $(EXECS) $(PROCS) $(OTHERS)
	(cd $(popsdir)sys.unipops; make all)
	(cd $(popsdir)procedures; make all)

# End of Execuatable updating routines
#
# Start of include file targets
#
$(AMBIGINC): $(INCLUDES)ambig.inc

$(APPLINC): $(INCLUDES)appl.inc

$(CFORMINC): $(INCLUDES)cform.inc

$(CIOINC): $(INCLUDES)cio.inc

$(COREINC): $(INCLUDES)core.inc

$(ERRORSINC): $(INCLUDES)errors.inc

$(FOXPLOTINC): $(INCLUDES)foxplot.inc

$(LSFINC): $(INCLUDES)lsf.inc

$(MAPPLINC): $(INCLUDES)mappl.inc

$(MESAGEINC): $(INCLUDES)mesage.inc

$(MFORMINC): $(INCLUDES)mform.inc

$(PARAMSINC): $(INCLUDES)params.inc

$(RIPINC): $(INCLUDES)rip.inc

$(SMSTUFINC): $(INCLUDES)smstuf.inc

$(STKINC): $(INCLUDES)stk.inc

$(TAGSINC): $(INCLUDES)tags.inc

$(I140H): $(INCLUDES)140.h

$(FILESH): $(INCLUDES)files.h

$(MODESH): $(INCLUDES)modes.h

$(SDDH): $(INCLUDES)sdd.h

$(SPHDR): $(INCLUDES)sp_hdr.h

$(TUCACCESSH): $(TUCANINCS)tucaccess.h

$(PDFLH): $(TUCANINCS)pdfl.h

$(DEFAULTINC): $(INCLUDES)defaults.inc

# End of include file targets
#
# Start of Peculiar targets

$(CONOBJ): 
	@if [ `echo $(<F) | awk -F\. '{print $$NF}'` = f ]; \
	then cd $(<D); $(FCMP) $(FFLAGS) -c -DCONTINUUM $(<F); \
	elif [ `echo $(<F) | awk -F\. '{print $$NF}'` = F ]; \
	then cd $(<D); $(FCMP) $(FFLAGS) -c -DCONTINUUM $(<F); \
	else cd $(<D); $(CCMP) $(CFLAGS) -c -DCONTINUUM $(<F); \
	fi

$(CONLINE)sread.o: $(CONLINE)srw.F $(INCLUDES)core.inc $(INCLUDES)stk.inc 
	(cd $(CONLINE); $(CPP) -P -DSREAD srw.F >! sread.f) 
	(cd $(CONLINE); $(CPP) -P -DSWRITE srw.F >> sread.f) 
	(cd $(CONLINE); $(CPP) -P -DFREAD srw.F >> sread.f) 
	(cd $(CONLINE); $(CPP) -P -DFWRITE srw.F >> sread.f) 
	(cd $(CONLINE); $(FCMP) -c $(FFLAGS) sread.f)

$(CONLINE)cleanup.o: $(CONLINE)cleanup.c
	(cd $(CONLINE); \
	 $(CCMP) -c $(CDEBOROPT) $(FLOATOPT) $(USEHACK) cleanup.c)

$(INCLUDES)defaults.inc: defaults.exe $(INCLUDES)core.inc POPSDAT.ADVERBS
	rm -f $(INCLUDES)defaults.inc
	defaults.exe < $(INCLUDES)core.inc > $(INCLUDES)defaults.inc
	(cd $(CONLINE); rm -f defaults.inc; ln -s $(INCLUDES)defaults.inc .)

$(ANALOBJO):
	(cd $(MAC2); make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" \
	CCMP="$(CCMP)" FCMP="$(FCMP)" LLIBS="$(LLIBS)" gbonline)

$(ANALOBJR): $(GBANINCS)access.h
	(cd $(ANAL); make FFLOAT="$(FFLOAT)" CFLOAT="$(CFLOAT)" LLIBS="$(LLIBS)" \
	   FCMP="$(FCMP)" CCMP="$(CCMP)" all)

#
# End of Peculiar targets
#
