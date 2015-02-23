      program sdd2ieee
c
c     @(#)sdd2ieee.f	5.1 06/22/94
c
c     Reads in the data part of a SDD SAVE/KEEP/DATA file and converts the
c     data to IEEE KEEP format.
c
      character*512 cindat(100), garbage, cpaddat(100)
      character*5120 cout
      character*1 arg
      real rout(1280)
      integer*2 iindat(256,100), numclass, maxsize, iout(2560), i,
     .          ier2
      integer*4 insize, outsize, ierr, numbytes, numtoread, i1, lorc,
     .		long, iargc
      logical cont, line
      real*8 dout(640), dindat(64,100)
c
      equivalence (cindat, iindat, dindat), (cout, dout, iout, rout)
c
      data insize/512/, outsize/5120/, maxsize/100/, i1/1/
c
      cont = .true.
      line = .true.     
      if (iargc() .gt. 0) then
	call getarg(long(1), arg)
	if (arg .eq. 'c') then
	  line = .false.
	else if (arg .eq. 'l') then
	  cont = .false.
	endif
      endif
c     Reads command line and determines whether to eliminate either
c     spectral line or continuum scans
c	
100	call readbuff(ierr, insize, cindat(1))
	if (ierr .eq. 0) then
	  goto 999
	else if (ierr .ne. insize) then
	  write(0,*) "SDD2IEEE: Error on read"
	  goto 999
	endif
c       Read in the first possible data record of the scan
c 
	numclass = iindat(1,1)
        if (numclass .le. 0 .or. numclass .gt. 14) goto 100
c
        numbytes = dindat(5,1)+dindat(6,1)+4
	if (mod(numbytes, insize) .ne. 0) then
	   numtoread = numbytes/insize + 1
	else
	   numtoread = numbytes/insize
	endif
c
        do 50 i = 2, numtoread
	   if (i .le. maxsize) then
		call readbuff(ierr, insize, cindat(i))
	   else
		call readbuff(ierr, insize, garbage)
	   endif
	   if (ierr .ne. insize) then
		write(0,*) "SDD2IEEE: Error on read"
		goto 999
	   endif
50	   continue
c       Read in the rest of the scan and store as much as can be stored in indat
c
	do 60 i = 1, outsize/2
	   iout(i) = 0
60	   continue
c       Zero out the output buffer
c
        call conversion2(cindat, cpaddat, ier2)
        if (ier2 .ne. 0) then
           write(0, *) 'SDD2IEEE: Internal conversion of scan failed ',
     .                 'with error', ierr
           goto 999
        endif
        call lineorcont(cpaddat,lorc)
c	Is this a line or continuum scan?  LORC = 0 for CONT, 1 for LINE, 
c	-1 for unknown.
c
	if (lorc .eq. 1 .and. line) then
	   call lcvtsddieee(cpaddat, dout, i1, ierr)
	else if (lorc .eq. 0 .and. cont) then
	   call ccvtsddieee(cpaddat, dout, i1, ierr)
	else
	   write(0,*) "SDD2IEEE: Bad type of scan"
	   goto 999
	endif
        if (ierr .ne. 0) then
	    write(0, *) 'SDD2IEEE: conversion of scan failed with error',
     .			    ierr
            goto 999
        endif
c	Convert from SDD to IEEE
c
	call writebuff(ierr, outsize, cout)
	if (ierr .ne. outsize) then
	   write(0,*) "SDD2IEEE: Error on write"
	   goto 999
	endif
c
      goto 100
c
999   stop
      end
c
