      program ieee2sdd
c
c     @(#)ieee2sdd.f	5.2 09/10/98
c
c     Reads in a record from presumably a IEEE keep file and outputs a 
c     record in SDD#1 format.  Uses Standard Input and Standard
c     Output so that the user can implement pipes to do further conversions.
c
c     NOTE: the user should redirect both input and output when using this 
c     program. Program uses standard error to report on its progress
c
      include 'params.inc'
c
      character*5120 cinbuff
      character*(HDU_FLOAT_SIZE*4) coutbuff
      integer*4 ibuffsize, ierr, ioutsize1, ioutsize2, ifeed, iargc,
     .		long, nskipped, ifd
      character*1 arg
      integer*2 iinbuff(2560), kmd, numfeed, itype, ktos, ksno, li
      real rinbuff(1280)
      real*8 dinbuff(640), doutbuff(HDU_FLOAT_SIZE/2)
      logical cont, line, isitline, isitcont
c
      equivalence (dinbuff, cinbuff), (iinbuff, cinbuff)
      equivalence (rinbuff, cinbuff)
      equivalence (doutbuff, coutbuff)
c
      isitline(li) =  (li.eq.1 .or. li.eq.2 .or. li.eq.5 .or. li.eq.6)
      isitcont(li) =  (li.eq.3 .or. li.eq.4 .or. li.eq.8)
c
      data ibuffsize/5120/, kmd/36/, ktos/34/, ksno/1/
c     IBUFFSIZE = Size of input/output buffers
c     KMD = Location in input format where number of feeds is stored.
c     KTOS = Location of obs. type.
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
      nskipped = 0
100   continue
c
	call readbuff(ierr, ibuffsize, cinbuff)
	if (ierr .eq. 0) then
	  goto 999
	else if (ierr .ne. ibuffsize) then
	  write(0,*) "IEEE2SDD: Error on read"
	  goto 999
	endif
c	Read in a record
c
        itype = iinbuff(ktos)
	if (.not. isitcont(itype) .and. .not. isitline(itype)) then
	   write(0,*) "IEEE2SDD: Bad scan type:", itype
	   goto 999
	endif
c	ITYPE = flag telling whether this is a continuum or spectral line
c		scan (1 = SPOWER, 2 = TPOWER, 3 = ONOFF, 4 = CONT, 8 = DCR)
c
	if ( (line .and. isitline(itype)) .or.
     .       (cont .and. isitcont(itype)) ) then
c
	  if (isitline(itype)) then
	    numfeed = iinbuff(kmd)
	  else
	    numfeed = 1
	  endif
c
	  do 120 ifeed = 1, numfeed
	    call unpadraz(doutbuff)
c
	    if (isitline(itype)) then
	       ifd = ifeed
	       if ( nint( 100*mod(rinbuff(ksno),1.) ) .ne. 0) 
     .		   rinbuff(ksno) = rinbuff(ksno) - float(ifd)/100.
	       call lconversion(dinbuff, doutbuff(1), ifd, ierr)
	    else
	       ifd = nint( 100.*mod(rinbuff(ksno),1.) )
	       rinbuff(ksno) = rinbuff(ksno) - float(ifd)/100.
	       if ( ifd .eq. 0) ifd = 1
	       call cconversion(dinbuff, doutbuff(1), ifd, ierr)
	    endif
c			on error, write message to stderr (unit 0) and go on
            if (ierr .ne. 0) then
	       write(0, 1000) rinbuff(ksno), ifd, ierr
 1000          format('IEEE2SDD: conversion of scan ',f9.2,', feed ',i2,
     .               ' failed with error, ', i2,'.')
               goto 999
            endif
c
c	    Converts from input IEEE binary to SDD#1 format. 
c
	    ioutsize1 = doutbuff(5) + doutbuff(6) + 4
	    ioutsize2 = ioutsize1 / 512
	    if (mod(ioutsize1,512) .ne. 0) ioutsize2 = ioutsize2 + 1
c	    Must write out a multiple of 512 bytes
c
	    call writebuff(ierr, ioutsize2*512, coutbuff)
	    if (ierr .ne. ioutsize2*512) then
		write(0,*) "IEEE2SDD: Error on write"
		goto 999
	    endif
c	    Writes out converted scan, one feed at a time.
c
120	    continue
c
	endif
c
      goto 100
c
999   stop
c
      end
c
