      program tele2ieee
c
c     @(#)tele2ieee.f	5.1 06/22/94
c
c     Reads in a record presumably from a 140-ft telescope tape and outputs
c     a record in IEEE KEEP format.  Uses Standard Input and Standard
c     Output so that the user can implement pipes to do further conversions.
c
c     NOTE:  WORKS only for telescope tapes made AFTER JUNE 1989 when all 
c     continuum scans were made to have the same record size.
c
c     For Spectral line records, records within a scan are averaged using
c     the appropriate INT/Tsys**2 weighing so one output record is produced for
c     every scan regardless of number of input records in scan.
c
c     For continuum records, data for scans which exceed one record are tacked 
c     together to the maximum allowed by the output record size (1024 data 
c     points).  One output record is generated for each feed so, in principle, 
c     one input record could produce up to 18 output records (the maximum # of
c     feeds allowed plus two for positions), or many input records could produce 
c     only one output record.
c
c     The user can specify whether continuum or line records are to be ignored
c     by specifying a 'c' (for continuum only), an 'l' (for line only), or
c     nothing (for both line and continuum) on the command line.
c
c     NOTE: the user should redirect both input and output when using this 
c     program. Program uses standard error to report on its progress
c
      character*1 ltype(23), ctype(17), arg
      integer*4 lntype(23), cntype(17), iargc, icnum, ilnum, ierr, 
     .		inbytes, long	
      logical average, rearrange, cont, line, lnew, posit, first,
     .	      isitline, isitcont
      integer*2 nin, i, itype, nextbyte, nfeednew, nfeedold, li
c
      include 'cvttele.inc'
c
      isitline(li) =  (li.eq.1 .or. li.eq.2 .or. li.eq.5 .or. li.eq.6)
      isitcont(li) =  (li.eq.3 .or. li.eq.4 .or. li.eq.8)
c
      data ltype/'r','c','i','c','b','d','i','r','i','r','i',
     1           'r','i','r','d','c','d','r','i','b','i','b','r'/
      data lntype/2 , 12, 1 , 18, 2 , 1 , 4 , 2 , 20, 23, 2 ,
     1            1 , 4 , 73, 8 , 72, 6 , 4 , 22,392, 90, 20, 0 /
c
      data ctype/'r','i','c','i','c','b','d','i','r','i','r','i','r',
     1           'i','r','b','i'/
      data cntype/1 , 2 , 12, 1 , 18, 2 , 1 , 4 , 2 , 22, 22, 2 , 1 , 
     1            4 , 46,372,2016/
c
      data icnum/17/, ilnum/23/
c
c     Defines the format of the spectral line and continuum record read in.  
c     *TYPE defines the order of word types (d = d.prec, r = real, i = int*2, 
c     c = char*1, b = byte to bypass in input record, s = byte to skip in output
c     record) and *NTYPE the number of succesive words of the corresponding 
c     type (e.g. 2 reals, followed by 12 C*1, 1 Integer, 18 C*1, skip two 
c     ouput bytes, 1 double precision ...).
c
c     *CNUM and *LNUM give number of entries in the continuum and line versions
c     of *type and *ntype
c
      lnew = .true.
      first = .true.
      nin = 0
c     NIN = number of records read in so far
c     NOUT = number of records read in so far in the present scan
c     LNEW = true if the record just read in is for a new scan else it is
c     	     false.
c     FIRST = TRUE if this is the first scan successfully read in 
c
      cont = .true.
      line = .true.     
      posit = .false.
c
      do 9 i = 1, min(2,iargc())
	call getarg(long(i), arg)
	if (arg .eq. 'c') then
	  line = .false.
	else if (arg .eq. 'l') then
	  cont = .false.
	else if (arg .eq. 'p') then
	  posit = .true.
	endif
9       continue
c     Reads command line and determines whether to eliminate either
c     spectral line or continuum scans
c	
99    continue
c
	call readbuff(ierr, long(ktosin*2), cinbuff)
	if (ierr .eq. 0) then
	  goto 999
	else if (ierr .ne. ktosin*2) then
	  write(0,*) "TELE2IEEE: Error on read"
	  goto 999
	endif
c	Read in first KTOSIN*2 bytes (or KTOSIN I*2 words).  Must stop reading
c	in order to determine what kind of record (cont or line) and then how 
c	many more bytes are to be read.
c
	itype = iinbuff(ktosin)
	nextbyte = ktosin*2 + 1
	if (isitcont(itype)) then
	   inbytes = cinbuffsize - ktosin*2
	else if (isitline(itype)) then
	   inbytes = linbuffsize - ktosin*2
	   if (itype .eq. 1) then
		lntype(ilnum) = 2072
	   else
		lntype(ilnum) = 1048
	   endif
c	   Modify the last entry in LNTYPE depending upon whether the scan
c	   is Switched or Total power.  No need to convert the last 1024
c	   data points for a T-power scan.
c
	else
	   write(0,*) "TELE2IEEE: Bad scan type:", itype
	   goto 999
	endif
c	ITYPE = a flag telling whether this is a continuum or spectral line
c	        scan (1 = SPOWER, 2 = TPOWER, 3 = ONOFF, 4 = CONT, 8 = DCR)
c		NOTE:  We are relying on the fact that the integer*2 
c		representations in MODCOMP and in the host machines are 
c		identical.  More work would be needed if they weren't.
c	NEXTBYTE = number of byte which should be next read in.
c	INBYTES = number of bytes still to read to get to end of record.
c	
	call readbuff(ierr, inbytes, cinbuff(nextbyte:) )
	if (ierr .ne. inbytes) then
	   write(0,*) "TELE2IEEE: Error on read"
	   goto 999
	endif
c
	nin = nin + 1
c	Finish reading record
c
	if ( (line .and. isitline(itype)) .or.
     .       (cont .and. isitcont(itype)) ) then
c
	   if (isitline(itype)) then
	      call cvtmodieee(cinbuff,ccvtbuff,ltype,lntype,ilnum)
	      nfeednew = 1
	   else 
	      call cvtmodieee(cinbuff,ccvtbuff,ctype,cntype,icnum)
	      nfeednew = min(icvtbuff(klr),16)
	      if (posit) nfeednew = nfeednew + 2
	   endif
c	   Converts from input binary to the binary format of the host 
c	   machine.  Finds the number of output records possible (1 for
c	   line scans, up to 16 for cont scans; if positions are desired,
c	   then 18 feeds are allowed (two for x and y pos.) )
c
	   if (.not.lnew .and. 
     1	      int(rcvtbuff(lsno)).ne.int(routbuff(lsno,1)) ) then
		do 101 i = 1, nfeedold
			call writebuff(ierr, long(outbuffsize), 
     .					           coutbuff(i))
	   		if (ierr .ne. outbuffsize) then
				write(0,*) "TELE2IEEE: Error on write"
				goto 999
	  		endif
101			continue
c	    	Writes out already processed scan if a new scan has been
c	    	read in. Uses change in scan number to trigger a new scan.
c
		lnew = .true.
		nfeedold = nfeednew
c  	        Reset new scan flag (LNEW) and feed number for next record/scan.
c
	    endif
c
            if (isitline(itype)) then
c
	       if (average(lnew) ) then
		lnew = .false.
		if(first) then
		   nfeedold = nfeednew
		   first = .false.
		endif
	       endif
c	       Averages the record in cvtbuff into outbuff 
c
	    else
c
	       if (rearrange(lnew, posit) ) then
		lnew = .false.
		if(first) then
		   nfeedold = nfeednew
		   first = .false.
		endif
	       endif
c	       Adds Subscans together from cvtbuff to outbuff
c
	    endif
c
        endif
c
	goto 99
c
999   if (.not.lnew) then
	do 102 i = 1, nfeedold	
		call writebuff(ierr, long(outbuffsize), coutbuff(i))
	        if (ierr .ne. outbuffsize) then
		     write(0,*) "TELE2IEEE: Error on write"
		     stop
	        endif
102		continue
      endif
c     Writes the last scan out
c
      stop
      end
c
