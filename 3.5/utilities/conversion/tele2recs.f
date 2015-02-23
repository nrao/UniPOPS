      program tele2recs
c
c     @(#)tele2recs.f	5.1 06/22/94
c
c     Reads in a record presumably from a 140-ft telescope tape and outputs
c     a record in IEEE KEEP format.  Uses Standard Input and Standard
c     Output so that the user can implement pipes to do further conversions.
c
c     NOTE:  WORKS only for telescope tapes made AFTER JUNE 1989 when all 
c     continuum scans were made to have the same record size.
c
c     For Spectral line records, records are simply read and written (with
c     translation to IEEE).
c
c     For continuum records, data is SKIPPED.
c
c     NOTE: the user should redirect both input and output when using this 
c     program. Program uses standard error to report on its progress
c
      character*1 ltype(23)
      integer*4 lntype(23), ilnum, ierr, inbytes, long	
      integer*2 i, itype, nextbyte, li
      logical isitline, isitcont
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
      data ilnum/23/
c
c     Defines the format of the spectral line and continuum record read in.  
c     *TYPE defines the order of word types (d = d.prec, r = real, i = int*2, 
c     c = char*1, b = byte to bypass in input record, s = byte to skip in output
c     record) and *NTYPE the number of succesive words of the corresponding 
c     type (e.g. 2 reals, followed by 12 C*1, 1 Integer, 18 C*1, skip two 
c     ouput bytes, 1 double precision ...).
c
c     *LNUM give number of entries in the continuum and line versions
c     of *type and *ntype
c
99    continue
c
	call readbuff(ierr, long(ktosin*2), cinbuff)
	if (ierr .eq. 0) then
	  goto 999
	else if (ierr .ne. ktosin*2) then
	  write(0,*) "TELE2RECS: Error on read"
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
	   write(0,*) "TELE2RECS: Bad scan type:", itype
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
	   write(0,*) "TELE2RECS: Error on read"
	   goto 999
	endif
c	Finish reading record
c
        if (isitline(itype)) then
	      call cvtmodieee(cinbuff,ccvtbuff,ltype,lntype,ilnum)
	      call writebuff(ierr, long(outbuffsize), ccvtbuff)
	      if (ierr .ne. outbuffsize) then
		     write(0,*) "TELE2RECS: Error on write"
		     goto 999
	      endif
	      if (itype .eq. 1) then
c
c	        SPOWER so we have two records to write, one for
c	        each phase.  Have already written the 1st phase data but
c	        must move the data for the 2nd phase into the corect place.
c
		do 100 i = 1, 1024
		   rcvtbuff(i+256) = rcvtbuff(i+1280)
100		   continue
	        call writebuff(ierr, long(outbuffsize), ccvtbuff)
	     	if (ierr .ne. outbuffsize) then
		     write(0,*) "TELE2RECS: Error on write"
		     goto 999
	        endif
	      endif
        endif
c
	goto 99
c
999   stop
      end
c
