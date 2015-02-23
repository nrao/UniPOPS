c
      program ascii2ieee
c
c     @(#)ascii2ieee.f	5.1 06/22/94
c
c     Converts a scan from ASCII format to IEEE KEEP; uses standard
c     input and output.  The user can specify if only continuum or
c     spectral line scans are to be processed by including a c or
c     l on the command line; no arguement implies both kinds of scans
c     are desired.
c
      character*12 indicator
      character*1 arg
      character*5120 cbuf
      character*80 ctemp
      real*4 rbuf(1280)
      integer*4 iargc, ierr, ibytes, long
      logical cont, line, headerok, isitline, isitcont
      integer*2 ibuf(2560), jj, itype, li
      real*8 dbuf(640)
c
      equivalence (dbuf, rbuf), (dbuf,cbuf), (dbuf, ibuf)
c
      isitline(li) =  (li.eq.1 .or. li.eq.2 .or. li.eq.5 .or. li.eq.6)
      isitcont(li) =  (li.eq.3 .or. li.eq.4 .or. li.eq.8)
c
      data ibytes/5120/
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
999   continue
c
      headerok = .false.
c     HEADEROK will only be true if all of the header can be read in
c     correctly.
c
      read(5,1,err=200) rbuf(1), rbuf(2)
1     format(2e15.7)
c     On and Off scan number
c
      read(5,2,err=200,end=300) cbuf(9:20)
2     format(a12)
c     Source name
c
      read(5,3,err=200,end=300) ibuf(11)
3     format(i6)
c     Observer's number
c
      read(5,4,err=200,end=300) cbuf(23:40)
4     format(a18)
c     Observer's name
c
      read(5,5,err=200,end=300) dbuf(6)
5     format(d20.12)
c     Modified Julian date
c
      read(5,6,err=200,end=300) (ibuf(jj),jj=25,28)
6     format(4i6)
c     Day of year, month, day, year
c
      read(5,7,err=200,end=300) rbuf(15), rbuf(16)
7     format(2e15.7)
c     LST, EST
c
      read(5,81,err=200,end=300) ctemp
81    format(a80)
      read(ctemp,8,err=200,end=300) (ibuf(jj),jj=33,39)
8     format(7i6)
c     Telescope, Observing type, scan type, etc.
c
      itype = ibuf(34)
      if (.not. isitcont(itype) .and. .not. isitline(itype)) then
	   write(0,*) "ASCII2IEEE: Bad scan type:", itype
	   goto 200
      endif
c     ITYPE tells whether this is a spectral line (<=2) or continuum (>2)
c     scan.
c
      if (isitline(itype) .and. line) then
c        Current scan is a spectral line scan and is desired.
c
         read(5,9,err=200,end=300) (ibuf(jj),jj=40,52)
9        format(8i6,/,5i6)
c	 ISTART, ISTOP, VREF, VDEF, etc.
c
         read(5,11,err=200,end=300) rbuf(27)
11       format(e15.7)
c	 Scan Duration
c
         read(5,12,err=200,end=300) ibuf(55), ibuf(56)
12       format(2i6)
c 	 Status Words
c
         read(5,13,err=200,end=300) (rbuf(jj),jj=29,49)
13       format(2(4e15.7,/),2(3e15.7,/),5e15.7,/,2e15.7)
c	 INtg. Time, Unused, Epoch, H rate, V rate, etc.
c
         read(5,19,err=200,end=300) (ibuf(jj),jj=99,106)
19       format(8i6)
c	 Environmental values.
c
         read(5,20,err=200,end=300) (rbuf(jj),jj=54,125)
20       format(4e15.7)
c	 APP RA, DEC; Epoch RA, DEC; Gal. coords.; etc.
c
         read(5,38,err=200,end=300) ibuf(251), ibuf(252)
38       format(2i6)
c	 Unused
c
         read(5,39,err=200,end=300) (dbuf(jj),jj=64,71)
39       format(4d20.12)
c	 Center frequencies, Rest Frequencies
c
         read(5,41,err=200,end=300) cbuf(569:686)
         read(5,41,err=200,end=300) cbuf(587:604)
         read(5,41,err=200,end=300) cbuf(605:622)
         read(5,41,err=200,end=300) cbuf(623:640)
41       format(a18)
c	 Center frequency formulae
c
         read(5,45,err=200,end=300) (dbuf(jj),jj=81,86)
45       format(3d20.12)
c 	 L1, L1F1, L1F2, L2, etc.
c
         read(5,47,err=200,end=300) (rbuf(jj),jj=173,176)
47       format(4e15.7)
c	 LA, LB, LC, LD
c
         read(5,48,err=200,end=300) (ibuf(jj),jj=353,464)
48       format(10i6)
c	 A/C words
c
         read(5,60,err=200,end=300) (rbuf(jj),jj=233,256)
60       format(4e15.7)
c	 Ref. System. temps., Power counters, Channel zero values
c
	 headerok = .true.
         read(5,66,err=200,end=300) (rbuf(jj),jj=257,1280)
66       format(5e15.7)
c	 Spectral values
c
         read(5,67,err=200,end=300) indicator
67       format(a12)
c	 End of scan indicator
c
      else if (isitcont(itype) .and. cont) then
c	  Current scan is a continuum scans and is desired
c
          read(ctemp,801,err=200,end=300) (ibuf(jj),jj=33,44)
801	  format(12i6)
c         Reread Telescope, etc. for continuum scans
c
	  read(5,91,err=200,end=300) rbuf(23)	
91	  format(e15.7)
c	  DCR cal. factor
c
	  read(5,101,err=200,end=300) (ibuf(jj),jj=47,54)
101	  format(8i6)
c	  VREF, VDEF, POsition Code, etc.
c
	  read(5,111,err=200,end=300) (rbuf(jj),jj=28,49)
111	  format(5e15.7)
c	  DCR Sys. temp., LAMBDA, Beam size factor, etc.
c
	  read(5,116,err=200,end=300) (ibuf(jj),jj=99,106)
116 	  format(8i6)
c	  Environmental values
c
	  read(5,171,err=200,end=300) (rbuf(jj),jj=54,68)
171	  format(5e15.7)
c	  Epoch RA, DEC; App. RA, DEC; Gal. coords.; etc.
c
	  read(5,201,err=200,end=300) ibuf(137), ibuf(138)
201	  format(2i6)
c	  Unused and scans accummed.
c
	  read(5,211,err=200,end=300) (rbuf(jj),jj=70,87)
211	  format(5e15.7)
c	  ACCUM stack
c
	  headerok = .true.
	  read(5,212,err=200,end=300) (rbuf(jj),jj=88,687)
212	  format(6e12.4)
c	  Data points
c
          read(5,213,err=200,end=300) indicator
213       format(/,a12)
c	  End of scan indicator
c
      else
c       Scan of the current type is not desired.
c
	goto 200
c
      endif
c
      call writebuff(ierr, ibytes, cbuf)
      if (ierr .ne. 5120) then
	goto 300
      else
	goto 999
      endif
c     Scan is of the desired type and has been converted so write it out.
c     IF error on reading, stop the program else read in next scan.
c
200   continue
	if (headerok) then
      		call writebuff(ierr, ibytes, cbuf)
      		if (ierr .ne. 5120) goto 300
	endif
c	An error was detected while reading data... Write out scan anyway.
c
250	read(5,251,end=300) indicator
251	format(a12)
	if (indicator .eq. 'SCAN #######') then
	   goto 999
	else
	   goto 250
	endif
c       If an error on reading, keep reading file until end of file or until
c	another 'SCAN' string is seen
c
300   stop
      end
