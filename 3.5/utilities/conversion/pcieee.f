      program pcieee
c
c     @(#)pcieee.f	5.1 06/22/94
c
c     Reads in a record from either a IEEE KEEP and outputs a record in PC
c     KEEP or reads in a PC KEEP record and outputs a IEEE KEEP record.
c     Uses Standard Input and Standard Output so that the user can implement
c     pipes to do further conversions.
c
c     Works for both continuum and line keep files.  Scans of different
c     types can be intermixed in the input file.
c
c     The user can specify whether continuum or line records are to be ignored
c     by specifying a 'c' (for continuum only), an 'l' (for line only), or
c     nothing (for both line and continuum) on the command line.
c
c     NOTE: the user should redirect both input and output when using this 
c     program. Program uses standard error to report on its progress
c
      character*1 ltype(19), ctype(17), arg
      character*5120 cinbuff, coutbuff
      integer*4 nltype(19), nctype(17), icnum, ilnum, iargc, ibuffsize,
     .		ierr, long
      integer*2 iinbuff(2560), ioutbuff(2560), itype, nin, ktel,
     .		ktos, lsno, li 
      real rinbuff(1280), routbuff(1280)
      logical cont, line, isitline, isitcont
c
      equivalence (rinbuff, cinbuff), (iinbuff, cinbuff)
      equivalence (routbuff, coutbuff), (ioutbuff, coutbuff)
c
      isitline(li) =  (li.eq.1 .or. li.eq.2 .or. li.eq.5 .or. li.eq.6)
      isitcont(li) =  (li.eq.3 .or. li.eq.4 .or. li.eq.8)
c
      data ltype/'r','c','i','c','d','i','r','i','r','i','r','i',
     1		 'r','d','c','d','r','i','r'/
      data nltype/2 , 12, 1 , 18, 1 , 4 , 2 , 20, 23, 2 , 1 , 4 ,
     1            73, 8 , 72, 6 , 4 ,112, 0 /
c
      data ctype/'r','i','c','i','c','d','i','r','i','r','i','r',
     1		 'i','r','i','r','r' /
      data nctype/1 , 2 , 12, 1 , 18, 1 , 4 , 2 , 12, 1 , 8 , 22,
     1            2 , 1 , 4 , 16,1211/
c
      data icnum/17/, ilnum/19/
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
      data ibuffsize/5120/, lsno/1/, ktos/34/, ktel/33/
c     IBUFFSIZE = Size of input/output buffers
c     LSNO = location of scan number in input and output buffers
c     KTOS = location of obs. type
c     KTEL = telescope name (140 or 300)
c
      nin = 0
c     NIN = number of records read in so far
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
100   continue
c
	call readbuff(ierr, ibuffsize, cinbuff)
	if (ierr .eq. 0) then
	  goto 999
	else if (ierr .ne. ibuffsize) then
	  write(0,*) "PCIEEE: Error on read"
	  goto 999
	endif
	nin = nin + 1
c	Read in a record
c
 	itype = iinbuff(ktos)
	if (itype .ge. 256) itype = itype / 256
	if (.not. isitcont(itype) .and. .not. isitline(itype)) then
	   write(0,*) "PCIEEE: Bad scan type:", itype
	   goto 999
	endif
c	ITYPE = a flag telling whether this is a continuum or spectral line
c	        scan (1 = SPOWER, 2 = TPOWER, 3 = ONOFF, 4 = CONT, 8 = DCR)
c	If already converted, 256 = SPOWER, 512 = TPOWER, 768 = ONOFF, 
c	1024 = CONT, 2048 = DCR
c
	if ( (line .and. isitline(itype)) .or.
     .       (cont .and. isitcont(itype)) ) then
c
	   if (isitline(itype)) then
	      if (iinbuff(ktel) .eq. 300) then
		nltype(ilnum) = 408
	      else
		nltype(ilnum) = 1048
	      endif
c	      Converts only the number of data points specific for each
c	      telescope.
c
	      call cvtpcieee(cinbuff,coutbuff,ltype,nltype,ilnum)
	   else
	      call cvtpcieee(cinbuff,coutbuff,ctype,nctype,icnum)
	   endif
c	   Converts from input MODCOMP binary to the binary format of the 
c	   IEEE KEEP. 
c
	   call writebuff(ierr, ibuffsize, coutbuff)
	   if (ierr .ne. ibuffsize) then
		write(0,*) "PCIEEE: Error on write"
		goto 999
	   endif
c	   Writes out converted scan
c
	endif
c
	goto 100
c
999   stop
c
      end
c
