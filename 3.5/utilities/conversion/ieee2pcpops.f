c
c     @(#)ieee2pcpops.f	5.1 06/22/94
c
      character*1 ltype(19)
      character*3 filesuf
      character*7 dataout
      character*8 tableout
      character*5120 ctwh, couttwh
      real scannum
      character*79 ctable, filein, tablein
      integer*4 nltype(19), ilnum
      integer*2 itwh(2560), lsno, ktos, ktel, in, i, ierr, irecnum,
     .		irecmax, iout, itype, ijkl, li
      logical isitline
      real*4 twh(1280), outtwh(1280)
c
      equivalence (twh, itwh), (ctwh, twh)
      equivalence (outtwh, couttwh)
c
      isitline(li) =  (li.eq.1 .or. li.eq.2 .or. li.eq.5 .or. li.eq.6)
c
      data ltype/'r','c','i','c','d','i','r','i','r','i','r','i','r',
     1          'd','c','d','r','i','r' /
      data nltype/2 , 12, 1 , 18, 1 , 4 , 2 , 20, 23, 2 , 1 , 4 , 73,
     1           8 , 72, 6 , 4 ,112, 0/
      data ilnum/19/, lsno/1/, ktos/34/, ktel/33/
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
c     LSNO = location of scan number in input and output buffers
c     KTOS = location of obs. type
c     KTEL = telescope name (140 or 300)
c
901   write(6,*) 'What is the name of the input data file?'
      read(5,*) filein
      open(unit=10,access='direct',form='unformatted',status='old',
     1     recl=5120,file=filein,err=901)
c
9013  write(6,*) 'What is the name of the input table file?'
      read(5,*) tablein
      open(unit=9,access='sequential',form='formatted',status='old',
     1     file=tablein,err=9013)
      rewind(9)
c
1     write(6,*) 'What is the number of scans in one output file'
      read(5,*) irecmax
      if (irecmax .lt. 1) goto 1
c
      iout = 0
      in = 0
c
15      iout = iout + 1
        if (iout .gt. 999) then
		write(6,*) ' DATA file is too large.  999 data files'
		write(6,*) ' have already been generated'
		write(6,*) ' Abnormal termination at input record', in
		stop
	endif
c
	write(unit=filesuf,fmt='(i3)') iout
	dataout = 'DATA' // filesuf
	if (iout .lt. 100) dataout = 'DATA0' // filesuf(2:3)
	if (iout .lt. 10) dataout = 'DATA00' // filesuf(3:3)
	tableout = 'TABLE' // filesuf
	if (iout .lt. 100) tableout = 'TABLE0' // filesuf(2:3)
	if (iout .lt. 10) tableout = 'TABLE00' // filesuf(3:3)
c
	write(6,*) 'Attempting to create files ', dataout, ' ',tableout
c
	open(unit=11,access='direct',form='unformatted',status='new',
     1       recl=5120,file=dataout)
	open(unit=12,access='direct',form='formatted',status='new',
     1       recl=79,file=tableout)
c
        do 100 i = 1, irecmax
	  in = i + irecmax*(iout-1)
	  read(10, rec=in, end=999, err=999) twh
 	  itype = itwh(ktos)
c	  ITYPE = a flag telling whether this is a continuum or spectral line
c	        scan (1 = SPOWER, 2 = TPOWER, 3 = ONOFF, 4 = CONT, 8 = DCR)
c
	  if (.not. isitline(itype)) then
		write(6,*) 'Skipping continuum scan ', twh(lsno)
		in = in + 1
		goto 100
	  endif
c
          if (mod(twh(lsno),1.) .ge. .005) twh(lsno)=twh(lsno)*100
	  if (twh(lsno) .gt. 32767.) then
		write(6,*) 'Scan number out of range!!'
		write(6,*) 'Scan number changed from', twh(lsno), ' to ',
     .				aint(mod(twh(lsno),32767.))
		twh(lsno) = aint(mod(twh(lsno), 32767.))
	  endif
	  scannum = twh(lsno)
c	  Alter the scan number if it is out of range for PC-POPS.
c
9011      read(9, 16, end=999, err=999) ctable
16	  format(a79)
	  read(ctable(1:5), 17, iostat=ierr) irecnum
17	  format(i5)
	  if (ierr .ne. 0 .or. irecnum .ne. in) goto 9011
c	  Skip headers in TABLE file.
c
	  if (itwh(ktel) .eq. 300) then
		nltype(ilnum) = 408
	  else
		nltype(ilnum) = 1048
	  endif
	  call cvtpcieee(ctwh,couttwh,ltype,nltype,ilnum)
c	  Converts only the number of data points specific for each
c	  telescope.
c	  
95	  write(11, rec=i) outtwh
	  write(ctable(7:13),96) nint(scannum)
96	  format(i7)
	  write(12, 16, rec=i) ctable
100	  continue
c
        write(12, 101, rec=irecmax+1) (' ', ijkl = 1, 79)
101     format(79a1)
c
	write(6,*) irecmax, ' records written to ', dataout, ' ',tableout
c
	close (11)
	close (12)
c
        write(6,*) ' '
c
	goto 15
c
999   if (i .gt. 1) write(12, 101, rec=i) (' ', ijkl = 1, 79)
c
      close (11)
      close (12)
      close (9)
      close (10)
c
      write(6,*) 'Error at input and output records ',in, i
      write(6,*) i-1, ' records written to ', dataout, ' ', tableout
      write(6,*) ' '
      write(6,*) 'Terminating execution'
c
      stop
c
      end
c
