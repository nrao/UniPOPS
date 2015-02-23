      subroutine mread(filename)
c
c     @(#)mread.f	5.7 05/06/98
c
c     Reads in 2-dim FITS image file into matrix 0
c
      character*(*) filename
c
      character*80 head(36), hmask(36), stch
      character*1023 headname, fullname
      character*1 quote
      logical lend, inquirefile, okreal4, okreal8
      integer*4 n1, n2, long, i4, nwrds1, nwrds2, nwrds4, nwrds8, nwrds,
     +          l80, nchars, oldfits
c
      parameter (nwrds1 = 2880)
      parameter (nwrds2 = 1440)
      parameter (nwrds4 = 720)
      parameter (nwrds8 = 360)
c
      integer*2 ierr, j, i, k, lastblnk, ilen, irec, nv, numhead, kmax, 
     .		maxrec, mleft, ldata2(nwrds2), istch(40)
      integer*4 ldata4(nwrds4), iblank, ii
      integer*2 m1, m2, n80, n352, n357, n365
      character*1 cdata1(nwrds1)
      real tzero, tscale, rdata4(nwrds4), fblank, r_quiet_nan
      real*8 rmax2, rmax4, rdata8(nwrds8)
c
      parameter (rmax2 = 32767.0d0)
      parameter (rmax4 = 2147483647.0d0)
c
      include 'mappl.inc'
      include 'cio.inc'
      include 'mform.inc'
c
      equivalence (istch,stch)
c
      data m1, m2, n80, n352, n357, n365 / -1, -2, 80, 352, 357, 365/
      data quote /''''/
      data l80 /80/
c
      fblank = r_quiet_nan()
c
      call filecomp(filename, fullname)
c
      ilen = lastblnk(dirprefix(1))
      headname = dirprefix(1)(1:ilen) // '/sunbin/twodim.mask'
c     Get the file name of FITS template file
c
      open(unit=iiotmp,file=headname,access='sequential',
     .		form='formatted',status='old',iostat=ierr)
      if (ierr .ne. 0) 
     .	call oerror(n352, m1, 'MREAD: Template FITS-file not found')
c
      rewind(iiotmp,iostat=ierr)
      read(iiotmp,10,iostat=ierr) hmask
10    format(a)
      if (ierr .ne. 0) 
     .	call oerror(n357, m1, 'MREAD: Template FITS-file cannot be read')
      close (iiotmp,iostat=ierr)
c     Reads in FITS header template.  Program must know of any changes to
c     template.
c
      if (.not. inquirefile(fullname)) 
     .	call oerror(n352,m2, 'MREAD: FITS file does not exist')
c
      open(unit=iiotmp, file=fullname, access='direct', 
     .	form='unformatted', status='old',iostat=ierr,recl=2880)
      if (ierr .ne. 0) 
     .		call oerror(n352, m2, 'MREAD: FITS file cannot be opened')
c
      lend = .false.
c
      mhead(mrval1,1) = 1.
      mhead(mrval2,1) = 1.
      mhead(mnaxis1,1) = 0.
      mhead(mnaxis2,1) = 0.
      mhead(mdelt1,1) = 1.
      mhead(mdelt2,1) = 1.
      mhead(mpix1,1) = 1.
      mhead(mpix2,1) = 1.
      mhead(mequinox,1) = 0.
      cmhead(mtype1,1) = '        '
      cmhead(mtype2,1) = '        '
      mhead(mdate,1) = -1.
      cmhead(mobject,1) = '        '
      cmhead(mobject+1,1) = '        '
      cmhead(morigin,1) = '        '
      cmhead(mbunit,1) = '        '
      do 1011 i = 1, mheadsize-mcomment+1
	cmhead(i-1+mcomment,1) = '        '
1011	continue
c     Sets up defaults if the header doesn't contain the necessary
c     information.  No gaurentees if one of the above is not in the header
c
      irec = 0
c
1       irec = irec + 1
	write(stch,fmt="(a,i5,a)") 'Reading the ', irec, 
     .			' header record from FITS file'
	call pwrite(istch,n80)
      	read(iiotmp,rec=irec,iostat=ierr) head
	if (ierr .ne. 0) call oerror(n357, m2, 'Cannot read from FITS file')
c
 	stch = ' '
	call pwrite(istch,n80) 
	do 2 i = 1, 24
	   stch = head(i)
 	   call pwrite(istch,n80)
2	   continue
 	stch = ' '
	call pwrite(istch,n80)
c	Echo out the header record 
c
	if (irec .eq. 1) then
c	Certain things MUST be in the first record of the header.  Test that
c	those things have the proper value.
c
	if (head(1)(1:30) .ne. hmask(1)(1:30)) 
     .     call oerror(n365, m2, 'MREAD: ----> '// head(1)(1:30))
c
	if (head(2)(1:10) .ne. hmask(2)(1:10)) then 
     	   call oerror(n365, m2, 'READCUBE: ----> '// head(2)(1:30))
	else
	   read(unit=head(2) (11:30), fmt=20, iostat=ierr) nv
	   mhead(mbitpix,1) = nv
	endif
c
	if (nint(mhead(mbitpix,1)) .eq. 16) then
	    nwrds = nwrds2
	else if (nint(mhead(mbitpix,1)) .eq. 32) then
	    nwrds = nwrds4
	else if (nint(mhead(mbitpix,1)) .eq. 8) then
	    nwrds = nwrds1
	else if (nint(mhead(mbitpix,1)) .eq. -32) then
	    nwrds = nwrds4
	else if (nint(mhead(mbitpix,1)) .eq. -64) then
	    nwrds = nwrds8
	else
     	   call oerror(n365, m2, 'MREAD: ----> '// head(2)(1:30))
	endif
c
	if (head(3)(1:30) .ne. hmask(3)(1:30)) 
     .	   call oerror(n365, m2, 'MREAD: ----> '// head(3)(1:30))
c       Checks for SIMPLE, BITPIX, NAXIS=2
c
	if (head(4)(1:10) .ne. hmask(4)(1:10)) then
     	   call oerror(n365, m2, 'MREAD: ----> '// head(4)(1:30))
	else
	   read(unit=head(4) (11:30),fmt=20, iostat=ierr) nv
20    	   format(i20)
	   mhead(mnaxis1,1) = nv
	endif
	if (head(5)(1:10) .ne. hmask(5)(1:10)) then 
     	   call oerror(n365, m2, 'MREAD: ----> '// head(5)(1:30))
	else
	   read(unit=head(5) (11:30),fmt=20, iostat=ierr) nv
	   mhead(mnaxis2,1) = nv
	endif
c       Checks NAXIS1,NAXIS2
c
        endif
c             try and see if this is an old style unipops header
c             this had the DATE keyword with the order of mm/dd reversed
        oldfits = 0
        if (head(1)(32:32) .eq. '/' .and.
     +       head(1)(73:79) .eq. 'UniPops') then
           oldfits = 1
        endif
c
	do 50 j = 36, 1, -1
	   if (hmask(36)(1:10) .eq. head(j)(1:10) ) then
		stch = 'Last header record found'
		call pwrite(istch, n80)
		numhead = irec
		kmax = j - 1
		lend = .true.
		goto 60
	   endif
50	   continue
        kmax = 36
c       Checks if this is the last header record
c
60      continue
c
        do 200 i = 1, 36
	   do 100 j = 1, kmax
	      if (hmask(i) (1:10) .eq. head(j) (1:10) ) then
                if (i .eq. 6) cmhead(mtype1,1) = head(j) (12:19)
		if (i .eq. 10) cmhead(mtype2,1) = head(j) (12:19)
		if (i .eq. 14) then
                   cmhead(mobject,1) = head(j) (12:19) 
                   if (head(j) (20:20) .ne. quote) 
     .                cmhead(mobject+1,1) = head(j) (20:27)
                endif
		if (i .eq. 15) cmhead(morigin,1) = head(j) (12:19) 
		if (i .eq. 16) then
                   call fitskeyvalue(nchars, head(j), stch)
                   call todate(nchars, stch, mhead(mdate, 1), oldfits)
                endif
		if (i .eq. 20) cmhead(mbunit,1) = head(j) (12:19) 
c
      		if (i.eq.7) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							mhead(mrval1,1)
      		if (i.eq.8) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							mhead(mdelt1,1)
		if (i.eq.9) read(unit=head(j)(11:30),40) mhead(mpix1,1)
      		if (i .eq. 11) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							mhead(mrval2,1)
      		if (i .eq. 12) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							mhead(mdelt2,1)
		if (i .eq. 13) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							mhead(mpix2,1)
      		if (i .eq. 17) then
c                           look for bad unipops BLANK values which are
c                           floating point values, just watch for
c                           "."
                   do 405 ii=11,30
                      if (head(j)(ii:ii) .eq. '.') goto 410
 405               continue
 410               if (ii .le. 30) then
                      read(unit=head(j)(11:30),40,iostat=ierr) fblank
                   else
                      read(unit=head(j)(15:30),45,iostat=ierr) iblank
                      fblank = iblank
                   endif
                endif
      		if (i .eq. 18) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							tscale
      		if (i .eq. 19) read(unit=head(j)(11:30),40,iostat=ierr)
     .							tzero
      		if (i .eq. 23) read(unit=head(j) (11:30),40,iostat=ierr) 
     .							mhead(mequinox,1)
 40             format(1p1g20.0)
 45             format(i16)
c
      		if (i .eq. 24) then
		    do 101 k = 1, min(7,mheadsize-mcomment+1)
		       cmhead(k-1+mcomment,1) = head(j) (11+(k-1)*8:10+k*8)
101		       continue
		endif
c
	      endif
100           continue
200	   continue
c	Stuffs the FITS header parameters into the matrix header parameters
c
        if ( .not. lend) goto 1
c
      stch = 'Checking input FITS header parameters'
      call pwrite(istch,n80)
      stch = ' '
      call pwrite(istch,n80)
c
      n1 = nint(mhead(mnaxis1,1))
      n2 = nint(mhead(mnaxis2,1))
      if (n1 .le. 0 .or. n2 .le. 0 .or. n1*n2 .gt. mdatasize/mnumarrays) 
     .		call oerror(n365, m2, 'MREAD: Too many data points in matrix')
c     Make sure that data can fit in matrix 0
c
      maxrec = (n1*n2) / nwrds + numhead
      mleft = mod(n1*n2, nwrds)
      if (mleft .ne. 0) maxrec = maxrec + 1
      write(stch,fmt="(i5,a)") maxrec, ' records in FITS file'
      call pwrite(istch,n80)
c     Calculates how many records the data will occupy and how much of the
c	last record the data will fill.
c
      if (tscale .eq. 0.) 
     .		call oerror(n365, m2, 'MREAD: Bad BSCALE')
c			set equinox if not set during read and RA/DEC coord
      if (mhead(mequinox,1) .eq. 0 .and.
     .    cmhead(mtype1,1)(1:4) .eq. 'RA--' .or. 
     .    cmhead(mtype2,1)(1:4) .eq. 'RA--' .or.
     .    cmhead(mtype1,1)(1:4) .eq. 'HA--' .or. 
     .    cmhead(mtype2,1)(1:4) .eq. 'HA--' .or.
     .    cmhead(mtype1,1)(1:4) .eq. 'DEC-' .or. 
     .    cmhead(mtype2,1)(1:4) .eq. 'DEC-') mhead(mequinox,1) = 1950.
c
      do 92 irec = numhead+1, maxrec
	if (nint(mhead(mbitpix,1)) .eq. 16) then
		read(iiotmp,rec=irec,iostat=ierr) ldata2
	else if (nint(mhead(mbitpix,1)) .eq. 32) then
		read(iiotmp,rec=irec,iostat=ierr) ldata4
	else if (nint(mhead(mbitpix,1)) .eq. -32) then
		read(iiotmp,rec=irec,iostat=ierr) rdata4
	else if (nint(mhead(mbitpix,1)) .eq. -64) then
		read(iiotmp,rec=irec,iostat=ierr) rdata8
	else if (nint(mhead(mbitpix,1)) .eq. 8) then
		read(iiotmp,rec=irec,iostat=ierr) cdata1
	else
	    call oerror(n365, m1, 'Bad FITS header: Bad BITPIX')
	endif
	if (ierr .ne. 0) 
     .	   call oerror(n357,m2,'MREAD: Cannot read FITS file')
	if (irec .eq. maxrec) then
	   j = mleft
	else
	   j = nwrds
	endif
c	If it is the last record, reset the number of data values to read
c	into the matrix.
c
	do 91 i = 1, j
	   i4 = long(i) + nwrds*long(irec-numhead-1)
	   if (nint(mhead(mbitpix,1)) .eq. 16) then
	   	if (ldata2(i) .eq. fblank) then
			mdata(i4) = mhead(mblank,1)
	   	else
	        	mdata(i4) = float(ldata2(i))*tscale + tzero
	   	endif
	   else if (nint(mhead(mbitpix,1)) .eq. 32) then
	   	if (ldata4(i) .eq. fcblank) then
			mdata(i4) = mhead(mblank,1)
	   	else
	        	mdata(i4) = float(ldata4(i))*tscale + tzero
	   	endif
	   else if (nint(mhead(mbitpix,1)) .eq. 8) then
	   	if (ichar(cdata1(i)) .eq. fblank) then
			mdata(i4) = mhead(mblank,1)
	   	else
	        	mdata(i4) = float(ichar(cdata1(i)))*tscale + tzero
	   	endif
	   else if (nint(mhead(mbitpix,1)) .eq. -32) then
	   	if ( .not. okreal4(rdata4(i))) then
			mdata(i4) = mhead(mblank,1)
	   	else
	        	mdata(i4) = rdata4(i)*tscale + tzero
	   	endif
	   else if (nint(mhead(mbitpix,1)) .eq. -64) then
	   	if ( .not. okreal8(rdata8(i))) then
			mdata(i4) = mhead(mblank,1)
	   	else
	        	mdata(i4) = rdata8(i)*tscale + tzero
	   	endif
	   else
	        call oerror(n365, m1, 'Bad FITS header: Bad BITPIX')
	   endif
91	   continue
92	continue
c	Scale the data (if it is DEFINED) from I*2/4 to R*4 and store in
c	 matrix
c
      close(iiotmp,iostat=ierr)
c
      return
c
      end
c
