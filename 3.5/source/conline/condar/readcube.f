      subroutine readcube(filename)
c
c     @(#)readcube.f	5.5 05/06/98
c
c     Opens the FITS CUBE file with the name FILENAME and reads in the header.
c
      character*(*) filename
c
      character*80 head(36), hmask(36), stch
      character*1023 headname, fullname
      character*1 quote
      logical lend, inquirefile
      integer*2 ierr, j, i, ilen, irec, nv, kmax, maxrec, mleft, 
     .		istch(40), lastblnk, k, blankfits2, blankfits1
      integer*2 m1, m2, n80, n352, n357, n365
      integer*4 n1, n2, n3, nwrds2, nwrds4, nwrds1, nwrds8, blankfits4
      integer*4 nchars, oldfits, bp, iblank, ii
      real*8 rmax1, rmax2, rmax4
      real r_quiet_nan
c
      include 'mappl.inc'
      include 'cio.inc'
      include 'mform.inc'
c
      equivalence (istch,stch)
c
      parameter (rmax1 = 255.0d0)
      parameter (rmax2 = 32767.0d0)
      parameter (rmax4 = 2147483647.0d0)
      parameter (nwrds1 = 2880)
      parameter (nwrds2 = 1440)
      parameter (nwrds4 = 720)
      parameter (nwrds8 = 360)
      parameter (blankfits4 = -2147483647)
      parameter (blankfits2 = -32767)
      parameter (blankfits1 = 1)
c
      data m1, m2, n80, n352, n357, n365 /-1, -2, 80, 352, 357, 365/
      data quote /''''/
c
      call filecomp(filename, fullname)
c
      fcblank = r_quiet_nan
c
      ilen = lastblnk(dirprefix(1))
      headname = dirprefix(1)(1:ilen) // '/utilities/head.mask'
c     Get the file name of cube FITS template file
c
      open(unit=iiotmp,file=headname,access='sequential',
     .		form='formatted',status='old',iostat=ierr)
      if (ierr .ne. 0) 
     .	call oerror(n352, m1, 'READCUBE: Template FITS-file not found')
c
      rewind(iiotmp,iostat=ierr)
      if (ierr .eq. 0) read(iiotmp,10,iostat=ierr) hmask
10    format(a)
      if (ierr .ne. 0) 
     .	 call oerror(n357,m1,
     .          'READCUBE: Template FITS-file cannot be read')
      close (iiotmp,iostat=ierr)
c     Reads in cube FITS header template.  Program must know of any 
c     changes to template.
c
      if (.not. inquirefile(fullname)) 
     .	call oerror(n352,m2, 'READCUBE: FITS file does not exist')
c
      close(icube,iostat=ierr)
      open(unit=icube, file=fullname, access='direct', 
     .	form='unformatted', status='old',iostat=ierr,recl=2880)
      if (ierr .ne. 0) 
     .		call oerror(n352, m2, 'READCUBE: FITS file cannot be opened')
c
      lend = .false.
      chead(ctzero) = 0.
      chead(ctscale) = 1.
c
      chead(crval1) = 1.
      chead(crval2) = 1.
      chead(crval3) = 1.
      chead(cnaxis1) = 0.
      chead(cnaxis2) = 0.
      chead(cnaxis3) = 0.
      chead(cdelt1) = 1.
      chead(cdelt2) = 1.
      chead(cdelt3) = 1.
      chead(cpix1) = 1.
      chead(cpix2) = 1.
      chead(cpix3) = 1.
      chead(cequinox) = 0.0
      chead(ctype1) = '        '
      chead(ctype2) = '        '
      chead(ctype3) = '        '
      chead(cdate) = -1.0
      chead(cobject) = '        '
      chead(cobject+1) = '        '
      chead(corigin) = '        '
      chead(cbunit) = '        '
      do 1011 i = 1, cheadsize-ccomment+1
	cchead(i-1+ccomment) = '        '
1011	continue
c     Sets up defaults if the header doesn't contain the necessary
c     information.  No gaurentees if one of the above is not in the header
c
      irec = 0
c
1       irec = irec + 1
	write(stch,fmt="(a,i5,a)") 'Reading the ', irec, 
     .			' header record from cube FITS file'
	call pwrite(istch,n80)
      	read(icube,rec=irec,iostat=ierr) head
	if (ierr .ne. 0) call oerror(n357, m2, 'Cannot read from FITS file')
c
 	stch = ' '
	call pwrite(istch,n80) 
	do 2 i = 1, 33
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
     .     call oerror(n365, m2, 'READCUBE: ----> '// head(1)(1:30))
c
	if (head(2)(1:10) .ne. hmask(2)(1:10)) then
     	   call oerror(n365, m2, 'READCUBE: ----> '// head(2)(1:30))
	else
	   read(unit=head(2) (11:30), fmt=20, iostat=ierr) nv
	   chead(cbitpix) = nv
	endif
c
        bp = nint(chead(cbitpix))
        if (bp .ne. 16 .and. bp .ne. 32 .and. bp .ne. 8 .and.
     .       bp .ne. -32 .and. bp .ne. -64) then
     	    call oerror(n365, m2, 'READCUBE: ----> '// head(2)(1:30))
	endif
c
	if (head(3)(1:30) .ne. hmask(3)(1:30)) 
     .	   call oerror(n365, m2, 'READCUBE: ----> '// head(3)(1:30))
c       Checks for SIMPLE, BITPIX, NAXIS=2
c
	if (head(4)(1:10) .ne. hmask(4)(1:10)) then
     	   call oerror(n365, m2, 'READCUBE: ----> '// head(4)(1:30))
	else
	   read(unit=head(4) (11:30),fmt=20, iostat=ierr) nv
20    	   format(i20)
	   chead(cnaxis1) = nv
	endif
	if (head(5)(1:10) .ne. hmask(5)(1:10)) then 
     	   call oerror(n365, m2, 'READCUBE: ----> '// head(5)(1:30))
	else
	   read(unit=head(5) (11:30),fmt=20, iostat=ierr) nv
	   chead(cnaxis2) = nv
	endif
	if (head(6)(1:10) .ne. hmask(6)(1:10)) then 
     	   call oerror(n365, m2, 'READCUBE: ----> '// head(6)(1:30))
	else
	   read(unit=head(6) (11:30),fmt=20, iostat=ierr) nv
	   chead(cnaxis3) = nv
	endif
c       Checks NAXIS1,NAXIS2,NAXIS3
c
        endif
c             try and see if this is an old style unipops header
c             this had the DATE keyword with the order of mm/dd reversed
        oldfits = 0
        if (head(2)(32:32) .eq. '/' .and.
     +      head(2)(53:59) .eq. 'UniPops') then
           oldfits = 1
        endif
c
	do 50 j = 36, 1, -1
	   if (hmask(36)(1:10) .eq. head(j)(1:10) ) then
		stch = 'Last header record found'
		call pwrite(istch, n80)
		cnumhead = irec
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
                if (i .eq. 7) cchead(ctype1) = head(j) (12:19)
		if (i .eq. 12) cchead(ctype2) = head(j) (12:19)
		if (i .eq. 17) cchead(ctype3) = head(j) (12:19)
		if (i .eq. 23) then
                   cchead(cobject) = head(j) (12:19) 
                   if (head(j) (20:20) .ne. quote) 
     .                 cchead(cobject+1) = head(j) (20:27)
                endif
		if (i .eq. 24) cchead(corigin) = head(j) (12:19) 
		if (i .eq. 25) then 
                   call fitskeyvalue(nchars, head(j), stch)
                   call todate(nchars, stch, chead(cdate), oldfits)
                endif
		if (i .eq. 29) cchead(cbunit) = head(j) (12:19) 
c
      		if (i.eq.8) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(crval1)
      		if (i.eq.9) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(cdelt1)
		if (i.eq.10) read(unit=head(j)(11:30),40) chead(cpix1)
      		if (i .eq. 13) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(crval2)
      		if (i .eq. 14) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(cdelt2)
		if (i .eq. 15) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(cpix2)
      		if (i .eq. 18) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(crval3)
      		if (i .eq. 19) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(cdelt3)
		if (i .eq. 20) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(cpix3)
      		if (i .eq. 26) then 
c                       look for bad unipops BLANK values which may be
c                       floating point values, watch for a "."
                   do 405 ii=11,30
                      if (head(j)(ii:ii) .eq. '.') goto 410
 405               continue
 410               if  (ii .le. 30) then
                      read(unit=head(j)(11:30),45,iostat=ierr) fcblank
                   else
                      read(unit=head(j)(15:30),45,iostat=ierr) iblank
                      fcblank = iblank
                   endif
                endif
      		if (i .eq. 27) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(ctscale)
      		if (i .eq. 28) read(unit=head(j)(11:30),40,iostat=ierr)
     .							chead(ctzero)
      		if (i .eq. 30) read(unit=head(j)(11:30),40,iostat=ierr) 
     .							chead(ctmin)
      		if (i .eq. 31) read(unit=head(j) (11:30),40,iostat=ierr) 
     .							chead(ctmax)
      		if (i .eq. 32) read(unit=head(j) (11:30),40,iostat=ierr) 
     .							chead(cequinox)
 40             format(1g20.0)
 45             format(i16)
c
      		if (i .eq. 33) then
		    do 101 k = 1, min(7,cheadsize-ccomment+1)
		       cchead(k-1+ccomment) = head(j) (11+(k-1)*8:10+k*8)
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
      n1 = nint(chead(cnaxis1))
      n2 = nint(chead(cnaxis2))
      n3 = nint(chead(cnaxis3))
      if ( abs(nint(chead(cbitpix))) .eq. 16) then
	maxrec = (n1*n2*n3) / nwrds2 + cnumhead
        mleft = mod(n1*n2*n3, nwrds2)
      else if ( abs(nint(chead(cbitpix))) .eq. 32) then
	maxrec = (n1*n2*n3) / nwrds4 + cnumhead
        mleft = mod(n1*n2*n3, nwrds4)
      else if ( abs(nint(chead(cbitpix))) .eq. 64) then
	maxrec = (n1*n2*n3) / nwrds8 + cnumhead
        mleft = mod(n1*n2*n3, nwrds8)
      else if ( abs(nint(chead(cbitpix))) .eq. 8) then
	maxrec = (n1*n2*n3) / nwrds1 + cnumhead
        mleft = mod(n1*n2*n3, nwrds1)
      else
     	call oerror(n365, m2, 'READCUBE: ----> CBITPIX')
      endif
      if (mleft .ne. 0) maxrec = maxrec + 1
      write(stch,fmt="(i5,a)") maxrec, ' records in FITS file'
      call pwrite(istch,n80)
c     Calculates how many records the data will occupy and how much of the
c	last record the data will fill.
c			make sure equinox is set appropriately
         if (chead(cequinox) .eq. 0 .and.
     .       (cchead(ctype1)(1:4) .eq. 'DEC-' .or.
     .        cchead(ctype2)(1:4) .eq. 'DEC-')) 
     .      chead(cequinox) = 1950.0
c
      if (chead(ctscale) .eq. 0.) 
     .		call oerror(n365, m2, 'READCUBE: Bad BSCALE')
c
      if ( nint(chead(cbitpix)) .eq. 16) then
	chead(ctmin) = chead(ctzero) - rmax2*chead(ctscale)
      	chead(ctmax) = chead(ctscale)*(2.*rmax2) + chead(ctmin)
      else if ( nint(chead(cbitpix)) .eq. 32) then
	chead(ctmin) = chead(ctzero) - rmax4*chead(ctscale)
      	chead(ctmax) = chead(ctscale)*(2.*rmax4) + chead(ctmin)
      else if ( nint(chead(cbitpix)) .eq. 8) then
	chead(ctmin) = chead(ctzero)
      	chead(ctmax) = chead(ctscale)*rmax1 + chead(ctmin)
      else if ( nint(chead(cbitpix)) .ne. -32 .and. 
     .          nint(chead(cbitpix)) .ne. -64) then
     	call oerror(n365, m2, 'READCUBE: ----> CBITPIX')
      endif
c     Recalculate TMIN and TMAX from TSCALE and TZERO
c
      ccube = filename
c
99    return
c
      end
c
