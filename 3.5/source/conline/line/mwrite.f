      subroutine mwrite(filename)
c
c     @(#)mwrite.f	5.5 05/06/98
c
c     Writes 2-dim FITS image file from matrix 0 to file FILENAME
c
      character*(*) filename
c
      character*24 sdate
      character*80 hmask(36), stch
      character*1023 headname, fullname
      integer*4 n1, n2, unlink, nwrds2, i4, long, hcmt, nwrds4, nwrds,
     .          nwrds1, nwrds8, n24, nchars, nend
c
      parameter (nwrds1 = 2880)
      parameter (nwrds2 = 1440)
      parameter (nwrds4 = 720)
      parameter (nwrds8 = 360)
c
      integer*2 ierr, ilen, lastblnk, i, j, ldata2(nwrds2), maxrec,  
     .		mleft, irec, istch(40), ixmin, iymin, ixmax, iymax, 
     .		blankfits2, blankfits1
      integer*4 ldata4(nwrds4), blankfits4
      integer*2 m1, m2, n0, n80, n255, n273, n352, n355, n357, n366
      real value, zmin, zmax, tscale, tzero, rdata4(nwrds4),
     .     rnan, r_quiet_nan
      logical inquirefile
      character*1 answr, cdata1(nwrds1)
      real*8 rmax2, rmax4, rmax1, rdata8(nwrds8), dnan, d_quiet_nan
c
      parameter (rmax1 = 255.0d0)
      parameter (rmax2 = 32767.0d0)
      parameter (rmax4 = 2147483647.0d0)
      parameter (blankfits4 = -2147483647)
      parameter (blankfits2 = -32767)
      parameter (blankfits1 = 0)
c
      include 'cio.inc'
      include 'mappl.inc'
      include 'mform.inc'
c
      equivalence (istch,stch)
c
      data m1, m2, n0, n80, n255, n273, n352, n355, n357, n366
     .     /-1, -2, 0, 80, 255, 273, 352, 355, 357, 366/
      data n24 /24/
c
      rnan = r_quiet_nan()
      dnan = d_quiet_nan()
c
      call filecomp(filename, fullname)
c
      n1 = nint(mhead(mnaxis1,1))
      n2 = nint(mhead(mnaxis2,1))
      if (n1 .le. 0 .or. n2 .le. 0 .or. n1*n2 .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MWRITE')
c     Make sure that data exists in matrix 0
c
      if (abs(nint(mhead(mbitpix,1))) .eq. 16) then
	nwrds = nwrds2
      else if (abs(nint(mhead(mbitpix,1))) .eq. 32) then
	nwrds = nwrds4
      else if (abs(nint(mhead(mbitpix,1))) .eq. 8) then
	nwrds = nwrds1
      else if (abs(nint(mhead(mbitpix,1))) .eq. 64) then
	nwrds = nwrds8
      else
	call oerror(n273, m1, 'MWRITE: Bad MBITPIX')
      endif
c
      ilen = lastblnk(dirprefix(1))
      headname = dirprefix(1)(1:ilen) // '/sunbin/twodim.mask'
c     Get the file name of FITS template file
c
      open(unit=iiotmp,file=headname,access='sequential',
     .		form='formatted',status='old',iostat=ierr)
      if (ierr .ne. 0) 
     .	call oerror(n352, m1, 'MWRITE: Template FITS-file not found')
c
      rewind(iiotmp,iostat=ierr)
      read(iiotmp,10,iostat=ierr) hmask
10    format(a)
      if (ierr .ne. 0) 
     .	call oerror(n357, m1, 'MWRITE: FITS header-file cannot be read')
      close (iiotmp,iostat=ierr)
c     Reads in FITS header template.  Program must know of any changes to
c     template.
c
      if (inquirefile(fullname)) then
	write(istderr,*) 'File ',filename,' exists....'
	write(istderr,109) 'Do you want to overwrite (y or n [default = n] )? '
109	format(1x,a,$)
	read(istdin,11) answr
11      format(a)
	if (answr .eq. 'n' .or. answr .eq. 'N' .or. answr .eq. ' ') then
	    goto 99
	else
	    ierr = unlink(fullname)
	    if (ierr .ne. 0) call oerror(n366, m2, 'MWRITE')
	endif
      endif
c     Don't overwrite file unless user wants to
c
      open(unit=iiotmp, file=fullname, access='direct', 
     .	form='unformatted',status='new',iostat=ierr,recl=2880)
      if (ierr .ne. 0) 
     .	call oerror(n352, m2, 'MWRITE: FITS file cannot be created')
c
      value = mhead(mblank,1)
      call zlimit(mdata(1), n1, n2, zmin, ixmin, iymin, zmax, ixmax, 
     .		  iymax, value)
      if (nint(mhead(mbitpix,1)) .eq. 16) then
	tscale = (zmax - zmin)/(2*rmax2)
      	tzero = zmin + rmax2*tscale
      else if (nint(mhead(mbitpix,1)) .eq. 32) then
	tscale = (zmax - zmin)/(2*rmax4)
      	tzero = zmin + rmax4*tscale
      else if (nint(mhead(mbitpix,1)) .eq. 8) then
	tscale = (zmax - zmin)/rmax1
      	tzero = zmin
      else if (nint(mhead(mbitpix,1)) .eq. -32 .or.
     .         nint(mhead(mbitpix,1)) .eq. -64) then
	tscale = 1.
      	tzero = 0.
      else
	call oerror(n273, m1, 'MWRITE: Bad MBITPIX')
      endif
c     Calculate max,min, and scale factors from min/max in matrix 0.
c
      write(unit=hmask(2) (24:30),20,iostat=ierr) nint(mhead(mbitpix,1))
      write(unit=hmask(4) (24:30),20,iostat=ierr) nint(mhead(mnaxis1,1))
      write(unit=hmask(5) (24:30),20,iostat=ierr) nint(mhead(mnaxis2,1))
20    format(i7)
c
      hmask(6) (12:19) = cmhead(mtype1,1)
      hmask(10) (12:19) = cmhead(mtype2,1)
      hmask(14) (12:19) = cmhead(mobject,1)
      hmask(14) (20:27) = cmhead(mobject+1,1)
      hmask(15) (12:19) = cmhead(morigin,1)
      call fromdate(n24, nchars, mhead(mdate, 1), sdate)
      hmask(16) (11:11) = '\''
      nend = nchars + 11
      hmask(16) (12:nend) =  sdate(1:nchars)
      nend = nend + 1
      hmask(16) (nend:nend) = '\''
      hmask(20) (12:19) = cmhead(mbunit,1)
c
      write(unit=hmask(7) (15:30),40,iostat=ierr) mhead(mrval1,1)
      write(unit=hmask(8) (15:30),40,iostat=ierr) mhead(mdelt1,1)
      write(unit=hmask(9) (15:30),40,iostat=ierr) mhead(mpix1,1)
      write(unit=hmask(11) (15:30),40,iostat=ierr) mhead(mrval2,1)
      write(unit=hmask(12) (15:30),40,iostat=ierr) mhead(mdelt2,1)
      write(unit=hmask(13) (15:30),40,iostat=ierr) mhead(mpix2,1)
      if (nint(mhead(mbitpix,1)) .eq. 16) then
        write(unit=hmask(17) (15:30),42,iostat=ierr) (blankfits2-1)
      else if (nint(mhead(mbitpix,1)) .eq. 32) then
        write(unit=hmask(17) (15:30),42,iostat=ierr) (blankfits4-1)
      else if (nint(mhead(mbitpix,1)) .eq. 8) then
        write(unit=hmask(17) (15:30),42,iostat=ierr) blankfits1
      else if (nint(mhead(mbitpix,1)) .lt. 0) then
        write(unit=hmask(17), 45,iostat=ierr) ' '
      else
	call oerror(n273, m1, 'MWRITE: Bad MBITPIX')
      endif
      write(unit=hmask(18) (15:30),40,iostat=ierr) tscale
      write(unit=hmask(19) (15:30),40,iostat=ierr) tzero
      write(unit=hmask(21) (15:30),40,iostat=ierr) zmin
      write(unit=hmask(22) (15:30),40,iostat=ierr) zmax
      if (cmhead(mtype1,1)(1:4) .eq. 'RA--' .or.
     .    cmhead(mtype2,1)(1:4) .eq. 'RA--' .or.
     .    cmhead(mtype1,1)(1:4) .eq. 'HA--' .or.
     .    cmhead(mtype2,1)(1:4) .eq. 'HA--' .or.
     .    cmhead(mtype1,1)(1:4) .eq. 'DEC-' .or.
     .    cmhead(mtype2,1)(1:4) .eq. 'DEC-') then
         if (mhead(mequinox,1) .eq. 0) mhead(mequinox,1) = 1950.0
         write(unit=hmask(23) (15:30),40,iostat=ierr) mhead(mequinox,1)
         hcmt = 24
      else
         do 102 i = 23, 34
            hmask(i) = hmask(i+1)
 102     continue
         hmask(35)(9:9) = 'C'
         hcmt = 23
      endif
 40   format(1pe16.9)
 42   format(i16)
 45   format(80a1)
c
      do 101 i = 1, min(7,mheadsize-mcomment+1)
	       hmask(hcmt) (11+(i-1)*8:10+i*8) = cmhead(i-1+mcomment,1) 
101	       continue
c     Prepare the header using the template as a mask.
c
      stch = ' '
      call pwrite(istch,n80)
      stch ='Header = '
      call pwrite(istch,n80)
      do 2 i = 1, hcmt
	stch = hmask(i)
	call pwrite(istch,n80)
2	continue
      stch = ' '
      call pwrite(istch,n80)
c     Echo out the header record
c
      write(iiotmp,rec=1,iostat=ierr) hmask
      if (ierr .ne. 0) 
     .	call oerror(n355, m2, 'MWRITE: FITS file cannot be written to')
c     Write the header first
c	
      maxrec = (n1*n2) / nwrds + 1
      mleft = mod(n1*n2, nwrds)
      if (mleft .ne. 0) maxrec = maxrec + 1
      write(stch,fmt="(i5,a)") maxrec, ' records in FITS file'
      call pwrite(istch,n80)
c     Calculates how many records the data will occupy and how much of the
c	last record the data will fill.
c
      do 92 irec = 2, maxrec
	if (irec .eq. maxrec) then
	   j = mleft
	   do 901 i = mleft+1, nwrds
		if (nint(mhead(mbitpix,1)) .eq. 16) then
		   ldata2(i) = blankfits2 - 1
		else if (nint(mhead(mbitpix,1)) .eq. 32) then
		   ldata4(i) = blankfits4 - 1
		else if (nint(mhead(mbitpix,1)) .eq. 8) then
		   cdata1(i) = char(blankfits1)
		else if (nint(mhead(mbitpix,1)) .eq. -32) then
		   rdata4(i) = rnan
		else if (nint(mhead(mbitpix,1)) .eq. -64) then
		   rdata8(i) = dnan
      		else
	   	   call oerror(n273, m1, 'MWRITE: Bad MBITPIX')
		endif
901		continue
	else
	   j = nwrds
	endif
c	If it is the last record, reset the number of data values to use
c	from the matrix as well as zeroing out unused portion of the
c	last record.
c
	do 91 i = 1, j
	   i4 = long(i) + nwrds*long(irec-2)
	   if (nint(mhead(mbitpix,1)) .eq. 16) then
	   	if (mdata(i4) .eq. sngl(mhead(mblank,1))) then
			ldata2(i) = blankfits2 - 1
	   	else
	        	ldata2(i) = (mdata(i4) - tzero)/tscale
	   	endif
	   else if (nint(mhead(mbitpix,1)) .eq. 32) then
	   	if (mdata(i4) .eq. sngl(mhead(mblank,1))) then
			ldata4(i) = blankfits4 - 1
	   	else
	        	ldata4(i) = (mdata(i4) - tzero)/tscale
	   	endif
	   else if (nint(mhead(mbitpix,1)) .eq. 8) then
	   	if (mdata(i4) .eq. sngl(mhead(mblank,1))) then
			cdata1(i) = char(blankfits1)
	   	else
	        	cdata1(i) = char((mdata(i4)-tzero)/tscale)
	   	endif
	   else if (nint(mhead(mbitpix,1)) .eq. -32) then
	   	if (mdata(i4) .eq. sngl(mhead(mblank,1))) then
			rdata4(i) = rnan
	   	else
	        	rdata4(i) = (mdata(i4)-tzero)/tscale
	   	endif
	   else if (nint(mhead(mbitpix,1)) .eq. -64) then
	   	if (mdata(i4) .eq. sngl(mhead(mblank,1))) then
			rdata8(i) = dnan
	   	else
	        	rdata8(i) = (mdata(i4)-tzero)/tscale
	   	endif
      	   else
	   	call oerror(n273, m1, 'MWRITE: Bad MBITPIX')
	   endif
91	   continue
c	Scale the data (if it is DEFINED) from R*4 to I*2/4 and store in
c	 output buffer
c
	if (nint(mhead(mbitpix,1)) .eq. 16) then
		write(iiotmp,rec=irec,iostat=ierr) ldata2
	else if (nint(mhead(mbitpix,1)) .eq. 32) then
		write(iiotmp,rec=irec,iostat=ierr) ldata4
	else if (nint(mhead(mbitpix,1)) .eq. 8) then
		write(iiotmp,rec=irec,iostat=ierr) cdata1
	else if (nint(mhead(mbitpix,1)) .eq. -32) then
		write(iiotmp,rec=irec,iostat=ierr) rdata4
	else if (nint(mhead(mbitpix,1)) .eq. -64) then
		write(iiotmp,rec=irec,iostat=ierr) rdata8
      	else
	   	call oerror(n273, m1, 'MWRITE: Bad MBITPIX')
	endif
	if (ierr .ne. 0)
     .	   call oerror(n355,m2,'MWRITE: Cannot write to FITS file')
92	continue
c     Write the records out.
c
      close(iiotmp,iostat=ierr)
c
99    return
c
      end
c
