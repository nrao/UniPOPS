      real function cube(ii,jj,kk)
c
c     @(#)cube.f	5.4 05/04/98
c
c     Returns the value of the (II,JJ,KK) pixel in the CUBE on device IDEV.
c
      integer*2 ii, jj, kk, nwrds4, nwrds2, nwrds, nwrds1, nwrds8
c
      parameter (nwrds1 = 2880)
      parameter (nwrds2 = 1440)
      parameter (nwrds4 = 720)
      parameter (nwrds8 = 360)
c
      integer*2 oldrec, record, ldata2(nwrds2), blankfits2, ierr
      character*1 cdata1(nwrds1)
      real rdata4(nwrds4)
      double precision rdata8(nwrds8)
      integer*2 m1, n357, n365
      integer*4 l1, long, ldata4(nwrds4)
      integer*4 iloc, position, blankfits1, blankfits4
      logical okreal4, okreal8
c
      parameter (blankfits4 = -2147483647)
      parameter (blankfits2 = -32767)
      parameter (blankfits1 = 0)
cc
      include 'mappl.inc'
      include 'mform.inc'
      include 'cio.inc'
c
      data m1, n357, n365 / -1, 357, 365/
      data l1 /1/
c
      data oldrec/0/
c     OLDREC = value of last record read in
c
      iloc = long(ii) + nint(chead(cnaxis1))* ( long(jj-1) 
     .           + nint(chead(cnaxis2))* long(kk-1) )
c
      if ( abs(nint(chead(cbitpix))) .eq. 16) then
	nwrds = nwrds2
      else if ( abs(nint(chead(cbitpix))) .eq. 32) then
	nwrds = nwrds4
      else if ( abs(nint(chead(cbitpix))) .eq. 8) then
	nwrds = nwrds1
      else if ( abs(nint(chead(cbitpix))) .eq. 64) then
	nwrds = nwrds8
      else
	call oerror(n365, m1, 'Bad FITS header: Bad BITPIX')
      endif
c
      record = cnumhead + (iloc-l1) / nwrds + l1
c
      if (oldrec .ne. record) then
	if (nint(chead(cbitpix)) .eq. 16) then
	    read(icube,rec=record,iostat=ierr) ldata2
	else if (nint(chead(cbitpix)) .eq. 32) then
	    read(icube,rec=record,iostat=ierr) ldata4
	else if (nint(chead(cbitpix)) .eq. -32) then
	    read(icube,rec=record,iostat=ierr) rdata4
	else if (nint(chead(cbitpix)) .eq. -64) then
	    read(icube,rec=record,iostat=ierr) rdata8
	else if (nint(chead(cbitpix)) .eq. 8) then
	    read(icube,rec=record,iostat=ierr) cdata1
	else
	    call oerror(n365, m1, 'Bad FITS header: Bad BITPIX')
	endif
	if (ierr .ne. 0) call oerror(n357, m1, 'Cannot read from FITS file')
	oldrec = record
      endif
c
      position = mod(iloc-l1, nwrds) + l1
      if (nint(chead(cbitpix)) .eq. 16) then
	if (ldata2(position) .eq. fcblank) then
	   cube = chead(cblank)
      	else
	   cube = float(ldata2(position))*chead(ctscale) + chead(ctzero)
      	endif
      else if (nint(chead(cbitpix)) .eq. 32) then
	if (ldata4(position) .eq. fcblank) then
	   cube = chead(cblank)
      	else
	   cube = float(ldata4(position))*chead(ctscale) + chead(ctzero)
      	endif
      else if (nint(chead(cbitpix)) .eq. 8) then
	if (ichar(cdata1(position)) .eq. fcblank) then
	   cube = chead(cblank)
      	else
	   cube = float(ichar(cdata1(position)))*chead(ctscale) + chead(ctzero)
      	endif
      else if (nint(chead(cbitpix)) .eq. -32) then
	if ( .not. okreal4(rdata4(position))) then
	   cube = chead(cblank)
      	else
	   cube = rdata4(position)*chead(ctscale) + chead(ctzero)
      	endif
      else if (nint(chead(cbitpix)) .eq. -64) then
	if ( .not. okreal8(rdata8(position))) then
	   cube = chead(cblank)
      	else
	   cube = rdata8(position)*chead(ctscale) + chead(ctzero)
      	endif
      else
	   call oerror(n365, m1, 'Bad FITS header: Bad BITPIX')
      endif
c
      return
      end
c
