      subroutine plot(nchan)
c
c     @(#)plot.f	5.2 08/21/96
c
c     Plots YDATA versus XDATA for NCHAN values of XDATA and YDATA
c
c
      include 'core.inc'
      include 'appl.inc'
c
      integer*2 nchan, iplt, ix, i, ipuf(4), ilen, lastblnk, ier, iy,
     .		short, mint, idely, nxl, nyl, itemp, ipixel, sclc, idelx
      integer*2 m2, n1, n2, n5, n8, n12, n14, n20, n30, n33, 
     .          n55, n219, n220, n10, curcur, npp1
      character*10 getformat, format
      integer*4 ibx, iby, ix4, iy4
      character*8 cpuf
      character*2 ctemp
      real*4 ybtm, ytop, ylog, yval, dy, ymax1, ylow2, fupy, xmx, xmn,
     .	     fy1, dfy1, ymax2, ymin1, fy2, dfy2, ymin2, yf, yl
      real*4 xlft, xrght, xlog, xval, dx, xmax1,xlow2, fupx, ymn, ymx,
     .	     fx1, dfx1, xmax2, xmin1, fx2, dfx2, xmin2, xf, xl, aa
      logical okreal4, placenxt
c
      data m2, n1, n2, n5, n8, n12, n14, n20, n30, n33, 
     .          n55, n219, n220, n10
     .     /-2, 1, 2, 5, 8, 12, 14, 20, 30, 33, 55, 219, 220, 10/
c
      equivalence (cpuf, ipuf), (itemp, ctemp)
c
      mint(aa) = nint(aa-0.4999999)
      sclc(ipixel) = max(n1, nint(sclchar*float(ipixel))) 
c     Truncation and scaling functions
c
      npp1 = curcur()
      IX0=XORG(npp1) + sclfctx*140
      IY0=YORG(npp1) + sclfcty*150
      IXM=XMAX(npp1) - sclfctx*40
      IYM=YMAX(npp1) - sclfcty*60
c     Plot limits
c
      IPLT=SPLT
c     iplt = plot type (LINE or POINTS)
c
c     Plot Border
c
      CALL PLACE (IX0,IY0)
      CALL VCTR	 (IXM,IY0)
      CALL VCTR	 (IXM,IYM)
      CALL VCTR	 (IX0,IYM)
      CALL VCTR	 (IX0,IY0)
c
c     Calculate and Draw X - tick marks
c
      xlft = xmnp
      xrght = xmxp  
      if (xlft .eq. xrght) then
	  xlft = 1.e34
	  xrght = -1.e34
	  do 802 i = 1, nchan
	     if (okreal4(xdata(i))) then
	 	 xlft = min(xlft, xdata(i))
	 	 xrght = max(xrght, xdata(i))
	     endif
802	     continue
      endif
c     XLFT, XRGHT = The minimum and maximum plot limits either specified
c     by the user or calculated from the min/max of the values of XDATA.
c
      if (xlog .gt. 0.5) then
	xmax1 = max(xlft, xrght)
	xmin1 = min(xlft, xrght)
	if(xmin1 .lt. 0) call oerror(n220, m2, 'PLOT')
	nxl = 9
	fx1 = 10.**(mint(alog10(xmax1)))
	fx2 = 10.**(mint(alog10(xmin1)))
c
8019	  continue
	  dx = 9.0 / nxl
	  dfx1 = fx1 * dx
	  dfx2 = fx2 * dx
	  xmax2 = xmax1 - mod(xmax1-fx1, dfx1)
	  if (xmax2 .lt. xmax1) xmax2 = xmax2 + dfx1
	  xmin2 = xmin1 - mod(xmin1 - fx2, dfx2)
	  limx = nxl * nint( alog10(fx1/fx2)) + nint( (xmax2-fx1)/dfx1) -
     .		nint( (xmin2-fx2)/dfx2)
	  if (limx .lt. 3) then
	     nxl = 2 * nxl
	     goto 8019
	  else if (limx .gt. 10 .and. nxl .gt. 1) then
	     nxl = nxl / 3
	     goto 8019
	  endif
c
	if (xlft .gt. xrght) then
	   xrght = xmin2
	   xlft = xmax2
	else
	   xrght = xmax2
	   xlft = xmin2
	endif
c
        xlow2 = min(xlft, xrght)
	xmx = alog10(xrght)
	xmn = alog10(xlft)
	fupx = fx2*10.
	limx = min(maxregn, limx)
c
      else
c
	call tickmrk( xlft, xrght, n10, xf, xl, dx, limx)
	xmx = xrght
	xmn = xlft
c
      endif
c
      ax(npp1) = float(ixm-ix0)/(xmx - xmn)
      bx(npp1) = ix0 - xmn*ax(npp1)
c     Store away AX, BX, etc. for use by other routines.
c
      idelx = 1
29    if ( idelx*(ixm - ix0 + 1)/(limx+1) .le. sclchar*10*14 .and. 
     .     idelx .lt. limx-1) then
	idelx = idelx * 2
	goto 29
      endif
c     Prevents overlapping x labels
c
      do 805 i = 1, limx
	if (xlog .gt. 0.5) then
	   xlow2 = xlow2 + dfx2
	   if (abs(fupx-xlow2) .lt. 0.5*dfx2) then
		xlow2 = fupx
		fupx = fupx*10.
		dfx2 = dfx2*10.
	   endif
	   ix = nint(alog10(xlow2)*ax(npp1)+bx(npp1))
	else
	   xlow2 = xf + dx*(i-1)
	   ix = nint(xlow2*ax(npp1)+bx(npp1))
	endif
c	Bump up the value for XLOW2 for next tick mark
c
	rixt(i) = ix
	call place(ix, iy0)
	call vctr(ix, iy0+sclc(n12))
	call place(ix, iym)
	call vctr(ix, iym - sclc(n12))
c
	if (slabel .gt. 0.0 .and. sclchar .ne. 1.) call charsize(sclchar) 
c
	if (slabel .gt. 0.0 .and. mod(i-1,idelx) .eq. 0) then
c	   Label tick mark if desired and label wont overlap previous one.
c
	   call place(ix, iy0)
	   call vctr(ix, iy0+sclc(n20))
	   call place(ix, iym)
	   call vctr(ix, iym - sclc(n20))
           format = getformat(xlow2, 2.*xlow2, n8)
	   write(cpuf, format,iostat=ier) xlow2
	   call place(ix-sclc(n8*n14/n2), iy0-sclc(n30))
	   call pchar(ipuf, n8)
        endif
c	Draw tick marks and x-axis labels
c
805     continue
c
c     Calculate and Draw Y - tick marks
c
      ybtm = ymnp
      ytop = ymxp  
      if (ybtm .eq. ytop) then
	  ybtm = 1.e34
	  ytop = -1.e34
	  do 801 i = 1, nchan
	     if (okreal4(ydata(i))) then
	 	 ybtm = min(ybtm, ydata(i))
	 	 ytop = max(ytop, ydata(i))
	     endif
801	     continue
      endif
c     Do the same for the y-axis
c
      if (ylog .gt. 0.5) then
	ymax1 = max(ybtm, ytop)
	ymin1 = min(ybtm, ytop)
	if(ymin1 .lt. 0) call oerror(n219, m2, 'PLOT')
	nyl = 9
	fy1 = 10.**(mint(alog10(ymax1)))
	fy2 = 10.**(mint(alog10(ymin1)))
c
8020	  continue
	  dy = 9.0 / nyl
	  dfy1 = fy1 * dy
	  dfy2 = fy2 * dy
	  ymax2 = ymax1 - mod(ymax1-fy1, dfy1)
	  if (ymax2 .lt. ymax1) ymax2 = ymax2 + dfy1
	  ymin2 = ymin1 - mod(ymin1 - fy2, dfy2)
	  limy = nyl * nint( alog10(fy1/fy2)) + nint( (ymax2-fy1)/dfy1) -
     .		nint( (ymin2-fy2)/dfy2)
	  if (limy .lt. 3) then
	     nyl = 2 * nyl
	     goto 8020
	  else if (limy .gt. 10 .and. nyl .gt. 1) then
	     nyl = nyl / 3
	     goto 8020
	  endif
c
	if (ybtm .gt. ytop) then
	   ytop = ymin2
	   ybtm = ymax2
	else
	   ytop = ymax2
	   ybtm = ymin2
	endif
c
        ylow2 = min(ybtm, ytop)
	ymx = alog10(ytop)
	ymn = alog10(ybtm)
	fupy = fy2*10.
	limy = min(maxregn, limy)
c
      else
c
	call tickmrk( ybtm, ytop, n10, yf, yl, dy, limy)
	ymx = ytop
	ymn = ybtm
c
      endif
c
      ay(npp1) = float(iym-iy0)/(ymx - ymn)
      by(npp1) = iy0 - ymn*ay(npp1)
c     Store away AY, BY, etc. for use by other routines.
c
      idely = 1
39    if ( idely*(iym - iy0 + 1)/(limy+1) .le. sclchar*25 .and. 
     .     idely .lt. limy-1) then
	idely = idely * 2
	goto 39
      endif
c     Prevent overlapping y labels
c
      do 806 i = 1, limy
	if (ylog .gt. 0.5) then
	   ylow2 = ylow2 + dfy2
	   if (abs(fupy-ylow2) .lt. 0.5*dfy2) then
		ylow2 = fupy
		fupy = fupy*10.
		dfy2 = dfy2*10.
	   endif
	   iy = nint(alog10(ylow2)*ay(npp1)+by(npp1))
	else
	   ylow2 = yf + dy*(i-1)
	   iy = nint(ylow2*ay(npp1)+by(npp1))
	endif
c	Bump up the value for YLOW2 for next tick mark
c
	riyt(i) = iy
	call place(ix0, iy)
	call vctr(ix0+sclc(n12), iy)
	call place(ixm, iy)
	call vctr(ixm-sclc(n12), iy)
c
	if (slabel .gt. 0.0 .and. mod(i-1,idely) .eq. 0) then
	   call place(ix0, iy)
	   call vctr(ix0+sclc(n20), iy)
	   call place(ixm, iy)
	   call vctr(ixm-sclc(n20), iy)
           format = getformat(ylow2,2.*ylow2, n8)
	   write(cpuf, format,iostat=ier) ylow2
	   call place(ix0-sclc(n8*n14), iy-sclc(n12))
	   call pchar(ipuf, n8)
  	endif
c	Draw tick marks and y-axis labels
c
806     continue
c
C     Plot Spectra
c
      placenxt = .true.
      do 803 i = 1, nchan
	xval = xdata(i)
	yval = ydata(i)
	if (.not. okreal4(xval) .or. .not. okreal4(yval)) then
		placenxt = .true.
		goto 803
	endif
	if (xlog .gt. 0.5) xval = alog10(xval)
	if (ylog .gt. 0.5) yval = alog10(yval)
	ix4 = xval*ax(npp1)+bx(npp1)
	iy4 = yval*ay(npp1)+by(npp1)
	if (ix4 .ge. ix0 .and. ix4 .le. ixm .and. iy4 .ge. iy0
     .	    .and. iy4 .le. iym) then
           ix = ix4
           iy = iy4
	   if (iplt .eq. 0) then
	      if (placenxt) then
		call placewp(ix, iy, n33)
	      else
		call vctrwp(ix, iy, n33, sclchar, 0.)
	      endif
	   else
		call place(ix-sclc(n5), iy)
		call vctr(ix+sclc(n5), iy)
		call place(ix, iy-sclc(n5))
		call vctr(ix, iy+sclc(5))
	   endif
	else
           ix = max(ix0,min(ix4, float(ixm)))
           iy = max(iy0,min(iy4, float(iym)))
	   call placewp(ix, iy, n33)
	endif
      	placenxt = .false.
803	continue
c
      if (slabel .gt. 0.0) then
	ilen = lastblnk(ctitle)
        if (ilen .gt.0) then
	  call place( (ixm+ix0)/n2-sclc(ilen*n14/n2), iym+sclc(n5))
	  call pchar(ctitle, ilen)
        endif
        ilen = lastblnk(cxtitle)
        if (ilen .gt. 0) then	
	  call place( (ixm+ix0)/n2-sclc(ilen*n14/n2), iy0-sclc(n55))
	  call pchar(cxtitle, ilen)
        endif
        ilen = lastblnk(cytitle)
        if (ilen .gt. 0) then
	    call place(ix0-sclc(n8*n14), (iy0+iym-sclc(n14*ilen))/n2)
	    call vchar(cytitle, ilen, .true.)
807	    continue
        endif
	if (sclchar .ne. 1.) call charsize(1.)
      endif
c     Write out X, Y and plot labels.
c
      IF (ZLLINE.ge.1.0 .and. ylog .lt. 0.5) then
	 IBY=nint(by(npp1))
	 if (iby .ge. iy0 .and. iby .le. iym) then
	   CALL PLACE (IX0,short(IBY))
	   CALL VCTR  (IXM,short(IBY))
	 endif
      endif
c     Draw y=0 line
c
      DO 40 I = 1, maxmrk
	if (hmark(i) .le. -999999.0) goto 401
	 if (ylog .lt. 0.5) then
	     IbY = hmark(i)*ay(npp1)+by(npp1) + 0.5
	 else
	     if (hmark(i) .gt. 0.) then
		iby = alog10(hmark(i))*ay(npp1)+by(npp1) + 0.5
	     else
		iby = 0
	     endif
	 endif
	 if (iby .ge. iy0 .and. iby .le. iym) then
            CALL PLACE(IX0,short(IbY))
            CALL VCTR (IXM,short(IbY))
	 endif
  40     CONTINUE
c        Draw temperatue markers
c
401    DO 41 I = 1, maxmrk
	 if (cmark(i) .le. -999999.0) goto 411
	 if (xlog .lt. 0.5) then
	     IbX = cmark(i)*ax(npp1)+bx(npp1) + 0.5
	 else
	     if (cmark(i) .gt. 0.) then
		ibx = alog10(cmark(i))*ax(npp1)+bx(npp1) + 0.5
	     else
		ibx = 0
	     endif
	 endif
	 if (ibx .ge. ix0 .and. ibx .le. ixm) then
	    CALL PLACE (short(IbX),IYM)
	    CALL VCTR  (short(IbX),IY0)
	 endif
  41     CONTINUE
c        Draw channel markers
c
411   continue
c
99    return
      end
