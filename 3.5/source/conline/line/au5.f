      SUBROUTINE AU5 (J)
C-------------------------------------------------------------------------------
C  @(#)au5.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     au5 - mtable, mheader, mblank, mcopy, mmove, mremove, minit, mrange,
c		hlftne, label, setlev, autolev, raster, setclut, contour,
c		mread, mwrite, readcube, slice, mlimits, cheader, mfft, mifft,
c		holwindow, holfitfoc, eqhistlev, funclev, putrow, putcol, 
c		getrow, getcol, mplus, mminus, mmultiply, mdivide, mscale,
c               mbias
c
C-----------------------------------------------------------------------
c
      integer*2 j, iptwh, iarrayin, iarrayout, i1, i2, j1, j2, irealp,
     .		ierr, jj, numlevs, loc, itype, isize, curcur, 
     .		itable, face, i3
      integer*2 m1, m2, m3, n0, n80, n112, n120, n254, n255, n256, n257,
     .          n258, n259, n264, n296, nconj, n272, n1, n2, n3, n4, n5,
     .		n267, n252, n274, n275
      integer*2 is1, n25, n780, ixmin, iymin, ixmax, iymax, ntick, col, 
     .		row
      integer*4 istart, istop, numx, numy, istart1, i, x1, x2 
      integer*4 l1, long, ilct, numx1, numy1, numtot, numtot1
      real value, zmin, zmax, zdiff, percents(32), mratio1, cfactor,
     .	   mtemp, rstch(10), xmn, xmx, ymn, ymx, nyqrate, dishdia,
     .     focalpt, z1, z2, func(32), rinfinity, hues(32), sats(32), 
     .     shds(32), undef0, undef1, value0, value1, sfact
      double precision xtochan, chantox
      logical quick, flipx, flipy, invertxy, invers, okreal8, okreal4
      character*80 stch
c
      include 'cio.inc'
      include 'core.inc'
      INCLUDE 'mappl.inc'
      INCLUDE 'appl.inc'
      INCLUDE 'stk.inc'
      INCLUDE 'cform.inc'
      INCLUDE 'mform.inc'
c
      equivalence (rstch,stch)
c
      data m1, m2, m3, n0, n80, n112, n120,n252,n254,n255,n256,n257,
     .          n258, n259, n264, n267, n296, n272, n1, n2, n3, n4, n5
     .    /-1, -2, -3, 0, 80, 112, 120, 252, 254, 255, 256, 257, 258,
     .     259, 264, 267, 296, 272, 1, 2, 3, 4, 5/
      data is1, n25, n780 /1, 25, 780/
      data n274, n275 /274, 275/
c
      data l1 /1/
c
C=======================================================================
C
      IPTWH = 1
c
      imin = nint(mxmin)
      if (imin .lt. 1 .or. imin .gt. nint(mhead(mnaxis1,iptwh)))
     .		 imin = 1
      jmin = nint(mymin)
      if (jmin .lt. 1 .or. jmin .gt. nint(mhead(mnaxis2,iptwh))) 
     .		jmin = 1
      imax = nint(mxmax)
      if (imax .lt. 1 .or. imax .gt. nint(mhead(mnaxis1,iptwh))) 
     .			imax = nint(mhead(mnaxis1,iptwh))
      jmax = nint(mymax)
      if (jmax .lt. 1 .or. jmax .gt. nint(mhead(mnaxis2,iptwh))) 
     .			jmax = nint(mhead(mnaxis2,iptwh))
c
      JJ=J
      GO TO (10,20,30,40,50,60,70,80,90,100,110,120,130,140,150,
     .	     160,170,180,190,200,210,220,230,240,250,260,270,280,
     .	     290,300,310,320,330,340,350,360,370,380,390,400,410,
     .       420), JJ
      call oerror(n120, m3, 'AU5')
      GO TO 99
c
c------------------------------------------------------------------------
c     MTABLE
c------------------------------------------------------------------------
10    continue
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MTABLE')
C     Make sure matrix 0 has been initialized.
c
      call mtable(mdata(istart),numx,numy)
c
      goto 99
c
c------------------------------------------------------------------------
c     MHEADER
c------------------------------------------------------------------------
20    continue
c
      call mheader(iptwh)
c
      goto 99
c
c------------------------------------------------------------------------
c     MBLANK
c------------------------------------------------------------------------
30    continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'MBLANK: One needed')
c
      iarrayin = nint(v(sp)) + 1
      sp = sp - 1
c
      if (iarrayin .lt. 1 .or. iarrayin .gt. mnumarrays) 
     .		call oerror(n112, m2, 'MBLANK')
c     User has specified the IARRAYIN matrix; check it's value
c
      istart = long(iarrayin-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iarrayin))
      numy = nint(mhead(mnaxis2,iarrayin))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n256, m2, 'MBLANK')
c     Make sure matrix specified has been initialized.
c
      istop = istart + numx*numy - 1
      value = mhead(mblank,iarrayin)
c
      do 32 i = istart, istop
	mdata(i) = value
32	continue
c     Assign undefined values to all elements of matrix
c
      goto 99
c
c------------------------------------------------------------------------
c     MCOPY
c------------------------------------------------------------------------
40    continue
c
      if (sp .lt. 2) call oerror(n112, m1, 'MCOPY: Two needed')
c
      iarrayout = nint(v(sp)) + 1
      iarrayin = nint(v(sp-1)) + 1
      sp = sp - 2
c
      if (iarrayin .lt. 1 .or. iarrayin .gt. mnumarrays .or.
     1    iarrayout .lt. 1 .or. iarrayout .gt. mnumarrays) 
     2    call oerror(n112, m2, 'MCOPY')
      if (iarrayin .eq. iarrayout) call oerror(n257, m2, 'MCOPY')
c     User has specified IRARRAYIN and OUT matrices for copying; check
c	their values
c
      istart = long(iarrayin-is1)*mdatasize/mnumarrays + l1
      istart1 = long(iarrayout-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iarrayin))
      numy = nint(mhead(mnaxis2,iarrayin))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n256, m2, 'MCOPY')
c     Make sure the input matrix has been initialized
c
      istop = istart + numx*numy - 1
c
      do 43 i = 1, mheadsize
	mhead(i,iarrayout) = mhead(i,iarrayin)
43	continue
      do 42 i = istart, istop
	mdata(i-istart+istart1) = mdata(i)
42	continue
c     Copy matrix header and data
c
      goto 99
c
c------------------------------------------------------------------------
c     MMOVE
c------------------------------------------------------------------------
50    continue
c
      if (sp .lt. 2) call oerror(n112, m1, 'MMOVE: Two needed')
c
      iarrayout = nint(v(sp)) + 1
      iarrayin = nint(v(sp-1)) + 1
      sp = sp - 2
c
      if (iarrayin .lt. 1 .or. iarrayin .gt. mnumarrays .or.
     1    iarrayout .lt. 1 .or. iarrayout .gt. mnumarrays) 
     2    call oerror(n112, m2, 'MMOVE')
      if (iarrayin .eq. iarrayout) call oerror(n257, m2, 'MMOVE')
c     User has specified IRARRAYIN and OUT matrices for moving; check
c	their values
c
      istart = long(iarrayin-is1)*mdatasize/mnumarrays + l1
      istart1 = long(iarrayout-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iarrayin))
      numy = nint(mhead(mnaxis2,iarrayin))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n256, m2, 'MMOVE')
c     Make sure the input matrix has been initialized
c
      istop = istart + numx*numy - 1
c
      do 53 i = 1, mheadsize
	mhead(i,iarrayout) = mhead(i,iarrayin)
53	continue
      do 52 i = istart, istop
	mdata(i-istart+istart1) = mdata(i)
52	continue
c     Copy matrix header and data
c
      call mraz(iarrayin)
c     Now zero out original matrix
c
      goto 99
c
c------------------------------------------------------------------------
c     MREMOVE
c------------------------------------------------------------------------
60    continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'MREMOVE: One needed')
c
      iarrayin = nint(v(sp)) + 1
      sp = sp - 1
c
      if (iarrayin .lt. 1 .or. iarrayin .gt. mnumarrays) 
     .		call oerror(n112, m2, 'MREMOVE')
c     User has specified IRARRAYIN matrix for removing; check
c	its values
c
      call mraz(iarrayin)
c
c
      goto 99
c
c------------------------------------------------------------------------
c     MINIT
c------------------------------------------------------------------------
70    continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'MINIT: One needed')
c
      iarrayin = nint(v(sp)) + 1
      sp = sp - 1
c
      if (iarrayin .lt. 1 .or. iarrayin .gt. mnumarrays) 
     .		call oerror(n112, m1, 'MINIT')
c     User has specified IRARRAYIN matrix for initializing; check
c	its values
c
      call minit(iarrayin,.true.)
c
      goto 99
c
c------------------------------------------------------------------------
c     MRANGE
c------------------------------------------------------------------------
80    continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'MRANGE: Four needed')
c
      if (.not. okreal8(v(sp)) .and. .not. okreal8(v(sp-1)) ) then
	  mymax = nint(mhead(mnaxis2,iptwh)) + 1
	  mymin = 0
      else if (.not. okreal8(v(sp)) ) then
	  mymax = nint(mhead(mnaxis2,iptwh)) + 1
          mymin = nint(xtochan(m1, mhead(1,1), sngl(v(sp-1)), ierr))
          if (ierr .ne. 0) call oerror(n272, m2, 'MRANGE')
      else if (.not. okreal8(v(sp-1)) ) then
      	  mymax = nint(xtochan(m1, mhead(1,1), sngl(v(sp)), ierr))
          if (ierr .ne. 0) call oerror(n272, m2, 'MRANGE')
          mymin = 0
      else
         j2 = nint(xtochan(m1, mhead(1,1), sngl(v(sp)), ierr))
         if (ierr .ne. 0) call oerror(n272, m2, 'MRANGE')
         j1 = nint(xtochan(m1, mhead(1,1), sngl(v(sp-1)), ierr))
         mymin = min(j1, j2)
         mymax = max(j1, j2)
      endif
      sp = sp -2
c
      if (.not. okreal8(v(sp)) .and. .not. okreal8(v(sp-1)) ) then
	  mxmax = nint(mhead(mnaxis1,iptwh)) + 1
	  mxmin = 0
      else if (.not. okreal8(v(sp)) ) then
	  mxmax = nint(mhead(mnaxis1,iptwh)) + 1
          mxmin = nint(xtochan(m2, mhead(1,1), sngl(v(sp-1)), ierr))
          if (ierr .ne. 0) call oerror(n272, m2, 'MRANGE')
      else if (.not. okreal8(v(sp-1)) ) then
      	  mxmax = nint(xtochan(m2, mhead(1,1), sngl(v(sp)), ierr))
          if (ierr .ne. 0) call oerror(n272, m2, 'MRANGE')
          mxmin = 0
      else
         i2 = nint(xtochan(m2, mhead(1,1), sngl(v(sp)), ierr))
         if (ierr .ne. 0) call oerror(n272, m2, 'MRANGE')
         i1 = nint(xtochan(m2, mhead(1,1), sngl(v(sp-1)), ierr))
         mxmin = min(i1, i2)
         mxmax = max(i1, i2)
      endif
      sp = sp -2
c     Get the user's input parameters; arrange in increasing order regardless
c     of how the user has entered them.  Watch out and trap for default
c     values.
c
      if (mxmin .le. 0.5) then
	mxmin = 1.
	call oerror(n258, n0, 'MRANGE: Setting MXMIN to 1')
      endif
      if (mymin .le. 0.5) then
	mymin = 1.
	call oerror(n258, n0, 'MRANGE: Setting MYMIN to 1')
      endif
      if (mxmax .gt. mhead(mnaxis1,iptwh) .or. mxmax .le. 0.5) then
	mxmax = nint(mhead(mnaxis1,iptwh))
        write(stch,71,iostat=ierr) nint(mxmax)
71	format(i10)
	call oerror(n258, n0, 'MRANGE:  Setting MXMAX to ' // stch(1:10))
      endif
      if (mymax .gt. mhead(mnaxis2,iptwh) .or. mymax .le. 0.5) then
	mymax = nint(mhead(mnaxis2,iptwh))
        write(stch,71,iostat=ierr) nint(mymax)
	call oerror(n258, n0, 'MRANGE:  Setting MYMAX to ' // stch(1:10))
      endif
      if (mxmin .eq. mxmax) then
	mxmin = 1.
	mxmax = nint(mhead(mnaxis1,iptwh))
	call oerror(n258, n0, 'MRANGE: Setting MXMIN to 1')
        write(stch,71,iostat=ierr) nint(mxmax)
	call oerror(n258, n0, 'MRANGE:  Setting MXMAX to ' // stch(1:10))
      endif
      if (mymin .eq. mymax) then
	mymin = 1.
	mymax = nint(mhead(mnaxis2,iptwh))
	call oerror(n258, n0, 'MRANGE: Setting MYMIN to 1')
        write(stch,71,iostat=ierr) nint(mymax)
	call oerror(n258, n0, 'MRANGE:  Setting MYMAX to ' // stch(1:10))
      endif
c     If any input parameter is out of range, reset to default value
c	(either 1 or max size of dimension of matrix 0).
c
      goto 99
c
c------------------------------------------------------------------------
c     HLFTNE
c------------------------------------------------------------------------
90    continue
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'HLFTNE')
C     Make sure matrix 0 has been initialized.
c
      value = mhead(mblank,iptwh)
      call zerror(mdata(istart),numx,numy,value,ierr)
c
      if (ierr .ge. 1024) 
     .		call oerror(n259,m2,'HLFTNE: All < Min of Matrix')
      if (ierr .ge. 512) 
     .		call oerror(n259,m2,'HLFTNE: Not in ascending order')
      if (ierr .ge. 256) 
     .		call oerror(n259,m2,'HLFTNE: All LEVS <= -999999')
      if (ierr .ne. 0) call oerror(n254,m2,'HLFTNE')
c     Check LEVS, IMIN, IMAX, ..., and matrix for errors
c
      if (mratio .le. 0.) call oerror(n296, m2, 'HLFTNE')
c
      do 91 numlevs = 1, maxregn-1
	if (levs(numlevs+1) .le. -999999) goto 92
91	continue
      numlevs = maxregn
92    percents(1) = 0.0
      do 93 i = 2, numlevs
	percents(i) = (float(i-1)/float(numlevs-1))**2
93	continue
c     Find number of levels and calculate percentages of half toning
c
      mratio1 = mratio*float(imax-imin+1)/float(jmax-jmin+1)
      if (mratio1 .ge. 1.) then
	IX0=XORG(curcur()) + sclfctx*140
        IXM=XMAX(curcur()) - sclfctx*269
        IYM=YMAX(curcur()) - sclfcty*60
        IY0=IYM - (IXM-IX0 + 1)/MRATIO + 1
      else
	IY0=YORG(curcur()) + sclfcty*125
        IYM=YMAX(curcur()) - sclfcty*60
	IX0=XORG(curcur()) + sclfctx*140
        IXM=IX0 + (IYM-IY0 + 1)*MRATIO - 1
      endif
      ax(curcur()) = float(ixm-ix0)/float(imax-imin)
      bx(curcur()) = float(ix0) - float(imin)*ax(curcur())
      ay(curcur()) = float(iym-iy0)/float(jmax-jmin)
      by(curcur()) = float(iy0) - float(jmin)*ay(curcur())
c
      value = mhead(mblank,iptwh)
      call hlftne(mdata(istart),numx,numy,levs,percents,numlevs,value)
c
      call str4cur(n2)
c
      goto 98
c
c------------------------------------------------------------------------
c     LABEL
c------------------------------------------------------------------------
100   continue
c
      if (showplot(numplots) .le. 1) call oerror(n264, m2, 'LABEL')
c
      call mlabel(showplot(numplots))
c
      goto 99
c
c------------------------------------------------------------------------
c     SETLEV
c------------------------------------------------------------------------
110   continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'SETLEV: Four needed')
c
      itype = nint(v(sp))
      numlevs = nint(v(sp-1))
      zdiff = v(sp-2)
      zmin = v(sp-3)
      sp = sp - 4
c     ZMIN = minimum contour level
c     NUMLEVS = number of levels
c     ITYPE = true (>=0) for linera levels else logarithmic
c     ZDIFF = diff between levels if linear, fctor between levels if log
c
      if (numlevs .lt. 2 .or. numlevs .gt. maxregn) then
	call oerror(n112, n0, 'SETLEV: Using maximum number of levels')
	numlevs = maxregn
      endif
c     If NUMLEVS out of range, use MAXREGN levels
c
      if (zdiff .le. 0) call oerror(n112, m2, 'SETLEV: Increment <= 0')
      if (itype .lt. 0) then
	if (zmin .eq. 0) then
	   call oerror(n112, m2, 'SETLEV: For logs, min. cannot = 0')
        else if (zmin .gt. 0 .and. zdiff .le. 1.) then
     	   call oerror(n112, m2,
     .		 'SETLEV: For logs, inc. must be > 1 if Min > 0')
        else if (zmin .lt. 0 .and. zdiff .ge. 1.) then
     	  call oerror(n112, m2, 
     .		'SETLEV: For logs, inc. must be < 1 if Min < 0')
	endif
      endif
c     make sure inputs are correct for log and linear contour levels
c
      do 111 i = 1, numlevs
        if (itype .ge. 0) then
		levs(i) = zmin + zdiff*(i-1)
	else
		levs(i) = zmin*(zdiff**(i-1))
	endif
111     continue
c     Calculate levels
c
      do 112 i = numlevs+1, maxregn
	levs(i) = -999999.
112	continue
c     Void remaining levels
c
      goto 99
c
c------------------------------------------------------------------------
c     AUTOLEV
c------------------------------------------------------------------------
120   continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'AUTOLEV: One needed')
c
      numlevs = nint(v(sp))
      sp = sp - 1
      if (numlevs .lt. 2 .or. numlevs .gt. maxregn) then
	call oerror(n112, n0, 'AUTOLEV: Using maximum number of levels')
	numlevs = maxregn
      endif
c     NUMLEVS = number of desired contour levels; reset to MAXREGN if
c	bad value.
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'AUTOLEV')
c     Make sure that data exists in matrix 0
c
      value = mhead(mblank,iptwh)
      call zlimit(mdata(1), numx, numy, zmin, ixmin, iymin, zmax, ixmax, 
     .		  iymax, value)
c     Fins max/min values in matrix -- skip over undefined points
c
      if (zmin .eq. zmax) then
         z1 = zmin
         z2 = zmax
         zdiff = 0
         ntick = 1
         if (zmin .eq. value) then
            ntick = 0
            write(stch, 1212)
1212  format('AUTOLEV: all values in matrix 0 are undefined.')
            call pwrite(rstch, n80)
         else
            write(stch, 1210)
1210  format('AUTOLEV: matrix 0 is single-valued, only 1 level set.')
            call pwrite(rstch, n80)
         endif
      else
         call autolev(zmin, zmax, numlevs, z1, z2, zdiff)
         ntick = numlevs
      endif
c
      do 121 i = 1, ntick
	levs(i) = z1 + float(i-1)*zdiff
121	continue
c     Calculate levels
c
      do 122 i = ntick+1, maxregn
	levs(i) = -999999.
122	continue
c     Void any remaining levels
c
      goto 99
c
c------------------------------------------------------------------------
c     RASTER
c------------------------------------------------------------------------
130   continue
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'RASTER')
C     Make sure matrix 0 has been initialized.
c
      value = mhead(mblank,iptwh)
      call zerror(mdata(istart),numx,numy,value,ierr)
c
      if (ierr .ge. 1024) 
     .		call oerror(n259,m2,'RASTER: All < Min of Matrix')
      if (ierr .ge. 512) 
     .		call oerror(n259,m2,'RASTER: Not in ascending order')
      if (ierr .ge. 256) 
     .		call oerror(n259,m2,'RASTER: All LEVS <= -999999')
      if (ierr .ne. 0) call oerror(n254,m2,'RASTER')
c     Check LEVS, IMIN, IMAX, ..., and matrix for errors
c
      if (mratio .le. 0.) call oerror(n296, m2, 'RASTER')
c
      do 131 numlevs = 1, maxregn-1
	if (levs(numlevs+1) .le. -999999) goto 132
131	continue
      numlevs = maxregn
132   do 133 i = 1, numlevs
	hues(i) = clut(1,i)
	shds(i) = clut(2,i)
	sats(i) = clut(3,i)
133	continue
c     Find number of levels and stores color values into arrays
c	 HUES, SHDS, and SATS
c
      mratio1 = mratio*float(imax-imin+1)/float(jmax-jmin+1)
      if (mratio1 .ge. 1.) then
	IX0=XORG(curcur()) + sclfctx*140
        IXM=XMAX(curcur()) - sclfctx*269
        IYM=YMAX(curcur()) - sclfcty*60
        IY0=IYM - (IXM-IX0 + 1)/MRATIO + 1
      else
	IY0=YORG(curcur()) + sclfcty*125
        IYM=YMAX(curcur()) - sclfcty*60
	IX0=XORG(curcur()) + sclfctx*140
        IXM=IX0 + (IYM-IY0 + 1)*MRATIO - 1
      endif
      ax(curcur()) = float(ixm-ix0)/float(imax-imin)
      bx(curcur()) = float(ix0) - float(imin)*ax(curcur())
      ay(curcur()) = float(iym-iy0)/float(jmax-jmin)
      by(curcur()) = float(iy0) - float(jmin)*ay(curcur())
c
      value = mhead(mblank,iptwh)
      call raster(mdata(istart),numx,numy,levs,hues,shds,sats,
     .		  numlevs,value)
c
      call str4cur(n3)
c
      goto 98
c
c------------------------------------------------------------------------
c     SETCLUT
c------------------------------------------------------------------------
140   continue
c
      if (sp .lt. 2) call oerror(n112, m1, 'SETCLUT: Two needed')
c
      itable = nint(v(sp-1))
      numlevs = nint(v(sp)) 
      sp = sp - 2
c
      if (numlevs .lt. 2 .or. numlevs .gt. maxregn) then
	call oerror(n112, n0, 'SETCLUT: Using maximum number of levels')
	numlevs = maxregn
      endif
c     NUMLEVS = number of desired contour levels; reset to MAXREGN if
c	bad value.
c
      if (itable .le. 0 .or. itable .gt. 6)
     .		call oerror(n112,m2,'SETCLUT: Bad color table number')
c
      do 1409 i = 1, maxregn
	clut(1,i) = 0.0
	clut(2,i) = 0.0
	clut(3,i) = 0.0
1409	continue
c     Zero out CLUT first
c
      if (itable .eq. 1) then
	do 141 i = 1, numlevs
	   clut(1,i) = -16. + float(i-1)*15./(numlevs-1)
	   clut(2,i) = -16. + float(i-1)*15./(numlevs-1)
	   clut(3,i) = -16. + float(i-1)*15./(numlevs-1)
141	   continue
c	   Gray scale (-16,-16,-16) -> (-1,-1,-1)
c
      else if (itable .eq. 2) then
	do 1419 i = 1, numlevs
	   clut(1,i) = -1. - float(i-1)*15./float(numlevs-1)
	   clut(2,i) = -1. - float(i-1)*15./float(numlevs-1)
	   clut(3,i) = -1. - float(i-1)*15./float(numlevs-1)
1419	   continue
c	   Reverse Gray scale (-1,-1,-1) -> (-16,-16,-16)
c
      else if (itable .eq. 3) then
	do 142 i = 1, numlevs
	   clut(1,i) = 32. - float(i-1)*32/float(numlevs-1)
	   clut(2,i) = 4.
	   clut(3,i) = 6.
142	   continue
c	   Blue -> Red (32,4,6) -> (0,4,6)
c
      else if (itable .eq. 4) then
	do 143 i = 1, numlevs
	   clut(1,i) = float(i-1)*32./float(numlevs-1)
	   clut(2,i) = 4.
	   clut(3,i) = 6.
143	   continue
c	   Red -> Blue (0,4,6) -> (32,4,6)
c
      else if (itable .eq. 5) then
	do 144 i = 1, numlevs
	   clut(1,i) = 32. + float(i-1)*30./float(numlevs-1)
	   clut(2,i) = 4.
	   clut(3,i) = 6.
144	   continue
c	   Blue -> Brown  (32,4,6) -> (62,4,6)
c
      else if (itable .eq. 6) then
	do 145 i = 1, numlevs
	   clut(1,i) = 62. - float(i-1)*30./float(numlevs-1)
	   clut(2,i) = 4.
	   clut(3,i) = 6.
145	   continue
c	   Brown -> Blue  (62,4,6) -> (32,4,6)
c
      endif
c
      goto 99
c
c------------------------------------------------------------------------
c     CONTOUR
c------------------------------------------------------------------------
150   continue
c
      quick = .false.
c
      goto 155
c
210   quick = .true.
c
155   istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'CONTOUR')
C     Make sure matrix 0 has been initialized.
c
      value = mhead(mblank,iptwh)
      call zerror(mdata(istart),numx,numy,value,ierr)
c
      if (ierr .ge. 1024) 
     .		call oerror(n259,m2,'CONTOUR: All < Min of Matrix')
      if (ierr .ge. 512) 
     .		call oerror(n259,m2,'CONTOUR: Not in ascending order')
      if (ierr .ge. 256) 
     .		call oerror(n259,m2,'CONTOUR: All LEVS <= -999999')
      if (ierr .ne. 0) call oerror(n254,m2,'CONTOUR')
c     Check LEVS, IMIN, IMAX, ..., and matrix for errors
c
      if (mratio .le. 0.) call oerror(n296, m2, 'CONTOUR')
c
      if (.not. quick) then
	do 151 numlevs = 1, maxregn-1
	  if (levs(numlevs+1) .le. -999999) goto 152
151	  continue
        numlevs = maxregn
152     do 153 j = 1, numlevs
          nconj = nint(conline(j))
	  call linetype(nconj,j)
153	  continue
      endif
c     Set up and check linetypes (if not a quick contour map)
c
      mratio1 = mratio*float(imax-imin+1)/float(jmax-jmin+1)
      if (mratio1 .ge. 1.) then
	IX0=XORG(curcur()) + sclfctx*140
        IXM=XMAX(curcur()) - sclfctx*269
        IYM=YMAX(curcur()) - sclfcty*60
        IY0=IYM - (IXM-IX0 + 1)/MRATIO + 1
      else
	IY0=YORG(curcur()) + sclfcty*125
        IYM=YMAX(curcur()) - sclfcty*60
	IX0=XORG(curcur()) + sclfctx*140
        IXM=IX0 + (IYM-IY0 + 1)*MRATIO - 1
      endif
      ax(curcur()) = float(ixm-ix0)/float(imax-imin)
      bx(curcur())= float(ix0) - float(imin)*ax(curcur())
      ay(curcur()) = float(iym-iy0)/float(jmax-jmin)
      by (curcur())= float(iy0) - float(jmin)*ay(curcur())
c
      value = mhead(mblank,iptwh)
      call tracer(mdata(istart),numx,numy,value,quick)
c
      call str4cur(n4)
c
      goto 98
c
c------------------------------------------------------------------------
c     MREAD
c------------------------------------------------------------------------
160   continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'MREAD')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'MREAD')
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize = stack(sp-2)
      else 
	call oerror(n112, m1, 'MREAD')
      endif
c     Get the correct isize which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      stch = ' '
      do 161 i = 1, isize
	rstch(i) = c(loc+i-1)
161	continue
c
      sp = sp - 4
c
      call mread(stch)
c
      goto 99
c
c------------------------------------------------------------------------
c     MWRITE
c------------------------------------------------------------------------
170   continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'MWRITE')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'MWRITE')
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize = stack(sp-2)
      else 
	call oerror(n112, m1, 'MWRITE')
      endif
c     Get the correct isize which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      stch = ' '
      do 171 i = 1, isize
	rstch(i) = c(loc+i-1)
171	continue
c     Get the filename
c
      sp = sp - 4
c			reset imin, imax, jmin, jmax to use full array
      imin = 1
      jmin = 1
      imax = nint(mhead(mnaxis1, iptwh))
      jmax = nint(mhead(mnaxis2, iptwh))
c
      call mwrite(stch(1:4*isize))
c
      goto 99
c
c------------------------------------------------------------------------
c     READCUBE
c------------------------------------------------------------------------
180   continue
c
      if (sp .lt. 4) call oerror(n112, m1, 'READCUBE')
c
      if (stack(sp) .ne. 3) call oerror(n112, m1, 'READCUBE')
c
      LOC=STACK(SP-1)
      itype = stack(sp-3)
      if  (itype .eq. 14) then
	isize = irealp(stack(sp-2))
      else if (itype .eq. 7) then
	isize = stack(sp-2)
      else 
	call oerror(n112, m1, 'READCUBE')
      endif
c     Get the correct isize which depends if its a literal or variable.
c     Check to make sure that it is a literal or string variables (types
c     7 and 14).
c
      stch = ' '
      do 181 i = 1, isize
	rstch(i) = c(loc+i-1)
181	continue
c     Get the filename
c
      sp = sp - 4
c
      call readcube(stch)
c
      goto 99
c
c------------------------------------------------------------------------
c     SLICE
c------------------------------------------------------------------------
190   continue
c
      if (sp .lt. 3) call oerror(n112, m1, 'SLICE: Three needed')
c
      x2 = nint(v(sp))
      x1 = nint(v(sp-1))
      face = nint(v(sp-2))
      sp = sp - 3
c
c     FACE = 1, 2, or 3; the face parrallel to which a slice will be made
c     X1, X2 = pixel numbers from that face at which the slice is to
c	be made.
c
      call slice(face, x1, x2)
c
      goto 99
c
c------------------------------------------------------------------------
c     MLIMITS
c------------------------------------------------------------------------
200   continue
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MLIMITS')
c     Make sure that data exists in matrix 0
c
      value = mhead(mblank,iptwh)
      call zlimit(mdata(1), numx, numy, zmin, ixmin, iymin, zmax, ixmax, 
     .		  iymax, value)
      xmn = chantox(m2, mhead(1,1), float(ixmin), ierr)
      if (ierr .ne. 0) call oerror(n272, m2, 'MLIMITS')
      xmx = chantox(m2, mhead(1,1), float(ixmax), ierr)
      ymn = chantox(m1, mhead(1,1), float(iymin), ierr)
      if (ierr .ne. 0) call oerror(n272, m2, 'MLIMITS')
      ymx = chantox(m1, mhead(1,1), float(iymax), ierr)
c     Fins max/min values in matrix -- skip over undefined points
c
      write(stch,211,iostat=ierr) cmhead(mbunit,iptwh), 
     .		cmhead(mtype1,iptwh), cmhead(mtype2,iptwh)
211   format(t12, a, t30, a, t46, a, t64, 'Pixels')
      call pwrite(rstch,n80)
c
      write(stch,201,iostat=ierr) 'Min. = ',zmin,xmn,ymn,ixmin,iymin 
      call pwrite(rstch,n80)
201   format(a, 1pg15.7, ' at (', g15.7, ',', g15.7, 
     .	     ') (', i4, ',', i4, ')')
      write(stch,201,iostat=ierr) 'Max. = ',zmax,xmx,ymx,ixmax,iymax
      call pwrite(rstch,n80)
c
      mlims(1,1)=zmin
      mlims(1,2)=xmn
      mlims(1,3)=ymn
      mlims(1,4)=ixmin
      mlims(1,5)=iymin
      mlims(2,1)=zmax
      mlims(2,2)=xmx
      mlims(2,3)=ymx
      mlims(2,4)=ixmax
      mlims(2,5)=iymax
c
      goto 99
c
c------------------------------------------------------------------------
c     cheader
c------------------------------------------------------------------------
220   continue
c
      call cheader
c
      goto 99
c
c------------------------------------------------------------------------
c     PLOTDPTS
c------------------------------------------------------------------------
230   continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'PLOTDOTS: One needed')
c
      i1 = nint(v(sp)) 
      sp = sp - 1
c
      if (abs(i1) .gt. 2) 
     .		call oerror(n112, m2, 'PLOTDOTS: -2, -1, 0, 1, or 2')
c     I1 = type of dots wanted
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'PLOTDOTS')
C     Make sure matrix 0 has been initialized.
c
      if (mratio .le. 0.) call oerror(n296, m2, 'PLOTDOTS')
c
      mratio1 = mratio*float(imax-imin+1)/float(jmax-jmin+1)
      if (mratio1 .ge. 1.) then
	IX0=XORG(curcur()) + sclfctx*140
        IXM=XMAX(curcur()) - sclfctx*269
        IYM=YMAX (curcur())- sclfcty*60
        IY0=IYM - (IXM-IX0 + 1)/MRATIO + 1
      else
	IY0=YORG(curcur()) + sclfcty*125
        IYM=YMAX(curcur()) - sclfcty*60
	IX0=XORG(curcur()) + sclfctx*140
        IXM=IX0 + (IYM-IY0 + 1)*MRATIO - 1
      endif
      ax(curcur()) = float(ixm-ix0)/float(imax-imin)
      bx(curcur()) = float(ix0) - float(imin)*ax(curcur())
      ay(curcur()) = float(iym-iy0)/float(jmax-jmin)
      by(curcur()) = float(iy0) - float(jmin)*ay(curcur())
c
      value = mhead(mblank,iptwh)
      call pltdots(mdata(istart),numx,numy,i1,value)
c
      call str4cur(n5)
c
      goto 98
c
c------------------------------------------------------------------------
c     MSMOOTH
c------------------------------------------------------------------------
240   continue
c
      if (sp .lt. 2) call oerror(n112, m1, 'MSMOOTH: Two needed')
c
      i1 = nint(v(sp))
      cfactor = v(sp-1)
      sp = sp - 2
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
      istart1 = l1*mdatasize/mnumarrays + l1
      istop = istart + numx*numy - l1
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MSMOOTH')
c     Make sure the input matrix has been initialized
c
      do 243 i = 1, mheadsize
	mhead(i,2) = mhead(i,iptwh)
243	continue
      do 242 i = istart, istop
	mdata(i-istart+istart1) = mdata(i)
242	continue
c     Copy matrix header and data into Matrix (2)
c
      value = mhead(mblank,iptwh)
      if (i1 .gt. 0) then
	call msmooth(cfactor,mdata(istart1),mdata(istart),numx,numy,value)
      else
	call msmoth2(cfactor,mdata(istart1),mdata(istart),numx,numy,value)
      endif
c
      goto 99
c
c------------------------------------------------------------------------
c     MFILL
c------------------------------------------------------------------------
250   continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'MFILL: One needed')
c
      i1 = nint(v(sp))
      sp = sp - 1
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
      istart1 = l1*mdatasize/mnumarrays + l1
      istop = istart + numx*numy - 1
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MFILL')
c     Make sure the input matrix has been initialized
c
      do 253 i = 1, mheadsize
	mhead(i,2) = mhead(i,iptwh)
253	continue
      do 252 i = istart, istop
	mdata(i-istart+istart1) = mdata(i)
252	continue
c     Copy matrix header and data into Matrix (2)
c
      value = mhead(mblank,iptwh)
      call mfill(mdata(istart1),mdata(istart),numx,numy,i1,value)
c
      goto 99
c
c------------------------------------------------------------------------
c     ROTATE
c------------------------------------------------------------------------
260   continue
c
      if (sp .lt. 3) call oerror(n112, m1, 'ROTATE: Three needed')
c
      i3 = nint(v(sp))
      i2 = nint(v(sp-1))
      i1 = nint(v(sp-2))
      sp = sp - 3
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
      istart1 = l1*mdatasize/mnumarrays + l1
      istop = istart + numx*numy - l1
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'ROTATE')
c     Make sure the input matrix has been initialized
c
      flipx = .false.
      flipy = .false.
      invertxy = .false.
      if (i3 .gt. 0) flipy = .true.
      if (i2 .gt. 0) flipx = .true.
      if (i1 .gt. 0) invertxy = .true.
c
      do 263 i = 1, mheadsize
	mhead(i,2) = mhead(i,iptwh)
263	continue
      do 262 i = istart, istop
	mdata(i-istart+istart1) = mdata(i)
262	continue
c     Copy matrix header and data into Matrix (2)
c
      if (invertxy) then
	mhead(mnaxis2,1) = mhead(mnaxis1,2)
	mhead(mnaxis1,1) = mhead(mnaxis2,2)
	cmhead(mtype2,1) = cmhead(mtype1,2)
	cmhead(mtype1,1) = cmhead(mtype2,2)
	mhead(mrval2,1) = mhead(mrval1,2)
	mhead(mrval1,1) = mhead(mrval2,2)
	mhead(mdelt2,1) = mhead(mdelt1,2)
	mhead(mdelt1,1) = mhead(mdelt2,2)
	mhead(mpix2,1) = mhead(mpix1,2)
	mhead(mpix1,1) = mhead(mpix2,2)
	mtemp = mxmin
	mxmin = mymin
      	mymin = mtemp
	mtemp = mxmax
      	mxmax = mymax
      	mymax = mtemp
      endif
c
      if (flipx) then
	mhead(mpix1,1) = mhead(mnaxis1,1) - mhead(mpix1,1) + 1
	mhead(mdelt1,1) = -mhead(mdelt1,1)
	mtemp = mxmin
	mxmin = mhead(mnaxis1,1) - mxmax + 1
      	mxmax = mhead(mnaxis1,1) - mtemp + 1
      endif
c
      if (flipy) then
	mhead(mpix2,1) = mhead(mnaxis2,1) - mhead(mpix2,1) + 1
	mhead(mdelt2,1) = -mhead(mdelt2,1)
	mtemp = mymin
	mymin = mhead(mnaxis2,1) - mymax + 1
      	mymax = mhead(mnaxis2,1) - mtemp + 1
      endif
c     Update matrix header
c
      call rotate(mdata(istart1),mdata(istart), numx, numy,
     .		   invertxy, flipx, flipy)
c     rotate/invert matrix
c
      goto 99
c
c------------------------------------------------------------------------
c     MFFT
c------------------------------------------------------------------------
270   continue
c
      invers = .false.
      call virfft(invers)
c
      goto 99
c------------------------------------------------------------------------
c     MIFFT
c------------------------------------------------------------------------
280   continue
c
      invers = .true.
      call virfft(invers)
c
      goto 99
c
c------------------------------------------------------------------------
c     HOLWINDOW
c------------------------------------------------------------------------
290   continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'HOLWINDOW: One needed')
c
      i1 = nint(v(sp))
      sp = sp - 1
c
      call window(i1)
c
      goto 99
c
c------------------------------------------------------------------------
c     HOLFITFOC
c------------------------------------------------------------------------
300   continue
c
      if (sp .lt. 3) call oerror(n112, m1, 'HOLFITFOC: Three needed')
c
      focalpt = v(sp)
      dishdia = v(sp-1)
      nyqrate = v(sp-2)
      sp = sp - 3
c
      call fitfoc(nyqrate, dishdia, focalpt)
c
      goto 99
c
c------------------------------------------------------------------------
c     EQHISTLEV
c------------------------------------------------------------------------
310   continue
c
      if (sp .lt. 3) call oerror(n112, m1, 'EQHISTLEV: Three needed')
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'EQHISTLEV')
c     Make sure that data exists in matrix 0
c
      numlevs = nint(v(sp))
      sp = sp - 1
      if (numlevs .lt. 2 .or. numlevs .gt. maxregn) then
	call oerror(n112, n0, 'EQHISTLEV: Using maximum number of levels')
	numlevs = maxregn
      endif
c     NUMLEVS = number of desired contour levels; reset to MAXREGN if
c	bad value.
c
      if ( .not. okreal8(v(sp)) .or. .not. okreal8(v(sp)) ) then
         value = mhead(mblank,iptwh)
         call zlimit(mdata(istart), numx, numy, zmin, ixmin, iymin, 
     .		  zmax, ixmax, iymax, value)
	 if ( okreal8(v(sp)) ) zmax = v(sp)
	 if ( okreal8(v(sp-1)) ) zmin = v(sp-1)
      else
         zmax = v(sp)
         zmin = v(sp-1)
      endif
      sp = sp - 2
c
      do 311 i = 1, numlevs
	func(i) = 1.
311	continue
c
      value = mhead(mblank,iptwh)
      call funclev(mdata(istart), numx, numy, value, func, zmin, zmax, 
     .             levs, numlevs, ierr)
c
      if (ierr .eq. 1) then
	call oerror(n112, m2, 'EQHISTLEV')
      else if (ierr .eq. 3) then
	call oerror(n120, m3, 'EQHISTLEV')
      else if (ierr .eq. 4) then
	call oerror(n120, m3, 'EQHISTLEV')
      else if (ierr .eq. 5) then
	call oerror(n258, m2, 'EQHISTLEV: Bad max or min value')
      else if (ierr .ne. 0) then
	call oerror(n112, m2, 'EQHISTLEV')
      endif
c
      do 312 i = numlevs+1, maxregn
	levs(i) = -999999.
312	continue
c     Void any remaining levels
c
      goto 99
c
c------------------------------------------------------------------------
c     FUNCLEV
c------------------------------------------------------------------------
320   continue
c
      if (sp .lt. 2) call oerror(n112, m1, 'FUNCLEV: Two needed')
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'EQHISTLEV')
c     Make sure that data exists in matrix 0
c
      if ( .not. okreal8(v(sp)) .or. .not. okreal8(v(sp)) ) then
         value = mhead(mblank,iptwh)
         call zlimit(mdata(istart), numx, numy, zmin, ixmin, iymin, 
     .		 zmax,  ixmax, iymax, value)
	 if ( okreal8(v(sp)) ) zmax = v(sp)
	 if ( okreal8(v(sp-1)) ) zmin = v(sp-1)
      else
         zmax = v(sp)
         zmin = v(sp-1)
      endif
      sp = sp - 2
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'FUNCLEV')
c     Make sure that data exists in matrix 0
c
      do 321 i = 1, maxregn
	if (flevs(i) .lt. 0) then
	    numlevs = i
	    goto 325
        endif
        func(i) = flevs(i)
321	continue
      if (numlevs .lt. 2 .or. numlevs .gt. maxregn) 
     .	call oerror(n112, m2, 'FUNCLEV: Too few values in FLEVS')
c
325   value = mhead(mblank,iptwh)
      call funclev(mdata(istart), numx, numy, value, func, zmin, zmax, 
     .             levs, numlevs, ierr)
c
      if (ierr .eq. 1) then
	call oerror(n112, m2, 'FUNCLEV')
      else if (ierr .eq. 3) then
	call oerror(n120, m3, 'FUNCLEV')
      else if (ierr .eq. 4) then
	call oerror(n120, m3, 'FUNCLEV')
      else if (ierr .eq. 5) then
	call oerror(n258, m2, 'FUNCLEV')
      else if (ierr .ne. 0) then
	call oerror(n112, m2, 'FUNCLEV')
      endif
c
      do 322 i = numlevs+1, maxregn
	levs(i) = -999999.
322	continue
c     Void any remaining levels
c
      goto 99
c
c------------------------------------------------------------------------
c     PUTROW
c------------------------------------------------------------------------
330   continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'PUTROW')
c
      row = nint(v(sp))
      sp = sp - 1
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      istop = nint(dtwh(c12spn,iptwh) + dtwh(c12ni,iptwh)) - 1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'PUTROW')
c     Make sure that data exists in matrix 0
c
      if (istop .eq. 0) call oerror(n252, m2, 'PUTROW')
c
      if (row .le. 0 .or. row .gt. numy )
     .				call oerror(n112, m2, 'PUTROW')
c
      if (istop .gt. numx ) then
	write(stch,'(i5)',iostat=ierr) numx
	call oerror(n267, n0, 'PUTROW: Using the first ' // stch(1:5) // 
     .				' values in Array 0')
      else if (istop .lt. numx ) then
	write(stch,'(i5)',iostat=ierr) istop
	call oerror(n267, n0, 'PUTROW: Setting the first ' // stch(1:5) // 
     .				' in the row of Matrix 0')
      endif
c     Do the proper things if the matrix is bigger than the array and
c     vice-versa.
c
      do 331 i1 = 1, min(istop,numx) 
	if (okreal4(twh(idatoff+i1,iptwh)) ) then 
            mdata(ilct(i1,row,iptwh)) = twh(idatoff+i1,iptwh)
        else
	    mdata(ilct(i1,row,iptwh)) = mhead(mblank,iptwh)
        endif
331	continue
c     Place values into row of matrix
c
      do 332 i1 = min(istop,numx)+1, numx
	    mdata(ilct(i1,row,iptwh)) = mhead(mblank,iptwh)
332	    continue
c     Blank out any unused part of row.   
c
      goto 99
c
c------------------------------------------------------------------------
c     PUTCOL
c------------------------------------------------------------------------
340   continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'PUTCOL')
c
      col = nint(v(sp))
      sp = sp - 1
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      istop = nint(dtwh(c12spn,iptwh) + dtwh(c12ni,iptwh)) - 1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'PUTCOL')
c     Make sure that data exists in matrix 0
c
      if (istop .eq. 0) call oerror(n252, m2, 'PUTCOL')
c
      if (col .le. 0 .or. col .gt. numx )
     .				call oerror(n112, m2, 'PUTCOL')
c
      if (istop .gt. numy ) then
	write(stch,'(i5)',iostat=ierr) numy
	call oerror(n267, n0, 'PUTCOL: Using the first ' // stch(1:5) // 
     .				' values in Array 0')
      else if (istop .lt. numy ) then
	write(stch,'(i5)',iostat=ierr) istop
	call oerror(n267, n0, 'PUTCOL: Setting the first ' // stch(1:5) // 
     .				' in the column of Matrix 0')
      endif
c     Do the proper things if the matrix is bigger than the array and
c     vice-versa.
c
      do 341 i1 = 1, min(istop,numy) 
	if (okreal4(twh(idatoff+i1,iptwh)) ) then 
            mdata(ilct(col,i1,iptwh)) = twh(idatoff+i1,iptwh)
        else
	    mdata(ilct(col,i1,iptwh)) = mhead(mblank,iptwh)
        endif
341	continue
c     Place values into column of matrix
c
      do 342 i1 = min(istop,numy)+1, numy
	    mdata(ilct(col,i1,iptwh)) = mhead(mblank,iptwh)
342	    continue
c     Blank out any unused part of column.   
c
      goto 99
c
c------------------------------------------------------------------------
c     GETROW
c------------------------------------------------------------------------
350   continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'GETROW')
c
      row = nint(v(sp))
      sp = sp - 1
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      istop = nint(dtwh(c12spn,iptwh) + dtwh(c12ni,iptwh)) - 1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'GETROW')
c     Make sure that data exists in matrix 0
c
      if (istop .eq. 0) call oerror(n252, m2, 'GETROW')
c
      if (row .le. 0 .or. row .gt. numy )
     .				call oerror(n112, m2, 'GETROW')
c
      if (istop .gt. numx ) then
	write(stch,'(i5)',iostat=ierr) numx
	call oerror(n267, n0, 'GETROW: Setting the first ' // stch(1:5) // 
     .				' values in Array 0')
      else if (istop .lt. numx ) then
	write(stch,'(i5)',iostat=ierr) istop
	call oerror(n267, n0, 'GETROW: Using the first ' // stch(1:5) // 
     .				' in the row of Matrix 0')
      endif
c     Do the proper things if the matrix is bigger than the array and
c     vice-versa.
c
      undef0 = sngl(mhead(mblank,iptwh))
      do 351 i1 = 1, min(istop,numx) 
	value = mdata(ilct(i1,row,iptwh))
	if (value .ne. undef0 ) then
	    twh(idatoff+i1,iptwh) = value
	else
	    twh(idatoff+i1,iptwh) = rinfinity()
        endif
351	continue
c     Place values into row of matrix
c
      do 352 i1 = min(istop,numx)+1, istop
	    twh(idatoff+i1,iptwh) = rinfinity()
352	    continue
c     Blank out any unused part of row.   
c
      goto 99
c
c------------------------------------------------------------------------
c     GETCOL
c------------------------------------------------------------------------
360   continue
c
      if (sp .lt. 1) call oerror(n112, m1, 'GETCOL')
c
      col = nint(v(sp))
      sp = sp - 1
c
      istart = long(iptwh-is1)*mdatasize/mnumarrays + l1
      istop = nint(dtwh(c12spn,iptwh) + dtwh(c12ni,iptwh)) - 1
      numx = nint(mhead(mnaxis1,iptwh))
      numy = nint(mhead(mnaxis2,iptwh))
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numx*numy .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'GETCOL')
c     Make sure that data exists in matrix 0
c
      if (istop .eq. 0) call oerror(n252, m2, 'GETCOL')
c
      if (col .le. 0 .or. col .gt. numx )
     .				call oerror(n112, m2, 'GETCOL')
c
      if (istop .gt. numy ) then
	write(stch,'(i5)',iostat=ierr) numy
	call oerror(n267, n0, 'GETCOL: Setting the first ' // stch(1:5) // 
     .				' values in Array 0')
      else if (istop .lt. numy ) then
	write(stch,'(i5)',iostat=ierr) istop
	call oerror(n267, n0, 'GETCOL: Using the first ' // stch(1:5) // 
     . 				' in the row of Matrix 0')
      endif
c     Do the proper things if the matrix is bigger than the array and
c     vice-versa.
c
      undef0 = sngl(mhead(mblank,iptwh))
      do 361 i1 = 1, min(istop,numy) 
	value = mdata(ilct(col,i1,iptwh))
	if (value .ne. undef0 ) then
	    twh(idatoff+i1,iptwh) = value
	else
	    twh(idatoff+i1,iptwh) = rinfinity()
        endif
361	continue
c     Place values into row of matrix
c
      do 362 i1 = min(istop,numy)+1, istop
	    twh(idatoff+i1,iptwh) = rinfinity()
362	    continue
c     Blank out any unused part of row.   
c
      goto 99
c

c------------------------------------------------------------------------
c		MPLUS
c------------------------------------------------------------------------
 370  continue
      istart = long(n1-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,n1))
      numy = nint(mhead(mnaxis2,n1))
      numtot = numx * numy
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numtot .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MPLUS')
C     		Make sure matrix 0 has been initialized.
      istart1 = long(n2-is1)*mdatasize/mnumarrays + l1
      numx1 = nint(mhead(mnaxis1,n2))
      numy1 = nint(mhead(mnaxis2,n2))
      numtot1 = numx1 * numy1
c
      if (istart1.le. 0 .or. istart1.gt. mdatasize .or. numx1.le. 0 .or.
     .	  numy1.le. 0 .or. numtot1 .gt. mdatasize/mnumarrays) 
     .		call oerror(n274, m2, 'MPLUS')
C     		Make sure matrix 1 has been initialized.
      if (numtot .ne. numtot1 .or. numx .ne. numx1 .or.
     .    numy .ne. numy1) call oerror(n275, m2, 'MPLUS')
c 		Make sure they have the same shape
      undef0 = mhead(mblank, n1)
      undef1 = mhead(mblank, n2)
      do 375 i = 1, numtot
         value0 = mdata(i+istart-1)
         value1 = mdata(i+istart1-1)
         if (value0 .ne. undef0 .and. value1 .ne. undef1) then
            mdata(i+istart-1) = value0 + value1
         else if (defmode .ge. 0.5 .and. value1 .ne. undef1) then
            mdata(i+istart-1) = value1
         else if (defmode .lt. 0.5) then
            mdata(i+istart-1) = undef0
         endif
 375  continue
c
      goto 99
c
c------------------------------------------------------------------------
c		MMINUS
c------------------------------------------------------------------------
 380  continue
      istart = long(n1-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,n1))
      numy = nint(mhead(mnaxis2,n1))
      numtot = numx * numy
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numtot .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MMINUS')
C     		Make sure matrix 0 has been initialized.
      istart1 = long(n2-is1)*mdatasize/mnumarrays + l1
      numx1 = nint(mhead(mnaxis1,n2))
      numy1 = nint(mhead(mnaxis2,n2))
      numtot1 = numx1 * numy1
c
      if (istart1.le. 0 .or. istart1.gt. mdatasize .or. numx1.le. 0 .or.
     .	  numy1.le. 0 .or. numtot1 .gt. mdatasize/mnumarrays) 
     .		call oerror(n274, m2, 'MMINUS')
C     		Make sure matrix 1 has been initialized.
      if (numtot .ne. numtot1 .or. numx .ne. numx1 .or.
     .    numy .ne. numy1) call oerror(n275, m2, 'MPLUS')
c 		Make sure they have the same shape
      undef0 = mhead(mblank, n1)
      undef1 = mhead(mblank, n2)
      do 385 i = 1, numtot
         value0 = mdata(i+istart-1)
         value1 = mdata(i+istart1-1)
         if (value0 .ne. undef0 .and. value1 .ne. undef1) then
            mdata(i+istart-1) = value0 - value1
         else if (defmode .ge. 0.5 .and. value1 .ne. undef1) then
            mdata(i+istart-1) = - value1
         else if (defmode .lt. 0.5) then
            mdata(i+istart-1) = undef0
         endif
 385  continue
c
      goto 99
c
c------------------------------------------------------------------------
c		MMULTIPLY
c------------------------------------------------------------------------
 390  continue
      istart = long(n1-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,n1))
      numy = nint(mhead(mnaxis2,n1))
      numtot = numx * numy
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numtot .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MMULTIPLY')
C     		Make sure matrix 0 has been initialized.
      istart1 = long(n2-is1)*mdatasize/mnumarrays + l1
      numx1 = nint(mhead(mnaxis1,n2))
      numy1 = nint(mhead(mnaxis2,n2))
      numtot1 = numx1 * numy1
c
      if (istart1.le. 0 .or. istart1.gt. mdatasize .or. numx1.le. 0 .or.
     .	  numy1.le. 0 .or. numtot1 .gt. mdatasize/mnumarrays) 
     .		call oerror(n274, m2, 'MMULTIPLY')
C     		Make sure matrix 1 has been initialized.
      if (numtot .ne. numtot1 .or. numx .ne. numx1 .or.
     .    numy .ne. numy1) call oerror(n275, m2, 'MPLUS')
c 		Make sure they have the same shape
      undef0 = mhead(mblank, n1)
      undef1 = mhead(mblank, n2)
      do 395 i = 1, numtot
         value0 = mdata(i+istart-1)
         value1 = mdata(i+istart1-1)
         if (value0 .ne. undef0 .and. value1 .ne. undef1) then
            mdata(i+istart-1) = value0 * value1
         else if (defmode .ge. 0.5 .and. value1 .ne. undef1) then
            mdata(i+istart-1) = value1
         else if (defmode .lt. 0.5) then
            mdata(i+istart-1) = undef0
         endif
 395  continue
c
      goto 99
c
c------------------------------------------------------------------------
c		MDIVIDE
c------------------------------------------------------------------------
 400  continue
      istart = long(n1-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,n1))
      numy = nint(mhead(mnaxis2,n1))
      numtot = numx * numy
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numtot .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MDIVIDE')
C     		Make sure matrix 0 has been initialized.
      istart1 = long(n2-is1)*mdatasize/mnumarrays + l1
      numx1 = nint(mhead(mnaxis1,n2))
      numy1 = nint(mhead(mnaxis2,n2))
      numtot1 = numx1 * numy1
c
      if (istart1.le. 0 .or. istart1.gt. mdatasize .or. numx1.le. 0 .or.
     .	  numy1.le. 0 .or. numtot1 .gt. mdatasize/mnumarrays) 
     .		call oerror(n274, m2, 'MDIVIDE')
C     		Make sure matrix 1 has been initialized.
      if (numtot .ne. numtot1 .or. numx .ne. numx1 .or.
     .    numy .ne. numy1) call oerror(n275, m2, 'MPLUS')
c 		Make sure they have the same shape
      undef0 = mhead(mblank, n1)
      undef1 = mhead(mblank, n2)
      do 405 i = 1, numtot
         value0 = mdata(i+istart-1)
         value1 = mdata(i+istart1-1)
         if (value0 .ne. undef0 .and. value1 .ne. undef1) then
            if (value1 .ne. 0.0) then
               mdata(i+istart-1) = value0 / value1
            else
               mdata(i+istart-1) = undef0
            endif
         else if (defmode .ge. 0.5 .and. value1 .ne. undef1) then
            if (value1 .ne. 0.0) then
               mdata(i+istart-1) = 1.0 / value1
            else
               mdata(i+istart-1) = undef0
            endif
         else if (defmode .lt. 0.5) then
            mdata(i+istart-1) = undef0
         endif
 405  continue
c
      goto 99
c
c------------------------------------------------------------------------
c		MSCALE
c------------------------------------------------------------------------
 410  continue
      if (sp .lt. 1) call oerror(n112, m1, 'MSCALE: One needed')
c
      sfact = v(sp)
      sp = sp - 1
c
      istart = long(n1-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,n1))
      numy = nint(mhead(mnaxis2,n1))
      numtot = numx * numy
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numtot .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MSCALE')
C     		Make sure matrix 0 has been initialized.
      undef0 = mhead(mblank, n1)
      do 415 i = 1, numtot
         if (mdata(i+istart-1) .ne. undef0) then
            mdata(i+istart-1) = mdata(i+istart-1) * sfact
         endif
 415  continue
c
      goto 99
c
c------------------------------------------------------------------------
c		MBIAS
c------------------------------------------------------------------------
 420  continue
      if (sp .lt. 1) call oerror(n112, m1, 'MBIAS: One needed')
c
      sfact = v(sp)
      sp = sp - 1
c
      istart = long(n1-is1)*mdatasize/mnumarrays + l1
      numx = nint(mhead(mnaxis1,n1))
      numy = nint(mhead(mnaxis2,n1))
      numtot = numx * numy
c
      if (istart .le. 0 .or. istart .gt. mdatasize .or. numx .le. 0 .or.
     .	  numy .le. 0 .or. numtot .gt. mdatasize/mnumarrays) 
     .		call oerror(n255, m2, 'MBIAS')
C     		Make sure matrix 0 has been initialized.
      undef0 = mhead(mblank, n1)
      do 425 i = 1, numtot
         if (mdata(i+istart-1) .ne. undef0) then
            mdata(i+istart-1) = mdata(i+istart-1) + sfact
         endif
 425  continue
c
      goto 99
c
c------------------------------------------------------------------------
98    continue
      call place(n0, n780 - n25*(inline-n1))
      call pchar('',n0)
99    continue
      RETURN
      END
c
