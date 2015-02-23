      SUBROUTINE mlabel(itype)
C
C  @(#)mlabel.f	5.2 05/04/98
C
c     Draws a labeled and anotated box for the previous 2-dim map
c
      integer*2 itype, ix, iy, sclc, ipixel, istch(40), i, j, ier, 
     .		numlevs, ierr, idelx, idely, ilen, lastblnk, short,
     .		imintmp, imaxtmp, jmintmp, jmaxtmp, curcur
      integer*2 n1, n8, m2, m1
      integer*2 n2, n5, n10, n12, n14, n18, n20, n30, n38, n40,
     .          n55, n90, n100, n125, n272
      integer*4 l2, l34, l80
      integer*4 iby, ibx, nchars
      real xlft, xrght, ybot, ytop, xf, xl, dx, yf, yl, dy, xz, yz,
     .	   percents(34), value, aindex, blah(2,34), axtmp, aytmp, 
     .     bxtmp, bytmp, hues(34), shds(34), sats(34)
      double precision chantox, xtochan
      logical okarray
      character*10 format, getformat
      character*80 stch
c
      include 'appl.inc'
      include 'core.inc'
      include 'mappl.inc'
      include 'mform.inc'
c
      equivalence (istch, stch)
      data m1, n1, n2, n5, n8, n10, n12, n14, n18, n20, n30,
     .           n38, n40, n55, n90, n100, n125, n272, m2
     .     /-1, 1, 2, 5, 8, 10, 12, 14, 18, 20, 30, 38, 40, 55, 90,
     .      100, 125, 272, -2/
      data l2/2/, l34/34/, l80/80/
c
      sclc(ipixel) = max(n1, nint(sclchar*float(ipixel))) 
c
      imintmp = imin
      imaxtmp = imax
      jmintmp = jmin
      jmaxtmp = jmax
      axtmp = ax(curcur())
      bxtmp = bx(curcur())
      aytmp = ay(curcur())
      bytmp = by(curcur())
c
      if (.not. okarray(m2, mhead(1,1)) .or. 
     .    .not. okarray(m1, mhead(1,1))) call oerror(n272, m2, 'LABEL')
c  
c     Draw Box
      CALL PLACE(ix0,iy0)
      CALL VCTR(ixm,iy0)
      CALL VCTR(ixm,iym)
      CALL VCTR(ix0,iym)
      CALL VCTR(ix0,iy0)
c
C     X TICKS AND ANNOTATION
c
      xlft = chantox(m2, mhead(1,1), float(imin), ierr)
      xrght = chantox(m2, mhead(1,1), float(imax), ierr)
      call tickmrk(xlft, xrght, n10, xf, xl, dx, limx)
c     Gets the tick mark parameters for lower axis labelling
c
      idelx = 1
29    if ( idelx*(ixm - ix0 + 1)/(limx+1) .le. sclchar*10*14 .and. 
     .     idelx .lt. limx-1) then
	idelx = idelx * 2
	goto 29
      endif
c     Prevents overlapping x labels
c
      do 334 j = 1, limx
	    xz = (j-1)*dx + xf
	    aindex = xtochan(m2, mhead(1,1), xz, ierr)
	    ix = nint(aindex*ax(curcur()) + bx(curcur()))
c
	    rixt(j) = ix
c	    Finds the pixel number where the tick mark is to be drawn
c
            CALL PLACE (IX,iym)
            CALL VCTR  (IX,iym - sclc(n12))
            CALL PLACE (IX,iy0)
            CALL VCTR  (IX,iy0 + sclc(n12))
c	    Draw tick marks
c
	    if (slabel .gt. 0.0 .and. sclchar .ne. 1.) 
     .			call charsize(sclchar) 
	    if (slabel .gt. 0.0 .and. mod(j-1,idelx) .eq. 0) then
c	    Label tick mark if desired and label wont overlap previous one.
c
               CALL PLACE (IX,iym)
               CALL VCTR  (IX,iym - sclc(n20))
               CALL PLACE (IX,iy0)
               CALL VCTR  (IX,iy0 + sclc(n20))
	       call place(ix - sclc(n8*n14/n2), iy0 - sclc(n30))
               format = getformat(xz, 2.*xz, n8)
	       write(stch, format,iostat=ier) xz
	       call pchar(istch, n8)
            endif
c
  334       CONTINUE
c
C     Y TICKS AND ANNOTATION
c
      ybot = chantox(m1, mhead(1,1), float(jmin), ierr)
      ytop = chantox(m1, mhead(1,1), float(jmax), ierr)
      call tickmrk(ybot, ytop, n10, yf, yl, dy, limy)
c     Gets the tick mark parameters  for y axis labelling
c
      idely = 1
39    if ( idely*(iym - iy0 + 1)/(limy+1) .le. sclchar*25 .and. 
     .     idely .lt. limy-1) then
	idely = idely * 2
	goto 39
      endif
c     Prevent overlapping y labels
c
      do 335 j = 1, limy
	    yz = (j-1)*dy + yf
	    aindex = xtochan(m1, mhead(1,1), yz, ierr)
	    iy = nint(aindex*ay(curcur()) + by(curcur()))
c
	    riyt(j) = iy
c	    Finds the pixel number where the tick mark is to be drawn
c
            CALL PLACE (IX0,iy)
            CALL VCTR  (ix0 + sclc(n12),iy)
            CALL PLACE (IXM,iy)
            CALL VCTR  (ixm - sclc(n12),iy)
c	    Draw tick marks
c
	    if (slabel .gt. 0.0 .and. mod(j-1,idely) .eq. 0) then
               CALL PLACE (IX0,iy)
               CALL VCTR  (ix0 + sclc(n20),iy)
               CALL PLACE (IXM,iy)
               CALL VCTR  (ixm - sclc(n20),iy)
               format = getformat(yz,2.*yz, n8)
	       write(stch, format,iostat=ier) yz
	       call place(ix0 - sclc(n8*n14), iy - sclc(n12))
	       call pchar(istch, n8)
  	    endif
c	    Draw tick marks and y-axis labels
c
  335       CONTINUE
c
c     Add Documentation
c
      if (slabel .gt. 0.0) then
	stch = cmhead(mtype1, 1)
	ilen = lastblnk(stch)
	call place((ixm+ix0)/n2 - sclc(ilen*n14/n2), iy0 - sclc(n55))
        call pchar( istch, ilen)
c	X-axis label
C	
	ilen = lastblnk(cmhead(mtype2,1))
 	stch = cmhead(mtype2,1)
	ilen = lastblnk(stch)
	call place(ix0 - sclc(n8*n14), (iy0+iym-sclc(n14*ilen))/n2)
        CALL vchar (istch,ilen, .true.)
c	Y-axis label
c
        stch = cmhead(mobject,1) // cmhead(mobject+1,1)
	ilen = lastblnk(stch)
	call place( ix0, iy0 - sclc(n100))
        CALL PCHAR(istch,ilen)
c	Object name
c
        stch = cmhead(morigin,1)
	ilen = lastblnk(stch)
	call place(ix0 + sclc(n20*n14), iy0 - sclc(n100))
        CALL PCHAR(istch,ilen)
c	Origin 
c
        call fromdate(l80, nchars, mhead(mdate, 1), stch)
	ilen = nchars
	call  place(ix0 + sclc(n38*n14), iy0 - sclc(n100))
        CALL PCHAR(istch,ilen)
c	Date
c
        stch = 'Comment:'
	do 91 j = mcomment, min(mheadsize, mcomment+7)
	   ilen = 8 + (j-mcomment)*8
	   stch = stch(1:ilen) // cmhead(j,1)
91	   continue
	ilen = lastblnk(stch)
	call place(ix0, iy0 - sclc(n125))
        CALL PCHAR(istch,ilen)
c	Comment
c
	CALL PLACE(ixm,iym + sclc(n5))
	ilen = lastblnk(cmhead(mbunit,1))
c
        if (itype .ne. 5) then
c	Skip the LEVELS printing part for type 5 plots
c
	  if (ilen .gt. 0) then
	     stch = 'Levels: (' // cmhead(mbunit,1)(1:ilen) // ')'
	  else
	     stch = 'Levels'
	  endif
	  ilen = lastblnk(stch)
          CALL PCHAR(istch,ilen)
c
	  do 133 numlevs = 1, maxregn-1
               IF (LEVS(numlevs+1).le.-999999.) goto 134
133	       continue
	  numlevs = maxregn
c		
134       call charsize(0.75*sclchar)
	  format = getformat(levs(1), levs(min(2,numlevs)), n8)
          DO 135 I=1, numlevs
               WRITE  (stch,format,IOSTAT=IER) LEVS(I)
               CALL PLACE(ixm + sclc(n90), iym - sclc(i*n18))
               CALL PCHAR (istch,n8)
  135          CONTINUE
c
          if (itype .eq. 2 .or. itype .eq. 3) then
c
	    imin = 1
	    imax = 2
	    jmin = 1
	    jmax = numlevs + 2
	    ax(curcur()) = sclc(n40)
	    bx(curcur()) = ixm
	    ay(curcur()) = float(sclc(n18))
	    by(curcur()) = float(iym-sclc(n18)*(numlevs+1)) - ay(curcur())/2.
c
            value = mhead(mblank,1)
      	    if (itype .eq. 2) then
                blah(1,1) = value
                blah(2,1) = value
    	    	do 93 i = 1, numlevs
		    percents(i) = (float(i-1)/float(numlevs-1))**2
		    blah(1,i+1) = levs(numlevs-i+1)
		    blah(2,i+1) = levs(numlevs-i+1)
93		    continue
                blah(1,numlevs+2) = value
                blah(2,numlevs+2) = value
            	call hlftne(blah,l2,l34,levs,percents,numlevs,value)
	    else
                blah(1,1) = value
                blah(2,1) = value
    	    	do 94 i = 1, numlevs
		    hues(i) = clut(1,i)
		    shds(i) = clut(2,i)
		    sats(i) = clut(3,i)
		    blah(1,i+1) = levs(numlevs-i+1)
		    blah(2,i+1) = levs(numlevs-i+1)
94		    continue
                blah(1,numlevs+2) = value
                blah(2,numlevs+2) = value
		call raster(blah,l2,l34,levs,hues,shds,sats,
     .                      numlevs,value)
	    endif
c
        endif
c
        endif
c
      endif	  
c
      imin = imintmp
      imax = imaxtmp
      jmin = jmintmp
      jmax = jmaxtmp
      ax(curcur()) = axtmp
      bx(curcur()) = bxtmp
      ay(curcur()) = aytmp
      by(curcur()) = bytmp
c
      call charsize(1.)
c 
      IF (ZLLINE.ge.1.0) then
	 aindex = (0.-mhead(mrval2,1))/mhead(mdelt2,1) + 
     .			mhead(mpix2,1)
	 iby = aindex*ay(curcur()) + by(curcur()) + 0.5
	 if (iby .ge. iy0 .and. iby .le. iym) then
	    CALL PLACE (IX0,short(IBY))
	    CALL VCTR  (IXM,short(IBY))
	 endif
      endif
c     Draw y=0 line
c
      DO 400 I = 1, maxmrk
	if (hmark(i) .le. -999999.0) goto 401
	 IbY = hmark(i)*ay(curcur()) + by(curcur()) + 0.5
	 if (iby .ge. iy0 .and. iby .le. iym) then
            CALL PLACE(IX0,short(IbY))
            CALL VCTR(IXM,short(IbY))
	 endif
400	 continue
c        Draw temperature markers
c
401   DO 402 I = 1, maxmrk
	if (cmark(i) .le. -999999.0) goto 403
	 Ibx = cmark(i)*ax(curcur()) + bx(curcur()) + 0.5
	 if (ibx .ge. ix0 .and. ibx .le. ixm) then
	    CALL PLACE (short(Ibx),IYM)
	    CALL VCTR  (short(Ibx),IY0)
	 endif
402	 continue
c        Draw channel markers
c
403   DO 404 I = 1, maxmrk
	if (xmark(i) .le. -999999.0) goto 405
	 xz = xtochan(m2, mhead(1,1), xmark(i), ierr)
	 Ibx = xz*ax(curcur()) + bx(curcur()) + 0.5
	 if (ibx .ge. ix0 .and. ibx .le. ixm) then
	    CALL PLACE (short(Ibx),IYM)
            CALL VCTR(short(Ibx),IY0)
	 endif
404	 continue
c        VMARKs
c
405   DO 406 I = 1, maxmrk
	if (fmark(i) .le. -999999.0) goto 407
	 yz = xtochan(m1, mhead(1,1), fmark(i), ierr)
	 IbY = yz*ay(curcur()) + by(curcur()) + 0.5
	 if (iby .ge. iy0 .and. iby .le. iym) then
            CALL PLACE(IX0,short(IbY))
            CALL VCTR(IXM,short(IbY))
	 endif
406	 continue
c        FMARKSs
c
407   continue
c
      RETURN
      END

 
