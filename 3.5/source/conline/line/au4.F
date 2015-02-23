      SUBROUTINE AU4 (J)
C-------------------------------------------------------------------------------
C  @(#)au4.F	5.3 09/10/98
C-------------------------------------------------------------------------------
c
c     au4 - Show, Reshow, Crosshair, Flag, Fullgrid, Plot, Setpage,
c		Rstpage, Color, Linetype, Charsize, Rstcolor 
c
C-----------------------------------------------------------------
c
      INTEGER*2 iptwh, iy, itmp, short, nchan, ix, ier, j, ilct,
     .          ichar, iyz, ixy, ipixel, sclc, color, ierr, nplot, 
     .		curcur
      integer*2 n0, n1, n8, n10, n33, n112, n120, n222, n245, n299,
     .		n2, n5, n9, n14, n25, n50, m1, m2, m3, m16, n3,
     .		n271, n272, n273, n4, n244
      REAL*4 tmp, chn, ymark, vell, xz, csize1, satblu, shdgrn,
     .		huered 
      character*10 getformat, format, stch
      integer*4 ialt, ibx, istart1, istart2, istop1, istop2,
     .		jj, i
      real*8 chantox, xtochan
      logical okarray, seton, okreal8
c
      INCLUDE 'appl.inc'
c
      double precision dtmphdr(PADDED_HDR_SIZE)
      real rtmphdr(PADDED_HDR_SIZE*2)
      integer*2 itmphdr(PADDED_HDR_SIZE*4), cflag
      equivalence (dtmphdr, rtmphdr), (dtmphdr, itmphdr)
c
      INCLUDE 'mappl.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
      include 'core.inc'
      INCLUDE 'cform.inc'
      INCLUDE 'mform.inc'
c
      sclc(ipixel) = max(n1, nint(sclchar*float(ipixel))) 
c
      data ialt/-1/
      data m1, m2, m3, m16
     .    /-1, -2, -3, -16/
      data n0, n1, n3, n8, n10, n33, n112, n120, n222, n245, n299
     .    /0, 1, 3, 8, 10, 33, 112, 120, 222, 245, 299/
      data n2, n4, n5, n9, n14, n25, n50, n271, n272, n273, n244
     .    /2, 4, 5, 9, 14, 25, 50, 271, 272, 273, 244/
c
      IPTWH = 1
      JJ=J
      GO TO (10,20,30,40,110,50, 120, 130, 140, 150, 160, 170, 180), JJ
      call oerror(n120, m3, 'AU4')
      goto 99
C---------------------------------------------------------------------
C					SHOW  
C---------------------------------------------------------------------
 30   CONTINUE
c
      call show(n1, splt)
      call str4cur(n0)
c    
      GO TO 99
C---------------------------------------------------------------------
C					     RESHOW
C---------------------------------------------------------------------
  20  CONTINUE
c
      call show(n0, rplt)
      call str4cur(n0)
c
      GO TO 99
C---------------------------------------------------------------------
C					CROSSHAIR   
C---------------------------------------------------------------------
 10   CONTINUE
c
      if (numplots .eq. 0) call oerror(n299, m2, 'CROSSHAIR')
c
      ichar = 1
      if (sclchar .ne. 1.) call charsize(sclchar)
  12  CALL CURSOR (ICHAR, IX, IY)
      IF (ICHAR.ne.IALT) then
        if (sclchar .ne. 1.) call charsize(1.)
	GO TO 99
      endif
c
      cflag = crossflag
c
      CALL PLACE (ix - sclc(n5),IY)
      CALL VCTR (ix + sclc(n5),IY)
      CALL PLACE (IX,iy - sclc(n5))
      CALL VCTR (IX,iy + sclc(n5))
c     Draw plus sign
c
      call rtrn4cur(ix, iy, nplot, dtmphdr, ierr)
      if (ierr .ne. 0) then
	call oerror(n244, n0, 'CROSSHAIR')
	goto 12
      endif
c
C     Channel/Sample number for SHOW, X value for PLOT, X-pixel for Matrices
c
      CHN = (float(ix)-bx(nplot))/ax(nplot) 
      if (showplot(nplot) .eq. 1 .and. dtmphdr(5) .gt. 0.5) 
     .		chn = 10**chn
      if ( seton( cflag, n5)) then
         format = getformat(chn,2.*chn,n8)
         write(cpuf,format,iostat=ier) chn
         CALL PLACE (ix - sclc(n9*n14),IY)
	 call pchar(ipuf, n8)
      endif
c
c     y-value for SHOW and PLOT, Y-pixel for Matrices
c
      TMP = (FLOAT(IY)-BY(nplot))/AY(nplot)
      if (showplot(nplot) .eq. 1 .and. dtmphdr(6) .gt. 0.5) 
     .		tmp = 10**tmp
      if ( seton( cflag, n4)) then
        format = getformat(tmp,2.*tmp,n8)
        WRITE (CPUF,format,IOSTAT=IER) TMP
        CALL PLACE (IX,IY)
	CALL PCHAR(IPUF,n8)
      endif
c
C     Velocity/Position for SHOW, X value for Matrices, skip for PLOT
c
      if ( seton( cflag, n3)) then
         ierr = 1
         if (showplot(nplot) .eq. 0 .and. okarray(n3, dtmphdr) ) then
	   vell = chantox(n3, dtmphdr, chn, ierr)
           if (ierr .ne. 0) call oerror(n272, m2, 'CROSSHAIR')
         else if (showplot(nplot).gt.1 .and. okarray(m2, dtmphdr)) then
	   vell = chantox(m2, dtmphdr, chn, ierr)
           if (ierr .ne. 0) call oerror(n273, m2, 'CROSSHAIR')
        endif
c
         if (ierr .eq. 0) then
	   format = getformat(vell,2.*vell,n8)
           WRITE (CPUF,format,IOSTAT=IER) vell
           call place (ix - sclc(n9*n14), iy - sclc(n25))
           CALL PCHAR(IPUF,n8)
         endif
      endif
c		
C     Freq/time for SHOW, Y value for Matrices, skip for PLOT
c
      if ( seton( cflag, n2)) then
         ierr = 1
         if (showplot(nplot) .eq. 0 .and. okarray(n2, dtmphdr) ) then
          xz = chantox(n2, dtmphdr, chn, ierr)
          if (ierr .ne. 0) call oerror(n271, m2, 'CROSSHAIR')
         else if (showplot(nplot).gt.1 .and. okarray(m1, dtmphdr)) then
          xz = chantox(m1, dtmphdr, tmp, ierr)
          if (ierr .ne. 0) call oerror(n273, m2, 'CROSSHAIR')
         endif
c
         if (ierr .eq. 0) then
           format = getformat(xz,2.*xz,n8)
           WRITE (CPUF,format,IOSTAT=IER) xz
           CALL PLACE (IX,iy - sclc(n25))
           CALL PCHAR(IPUF,n8)
         endif
      endif
c
c     Z value for matrices, skip for PLOT and SHOW
c
      if ( seton( cflag, n1)) then
       if (showplot(nplot) .gt. 1 .and. numplots .eq. nplot .and.
     .	nint(chn).ge.1.and.nint(chn).le.mhead(mnaxis1,iptwh).and.
     .  nint(tmp).ge.1.and.nint(tmp).le.mhead(mnaxis2,iptwh)) then 
	     xz = mdata(ilct(nint(chn),nint(tmp),iptwh))
             format = getformat(xz,2.*xz,n8)
             WRITE (CPUF,format,IOSTAT=IER) xz
             call place (ix - sclc(n9*n14/n2), iy - sclc(n50))
             CALL PCHAR(IPUF,n8)
        endif
      endif
c
      GO TO 12
C
C---------------------------------------------------------------------
C					FLAG   166
C---------------------------------------------------------------------
  40  CONTINUE
c
      if (sp .le. 0) call oerror(n112, m1, 'FLAG')
      if (showplot(numplots) .lt. 0) call oerror(n299, m2, 'FLAG')
c
      YMARK=V(SP)
      SP=SP-1
      LIT=MOD(LIT+1,3)
c
      if (showplot(numplots) .eq. 1 .and. xlog .gt. 0.5) then
	chn = alog10(ymark)
      else if (showplot(numplots) .eq. 0) then
	chn = xtochan(lxa, dtwh(1,iptwh), ymark, ierr)
        if (ierr .ne. 0) call oerror(n272, m2, 'FLAG')
      else if (showplot(numplots) .ge. 2) then
	chn = xtochan(m2, mhead(1,iptwh), ymark, ierr)
        if (ierr .ne. 0) call oerror(n273, m2, 'FLAG')
      else
	chn = ymark
      endif
c     Calculate channel number for the desired flag
c
      IbX=CHN*AX(numplots)+bx(numplots) + 0.5
      if (ibx .ge. ix0 .and. ibx .le. ixm)  then
         if (sclchar .ne. 1.) call charsize(sclchar)
         call place(short(ibx), IY0)
         IYZ=IYM-(LIT+2)*sclc(n25)
         CALL VCTR(short(ibx),IYZ)
         format = getformat(ymark,2.*ymark,n10)
         write(cpuf,format,iostat=ier) ymark
         CALL PLACE(short(ibx) - sclc(n10*n14/n2),IYZ)
         CALL PCHAR(IPUF,n10)
         if (sclchar .ne. 1.) call charsize(1.)
      endif
c     Plot it if it is within the limits of the plot
c
      GO TO 99
C----------------------------------------------------------------------
C                                     FULLGRID  180
C----------------------------------------------------------------------
  110 CONTINUE
c
      if (showplot(numplots) .lt. 0) call oerror(n299, m2, 'FULLGRID')
c
      DO 111 I = 1,limy
           IYZ=riyt(i)
           CALL PLACE (IX0,IYZ)
           CALL VCTR (IXM, IYZ)
  111      CONTINUE
      DO 112 I = 1,limx
           IXY=rIXT(i)
           CALL PLACE (IXY,IY0)
           CALL VCTR (IXY,IYM)
  112      CONTINUE
      GO TO 99
C---------------------------------------------------------------------
C					     PLOT
C---------------------------------------------------------------------
 50   CONTINUE
c
      istart1 = dtwh(c12spn,1) + idatoff
      istart2 = dtwh(c12spn,2) + idatoff
      istop1 = istart1 + dtwh(c12ni,1) - 1
      istop2 = istart2 + dtwh(c12ni,2) - 1
      nchan = min(istop1-istart1+1, istop2-istart2+1)
c
      if(nchan .le. 0) call oerror(n222, m2, 'PLOT')
c
      call copy(nchan*n2, twh(istart1,1),ydata)
      call copy(nchan*n2, twh(istart2,2),xdata)
c
      call plot(nchan)
      call str4cur(n1)
c
      goto 99
c
C---------------------------------------------------------------------
C					SETPAGE  
C---------------------------------------------------------------------
120   CONTINUE
c
      if (sp .le. 3) call oerror(n112, m1, 'SETPAGE: four needed')
c     
      if (.not. okreal8(v(sp)) ) v(sp) = ymax0+1
      if (.not. okreal8(v(sp-1)) ) v(sp-1) = yorg0-1
      yorg(curcur()) = min(v(sp), v(sp-1))
      ymax(curcur()) = max(v(sp), v(sp-1))
      sp = sp - 2
      if (.not. okreal8(v(sp)) ) v(sp) = xmax0+1
      if (.not. okreal8(v(sp-1)) ) v(sp-1) = xorg0-1
      xorg(curcur()) = min(v(sp), v(sp-1))
      xmax(curcur()) = max(v(sp), v(sp-1))
      sp = sp - 2
c
      if (xorg(curcur()) .lt. xorg0) then
	xorg(curcur()) = xorg0
        write(stch,71,iostat=ierr) xorg0
71	format(i10)
	call oerror(n245, n0, 'SETPAGE:  Setting X-origin to ' // stch)
      endif
      if (yorg(curcur()) .lt. yorg0) then
	yorg(curcur()) = yorg0
        write(stch,71,iostat=ierr) yorg0
	call oerror(n245, n0, 'SETPAGE:  Setting Y-origin to ' // stch)
      endif
      if (xmax(curcur()) .gt. xmax0) then
	xmax(curcur()) = xmax0
        write(stch,71,iostat=ierr) xmax0
	call oerror(n245, n0, 'SETPAGE:  Setting X-max to ' // stch)
      endif
      if (ymax(curcur()) .gt. ymax0) then
	ymax(curcur()) = ymax0
        write(stch,71,iostat=ierr) ymax0
	call oerror(n245, n0, 'SETPAGE:  Setting Y-max to ' // stch)
      endif
c
      if (xmax(curcur()) .eq. xorg(curcur())) 
     .		xmax(curcur()) = xorg(curcur()) + 1
      if (ymax(curcur()) .eq. yorg(curcur())) 
     .		ymax(curcur()) = yorg(curcur()) + 1
c
      sclfctx = float(xmax(curcur()) - xorg(curcur()) + 1) / 
     .			float(xmax0 - xorg0 + 1)
      sclfcty = float(ymax(curcur()) - yorg(curcur()) + 1) / 
     .			float(ymax0 - yorg0 + 1)
      sclchar = min(sclfctx,sclfcty)
c      
      GO TO 98
C---------------------------------------------------------------------
C					RSTPAGE  
C---------------------------------------------------------------------
130   CONTINUE
c
      xorg(curcur()) = xorg0
      yorg(curcur()) = yorg0
      xmax(curcur()) = xmax0
      ymax(curcur()) = ymax0
      sclfctx = 1.
      sclfcty = 1.
      sclchar = 1.
c      
      GO TO 98
C---------------------------------------------------------------------
C					COLOR  
C---------------------------------------------------------------------
140   CONTINUE
c
      if (sp .le. 2) call oerror(n112, m1, 'COLOR: Three needed')
c    
      if (.not. okreal8(v(sp)) ) v(sp) = -16.
      if (.not. okreal8(v(sp-1)) ) v(sp-1) = -16.
      if (.not. okreal8(v(sp-2)) ) v(sp-2) = -16.
c
      satblu = v(sp-2)
      shdgrn = v(sp-1)
      huered = v(sp)
      sp = sp - 3
c
      if (satblu .eq. -16. .and. shdgrn .eq. -16. .and.
     .    huered .eq. -16.) then
	call resetcolor
      else
	idx = color(satblu, shdgrn, huered)
      endif
c
      goto 98
c
C---------------------------------------------------------------------
C					LINETYPE  
C---------------------------------------------------------------------
150   CONTINUE
c
      if (sp .le. 0) call oerror(n112, m1, 'LINETYPE: One needed')
c    
      if (.not. okreal8(v(sp)) ) v(sp) = 0.
c
      ltype = nint(v(sp))
      sp = sp - 1
c
      call linetype(ltype, n33)
c
      goto 98
c
C---------------------------------------------------------------------
C					CHARSIZE  
C---------------------------------------------------------------------
160   CONTINUE
c
      if (sp .le. 0) call oerror(n112, m1, 'CHARSIZE: One needed')
c    
      if (.not. okreal8(v(sp)) ) v(sp) = 11.
c
      csize1 = v(sp)
      sp = sp - 1
c
      if (csize1 .le. 0 .or. csize1 .gt. 44) 
     .		call oerror(n112, m2, 'CHARSIZE')
c
      sclchar = csize1/11.
c     SCLCHAR is ratio of current char. size to default (11 points)
c
      goto 98
c
C---------------------------------------------------------------------
C					RSTCOLOR  
C---------------------------------------------------------------------
170   CONTINUE
c
      call resetcolor
c
      goto 98
c
C---------------------------------------------------------------------
C					CLICK   
C---------------------------------------------------------------------
180   CONTINUE
c
      if (numplots .eq. 0) call oerror(n299, m2, 'CLICK')
c
      cclick = default
      tclick = default
      fclick = default
      vclick = default
      mclick = default
      xclick = default
      yclick = default
c
      ichar = 0
      CALL CURSOR (ICHAR, IX, IY)
c
      xclick = ix
      yclick = iy
c
      call rtrn4cur(ix, iy, nplot, dtmphdr, ierr)
      if (ierr .ne. 0) then
	call oerror(n244, n0, 'CLICK')
	goto 99
      endif
c
C     Channel/Sample number for SHOW, X value for PLOT, X-pixel for Matrices
c
      cclick = (float(ix)-bx(nplot))/ax(nplot) 
      if (showplot(nplot) .eq. 1 .and. dtmphdr(5) .gt. 0.5) 
     .		chn = 10**chn
c
c     y-value for SHOW and PLOT, Y-pixel for Matrices
c
      tclick = (FLOAT(IY)-BY(nplot))/AY(nplot)
      if (showplot(nplot) .eq. 1 .and. dtmphdr(6) .gt. 0.5) 
     .		tclick = 10**tclick
c
C     Velocity/Position for SHOW, X value for Matrices, skip for PLOT
c
      ierr = 1
      if (showplot(nplot) .eq. 0 .and. okarray(n3, dtmphdr) ) then
	   vclick = chantox(n3, dtmphdr, cclick, ierr)
           if (ierr .ne. 0) then
		call oerror(n272, n0, 'CLICK')
		vclick = default
	   endif
      else if (showplot(nplot) .gt. 1 .and. okarray(m2, dtmphdr) ) then
	   vclick = chantox(m2, dtmphdr, cclick, ierr)
           if (ierr .ne. 0) then
		call oerror(n272, n0, 'CLICK')
		vclick = default
	   endif
      endif
c		
C     Freq/time for SHOW, Y value for Matrices, skip for PLOT
c
      ierr = 1
      if (showplot(nplot) .eq. 0 .and. okarray(n2, dtmphdr) ) then
          fclick = chantox(n2, dtmphdr, cclick, ierr)
          if (ierr .ne. 0) then
		call oerror(n271, n0, 'CLICK')
		fclick = default
	  endif
      else if (showplot(nplot) .gt. 1 .and. okarray(m1, dtmphdr) ) then
          fclick = chantox(m1, dtmphdr, tclick, ierr)
          if (ierr .ne. 0) then
		call oerror(n271, n0, 'CLICK')
		fclick = default
	  endif
      endif
c
c     Z value for matrices, skip for PLOT and SHOW
c
      if (showplot(nplot) .gt. 1 .and. numplots .eq. nplot .and.
     .nint(cclick).ge.1.and.nint(cclick).le.mhead(mnaxis1,iptwh).and.
     .nint(tclick).ge.1.and.nint(tclick).le.mhead(mnaxis2,iptwh) ) 
     .      mclick = mdata(ilct(nint(cclick),nint(tclick),iptwh))
c
      GO TO 99
C
C --------------------------------------------------------------
99    continue
      itmp = int(750. - (float(inline)-1.)*22.5)
      call place(n0, itmp)
      call pchar('',n0)
98    return
C --------------------------------------------------------------
  219 FORMAT(F8.2)
      END
c
c-----------------------------------
c
      logical function seton(flag, byte)
c
c     Returns TRUE if the BYTE digit of FLAG is > 0 else it returns 
c     FALSE
c
      integer*2 flag, byte
c
      integer*2 test
c
      if (byte .gt. 1) then
         test = flag / (10**(byte-1))
      else
	 test = flag
      endif
c
      if (mod(test, 10) .eq. 0) then
	seton = .false.
      else
	seton = .true.
      endif
c
      return
      end
c
