      subroutine funfunct (lflag, J)
C-------------------------------------------------------------------------------
C  @(#)funfunct.F	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     lcursors - Flag, Ccur, Vcur, Tcur, Fcur, Mcur
c
C-------------------------------------------------------------------------------
c
      integer*2 ichar, ix, jj, iptwh, lflag, j, iy, ii, kk, ierr,
     .		nplot, itemp(8), numi2, itag
      integer*2 m2, m3, n120, n255, n264, n294, n299, n3,
     .		n2, n270, n271, n273, m1, n244
      real*4 rand
      integer*4 izero, ilct
      double precision chantox
      logical okarray
      character*8 ztime
      character*16 zdate, ctemp
c
      include 'stk.inc'
      INCLUDE 'cform.inc'
      INCLUDE 'mform.inc'
      INCLUDE 'appl.inc'
c
      double precision dtmphdr(PADDED_HDR_SIZE)
c
      equivalence (itemp, ctemp)
c
      INCLUDE 'mappl.inc'
      include 'core.inc'
c
      data m2, m3, n120, n255, n264, n294, n299, n2, n3,
     .	    n270, n271, n273, n244, m1
     .     /-2, -3, 120, 255, 264, 294, 299, 2, 3, 270,
     .	    271, 273, 244, -1/
c
      IPTWH = 1
c
      JJ=J
      GO TO (66,67,68,60,70,80,90,100,110,120), JJ
      call oerror(n120, m3, 'FUNFUNCT')
c-------------------------
C                                   XCUR, YCUR
c-------------------------
 66   ichar = 0
      CALL CURSOR (ichar,IX,IY)
      SP = SP +	1
      xx = FLOAT(IX)
      GO TO 99
c-------------------------
 67   ichar = 0
      CALL CURSOR (ichar,IX,IY)
      SP = SP +	1
      xx = FLOAT(IY)
      GO TO 99
c----------------------------------------
c				rand
c----------------------------------------
 68   SP = SP +	1
      izero = 0
      xx = 2*rand(izero) - 1
      GO TO 99
C----------------------------------------------------------------------
C					CCUR   
C----------------------------------------------------------------------
   60 CONTINUE
      if (numplots .le. 0) call oerror(n299, m2, 'CCUR')
c
      ichar = 0
      CALL CURSOR (ICHAR,IX,IY)
      SP = SP +	1
c
      call rtrn4cur(ix, iy, nplot, dtmphdr, ierr)
      if (ierr .ne. 0) call oerror(n244, m2, 'CCUR')
c
      XX = (FLOAT(IX)-bx(nplot))/AX(nplot)
      if (showplot(nplot) .eq. 1 .and. dtmphdr(5) .gt. 0.5) xx = 10**xx
c 
      GO TO 99
C-----------------------------------------------------------------------
C					VCUR   
C-----------------------------------------------------------------------
 70   CONTINUE
      if (numplots .le. 0) call oerror(n299, m2, 'VCUR')
c
      ichar = 0
      CALL CURSOR (ICHAR,IX,IY)
      SP=SP+1

      call rtrn4cur(ix, iy, nplot, dtmphdr, ierr)
      if (ierr .ne. 0) call oerror(n244, m2, 'VCUR')
c
      if (showplot(nplot) .eq. 1) call oerror(n294, m2, 'VCUR')
c
      if (showplot(nplot) .eq. 0) then
        if (.not. okarray(n3, dtmphdr) ) call oerror(n270, m2, 'VCUR')
      else
        if (.not. okarray(m2, dtmphdr) ) call oerror(n273, m2, 'VCUR')
      endif
c
      XX=(FLOAT(IX)-bx(nplot))/AX(nplot)
      if (showplot(nplot) .eq. 0) then
        xx = chantox(n3, dtmphdr, sngl(xx), ierr)
      else
	xx = chantox(m2, dtmphdr, sngl(xx), ierr)
      endif
c
      GO TO 99
C-----------------------------------------------------------------------
C					TCUR   
C-----------------------------------------------------------------------
 80   CONTINUE
      if (numplots .le. 0) call oerror(n299, m2, 'TCUR')
c
      ichar = 0
      CALL CURSOR (ICHAR,IX,IY)
      SP=SP+1
c
      call rtrn4cur(ix, iy, nplot, dtmphdr, ierr)
      if (ierr .ne. 0) call oerror(n244, m2, 'TCUR')
c
      XX=(FLOAT(IY)-BY(nplot))/AY(nplot)
      if (showplot(nplot) .eq. 1 .and. dtmphdr(6) .gt. 0.5) xx = 10**xx
c
      GO TO 99
C-----------------------------------------------------------------------
C					FCUR   
C-----------------------------------------------------------------------
 90   CONTINUE
      if (numplots .le. 0) call oerror(n299, m2, 'FCUR')
c
      ichar = 0
      CALL CURSOR (ICHAR,IX,IY)
      SP=SP+1
c
      call rtrn4cur(ix, iy, nplot, dtmphdr, ierr)
      if (ierr .ne. 0) call oerror(n244, m2, 'FCUR')
c
      if (showplot(nplot) .eq. 1) call oerror(n294, m2, 'FCUR')
c
      if (showplot(nplot) .eq. 0) then
        if (.not. okarray(n2, dtmphdr) ) call oerror(n271, m2, 'FCUR')
      else
        if (.not. okarray(m1, dtmphdr) ) call oerror(n273, m2, 'FCUR')
      endif
c
      if (showplot(nplot) .eq. 0) then
        XX=(FLOAT(IX)-bx(nplot))/AX(nplot)
        xx = chantox(n2, dtmphdr, sngl(xx), ierr)
      else
        XX=(FLOAT(iy)-by(nplot))/ay(nplot)
        xx = chantox(m1, dtmphdr, sngl(xx), ierr)
      endif
c
      goto 99
c
C-----------------------------------------------------------------------
C					MCUR   
C-----------------------------------------------------------------------
100   CONTINUE
      if (numplots .le. 0) call oerror(n299, m2, 'MCUR')
      if (showplot(numplots) .le. 1) call oerror(n264, m2, 'MCUR')
c
      ichar = 0
      CALL CURSOR (ICHAR,IX,IY)
      SP=SP+1
c
      call rtrn4cur(ix, iy, nplot, dtmphdr, ierr)
      if (ierr .ne. 0) call oerror(n244, m2, 'MCUR')
c
      if (numplots .ne. nplot) call oerror(n264, m2, 'MCUR')
c
      ii=nint(FLOAT(IX)-bx(1))/AX(1)
      kk=nint(float(iy)-by(1))/ay(1)
c
      if (ii .ge. 1 .and. ii .le. mhead(mnaxis1,iptwh) .and.
     .    kk .ge. 1 .and. kk .le. mhead(mnaxis2,iptwh) ) then      
         xx = mdata(ilct(ii,kk,iptwh))
      else
	 xx = default
      endif
c
      GO TO 99
c
C-----------------------------------------------------------------------
C					DATE   
C-----------------------------------------------------------------------
110   continue
      ctemp = zdate()
      numi2 = 8
      call runtag(numi2, itag)
      call copy(numi2, itemp, c(itag))
      sp = sp + 4
      stack(sp-3) = 14
      stack(sp-2) = numi2
      stack(sp-1) = itag
      stack(sp) = 3
      v(sp-3) = c(7)
      v(sp-2) = c(4)
      v(sp-1) = c(itag)
      v(sp) = c(2)
      goto 1000
C-----------------------------------------------------------------------
C					TIME   
C-----------------------------------------------------------------------
120   continue
      ctemp = ztime()
      numi2 = 4
      call runtag(numi2, itag)
      call copy(numi2, itemp, c(itag))
      sp = sp + 4
      stack(sp-3) = 14
      stack(sp-2) = numi2
      stack(sp-1) = itag
      stack(sp) = 3
      v(sp-3) = c(7)
      v(sp-2) = c(4)
      v(sp-1) = c(itag)
      v(sp) = c(2)
      goto 1000
C --------------------------------------------------------------
99    lflag = 5
      go to 10000
1000  lflag=2
      go to 10000
c
10000 continue
      return
      end
