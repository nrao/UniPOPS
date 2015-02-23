      SUBROUTINE AU9 (J)
C-------------------------------------------------------------------------------
C  @(#)au9.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     au9 - Ripple, Rmodel, Rshow
c
C---------------------------------------------------------------------
c
      INTEGER*4  ISTART, ISTOP
      integer*2 j, iptwh, jj, ix, iy, ierr
      integer*2 m2, m3, n0, n120, n225, n284, n298
      integer*2 n1, n25, n780, n33
      logical isfirst, okreal4
      real*4 rampltde, rphase, rperiod, ajj, twopi
c
      include 'core.inc'
      include 'cio.inc'
      INCLUDE 'cform.inc'
      INCLUDE 'appl.inc'
c
      data m2, m3, n0, n33, n120, n225, n284, n298
     .     /-2, -3, 0, 33, 120, 225, 284, 298/
      data n1, n25, n780 /1, 25, 780/
c
      parameter (twopi = 6.283185)
C
C=======================================================================
C
      IPTWH = 1
c
      JJ=J
      GO TO (70,80,85,90), JJ
      call oerror(n120, m3, 'AU9')
C---------------------------------------------------------------------
C							 RIPPLE	 
C---------------------------------------------------------------------
  70  CONTINUE
c
      ISTART = DTWH (C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH (C12NI,IPTWH) + ISTART - 1
c
      if (istop .lt. istart) call oerror(n225, m2, 'RIPPLE')
c
      call rshape2 (ierr)
      if (ierr .ne. 0) call oerror(ierr, m2, 'RIPPLE')
c
      DO 73 JJ=ISTART,ISTOP
            if (okreal4(TWH(JJ,IPTWH))) TWH(JJ,IPTWH)=TWH(JJ,IPTWH) - 
     .        rampltde*COS( (float(jj-istart) + rphase)*twopi/rperiod)
  73	    CONTINUE
      GO TO 99
C---------------------------------------------------------------------
C							 RMODEL	 
C---------------------------------------------------------------------
  80  CONTINUE
      ISTART = DTWH (C12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH (C12NI,IPTWH) + ISTART-1
c
      if (istop .lt. istart) call oerror(n225, m2, 'RMODEL')
c
      DO 83 JJ=ISTART,ISTOP
            TWH(JJ,IPTWH)=
     .        rampltde*COS( (float(jj-istart) + rphase)*twopi/rperiod)
  83	    CONTINUE
      GO TO 99
C---------------------------------------------------------------------
C							 RSHAPE	 
C---------------------------------------------------------------------
  85  CONTINUE
c
      call rshape2 (ierr)
      if (ierr .ne. 0) call oerror(ierr, m2, 'RSHAPE')
c
      goto 99
C---------------------------------------------------------------------
C							 RSHOW	 
C---------------------------------------------------------------------
  90  CONTINUE
      if (showplot(numplots) .ne. 0) call oerror(n298, m2, 'RSHOW')
c	
      ISTART = DTWH (C12SPN,IPTWH)
      ISTOP = DTWH (C12NI,IPTWH) + ISTART - 1
c
      if (istop .lt. istart) call oerror(n225, m2, 'RSHOW')
      isfirst = .true.
c
      DO 66 ix = ix0, ixm, 2
         ajj = (float(ix)-bx(numplots))/ax(numplots)
         IY=max(iy0, min(iym, 
     .    nint(AY(numplots)*rampltde*
     .          COS( (ajj-float(istart) + rphase)*twopi/rperiod) +
     .		BY(numplots))))
         if (nint(ajj) .ge. isbg) then
            if (isfirst) then
	       call placewp(ix, iy, n33)
               isfirst = .false.
            else
	       CALL vctrwp(IX, IY, n33, sclchar, 0.)
            endif
	 endif
   66 CONTINUE
      call place(n0,n780 - n25*(inline-n1))
      call pchar('',n0)
      goto 99
c----------------------------------------------------------------------
C
  99  CONTINUE
      RETURN
      END
c
