      SUBROUTINE ACCUMS(XSCALE, ierr)
C-------------------------------------------------------------------------------
C  @(#)accums.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
      real*4 XSCALE, newscale, oldscale, rinfinity 
      integer*2 iptwh, jj, lk, ierr
      INTEGER*4 ISTART, ISTOP, istarthold, istophold
      logical okreal4
c
      INCLUDE 'cform.inc'
      INCLUDE 'appl.inc'
      include 'core.inc'
c
C=======================================================================
C
      IPTWH = 1
      ierr = 0
c
      if (dtwh(c12ni,iptwh) .eq. 0) then
	ierr = 225
	goto 99
      endif
c     Needs a scan to be in Work
c
      ISTART = DTWH(c12SPN,IPTWH) + IDATOFF
      ISTOP = DTWH(c12SPN,IPTWH) + DTWH(c12NI,IPTWH)
     .        + IDATOFF - 1
c
      if (istart .ge. istop) then
	 ierr = 225
	 goto 99
      endif
c
      IF (NACCUM.EQ.0) then
c
        CALL COPY2(HDU_FLOAT_SIZE*2,iTWH(1,IPTWH),IACCUM(1))
c
        DACCUM(c12WT) = XSCALE
        DACCUM(c13NS)=1.
        DACCUM(c13FS)=DACCUM(c1SNO)
        DACCUM(c13LS)=DACCUM(c1SNO)
C
      else
c
        istarthold = DACCUM(c12SPN) + IDATOFF
        istophold = DACCUM(c12SPN) + DTWH(c12NI,IPTWH)
     .          + IDATOFF - 1
        if (istarthold .ge. istophold) then
	 ierr = 225
	 goto 99
        endif
c
c
        oldscale = daccum(c12wt)
 	newscale = DACCUM(c12WT) + XSCALE
        DACCUM(c12WT) = newscale
        DACCUM(c13NS) = DACCUM(c13NS) + 1.0
        DACCUM(c13LS) = DTWH(c1SNO,iptwh)
c
        LK = istarthold
        DO 12 JJ = ISTART,ISTOP
           if (lk .ge. istarthold .and. lk .le. istophold) then
	     if (okreal4(RACCUM(LK)) .and. okreal4(TWH(jj,IPTWH)) ) then
     		RACCUM(LK) = 
     1            ( RACCUM(LK)*oldscale+TWH(jj,IPTWH)*XSCALE )/newscale
	     else if (defmode .ge. 0.5 .and. okreal4(TWH(jj,IPTWH)) ) then
     		RACCUM(LK) = TWH(jj,IPTWH)
	     else if (defmode .lt. 0.5) then
     		RACCUM(LK) = rinfinity()
	     endif
	   endif
c	   Make sure that you don't go beyond the ends of the data
c
           LK=LK+1
   12      CONTINUE
c
      endif
c
  19  NACCUM=NACCUM+1
c
      IF(NACCUM.LE.100)THEN
      	DAINDEX(NACCUM) = DTWH(c1SNO,iptwh)
      ELSE
	DAINDEX(101) = DTWH(c1SNO,iptwh)
      ENDIF
c	
  99  CONTINUE
      RETURN
      END


