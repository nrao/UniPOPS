      SUBROUTINE SCAL   (VMAX,VMIN,MIN,INCR)
C-------------------------------------------------------------------------------
C  @(#)scal.f	5.2 12/21/95
C-------------------------------------------------------------------------------
c
      integer*2 i, is
      REAL SAVE(4),VMAX,VMIN,FIRSTV,DELTAV
      REAL P,T,MIN,INCR,FAD,AXLEN, rmaxnormal
c
      data save/1., 2., 5., 10./
c
      if ( abs(vmax) .lt. rmaxnormal() .and. 
     .     abs(vmin) .lt. rmaxnormal()) then
         AXLEN=5.
         FAD=0.01
         FIRSTV=VMIN
         IF (VMIN.LT.0.) FAD=-0.99
         DELTAV=(VMAX-FIRSTV)/AXLEN
         IF (DELTAV.LE.0.) DELTAV=ABS(2.*FIRSTV/AXLEN)+1.
         I=ALOG10(DELTAV)+1000.
         P=10.**(I-1000)
         DELTAV=DELTAV/P-0.01
         do 200 i = 1, 4
            IS=I
            IF (SAVE(I).GE.DELTAV) GO TO 300
200         CONTINUE
300      DELTAV=SAVE(IS)*P
         FIRSTV=DELTAV*FLOAT(IFIX(VMIN/DELTAV+FAD))
         T=FIRSTV+(AXLEN+0.01)*DELTAV
         IF (T.GE.VMAX) GO TO 400
c
         FIRSTV=P*FLOAT(IFIX(VMIN/P+FAD))
         T=FIRSTV+(AXLEN+0.01)*DELTAV
         IF (T.GE.VMAX) GO TO 400
         IS=IS+1
         if (is .gt. 4) then
c                             increase p by an order of magnitude
c                             and reset index into save to 1
            is = 1
            p = p*10.0
         end if
         GO TO 300
c
400      CONTINUE
         FIRSTV=FIRSTV-FLOAT(IFIX((AXLEN+(FIRSTV-VMAX)/DELTAV)
     .       *0.5))*DELTAV
         IF (dble(VMIN)*dble(FIRSTV).LE.0.d00) FIRSTV=0.
         MIN=FIRSTV
         INCR=DELTAV
c
      else
         min = 1.e30
	 incr = 1.e30
c	 Takes care of the case of bad numbers in array.
c
      endif
c
      RETURN
      END

