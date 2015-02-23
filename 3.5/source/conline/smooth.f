      SUBROUTINE SMOOTH (TWH,ISTART,ISTOP,WGT,N,WORK)
C-------------------------------------------------------------------------------
C  @(#)smooth.f	5.1 06/22/94
C-----------------------------------------------------------------------
C        SMOOTH convolves the input data (TWH) from ISTART to ISTOP
C     with the  weighting function found in WGTS.
C-----------------------------------------------------------------------
      integer*2 istart, istop, n, i, ib, ie, ic, j
      REAL TWH(*), WGT(*), twt, sum, rinfinity
      real*4 WORK(*)
      logical okreal4
c
      IB = ISTART
      IE = ISTOP  - (N-1)
      IC=N/2
      DO 20 I=IB,IE
         SUM = 0.
	 twt = 0.0
         DO 10 J=1,N
            if(okreal4(twh(i+j-1))) then
		SUM = SUM + WGT(J)*TWH(I+J-1)
		twt = twt + wgt(j)
	    endif
   10       CONTINUE
         if (twt .gt. 0.0) then
		WORK(I+IC)=SUM/twt
	 else
		work(i+ic)=rinfinity()
	 endif
   20    CONTINUE
      IB=IB+IC
      IE=IE+IC
      DO 40 I =IB,IE
         if (okreal4(twh(i))) TWH(I)=WORK(I)
   40    CONTINUE
      RETURN
      END

