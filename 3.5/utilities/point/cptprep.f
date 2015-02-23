      SUBROUTINE PREP ( IBASE,XDATA,YDATA)
c
c     @(#)cptprep.f	5.1 06/22/94
c
      INTEGER*2 IBUF(2560), BREG(4), ibase, istart, istop, npts, i,
     .		ibegin, iend, j
      REAL       XDATA(600), YDATA(600)
c
      include 'condappl.inc'
      include 'concfmt.inc'
c
      EQUIVALENCE (IBUF(1),RBUF(1))
C
      ISTART=IBUF(JBEG)
      ISTOP=IBUF(JEND)
      NPTS=ISTOP-ISTART+1
      BREG(1) = 1
      BREG(2) = 1 + NPTS*BPCT/100
      BREG(3) = NPTS - NPTS*EPCT/100
      BREG(4)=NPTS
      IBASE   = 0
      DO 30 I=1,4,2
         IBEGIN = BREG(I)+JDB-1
         IEND = BREG(I+1)+JDB-1
         DO 20 J=IBEGIN,IEND
            IBASE=IBASE+1
            XDATA(IBASE)=FLOAT(J)
            YDATA(IBASE) = RBUF(J)
  20        CONTINUE
  30     CONTINUE
      RETURN
      END
