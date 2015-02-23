      SUBROUTINE BASELINE
c
c     @(#)baseline.f	5.1 06/22/94
c
      INTEGER*2 IBUF(2560), istart, istop, ibase, ires, jj, npts
      REAL    XDATA(600), YDATA(600), BPARM(15), x0, xd, x, y, t
c
      include 'condappl.inc'
      include 'concfmt.inc'
c
      EQUIVALENCE (IBUF(1),RBUF(1))
c
      ISTART=IBUF(JBEG)
      ISTOP=IBUF(JEND)
      NPTS=ISTOP-ISTART+1
         CALL PREP ( IBASE,XDATA,YDATA)
         CALL SHAPE (IBASE,XDATA,YDATA,BPARM,IRES)
         X0=BPARM(14)
         XD=BPARM(15)
         X=FLOAT(ISTART)
         DO 16 JJ=ISTART,ISTOP
            T=X*XD+X0
            CALL CNPS (Y,T,BPARM,IRES)
            RBUF(JJ)=RBUF(JJ)-Y
  16        X = X + 1.0
  18     CONTINUE
      RETURN
      END
C
