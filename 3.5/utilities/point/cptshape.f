      SUBROUTINE SHAPE (IBASE,XDATA,YDATA,BPARM,IRES)
c
c     @(#)cptshape.f	5.1 06/22/94
c
      REAL XDATA(600), YDATA(600), BPARM(15), WORK(600), xd, x0, eps,
     .	   eta
      integer*2 ibase, ires, ip, ier, iop, i, iposn
C
C                                   Parabolic fit.
      IP=3
      CALL APCH (XDATA,YDATA,IBASE,IP,XD,X0,WORK,IER)
      EPS=1.0E-6
      ETA=0.
      IOP=-1
      CALL APFS (WORK,IP,IRES,IOP,EPS,ETA,IER)
      IPOSN=((IRES-1)*IRES)/2
      DO 10 I=1,IRES
  10     BPARM(I)=WORK(I+IPOSN)
      BPARM(14)=X0
      BPARM(15)=XD
      RETURN
      END
C
