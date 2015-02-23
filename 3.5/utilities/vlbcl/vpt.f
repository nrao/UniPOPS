C
      SUBROUTINE VPT (IBUF, rbuf, cbuf)
c
c     @(#)vpt.f	5.2 10/20/94
C
C     VPT prints information about a timed cal scan.
C         Assumes data is input on second A/D input channel and the
C         input is total power.
C
      INTEGER*2 IBUF(2560),  JHOLD(200), ivp, kona, lkona, ksna, lksna,
     .		ktos, kstc, klst, kest, kin, kday, kyr, ksno, jnt, klr, 
     .		jdb, kono, kzd, npts, iptr, iby, i, kmon, n1, n2, l, mpts
      character*2   ESTRING(6),  LSTRING(6), cbuf(2560)
      REAL        RBUF(1280),  NOISE, vave, v3, v1, v2, stemp, zd
      DATA        IVP  / 4/,
     .            KONA /12/,   LKONA /21/,   KSNA / 5/,   LKSNA /10/,
     .            KTOS /34/,   KSTC  /35/,   KLST /15/,   KEST  /16/,
     .            KIN  /39/,   KMON  /26/,   KDAY /27/,   KYR   /28/,
     .            KSNO / 1/,   JNT   /66/,   KLR  /38/,   JDB   /88/,
     .            KONO /11/,  KZD/39/
C
      CALL RNS (RBUF(KLST), 0, LSTRING)
      CALL RNS (RBUF(KEST), 0, ESTRING)
C
      NPTS = IBUF(KIN)
      IF (NPTS.LT.4) GO TO 99
      IF (NPTS.GT.1024) GO TO 99
      IPTR = JDB
      IBY = 1
      N1 = NPTS/4
C                                       Noise Source OFF.
      VAVE = 0.0
      DO 10 I=1,N1
         VAVE = VAVE + RBUF(IPTR)
         JHOLD(I) = IFIX (RBUF(IPTR))
  10     IPTR = IPTR + IBY
      V1 = VAVE/N1
C                                       Noise Source ON.
      N2 = N1*3
      N1 = N1 + 1
      VAVE = 0.0
      DO 20 I=N1,N2
         VAVE = VAVE + RBUF(IPTR)
         JHOLD(I) = IFIX (RBUF(IPTR))
  20     IPTR = IPTR + IBY
      V3 = VAVE/(N2-N1+1)
C                                       Noise Source OFF.
      N2 = N2 + 1
      VAVE = 0.0
      DO 30 I = N2,NPTS
         VAVE = VAVE + RBUF(IPTR)
         JHOLD(I) = IFIX (RBUF(IPTR))
  30     IPTR = IPTR + IBY
      V2 = VAVE/(NPTS-N2+1)
C
C                                       Calculate T system.
      NOISE = RBUF(JNT)
      L = JDB + NPTS - 1
      STEMP = NOISE / ((V3/((V1+V2)*0.5))-1.0)
C
      MPTS=NPTS
      ZD=RBUF(KZD) * 57.29578
      WRITE (IVP,1)  RBUF(KSNO), (cBUF(I),I=KSNA,LKSNA),
     .               ZD,
     .               (LSTRING(I),I=1,5), (ESTRING(I),I=1,5),
     .               STEMP, NOISE, MPTS
  99  RETURN
C---------------------------------------------------------------------------
   1  FORMAT (F8.2, 2X, 6A2, 2X, F5.1, 3x, 5A2, 1X, 5A2,
     .        1X, F8.2, 1X, F6.2, 2X, I3)
C---------------------------------------------------------------------------
      END
C
