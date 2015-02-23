C
      SUBROUTINE OPT (IBUF,RBUF,cbuf,INUMS)
C
c     @(#)opt.f	5.1 06/22/94
c
C     OPT prints information about an ON-OFF scan.
c
      INTEGER*2 JHOLD(45), IBUF(2560),IHOLD(45), inums, ivp, ksno, ksna,
     .		kono, klst, kest, kmon, kday, kyr, ktos, kin, jnt, jdb, 
     .		kr50, kd50, kzd, krid, kdid, jara, jadc, lkona, iptr,
     .		iby, npts, i, kona, lksna, npairs
      character*2 estring(6),lstring(6),rstring(6),dstring(6),hstring(6)
      character*2 cbuf(2560)
      REAL RBUF(1280),  RHOLD(45), ha, diffn, diff2, rms, sdv,  diff,
     .	   diffd, zd, tsrc, zdd, xmean
c
      DATA         IVP /4/,
     .             KSNO/ 1/,   KSNA/ 5/,   KONO/11/,   KONA/12/,
     .             KLST/15/,   KEST/16/,   KMON/26/,   KDAY/27/,
     .             KYR /28/,   KTOS/34/,   KIN /39/,   JNT /66/,
     .             JDB /88/,   KR50/54/,   KD50/55/,   KZD /39/,
     .             KRID/35/,   KDID/36/,
     .             JARA/56/,   JADC/57/
C
      LKONA = KONA + 9
      LKSNA = KSNA + 5
      CALL RNS (RBUF(KLST), 0, LSTRING)
      CALL RNS (RBUF(KEST), 0, ESTRING)
      CALL RNS (RBUF(JARA), 0, RSTRING)
      CALL RNS (RBUF(JADC), 1, DSTRING)
      HA = RBUF(KLST) - RBUF(JARA)
      CALL RNS (HA, 0, HSTRING)
C
      IPTR = JDB
      IBY = 1
      NPTS = IBUF(KIN)
      IF (NPTS.LT.3) GO TO 99
      IF (NPTS.GT.45) GO TO 99
C
      DO 10 I = 1,NPTS
         IHOLD(I)=I
         RHOLD(I) = RBUF(IPTR)
         JHOLD(I) = IFIX (RBUF(IPTR))
  10     IPTR = IPTR + IBY
C
      DIFFN=0.0
      DIFF2 = 0.0
      NPAIRS=(NPTS-3)
      RMS = 0.0
      sdv = 0.0
      DO 20 I=2,NPAIRS,2
         DIFF = RHOLD(I) - (RHOLD(I-1)+RHOLD(I+1)) * 0.5
         DIFFN = DIFFN + DIFF
         DIFF2 = DIFF2 + DIFF*DIFF
         IF (INUMS.EQ.1)
     .   WRITE (IVP,3) RHOLD(I-1), RHOLD(I), RHOLD(I+1), DIFF
   20    CONTINUE
      XMEAN=DIFFN/FLOAT(NPAIRS/2)
      DIFFN = DIFF2 - DIFFN*DIFFN/(NPAIRS/2)
      DIFFD = RHOLD(NPTS-1) - (RHOLD(NPTS-2)+RHOLD(NPTS)) * 0.5
      IF (INUMS.EQ.1)
     .WRITE (IVP,3) RHOLD(NPTS-2), RHOLD(NPTS-1), RHOLD(NPTS), DIFFD
      TSRC=XMEAN/DIFFD*RBUF(JNT)
      IF (NPAIRS.EQ.2) GO TO 25
         RMS = SQRT(DIFFN/(NPAIRS/2-1))
         SDV = RMS/DIFFD*RBUF(JNT)/SQRT(FLOAT(NPAIRS/2-1))
C
  25  ZD=RBUF(KZD)
      ZDD = ZD * 57.29578
      WRITE (IVP,1)  RBUF(KSNO), (cBUF(I),I=KSNA,LKSNA),
     .               (RSTRING(I), I=1,6), (DSTRING(I), I=1,5),
     .               IBUF(KMON), IBUF(KDAY), IBUF(KYR),
     .               (LSTRING(I),I=1,5), (ESTRING(I),I=1,5),
     .               (HSTRING(I),I=1,5),
     .               TSRC, SDV, RBUF(JNT), NPTS, ZDD
  99  RETURN
C---------------------------------------------------------------------------
   1  FORMAT (F8.2, 2X, 6A2, 6A2, 2X, 5A2,
     .        1X, I2, '/', I2, '/', I2,
     .        1X, 5A2, 1X, 5A2, 5A2,
     .        1X, F9.4, F7.4, 1X, F7.3, 2X, I6, F6.1)
   2  FORMAT (1X,10(I3,I6,3X))
   3  FORMAT (1X,3F8.0, F8.1)
   4  FORMAT (1X)
   5  FORMAT (17X,'ON = ',6A2,2X,5A2,2X,'DELTA ZA=',F7.2)
C---------------------------------------------------------------------------
      END
