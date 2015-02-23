      SUBROUTINE RNS (POS,IFLAG,STRING)
C-------------------------------------------------------------------------------
C  @(#)rns.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------
C---------------------------------------------------------------
      integer*2 STRING(6), STRNG2(6), iflag, minus, idd, imm, iss, 
     .          ier, ihh, i
      real*4 pos, dd, ss, hh
      CHARACTER*12 CSTRING
      REAL   MM
      EQUIVALENCE (STRNG2,CSTRING)
      DATA MINUS/' -'/
C                                          DEC, L, B.
      IF (IFLAG .EQ. 1 .OR. IFLAG .EQ. 3) THEN
         DD = ABS(POS)
         IDD = IFIX(DD)
         MM = (DD-FLOAT(IDD))*60.0
         IMM = IFIX(MM)
         SS = (MM-FLOAT(IMM))*60.
c         ISS = IFIX(SS)
         iss=nint(ss)
         IF (ISS.LT.60) GO TO 13
         ISS=0
         IMM=IMM+1
   13    IF (IMM.LT.60) GO TO 14
         IMM=0
         IDD=IDD+1
   14    CONTINUE
         IF (IFLAG .EQ. 3) 
     .   WRITE(CSTRING,103,IOSTAT=IER) IDD,IMM,ISS
         IF (IFLAG .EQ. 1) THEN
            WRITE(CSTRING,101,IOSTAT=IER) IDD, IMM, ISS
            IF (POS.LT.0.0) STRNG2(1)=MINUS
            ENDIF
         GO TO 99
      ENDIF
C                                          R.A.
      IF (IFLAG .EQ. 2 .OR. IFLAG .EQ. 0) THEN
         IF (IFLAG .EQ. 2) POS=POS/15.0
         HH = POS
         IHH = IFIX(HH)
         MM = (HH-FLOAT(IHH))*60.0
         IMM = IFIX(MM)
         SS = (MM-FLOAT(IMM))*60.0
         IF (SS.LT.59.9) GO TO 23
         SS=0.
         IMM=IMM+1
   23    IF (IMM.LT.60) GO TO 24
         IMM=0
         IHH=IHH+1
   24    CONTINUE
         WRITE(CSTRING,102,IOSTAT=IER) IHH,IMM,SS
         GO TO 99
         ENDIF
   99    CONTINUE
         DO 11 I = 1,6
            STRING(I)=STRNG2(I)
   11       CONTINUE
         RETURN
C---------------------------------------------------------------
 101  FORMAT (2X,I2.2,':',I2.2,':',I2.2)
 102  FORMAT (2X,I2.2,':',I2.2,':',F4.1)
  103 FORMAT (1X,I3,':',I2.2,':',I2.2)
      END
