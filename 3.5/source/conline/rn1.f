      SUBROUTINE RN1(POS,STRNG)
C-------------------------------------------------------------------------------
C  @(#)rn1.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c*************************************
c Modified 8903 [RJM] See CHANGES.DOC
c*************************************
      CHARACTER*8 CSTR
      integer*2 STRNG(4), STR(4), minus, idd, imm, isec, ier, i
      real*4 pos, pos1, dd, rem, sec
      character*1 BSTR(8), BMU
      REAL MM
c
      EQUIVALENCE (STR,BSTR,CSTR)
c
      DATA BMU/'-'/
      DATA MINUS/' -'/
c
      POS1=ABS(POS)
      DD=POS1/3600.0
      IDD=IFIX(DD)
      REM=POS1-IDD*3600.0
      MM=REM/60.0
      IMM=IFIX(MM)
      SEC=REM-IMM*60.0
      ISEC=IFIX(SEC+0.5)
      IF (ISEC.LT.60) GO TO 13
      ISEC=0
      IMM=IMM+1
   13 IF (IMM.LT.60) GO TO 14
      IMM=0
      IDD=IDD+1
   14 CONTINUE
      IF (IDD.NE.0) GO TO 15
      WRITE(CSTR,102,IOSTAT=IER) IMM,ISEC
      IF (POS.LT.0.0.AND.IMM.LT.10) BSTR(4)=BMU
      IF (POS.LT.0.0.AND.IMM.GE.10) BSTR(3)=BMU
      GO TO 16
   15 IF (POS.LT.0) IDD=-IDD
      WRITE(CSTR,101,IOSTAT=IER) IDD,IMM,ISEC
   16 CONTINUE
      DO 11 I=1,4
         STRNG(I)=STR(I)
   11    CONTINUE
  101 FORMAT (I2.2,':',I2.2,':',I2.2)
  102 FORMAT (3X,I2.2,':',I2.2)
      RETURN
      END
