      SUBROUTINE UDAT(UDATE,IMON,IDAY,IYEAR)
C-------------------------------------------------------------------------------
C  @(#)udat.f	5.2 09/10/98
C-------------------------------------------------------------------------------
C-------------------------------------------------------------
C     This subroutine takes universal time date (yyyy.mmdd)
C     and converts it to imon, iday, and iyear.
C-------------------------------------------------------------
      REAL*8 UDATE, DUMDATE
      INTEGER*4 IDATE
      INTEGER*2 IMON,IDAY,IYEAR
      IYEAR = UDATE
      DUMDATE=(UDATE-DBLE(IYEAR))*10000.0
      IDATE=(DUMDATE+0.5)
      IDAY = MOD(IDATE,100)
      IDATE = IDATE - IDAY
      IMON = IDATE/100
      RETURN
      END
