      INTEGER*2 FUNCTION LLOCAT (NWORDS,ARRAY,LPT)
C-------------------------------------------------------------------------------
C  @(#)llocat.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C-----------------------------------------------------------------------
C	 LLOCAT	allocates NWORDS from the array	ARRAY and links	the
C     last allocated block to the current block	by the pointer LPT.
C-----------------------------------------------------------------------
      INTEGER*2 ARRAY(*)
      integer*2 nwords, lpt, nfc, j, i
      integer*2 n1, n120, m3
      data n1, n120, m3 /1, 120, -3/
c
      NFC=ARRAY(3)
      J=ARRAY(5)-NFC-NWORDS 
      IF (J.lt.0) call oerror(n1,n1,'Use COMPRESS to regain space')
      LLOCAT=NFC
      ARRAY(LPT)=NFC
      NFC=NFC+NWORDS
      LPT=LLOCAT
      if (lpt .le. 0) call oerror(n120,m3,'LLOCAT')
      J = NFC 
      DO 10 I=LPT,J
   10	 ARRAY(I)=0
      IF (NFC.EQ.2*(NFC/2)) NFC=NFC+1
      ARRAY(3)=NFC
   99 CONTINUE
      RETURN
      END
