      SUBROUTINE ARAINIT
C-------------------------------------------------------------------------------
C  @(#)arainit.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C
      INCLUDE 'core.inc'
c
      integer*2 i, j
      real*4 rinfinity
C
C=======================================================================
C
C                               ADVERBS
C
C**********************************************************************
C******************************** NOTE ********************************
C**********************************************************************
C******************************** NOTE ********************************
C
C         MAKE SURE YOU ALTER THE ARRAY DEFAULT VALUE COLUMN IN POPSDAT
C	  TO REFLECT ALL CHANGES TO THIS ROUTINE.
C
C******************************** NOTE ********************************
C**********************************************************************
C******************************** NOTE ********************************
C**********************************************************************
c
      do 5 i = 1, 2
	 size(i) = 0.
   5     CONTINUE
c
      DO 21 I = 1,12
         XMARK(I)=-999999.
         CMARK(I)=-999999.
         FMARK(I)=-999999.
	 HMARK(i)=-999999.
         SMWGT(I)=0.
         BADPT(I)=0.
   21    CONTINUE
c
      DO 28 i=1,32 
         NREGON(i)=0.
	 levs(i) = -999999.
	 flevs(i) = -999999.
	 status(i) = 0.0
	 conline(i) = 0.0
	 do 319 j = 1, 3
	   clut(j,i) = -16.0
319	   continue
  28     CONTINUE
c
      do 29 i = 1, 24
         CENTER(I)=0.
         HEIGHT(I)=0.
         HWIDTH(I)=0.
         cnterr(i) = 0.
         hghterr(i) = 0.
         hwerr(i) = 0.
29	 continue
c
      do 30 i = 1, 48
	gregon(i) = 0.
30	continue
c
      do 33 i = 1, 15
	bparm(i) = 0.0
33	continue
c
      do 34 i = 1, 5
	gmfrac(i) = 0.0
	mlims(1,5) = default
	mlims(2,5) = default
	do 341 j = 1, 6
	  gout(j, i) = 0.0
341	  continue
34	continue
c
      ctitle = ' '
      cxtitle = ' '
      cytitle = ' '
      cs_object = ' '
      cs_mode = ' '
      cuserprompt = ' '
c
c		the following set the SELECT parameters to DEFAULT
      default = rinfinity()
      x_min = default
      x_max = default
      y_min = default
      y_max = default
      scan_min = default
      scan_max = default
      feed_min = default
      feed_max = default
      lst_min = default
      lst_max = default
      ut_min = default
      ut_max = default
      f_min = default
      f_max = default
      rate_min = default
      rate_max = default
      bw_min = default
      bw_max = default
      it_min = default
      it_max = default
c
c     The following are the CLICK adverbs
      cclick = default
      tclick = default
      fclick = default
      vclick = default
      mclick = default
      xclick = default
      yclick = default
c
C**********************************************************************
C******************************** NOTE ********************************
C**********************************************************************
C******************************** NOTE ********************************
C
C         MAKE SURE YOU ALTER THE ARRAY DEFAULT VALUE COLUMN IN POPSDAT
C	  TO REFLECT ALL CHANGES TO THIS ROUTINE.
C
C******************************** NOTE ********************************
C**********************************************************************
C******************************** NOTE ********************************
C**********************************************************************
C
      RETURN
      END

