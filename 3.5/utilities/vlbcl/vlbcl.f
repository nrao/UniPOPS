C
      PROGRAM BCL
c
c     @(#)vlbcl.f	5.1 06/22/94
C
C     Searches continuum data file (DK2) for all TIMED CAL scans
C     with the input user number.  For each scan found, prints
C     data points and header information on the printer.
C
      INTEGER*2 IBUF(2560), ird, ity, ipr, ksno, ktos, kono, itimed,
     .		kstc, icont, jfe, ifeed, j
      character*2 cbuf(2560)
      character*8 projcode, oobs, site, projcod1
      integer*4 kfeed,maxdata, irtn, hostnm, getlog
      integer*4    i4a,nactual,i4b,iscan
      REAL         RBUF(1280), strt, stp, rscans(32766)
      real*8 dprojcode, doobs, dsite
c
      EQUIVALENCE  ( IBUF(1), RBUF(1) ), (cbuf(1), ibuf(1))
      equivalence (projcode,dprojcode),(oobs,doobs),(site,dsite)
c
      DATA         IRD/5/,    ITY /6/,   IPR   /4/, ksno/1/,
     .             KTOS /34/,   KONO/11/,  ITIMED/2/,
     .             KSTC /35/,   ICONT/3/,  
     .             JFE  /53/,   KFEED/2/  
C                                     DATA FOR COMMON BLOCK
C
      WRITE (ITY,1)
      read (IRD,3,err=30) projcod1
      call uppercase(projcod1, projcode)
      if (projcode .eq. 'BYE') goto 30
c
      irtn = hostnm(site)
      irtn = getlog(oobs)
      call openaccess(dprojcode, doobs, dsite,irtn)
c
      if (irtn .ne. 0) then
	write (ity,*) 'Trouble with on-line data -- Terminating'
	goto 30
      endif
c
      open (unit=IPR,file='systemp',status='unknown')
      rewind (ipr)
c
C                   -- Read in desired feed --
 50   WRITE (ITY,101)
      read (IRD,103,err=50) ifeed
      IF (IFEED.GT.0.AND.IFEED.LT.17) KFEED=IFEED
      STRT=0.0
      STP=-1.0
 60   WRITE (ITY,7)
      READ (ird,8,err=60) STRT, STP
      IF (STP.LE.0.) STP=999999.
C
      write(ipr,4) projcode
      nactual = 32766
      maxdata=1190
      i4a = 0
      i4b = 32766
      call gcontlist2(nactual,i4b,rscans,i4a)
      do 10 j = 1, nactual
        iscan = rscans(j)
	if (iscan .ge. strt .and. iscan .le. stp) then
            call gcont(iscan,kfeed,i4a,maxdata,ibuf(1),irtn)
            if (irtn. ne. 0) go to 10
            IF ( IBUF(KTOS).NE.ICONT )  GO TO 10
            IF ( IBUF(KSTC).NE.ITIMED )  GO TO 10
            IF ((IBUF(JFE).LT.KFEED).OR.(IBUF(JFE).GT.KFEED)) GO TO 10
	    rbuf(ksno) = rbuf(ksno) + float(kfeed)/100.
            CALL VPT (IBUF, rbuf, cbuf)
	    endif
  10        CONTINUE
      call closeaccess()
c
      WRITE (IPR,5)
      WRITE (ITY,6)
  30  STOP
C----------------------------------------------------------------------
   1  FORMAT (/ 'PROGRAM VSST.SYSTEMP'
     .	    //'Searches the analysis disk for all timed cal scans under'
     .       /'a specified project code and prints the following'
     .       /'information: Scan Number, Source Name, Date, Lst, Est,'
     .       /'System Temperature, and Noise Tube.'
     +	    //'The ouput will go to a file by the name of systemp;'
     +       /'if the file exists, it will be overwritten.'
     +      //'To continue, type project code (4 chars MIN, 8 Max);',
     +           ' else, to stop, type BYE.')
   2  FORMAT (3A2)
   3  FORMAT (a8)
   4  FORMAT ('VSST System Temperature Calculations',13x,'User:',a8 
     .        /, 78('-')
     .        /'  Scan      Source     Z Dist      LST    '  ,
     .        '    EST    Sys. Temp.  T_cal Npts',
     .        /, 78('-'))
   5  FORMAT (/,30X,'End vsst.systemp'//)
   6  FORMAT ('End vsst.systemp')
   7  FORMAT (/,'Desired scan range (0.0 implies all scans)',
     .       /'format',/'XXXXXX.  XXXXXX.')
   8  FORMAT (F7.0,2X,F7.0)
 101  FORMAT (/ 'Type in desired feed; 0 implies feed 2',
     .          ' (Normal Sys Temp)') 
 103  FORMAT (I1)
C----------------------------------------------------------------------
      END
