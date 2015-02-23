C
      PROGRAM BFO
C
c     @(#)vlbfo.f	5.1 06/22/94
c
C     Searches continuum data file (DK2) for all ON-OFF scans
C     with the input user number.  For each scan found, prints
C     data points and header information on the printer.
C
      INTEGER*2 IBUF(2560), jfe, ionoff, kono, ktos, ksno, ird, ity, 
     .		ipr, ifeed, inums, j
      integer*4 kfeed,maxdata,irtn
      integer*4    i4a,nactual,i4b,iscan, hostnm, getlog
      character*1  iprint
      character*8 projcode, oobs, site, projcod1
      REAL RBUF(1280), strt, stp, rscans(32766)
      real*8 dprojcode, doobs, dsite
      character*2 cbuf(2560)
c
      EQUIVALENCE  ( IBUF(1), RBUF(1) ), (cbuf(1), ibuf(1))
      equivalence (projcode,dprojcode),(oobs,doobs),(site,dsite)
c
      DATA         IRD/5/,    ITY /6/,   IPR   /4/, ksno/1/,
     .             KTOS /34/,   KONO/11/,  IONOFF/4/,
     .             KFEED/1/, jfe/53/
C                                   DATA FOR COMMON BLOCK
C
C
      WRITE (ITY,1)
      read (IRD,3,err=30) projcod1
      call uppercase(projcod1, projcode)
      if(projcode .eq. 'BYE') goto 30
c
      irtn = hostnm(site)
      irtn = getlog(oobs)
      call openaccess(dprojcode, doobs, dsite,irtn)
c
      open (unit=IPR,file='sourcetemp',status='unknown')
      rewind (ipr)
c
      if (irtn .ne. 0) then
	write (ity,*) 'Trouble with on-line data -- Terminating'
	goto 30
      endif
c
 50   WRITE (ITY,101)
      read (IRD,103,err=50) ifeed
      IF (IFEED.GT.0.AND.IFEED.LT.17) KFEED=IFEED
      STRT = 0.0
      STP  =-1.0
 60   WRITE (ITY,7)
      READ  (ird,8,err=60) STRT, STP
      IF (STP.EQ.0.0) STP=999999.
c
      WRITE (ITY,11)
      read (IRD,12) iprint
      INUMS = 0
      if (iprint .eq. 'y' .or. iprint .eq. 'Y') inums = 1
C
      write(ipr,4) projcode
      nactual = 32766
      maxdata=1190
      i4a = 0
      i4b = 32766
      call gcontlist2 (nactual,i4b,rscans,i4a)
      do 10 j = 1, nactual
        iscan = rscans(j)
	if (iscan.ge.strt .and. iscan.le.stp) then
          call gcont (iscan,kfeed,i4a,maxdata,ibuf(1), irtn)
          if (irtn .ne. 0) go to 10
          IF ( IBUF(KTOS).NE.IONOFF )  GO TO 10
	  rbuf(ksno) = rbuf(ksno) + float(kfeed)/100.
          CALL OPT (IBUF,rbuf,cbuf,INUMS)
	  endif
  10      CONTINUE
  20     CONTINUE
      call closeaccess()
c
c
      WRITE (IPR,5) 
      WRITE (ITY,6)
 30   STOP
C----------------------------------------------------------------------
   1  FORMAT ('PROGRAM vsst.sourcetemp'
     .      //'Searches the analysis disk for all on-off scans under',
     .       /'the specified project code and prints the temperature of',
     .       /'the source along with information about the scan.',
     +	    //'The ouput will go to a file by the name of sourcetemp;'
     +       /'if the file exists, it will be overwritten.'
     +      //'To continue, type project code (4 chars MIN, 8 Max);',
     +           ' else, to stop, type BYE.')
   2  FORMAT (3A2)
   3  FORMAT (a8)
   4  FORMAT ('VSST Source Temperature Calculations',13X,'USER:',a8 
     .        /, 129('-')
     .        /'   Scan      Source       Ra   (APP)   Dec   '  ,
     .         '   Date        LST        EST       HA     '  ,
     .         '   T_source   Sem  T_cal   Npts  Z Dist  ' 
     .        /, 129('-'))
   5  FORMAT (/,48X,'End vsst.sourcetemp',//)
   6  FORMAT (' End vsst.sourcetemp')
   7  FORMAT (/,'Desired scan range (0.0 implies all scans)',
     .       /'format',/'XXXXXX.  XXXXXX.')
   8  FORMAT (F7.0,2X,F7.0)
  11  FORMAT (/, 'Print the raw numbers?, Reply "YES" or "NO"')
  12  FORMAT (A1)
 101  FORMAT (/ 'Type in desired feed; 0 implies feed 1',
     .          ' (Normal Source Temp)') 
 103  FORMAT (I1)
C----------------------------------------------------------------------
      END
C
