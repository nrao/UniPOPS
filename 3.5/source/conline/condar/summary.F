      subroutine summary
C---------------------------------------------------------------------------
C @(#)summary.F	5.2 09/10/98
C---------------------------------------------------------------------------
C
C        one line summary of each scan, limited by information in the SDD
C        index block
C
C        SUMMARY: on-line data and then off-line data file
C        DSCANS  SUMMARY: summarizes off-line data file
C        KSCANS  SUMMARY: summarizes keep file
C        SSCANS  SUMMARY: summarizes save file
C        GSCANS  SUMMARY: summarizes gains on-line then off-line file
C        RSCANS  SUMMARY: summarizes off-line records file
C
C  Format of summary line:
C 
C     Continuum:
C  Scan No  Source Name        Obs Pos  (COORDSYS)    Rate         Int. Time
C                              H coord     V coord    (AAA)         (AAA)
CA FFFFF.FF AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA GGGGGGGGGGGG FFF.FF
CA IIIII II AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA GGGGGGGGGGGG FFF.FF
C     Line:
C  Scan No  Source Name        Obs Pos  (COORDSYS)   Resolution   Rest Freq
C                              H coord     V coord    (AAA)         (AAA)
CA FFFFF.FF AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA GGGGGGGGGGGG GGGGGGGGGGGGG
CA IIIII II AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA GGGGGGGGGGGG GGGGGGGGGGGGG
C
C  Where the first A is either 'O','D','K','S','G','g','Z','z', or 'R'
C  depending on the file type.
C  The lower case g and z are for the off-line files.
C  Two AAA fields in the 2nd header line are independantly set to
C  kHz, MHz, or GHz as appropriate.  The COORDSYS field is set to an 8
C  character string which indicates the coordinate system of the observed
C  position as indicated by the position code in the index.  
C  The header lines are printed whenever any of these three fields change.  
C  The formats of the 12 character string fields for H coord and V coord 
C  are determined by the position code.  
C	RA-like:       xxHHxMMxSS.S
C	Dec-like:      xADDxMMxSSxx		A=sign
C       Gal. AZEL:     xDDD.ddddxxx xADD.ddddxxx
C
C  For the on-line data, no index block exists and consequently the values
C  must come from the full scan header - which takes time.
C
C  Before each block of 10 lines the following line is printed:
C---- (IIIII) --------------------------------------------------------------
C where IIIII is the number of the next scan location to be summarized
C
C  If the current scan location to be summarized does not contain an
C  appropriate summary (its empty, the scan number could
C  not be found [the list changed between the time it was gotten and when
C  summary got to this scan], etc) then the following line is printed:
C
C     -----
C
C---------------------------------------------------------------------------
      include 'params.inc'
c
      integer*4 idatsize
      parameter (idatsize = 1024)
      integer*2 i2dat(idatsize), ierr, index(32), iflag, lastblnk
      integer*2 m3, m2, m1, n80, n112, n120
      integer*4 ifun, ifile, numheadrecs, numindexperrec,
     1          numdatarecs, ibytperrec, ibytperheaditem, lastindexitem,
     2          irec, i1, ind, istart, istop, iposcode, i4pc, 
     3          i8pc, i16pc, ii, i, lastrecord, lastindex, loopmax, 
     4          ioldpos, irestf, iresol, imax, iactual,
     5          iscanno, irtn, numout, l0, i4dat(idatsize/2),
     6          number, lunit, lrtn, isubsc, oldscan, RECORDS
      integer*4 n1, n2, n4, n5, n6, n7, n8, n9, n13, n16, n17, n18, 
     .          n19, n20, n21
      logical*4 skipscan
#ifdef CONTINUUM
      integer*4 ifeed
#else
      integer*4 onlinesite
      logical lsetresol, lsetrestf
#endif
      logical lshowhead, lsomething, lfirst
      integer*4 bsummary, esummary
      real*4 coordh, coordv, scanno, rdat(idatsize/2), rindex(16)
      real*4 rscans(32767)
      real*8 freqresol, restfreq, funits(3), ddat(idatsize/4), 
     1       rflimit, frlimit
      real*8 xdata(HDU_FLOAT_SIZE/2), dindex(8)
      character*1 filetypes(6), prefix
      character*8 poscodes(14)
      character*3 cunits(3)
      character*16 sname, cdat(idatsize/8)
      character*12 ccoordh, ccoordv, flnme(4)
      character*32 crshfle
c
      include 'stk.inc'
      include 'cio.inc'
      include 'cform.inc'
      include 'core.inc'
c
      equivalence (i2dat, i4dat, rdat, ddat, cdat)
      equivalence (index, dindex, rindex)
c
#ifdef CONTINUUM
      data cunits /'   ','sec','"/s'/
#else
      data cunits /'GHz','MHz','kHz'/
#endif
      data funits /1000.0, 1.0, 0.001/
      data frlimit /1.0/ rflimit /10.0/
      data imax /32767/
      data poscodes /'GALACTIC','1950RADC','EPOCRADC','MEANRADC',
     1               'APPRADC ','APPHADC ','1950ECL ','EPOCECL ',
     2               'MEANECL ','APPECL  ','AZEL    ','USERDEF ',
     3               '2000RADC','INDRADC '/
      data l0 /0/
      data filetypes /'O','D','K','S','g','R'/
      data flnme /'Offline data', 'Keep', 'Save', 'Gains'/
c
      data m3, m2, m1, n80, n112, n120
     .    /-3, -2, -1, 80, 112, 120/
      data n1, n2, n4, n5, n6, n7, n8, n9, n13, n16, n17, n18, 
     .          n19, n20, n21
     .     /1, 2, 4, 5, 6, 7, 8, 9, 13, 16, 17, 18, 19, 20, 21/
c
      parameter (RECORDS=5)
c
      i4pc(ii) = (ii-n1)/n2 + n1
      i8pc(ii) = (ii-n1)/n4 + n1
      i16pc(ii)= (ii-n1)/n8 + n1
c
      iposcode = 2
      ioldpos = -1
      oldscan  = -32767
      ierr = 0
c			set bsummary esummary to bscan, escan
c			just get the integer part
c			if esummary = 0, no upper limit
      bsummary = ifix(bscan)
      esummary = ifix(escan)
      if (esummary .eq. 0) esummary = 100000
c			is anything else present on the stack?
      ifun = 0
      if (sp.lt.1) call oerror(n112, m1, 'SUMMARY')
      ifun = nint(v(sp)) 
      sp = sp - 1
c			is it likely to be appropriate
      if (ifun.lt.1 .or. ifun.gt.17) call oerror(n112, m1, 'SUMMARY')
      go to (10, 50, 60, 70, 75) ifun
      if (ifun .eq. 16) goto 20
      if (ifun .eq. 17) goto 10
c			if it gets here ifun was inappropriate
      call oerror(n112, m3, 'SUMMARY')
c---------------------------------------------------------------------------
c 			summary
c---------------------------------------------------------------------------
 10   continue
      prefix = filetypes(1)
      if (online) then
c				first, get the list of scans
#ifdef CONTINUUM
	 call gcontlist2(iactual, imax, rscans, irtn)
#else
	 call gscanlist2(iactual, imax, rscans, irtn)
#endif
c
         if (irtn.eq.0 .and. iactual.gt.0) then
            write(cpuf,1010) 
            call pwrite(ipuf,n80)
            write(cpuf, 1014) bscan, escan
            call pwrite(ipuf, n80)
            irestf = 0
            iresol = 0
            lshowhead = .false.
            numout = 0
c				loop through list of scans, getting each one
            lfirst = .true.
            do 15 i1 = 1, min(iactual, imax)
               scanno = rscans(i1)
               iscanno = ifix(scanno)
               isubsc = ifix((scanno - iscanno)*100.0 + 0.5)
c				is this scan in the appropriate range
               if (iscanno .ge. bsummary .and. 
     .             iscanno .le. esummary) then
c				get scan from on-line computer
#ifdef CONTINUUM
		  ifeed = 1
                  if (isubsc .ne. 0) ifeed = isubsc
		  call gchead(iscanno, ifeed, xdata, irtn)
#else
                  call gshead(iscanno,isubsc,xdata,irtn)
#endif
                  if (irtn .gt. 0) then
                     if (lfirst) then
                        lfirst = .false.
                        iresol = 3
                        irestf = 2
                        if (iposcode .eq. 0) iposcode = 2
                        write (cpuf, 1110) poscodes(iposcode)
                        call pwrite(ipuf,n80)
                        write (cpuf, 1120) cunits(iresol),cunits(irestf)
                        call pwrite(ipuf,n80)
                     end if
                     numout = numout + 1
                     if (mod(numout,10).eq.0) then
                        write(cpuf, 1105) numout
                        call pwrite(ipuf,n80)
                     end if
                     write(cpuf,1136) iscanno
                     call pwrite(ipuf,n80)
c
                  else
                     index(1) = isubsc
  160                call gnextindex(xdata, index)
                     if (index(1) .lt. 0) then
c				if its not -100 then the index hasn't changed
c				but we need to keep making calls to gnextindex
c				until it returns -100, i.e. no more feeds left
c				but only if isubsc == 0
                        if (index(1) .ne. -100 .and.
     .                      isubsc .eq. 0) goto 160
                     else if (index(1) .ge. 1) then
c		  		extract the relevant fields
                        iscanno = rindex(i4pc(n17))
                        coordh = rindex(i4pc(n5))
                        coordv = rindex(i4pc(n7)) 
                        write(sname,1011) 
     1                      dindex(i8pc(n9)), dindex(i8pc(n13))
                        restfreq = dindex(i8pc(n21))
                        freqresol = rindex(i4pc(n19))
                        ioldpos = iposcode
                        iposcode = index(31)
c				the rest is almost just like below for output
#ifdef CONTINUUM
		        lshowhead = iposcode .ne. ioldpos
			irestf = 2
			iresol = 3
#else
                        call setunits(freqresol, funits, frlimit,
     1                                iresol, lsetresol)
                        call setunits(restfreq, funits, rflimit, 
     1                                irestf, lsetrestf)
                        lshowhead = lsetresol .or. lsetrestf .or.
     1                               (iposcode .ne. ioldpos)
#endif
                        iflag = 1
                        if (iposcode .eq. 0) iposcode = 2
                        if (iposcode .eq. 1 .or. iposcode .eq. 11 .or.
     1                           iposcode .eq. 12) iflag = 3
                        call posstring(coordh, iflag, ccoordh)
                        iflag = iflag + 1
                        call posstring(coordv, iflag, ccoordv)
                        if (lshowhead) then
                           write(cpuf,1110) poscodes(iposcode)
                           call pwrite(ipuf,n80)
                           write(cpuf,1120) 
     1                         cunits(iresol), cunits(irestf)
                           call pwrite(ipuf,n80)
                        end if
                        numout = numout + 1
                        if (mod(numout,10).eq.0) then
                           write(cpuf, 1105) numout
                           call pwrite(ipuf,n80)
                        end if
                        write(cpuf,1131) prefix, iscanno, index(1), 
     1                    sname, ccoordh, ccoordv, freqresol, restfreq
                        call pwrite(ipuf,n80)
#ifdef CONTINUUM
#else
                        if (isubsc .eq. 0) go to 160
#endif
                     end if
                  end if
               end if
 15         continue
         else
            write(cpuf,1015)
            call pwrite(ipuf,n80)
         end if
      else
         write(cpuf,1020)
         call pwrite(ipuf,n80)
      end if
c
      if (ifun .eq. 17) goto 990
c
c---------------------------------------------------------------------------
c                       dscans summary, continuation of summary
c---------------------------------------------------------------------------
 20   continue
      ifile = 1
      go to 100
c---------------------------------------------------------------------------
c                       kscans summary
c---------------------------------------------------------------------------
 50   continue
      ifile = 2
      go to 100
c---------------------------------------------------------------------------
c                       records summary
c---------------------------------------------------------------------------
 75   continue
      ifile = 5
      go to 100
c---------------------------------------------------------------------------
c                       sscans summary
c---------------------------------------------------------------------------
 60   continue
      ifile = 3
      goto 100
c---------------------------------------------------------------------------
c                       gscans summary
c---------------------------------------------------------------------------
 70   continue
      ifile = 4
      prefix = 'G'
c
#ifdef CONTINUUM
c		gains are line only, don't look online
c		on the other hand, if a file is connected at
c		ifile = 4 and the user askes for summary gscans
c		we may as well give them the info.
#else
      if (online) then
        if (onlinesite() .eq. 1) then

c				first, get the list of scans
         call ggscanlist2(iactual, imax, rscans, irtn)
         if (irtn.eq.0 .and. iactual.gt.0) then
            write(cpuf,1013) 
            call pwrite(ipuf,n80)
            write(cpuf, 1014) bscan, escan
            call pwrite(ipuf, n80)
            irestf = 0
            iresol = 0
            lshowhead = .false.
            numout = 0
c				loop through list of scans, getting each one
            do 115 i1 = 1, min(iactual, imax)
               scanno = rscans(i1)
               iscanno = ifix(scanno)
               isubsc = ifix((scanno - iscanno)*100.0 + 0.5)
c			is this scan in the appropriate range
               if (iscanno .ge. bsummary .and. 
     .             iscanno .le. esummary) then
c				get scan from on-line computer
                  call ggscan(iscanno,isubsc,xdata,irtn)
                  if (irtn .gt. 0) then
                     if (i1 .eq. 1) then
                        iresol = 3
                        irestf = 2
                        if (iposcode .eq. 0) iposcode = 2
                        write (cpuf, 1110) poscodes(iposcode)
                        call pwrite(ipuf,n80)
                        write (cpuf, 1120) cunits(iresol),cunits(irestf)
                        call pwrite(ipuf,n80)
                     end if
                     numout = numout + 1
                     if (mod(numout,10).eq.0) then
                        write(cpuf, 1105) numout
                        call pwrite(ipuf,n80)
                     end if
                     write(cpuf,1136) iscanno
                     call pwrite(ipuf,n80)
c
                  else
                     index(1) = isubsc
  260                call gnextindex(xdata, index)
                     if (index(1) .lt. 0) then
                        if (index(1) .ne. -100 .and. 
     .                          isubsc .eq. 0) go to 260
                     else if (index(1) .ge. 1) then
c		  		extract the relevant fields
                        iscanno = rindex(i4pc(n17))
                        coordh = rindex(i4pc(n5))
                        coordv = rindex(i4pc(n7)) 
                        write(sname,1011) 
     1                      dindex(i8pc(n9)), dindex(i8pc(n13))
                        restfreq = dindex(i8pc(n21))
                        freqresol = rindex(i4pc(n19))
                        ioldpos = iposcode
                        iposcode = index(31)
c				the rest is almost just like below for output
                        call setunits(freqresol, funits, frlimit,
     1                                iresol, lsetresol)
                        call setunits(restfreq, funits, rflimit, 
     1                                irestf, lsetrestf)
                        lshowhead = lsetresol .or. lsetrestf .or.
     1                               (iposcode .ne. ioldpos)
                        iflag = 1
                        if (iposcode .eq. 0) iposcode = 2
                        if (iposcode .eq. 1 .or. iposcode .eq. 11 .or.
     1                           iposcode .eq. 12) iflag = 3
                        call posstring(coordh, iflag, ccoordh)
                        iflag = iflag + 1
                        call posstring(coordv, iflag, ccoordv)
                        if (lshowhead) then
                           write(cpuf,1110) poscodes(iposcode)
                           call pwrite(ipuf,n80)
                           write(cpuf,1120) 
     1                         cunits(iresol), cunits(irestf)
                           call pwrite(ipuf,n80)
                        end if
                        numout = numout + 1
                        if (mod(numout,10).eq.0) then
                           write(cpuf, 1105) numout
                           call pwrite(ipuf,n80)
                        end if
                        write(cpuf,1131) prefix, iscanno, index(1), 
     1                    sname, ccoordh, ccoordv, freqresol, restfreq
                        call pwrite(ipuf,n80)
                        if (isubsc .eq. 0) go to 260
                     end if
                  end if
               end if
 115         continue
         else
            write(cpuf,1016)
            call pwrite(ipuf,n80)
         end if
        else
           write(cpuf, 1018)
           call pwrite(ipuf,n80)
        endif
      else
         write(cpuf,1020)
         call pwrite(ipuf,n80)
      end if
c		Ok, that takes care of the on-line gains, now for the off-line
c
#endif
c			this is what does it all
c
 100  continue
c			is the file available
      lunit = iounit(ifile)
C
      prefix = filetypes(ifile+1)
      numout = 0
      lsomething = .false.
c
c			first get the bootstrap information
      call getsddbs(lunit, numheadrecs, numdatarecs, ibytperrec, 
     .           ibytperheaditem, lastindexitem, lrtn)
      if (lrtn .ne. 0) then
         if (lrtn .eq. 1) then
	    write(cpuf, 1095) flnme(ifile)(1:lastblnk(flnme(ifile))) // 
     .                     ' file does not exist or is unavailable.'
            call pwrite(cpuf, n80)
         endif
         if (lrtn .eq. 2) ierr = 357
         if (lrtn .ge. 3) ierr = 359
         goto 990
      endif
c
      write(cpuf,1090) crshfle(ciounit(ifile))
      call pwrite(ipuf,n80)
      write(cpuf, 1014) bscan, escan
      call pwrite(ipuf, n80)
c			is idat large enough to hold ibytperheaditem
      if (idatsize*2 .lt. ibytperheaditem) then
         ierr = 359
         goto 990
      endif
c			
      numindexperrec = ibytperrec / ibytperheaditem
      lastrecord = ((lastindexitem-1)/numindexperrec) + 1
      lastindex= lastindexitem - (lastrecord-1)*numindexperrec
      if (lastrecord+1 .gt. numheadrecs) then
         lastrecord = numheadrecs-1
         lastindex= numindexperrec
      end if
      loopmax = numindexperrec
c				now, process the index and produce the summary
      if (lastindexitem.gt.0 .and. numheadrecs.ge.2) then
         irestf = 0
         iresol = 0
         lshowhead = .false.
         do 200 irec = 1, lastrecord
c				read a record in
            ind = (irec - 1) * numindexperrec + 1
            if (irec .eq. lastrecord) loopmax = lastindex
            number = loopmax
            call getnindex(lunit, ind, i2dat, number, lrtn)
            if (lrtn .ne. 0 .or. number .ne. loopmax) then
               ierr = 357
               goto 990
            endif
c
            do 250 i = 1, loopmax
               i1 = (i-1)*ibytperheaditem/2 + 1
               ind = i + numindexperrec*(irec-1)
               scanno = rdat(i4pc(i1+n16))
               iscanno = ifix(scanno)
	       if (ifile .eq. RECORDS) then
		  if (nint(scanno) .ne. oldscan ) then
		     oldscan = nint(scanno)
		     skipscan = .false.
		  else
		     if ( nint(100.*(scanno-oldscan)) .le. 1.) skipscan = .true.
		     if (skipscan) goto 250
		  endif
	       endif
               if ((ifile .ne. 3. .and. iscanno .ge. bsummary .and. 
     1              iscanno .le. esummary) .or.
     2             (ifile .eq. 3 .and. ind .ge. bsummary .and.
     3              ind .le. esummary)) then
                  istart = i4dat(i4pc(i1))
                  istop = i4dat(i4pc(i1+2))
                  if (istop .ge.istart .and. istart.gt.0) then
                     ioldpos = iposcode
                     iposcode = i2dat(i1+30)
                     coordh = rdat(i4pc(i1+n4))
                     coordv = rdat(i4pc(i1+n6))
                     sname = cdat(i16pc(i1+n8))
                     freqresol = rdat( i4pc(i1+n18) )
                     restfreq = ddat( i8pc(i1+n20) )
#ifdef CONTINUUM
		     lshowhead = iposcode .ne. ioldpos
		     irestf = 2
		     iresol = 3
#else
                     call setunits(freqresol, funits, frlimit,
     1                             iresol, lsetresol)
                     call setunits(restfreq, funits, rflimit,
     1                             irestf, lsetrestf)
                     lshowhead = lsetresol .or. lsetrestf .or. 
     1                             (iposcode .ne. ioldpos)
#endif
                     iflag = 1
                     if (iposcode .eq. 1 .or. iposcode .eq. 11 .or.
     1                   iposcode .eq. 12) iflag = 3
                     call posstring(coordh, iflag, ccoordh)
                     iflag = iflag + 1
                     call posstring(coordv, iflag, ccoordv)
                     if (lshowhead) then 
                        if (iposcode .eq. 0) iposcode = 2
                        write(cpuf,1110) poscodes(iposcode)
                        call pwrite(ipuf,n80)
                        write(cpuf,1120) cunits(iresol), cunits(irestf)
                        call pwrite(ipuf,n80)
                     end if
                     if (ifile .ne. 3) then
                        numout = numout + 1
                        if (mod(numout,10).eq.0) then
                           write(cpuf,1105) numout
                           call pwrite(ipuf,n80)
                        end if
                     else
                        if (mod(ind,10).eq.0 .or. (ind.eq.bsummary 
     1                             .and. ind.ne.1)) then
                           write(cpuf,1105) ind
                           call pwrite(ipuf,n80)
                        end if
                     end if
                     write(cpuf,1130) prefix, scanno, sname,
     1                   ccoordh, ccoordv, freqresol, restfreq
                     call pwrite(ipuf,n80)
                     lsomething = .true.
                  else if (ifile .eq. 3) then
                     if (ind .eq. 1) then
		        iresol = 3
                        irestf = 2
                        if (iposcode .eq. 0) iposcode = 2
                        write (cpuf, 1110) poscodes(iposcode)
                        call pwrite(ipuf,n80)
                        write (cpuf, 1120) 
     1                       cunits(iresol), cunits(irestf)
                        call pwrite(ipuf,n80)
                     end if
                     if (mod(ind,10).eq.0 .or. (ind.eq.bsummary .and. 
     2                               ind.ne.1)) then
                        write(cpuf,1105) ind
                        call pwrite(ipuf,n80)
                     end if
                     write(cpuf,1135)
                     call pwrite(ipuf,n80)
                  end if
               end if
 250        continue
 200     continue
      end if
      if (.not. lsomething) then
         write(cpuf,1095) 'File is empty'
         call pwrite(ipuf,n80)
      end if
c
 990  if (ierr .ne. 0) then
c		ierr=370 is not a serious error in this instance
c		Also, the previous output from summary should indicate
c		that this error has come from summary.
         if (ierr .eq. 370) then
            call oerror(ierr, m2, 'SUMMARY')
         else
            call oerror(ierr, m2, 'SUMMARY')
         endif
      endif
      return
c
 1010 format('Summary of on-line data file.')
 1014 format('    Using bscan = ',f9.2,' escan = ',f9.2)
 1011 format(2a8)
 1013 format('Summary of on-line gains file')
 1015 format('On-line data file is empty or does not exist.')
 1016 format('On-line Gains file is empty or does not exist.')
 1018 format('On-line data is NOT 12-m data.')
 1020 format('Online data is not available ')
 1090 format('Summary of file : ', a32)
 1095 format(a)
 1105 format('---- (',i5,') ',62('-'))
#ifdef CONTINUUM
 1110 format(2x,'Scan No',2x,'Source Name',8x,'Obs Pos',
     1       2x,'(',a8,')',4x,'Rate',10x,'Int. Time')
#else
 1110 format(2x,'Scan No',2x,'Source Name',8x,'Obs Pos',
     1       2x,'(',a8,')',3x,'Resolution',3x,'Rest Freq')
#endif
 1120 format(30x,'H coord',5x,'V coord',4x,'(',a3,')',9x,'(',a3,')')
#ifdef CONTINUUM
 1130 format(a1,x,f8.2,x,a16,a12,a12,x,g12.6,x,f6.2)
 1131 format(a1,x,i5,x,i2,x,a16,a12,a12,x,g12.6,x,f6.2)
#else
 1130 format(a1,x,f8.2,x,a16,a12,a12,x,g12.6,x,g13.8)
 1131 format(a1,x,i5,x,i2,x,a16,a12,a12,x,g12.6,x,g13.8)
#endif
 1135 format(5x,'-----')
 1136 format(5x,'---- Scan ',i5,' not found.  May no longer exist.')
c
      end
