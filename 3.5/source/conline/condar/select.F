      subroutine select
c------------------------------------------------------------------------
c @(#)select.F	5.1 06/22/94
c------------------------------------------------------------------------
c
c        select puts scan numbers into astack if the index values for 
c        that scan fit the selection criteria.  select can alse be used
c        to examine the selection criteria.
c
c        select dscans or offline selects scans from the offline file
c        select sscans selects from the save file
c        select rscans selects from the records file
c        select online selects from the online data
c        select info summarizes the selection criteria.
c
c	 Scan number are added to astack at the end (i.e after any
c        values currently there as indicated by ACOUNT.
c                                        
c------------------------------------------------------------------------
c
      integer*4 idatsize
      parameter (idatsize = 1024)
      integer*2 idat(idatsize), ierr, index(32), is_mode, lastblnk
      integer*2 m3, m2, m1, n1, n2, n80, n112, n120, n281
      logical*2 itfits, lusescanno, skipscan
      integer*4 ifun, ifile, irec, i1, i4pc, i, lastindex, lastrecord,
     .          loopmax, ii, ind, l16, l17, maxadd, actno
      integer*4 imax, irtn, iacount, iacstart, iadded, i4, oldscan, 
     .          RECORDS, lunit, lrtn, number, numheadrecs, 
     .          numindexperrec, numdatarecs, ibytperrec, 
     .          ibytperheaditem, lastindexitem
      real*4 scanno, rdat(idatsize/2), rindex(16), sscale
      real*8 ddat(idatsize/4), dindex(8), params(24), dpar(2)
      character*2 curprog
      character*4 ucs_mode, c4par(4)
      character*16 cdat(idatsize/8), c16par
      character*32 mode_desc, crshfle
c
      include 'core.inc'
      include 'appl.inc'
      include 'stk.inc'
      include 'cio.inc'
c
      external qcomphi, qcomplow
c
      equivalence (idat, rdat, ddat, cdat), (index, dindex, rindex)
      equivalence (dpar, c16par, c4par)
c
      data imax /32767/
      data i4 /4/
      data m3, m2, m1, n1, n2, n80, n112, n120, n281
     .     /-3, -2, -1, 1, 2, 80, 112, 120, 281/
      data l16, l17 /16, 17/
c
      parameter (RECORDS=5)
c
      i4pc(ii) = (ii-1)/n2 + n1
c
c------------------------------------------------------------------------
c
c		Make sure there is something present on the stack.
      ifun = 0
      if (sp .lt. 1) call oerror(n112, m1, 'SELECT')
      ifun = nint(v(sp))
      sp = sp - 1
c		is it likely to be appropriate
      if (ifun .lt. 1 .or. ifun .gt. 18) call oerror(n112, m1, 'SELECT')
c		check ACOUNT and remember current status
      iacount = nint(acount)
      if (iacount .lt. 0 .or. iacount .gt. maxstck) 
     .     call oerror (n281, m2, 'SELECT')
      iacount = iacount + 1
      iacstart = iacount
c		force cs_mode to be upper case
      call uppercase(cs_mode, ucs_mode)
      cs_mode = ucs_mode
      oldscan = -32767
c
      if (ifun .le. 3) go to (10, 25, 20) ifun
      if (ifun .eq. 5) goto 45
      if (ifun .gt. 15) go to (10, 30, 40) (ifun - 15)
      call oerror(n112, m1, 'SELECT')
c------------------------------------------------------------------------
c			DSCANS or OFFDATA
c------------------------------------------------------------------------
 10   continue
      ifile = 1
      lusescanno = .true.
      sscale = -1.0
      go to 100
c------------------------------------------------------------------------
c			KSCANS
c------------------------------------------------------------------------
 25   continue
       ifile = 2
       lusescanno = .true.
       sscale = 1.0
       goto 100
c------------------------------------------------------------------------
c			RSCANS
c------------------------------------------------------------------------
 45   continue
       ifile = 5
       lusescanno = .true.
       sscale = 1.0
       goto 100
c------------------------------------------------------------------------
c			SSCANS
c------------------------------------------------------------------------
 20   continue
      ifile = 3
      lusescanno = .false.
      go to 100
c------------------------------------------------------------------------
c                       ONDATA
c------------------------------------------------------------------------
 30   continue
      if (online) then
         write(cpuf, 1030) 'Select : choosing scans from online data.'
         call pwrite(ipuf, n80)
c			This all happens on the on-line side
c			put the relevant values into the params array
         params(1) = default
         c16par = cs_object
         params(2) = dpar(1)
         params(3) = dpar(2)
#ifdef CONTINUUM
         c4par(1) = 'CONT'
#else
         c4par(1) = 'LINE'
#endif
         c4par(2) = cs_mode
         params(4) = dpar(1)
         params(5) = scan_min
         params(6) = scan_max
         params(7) = feed_min
         params(8) = feed_max
         params(9) = x_min 
         params(10) = x_max
         params(11) = y_min 
         params(12) = y_max
         params(13) = lst_min 
         params(14) = lst_max
         params(15) = ut_min 
         params(16) = ut_max
         params(17) = bw_min 
         params(18) = bw_max
         params(19) = f_min 
         params(20) = f_max
         params(21) = rate_min 
         params(22) = rate_max
         params(23) = it_min 
         params(24) = it_max
c			call gselect, which does all the work
         maxadd = maxstck - iacount + 1
         call gselect(params, actno, maxadd, astack(iacount), irtn)
         if (irtn .eq. 0) then
            iacount = iacount + actno
            acount = iacount - 1
         else
            write(cpuf, 1030) 
     .         'Select: On-line data file is empty or unavailable.'
            call pwrite(ipuf, n80)
         endif
      else
         write(cpuf, 1030) 'Select: On-line data is not available.'
         call pwrite(ipuf, n80)
      endif
c
      go to 990
c------------------------------------------------------------------------
c				INFO
c------------------------------------------------------------------------
 40   continue
      write(cpuf, 1030) ' '
         call pwrite(ipuf, n80)
      write(cpuf, 1030) 
     .   ' SELECT INFO : Status of adverbs used by SELECT verb.'
         call pwrite(ipuf, n80)
      write(cpuf, 1030) ' '
         call pwrite(ipuf, n80)
      if (lastblnk(cs_object) .ne. 0 .and. s_object(1) .ne. 0.0) then
         write(cpuf, 1040) cs_object
            call pwrite(ipuf, n80)
      else
         write(cpuf, 1030) '   Object : All Objects (S_OBJECT)'
            call pwrite(ipuf, n80)
      endif
      call modeid(cs_mode, is_mode, irtn)
      if (is_mode .ne. 0 .and. irtn .eq. 0) then
         call modedesc(is_mode, mode_desc, irtn)
         if (irtn .eq. 0) then
            write(cpuf, 1047) cs_mode, mode_desc
         else
            write(cpuf, 1041) cs_mode
         endif
         call pwrite(ipuf, n80)
      else if (irtn .ne. 0) then
         write(cpuf, 1044) cs_mode
            call pwrite(ipuf, n80)
      else
         write(cpuf, 1030) '   Mode   : All modes (S_MODE)'
            call pwrite(ipuf, n80)
      endif
      call progname(curprog)
      if (curprog .eq. 'Cc') then
         write(cpuf, 1042) 'CONT'
            call pwrite(ipuf, n80)
      else
         write(cpuf, 1042) 'LINE'
            call pwrite(ipuf, n80)
      endif
      write(cpuf, 1030) ' '
         call pwrite(ipuf, n80)
      write(cpuf, 1050)
         call pwrite(ipuf, n80)
      write(cpuf, 1030) ' '
         call pwrite(ipuf, n80)
      write(cpuf, 1043) 'Scan Number      ', 'SCAN_MIN, SCAN_MAX'
      if (scan_min .ne. default) write(cpuf(22:37), 1045) int(scan_min)
      if (scan_max .ne. default) write(cpuf(39:54), 1045) int(scan_max)
         call pwrite(ipuf, n80)
      write(cpuf, 1043) 'Subscan Number   ', 'FEED_MIN, FEED_MAX'
      if (feed_min .ne. default) write(cpuf(22:37), 1045) int(feed_min)
      if (feed_max .ne. default) write(cpuf(39:54), 1045) int(feed_max)
         call pwrite(ipuf, n80)
      write(cpuf, 1043) 'Source X Position', 'X_MIN, X_MAX'
      if (x_min .ne. default) write(cpuf(22:37), 1046) x_min
      if (x_max .ne. default) write(cpuf(39:54), 1046) x_max
         call pwrite(ipuf, n80)
      write(cpuf, 1043) 'Source Y Position', 'Y_MIN, Y_MAX'
      if (y_min .ne. default) write(cpuf(22:37), 1046) y_min
      if (y_max .ne. default) write(cpuf(39:54), 1046) y_max
         call pwrite(ipuf, n80)
      write(cpuf, 1030) '   (decimal degrees)'
         call pwrite(ipuf, n80)
      write(cpuf, 1043) 'LST (hours)      ', 'LST_MIN, LST_MAX'
      if (lst_min .ne. default) write(cpuf(22:37), 1046) lst_min
      if (lst_max .ne. default) write(cpuf(39:54), 1046) lst_max
         call pwrite(ipuf, n80)
      write(cpuf, 1043) 'Univ. Time Date  ', 'UT_MIN, UT_MAX'
      if (ut_min .ne. default) write(cpuf(22:37), 1046) ut_min
      if (ut_max .ne. default) write(cpuf(39:54), 1046) ut_max
         call pwrite(ipuf, n80)
      write(cpuf, 1030) '   (years)'
         call pwrite(ipuf, n80)
#ifdef CONTINUUM
      write(cpuf, 1043) 'Rate ("/s)       ', 'RATE_MIN, RATE_MAX'
      if (rate_min .ne. default) write(cpuf(22:37), 1046) rate_min
      if (rate_max .ne. default) write(cpuf(39:54), 1046) rate_max
         call pwrite(ipuf, n80)
      write(cpuf, 1043) 'Int. Time (s)    ', 'IT_MIN, IT_MAX'
      if (it_min .ne. default) write(cpuf(22:37), 1046) it_min
      if (it_max .ne. default) write(cpuf(39:54), 1046) it_max
         call pwrite(ipuf, n80)
#else
      write(cpuf, 1043) 'Rest Freq. (MHz) ', 'F_MIN, F_MAX'
      if (f_min .ne. default) write(cpuf(22:37), 1046) f_min
      if (f_max .ne. default) write(cpuf(39:54), 1046) f_max
         call pwrite(ipuf, n80)
      write(cpuf, 1043) 'Bandwidth (MHz)  ', 'BW_MIN, BW_MAX'
      if (bw_min .ne. default) write(cpuf(22:37), 1046) bw_min 
      if (bw_max .ne. default) write(cpuf(39:54), 1046) bw_max
         call pwrite(ipuf, n80)
#endif
      write(cpuf, 1030) ' '
         call pwrite(ipuf, n80)
      write(cpuf, 1048) (nint(acount) + 1)
         call pwrite(ipuf, n80)
      if (s_sort .gt. 0) write(cpuf, 1049) 'ascending'
      if (s_sort .lt. 0) write(cpuf, 1049) 'descending'
      if (s_sort .eq. 0) 
     .      write(cpuf, 1030) '   Additions will not be sorted (S_SORT)'
         call pwrite(ipuf, n80)
      write(cpuf, 1030) ' '
         call pwrite(ipuf, n80)
      go to 999
c------------------------------------------------------------------------
c				bad argument list
c------------------------------------------------------------------------
 90   continue
      call oerror(n112, m2, 'SELECT')
      go to 990
c------------------------------------------------------------------------
c			This section handles SDD files
c			ifile set above
c------------------------------------------------------------------------
 100  continue
c			check that the file is available
      lunit = iounit(ifile)
c			
      write(cpuf, 1030) 'Select: choosing scans from ' // 
     .       crshfle(ciounit(ifile))
      call pwrite(ipuf, n80)
c				get the bootstrap information
      call getsddbs(lunit, numheadrecs, numdatarecs, ibytperrec,
     .              ibytperheaditem, lastindexitem, lrtn)
      if (lrtn .ne. 0) then
         if (lrtn .eq. 1) then
            write(cpuf, 1030) 'Select: ' //
     .         crshfle(ciounit(ifile)) // ' is unavailable.'
            call pwrite(ipuf, n80)
         endif
         if (lrtn .eq. 2) ierr = 357
         if (lrtn .ge. 3) ierr = 359
         goto 990
      endif
c      			is idat large enough to hold ibytperheaditem?
      if (idatsize*2 .lt. ibytperheaditem) then
         ierr = 359
         goto 990
      endif
c
      numindexperrec = ibytperrec / ibytperheaditem
      lastrecord = ((lastindexitem-1)/numindexperrec) + 1
      lastindex = lastindexitem - (lastrecord-1)*numindexperrec
      if (lastrecord+1 .gt. numheadrecs) then
         lastrecord = numheadrecs - 1
         lastindex = numindexperrec
      endif
      loopmax = numindexperrec
c			process the index and select the scans
      if (lastindexitem .gt. 0 .and. numheadrecs .ge. 2) then
         do 110 irec = 1, lastrecord
c			read a record in
            ind = (irec - 1) * numindexperrec + 1
            if (irec .eq. lastrecord) loopmax = lastindex
            number = loopmax
            call getnindex(lunit, ind, idat, number, lrtn)
            if (lrtn .ne. 0 .or. number .ne. loopmax) then
               ierr = 357
               goto 990
            endif
c
            do 120 i = 1, loopmax
               i1 = (i-1)*ibytperheaditem/2 + 1
               scanno = rdat(i4pc(i1+l16))
	       if (ifile .eq. RECORDS) then
		  if (nint(scanno) .ne. oldscan ) then
		     oldscan = nint(scanno)
		     skipscan = .false.
		  else
		     if ( nint(100.*(scanno-oldscan)) .le. 1.) skipscan = .true.
		     if (skipscan) goto 120
		  endif
	       endif
               if (scanno .ne. 0) then
                  if (itfits(idat(i1))) then
                     if (iacount .le. maxstck) then
                        if (lusescanno) then
                           astack(iacount) = scanno * sscale
                        else
                           astack(iacount) = 
     .                        (irec-1)*numindexperrec + i
                        end if
                        acount = float(iacount)
                        iacount = iacount + 1
                     else
c				ascack is full, end now
                        go to 990
                     endif
                  endif
               endif
 120        continue
 110     continue
c
      end if
c
      go to 990
c------------------------------------------------------------------------
c				Finish up
c------------------------------------------------------------------------
 990  continue
c				any errors?
      if (ierr .ne. 0) then
         call oerror(ierr, m2, 'SELECT')
      end if
c				how many were added?
      
      iadded = (iacount - iacstart)
      if (iadded .eq. 0) then
         write(cpuf, 1030) 'Select: No scans were added to ASTACK'
      else if (iadded .eq. 1) then
         write(cpuf, 1030) 'Select: 1 scan was added to ASTACK'
      else
         write(cpuf, 1990) iadded
      end if
      call pwrite(ipuf, n80)
c				do we sort it?
      if (s_sort .ne. 0 .and. iadded .gt. 2) then
         write(cpuf, 1030) 'Select: sorting additions to ASTACK'
         call pwrite(ipuf, n80)
c				uses sun qsort routine
         if (s_sort .gt. 0) 
     .       call qsort(astack(iacstart),iadded,i4,qcomplow)
         if (s_sort .lt. 0) 
     .       call qsort(astack(iacstart),iadded,i4,qcomphi)
c
      endif
c			done
 999  return
c------------------------------------------------------------------------  
 1030 format(a)
 1040 format(3x,'Object : "',a16,'" (S_OBJECT)')
 1041 format(3x,'Mode   : "',a4,'" (S_MODE)')
 1042 format(3x,'Program: ',a4)
 1043 format(3x,a17,6x,'DEFAULT',8x,'DEFAULT',6x,a)
 1044 format(3x,'Mode   : "',a4,'" is an invalid mode (S_MODE)')
 1045 format(4x,i5,6x)
 1046 format(1PG15.7)
 1047 format(3x,'Mode   : "',a4,'" (S_MODE) ',a)
 1048 format(3x,'Additions to ASTACK will begin at location ',i4,
     .          ' (ACOUNT + 1)')
 1049 format(3x,'Additions will be sorted in ',a,' order (S_SORT)')
 1050 format(9x,'Field',13x,'From',12x,'To',16x,'Adverbs')
 1990 format('Select: ',i4,' scans were added to ASTACK')
c
      end
