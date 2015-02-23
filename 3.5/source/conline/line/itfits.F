      logical*2 function itfits(idat)
c
c @(#)itfits.F	5.1 06/22/94
c
c      function used by select to see if the values of the various
c      index fields fit the selection criteria stored in core
c
      integer*2 idat(1)
      integer*2 index(32), i4pc, i8pc, i16pc, ii, iobsmode, obs, mode
      integer*2 is_mode, irtn, lastblnk
      integer*2 n1, n2, n4, n5, n7, n8, n9, n17, n19, n21, n25, n27
      integer*4 i1, inrange, inrange8
      real*4 rindex(16), scanno, feedno, sourcex, sourcey, lst, ut_date
#ifdef CONTINUUM
      real*4 rate
      real*8 inttime, dit_min, dit_max
#else
      real*4 bw
      real*8 rest_freq, df_min, df_max
#endif
      real*8 dindex(8), ddefault
      character*2 pname(2), curprog
      character*16 cindex(4), sname, upcs_object
      logical*2 localfit
c
      data n1, n2, n4, n5, n7, n8, n9, n17, n19, n21, n25, n27
     .     /1, 2, 4, 5, 7, 8, 9, 17, 19, 21, 25, 27/
c
      include 'core.inc'
c
      data pname / 'Cc', 'Ll'/
c
      equivalence (index, rindex, dindex, cindex)
c
      i4pc(ii) = (ii-n1)/n2 + n1
      i8pc(ii) = (ii-n1)/n4 + n1
      i16pc(ii) = (ii-n1)/n8 + n1
c
      do 10 i1 = 1, 32
         index(i1) = idat(i1)
 10   continue
c				default is true
      localfit = .true.
c				check obs mode, make sure matches program
      iobsmode = index(29)
      if (iobsmode .ne. 0 .and. localfit) then
         obs = iobsmode / 256
         if (obs .ne. 0 .and. localfit) then
            call progname(curprog)
            localfit = localfit .and.
     .         ((curprog .eq. pname(1) .and. obs .eq. 1) .or.
     .          (curprog .eq. pname(2) .and. obs .eq. 2)) 
         end if
         mode = iobsmode - obs*256
         call modeid(cs_mode, is_mode, irtn)
         if (mode .ne. 0 .and. localfit .and. is_mode .ne. 0) 
     .         localfit = localfit .and. (mode .eq. is_mode)
      endif
c				scan and feed number
c				scan number is just truncated but
c				feed number needs to be nearest int
      scanno = int(rindex(i4pc(n17)))
      feedno = nint((rindex(i4pc(n17)) - scanno) * 100.0)
c
      if (localfit) localfit = localfit .and. 
     .     (inrange(scan_min, scan_max, scanno, default) .eq. 1)
      if (localfit) localfit = localfit .and. 
     .     (inrange(feed_min, feed_max, feedno, default) .eq. 1)
c				sourcex and y position
      if (localfit) then
         sourcex = rindex(i4pc(n5))
         localfit = localfit .and. 
     .     (inrange(x_min, x_max, sourcex, default) .eq. 1)
      end if
      if (localfit) then
         sourcey = rindex(i4pc(n7))
         localfit = localfit .and. 
     .     (inrange(y_min, y_max, sourcey, default) .eq. 1)
      end if
c				source name
      if (localfit .and. lastblnk(cs_object) .ne. 0 .and.
     .                    s_object(1) .ne. 0.0) then
         call uppercase(cs_object, upcs_object)
         call uppercase(cindex(i16pc(n9)), sname)
         localfit = localfit .and. (sname .eq. upcs_object)
      end if
c				lst and ut_date
      if (localfit) then
         lst = rindex(i4pc(n25))
         localfit = localfit .and. 
     .     (inrange(lst_min, lst_max, lst, default) .eq. 1)
      end if
      if (localfit) then
         ut_date = rindex(i4pc(n27))
         localfit = localfit .and. 
     .     (inrange(ut_min, ut_max, ut_date, default) .eq. 1)
      end if
c
#ifdef CONTINUUM
c				integration time and rate
      if (localfit) then
         rate = rindex(i4pc(n19))
         localfit = localfit .and. 
     .     (inrange(rate_min, rate_max, rate, default) .eq. 1)
      endif
      if (localfit) then
         inttime = dindex(i8pc(n21))
         dit_min = it_min
         dit_max = it_max
         ddefault = default
         localfit = localfit .and. 
     .     (inrange8(dit_min, dit_max, inttime, ddefault) .eq. 1)
      endif
#else
c				bw and rest_freq
      if (localfit) then
         bw = rindex(i4pc(n19))
         localfit = localfit .and. 
     .      (inrange(bw_min, bw_max, bw, default) .eq. 1)
      endif
      if (localfit) then
         rest_freq = dindex(i8pc(n21))
         df_min = f_min
         df_max = f_max
         ddefault = default
         localfit = localfit .and. 
     .      (inrange8(df_min, df_max, rest_freq, ddefault) .eq. 1)
      endif
#endif
c			thats it
      itfits = localfit
      return
      end
