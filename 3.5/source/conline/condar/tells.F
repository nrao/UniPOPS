      SUBROUTINE TELLS 
C-------------------------------------------------------------------------------
C  @(#)tells.F	5.1 06/22/94
C-------------------------------------------------------------------------------
c
      INTEGER*2 iactual2, ifun, i1, i2, kp, 
     .          ia, inonzero, ier, iacct
c
      real*4 scans(32767), scanno
      integer*2 m3, m2, m1, n1, n80, n112, n120, n232, n281
      integer*2 jmax, inonrpt
      integer*4 numscans, numout, istype, iscanno, isubsc
      integer*4 imax, iactual, irtn
      logical*2 ldoblank, lastblnk
      character*1 prefix
C
#ifdef CONTINUUM
#else
      integer*4 onlinesite
#endif
C
      include 'core.inc'
      INCLUDE 'appl.inc'
      include 'stk.inc'
      INCLUDE 'cform.inc'
      INCLUDE 'cio.inc'
c
      data imax, jmax /32767, 32767/
      data m3, m2, m1, n80, n112, n120, n232, n281, n1
     .     /-3, -2, -1, 80, 112, 120, 232, 281, 1/
c
C=======================================================================
C
      IF (SP.lt.1) call oerror(n112, m1, 'TELL')
      IFUN = nint(V(SP))
      SP = SP-1
      IF (IFUN.lt.1.or.IFUN.gt.19) call oerror(n112, m1, 'TELL')
c
      GO TO (10,40,50,70,75,80,80,80,80,80,80,80,
     .       20,30,60,90,10,80,100), IFUN
      call oerror(n120, m3, 'TELLS')
c--------------------------------------------------------------------
C                                               TELL dscans
c--------------------------------------------------------------------
   10 CONTINUE
c
      if (online) then
c
#ifdef CONTINUUM
        call gcontlist2(iactual, imax, scans, irtn)
#else
        call gscanlist2(iactual, imax, scans, irtn)
#endif
c
        if (irtn .eq. 0 .and. iactual .gt. 0) then
           numscans = min(iactual, imax)
           ldoblank = .false.
           prefix = 'O'
           istype = 1
c		check to see if they really are integers
           scanno = scans(1)
           iscanno = ifix(scanno)
           isubsc = ifix((scanno - iscanno)*100.0 + 0.5)
           if (isubsc .ne. 0) istype = 0
c
           call tellout(scans,numscans,ldoblank,prefix,istype,numout)
           if (numout .eq. 0) then
              write(cpuf, 159) 'Online data file is empty'
              call pwrite(cpuf, n80)
           end if
        else
	  write(cpuf,159) 'Online data file may not exist or is empty'
159       format(8a)
	  call pwrite(ipuf,n80)
        endif
      else
	write(cpuf,159) 'Online data file is not available'
	call pwrite(ipuf,n80)
      endif
c
      if (ifun .eq. 17) goto 99
c				tell offdata
90    if (iounit(1) .le. 0) then
	write(cpuf,159) 'Offline Data file does not exist'
	call pwrite(ipuf,n80)
      else
	call getlist(scans, jmax, iactual2, iounit(1), ciounit(1), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
	numscans = iactual2
        ldoblank = .false.
        prefix = 'D'
        istype = 0
        call tellout(scans, numscans, ldoblank, prefix, istype, numout)
c
	if (numout .eq. 0) then
	  write(cpuf,159) ciounit(1)(1:lastblnk(ciounit(1))),
     .			  ' (Offline Data) file is empty'
	  call pwrite(ipuf,n80)
        endif
c
      endif
c
      GO TO 99
c--------------------------------------------------------------------
C                                         TELL ACCMD
c--------------------------------------------------------------------
   20 CONTINUE
c
      if (nint(dtwh(c13ns,1)) .ge. 1) then
         WRITE (CPUF,2077,IOSTAT=IER) nint(dtwh(c13ns, 1)), 
     1				dtwh(c13fs, 1), dtwh(c13ls,1)
 2077    FORMAT (i7, ' scans from:', f9.2, ' to ',
     1           f9.2, ' accummed in Array(0).' )
       	 CALL PWRITE (IPUF,n80)
c
      else
	 cpuf = '      The scan in Array(0) was not produced by ACCUM !'
         CALL PWRITE(IPUF,n80)
      endif
c
      IF(NACCUM.GT.0)THEN
	IF(NACCUM.LE.100)THEN
		IACCT=NACCUM
		WRITE(CPUF,2080)
      		CALL PWRITE (IPUF,n80)
		WRITE(CPUF,2078,IOSTAT=IER)NACCUM
 2078		FORMAT('      The ',i7,' scans accumulated so far are :')
	ELSE
		IACCT=100
		WRITE(CPUF,2080)
      		CALL PWRITE (IPUF,n80)
		WRITE(CPUF,2079,IOSTAT=IER)NACCUM
 2079	FORMAT('      Of the 'i7,' scans accumed so far',
     .					', the first 100 are :')
	ENDIF
      CALL PWRITE (IPUF,n80)
      do 92 i1 = 1, iacct, 7
	     i2 = min(i1+6,iacct)
	     write(cpuf,9614) i1, (DAINDEX(kp),kp=i1,i2)
9614         format(1x,i7,':',7f9.2)
             CALL PWRITE(IPUF,n80)
   92        CONTINUE
      else
	WRITE(CPUF,2080)
	CALL PWRITE(IPUF,n80)
	CPUF = '      No ACCUMulation of scans is taking place currently.'
	CALL PWRITE(IPUF,n80)
	ENDIF
c
      WRITE(CPUF,2080)
      CALL PWRITE(IPUF,n80)
      IF(NACCUM.GT.100)THEN
	WRITE(CPUF,2081) DAINDEX(101)
 2081   FORMAT(12x, 'The scan most recently ACCUMulated was ', f9.2)
        CALL PWRITE(IPUF,n80)
      ENDIF
c
      GO TO 99

 2080 FORMAT()
c--------------------------------------------------------------------
C                        TELL STACK 
c--------------------------------------------------------------------
   30 CONTINUE
c
      IA=nint(ACOUNT)
c
      if (ia .lt. 0. .or. ia .gt. maxstck) 
     .		call oerror(n281, m2, 'TELL')
c
      if (ia .eq. 0) then
         write(cpuf, 9612)
9612     format(1x,'ASTACK is empty.')
         call pwrite(ipuf, n80)
      else
         do 91 i1 = 1, ia, 7
	    i2 = min(i1+6,ia)
	    write(cpuf,9613) i1, (astack(kp),kp=i1,i2)
9613        format(1x,i7,':',7f9.2)
            CALL PWRITE(IPUF,n80)
   91    CONTINUE
      endif
c
      GO TO 99
c--------------------------------------------------------------------
C                                    Tell KSCANS
c--------------------------------------------------------------------
   40 CONTINUE
c
      if (iounit(2) .le. 0) then
	write(cpuf,159) 'Keep file does not exist'
	call pwrite(ipuf,n80)
      else
        call getlist(scans, jmax, iactual2, iounit(2), ciounit(2), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
        numscans = iactual2
        ldoblank = .false.
        prefix = 'K'
        istype = 0
        call tellout(scans, numscans, ldoblank, prefix, istype, numout)
c
	if (numout .eq. 0) then
	  write(cpuf,159) ciounit(2)(1:lastblnk(ciounit(2))),
     .			  ' (Keep) file is empty'
	  call pwrite(ipuf,n80)
        endif
c
      endif
c
      GO TO 99
c--------------------------------------------------------------------
C                                    Tell RSCANS
c--------------------------------------------------------------------
   75 CONTINUE
c
      if (iounit(5) .le. 0) then
	write(cpuf,159) 'Records file does not exist'
	call pwrite(ipuf,n80)
      else
        call getlist(scans, jmax, iactual2, iounit(5), ciounit(5), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
c
        numscans = iactual2
        ldoblank = .false.
        prefix = 'R'
        istype = 0
c
        call rmextra(scans, iactual2, n1, inonrpt)
c
        call tellout(scans, numscans, ldoblank, prefix, istype, numout)
c
	if (numout .eq. 0) then
	  write(cpuf,159) ciounit(5)(1:lastblnk(ciounit(5))),
     .			  ' (Records) file is empty'
	  call pwrite(ipuf,n80)
        endif
c
      endif
c
      GO TO 99
c--------------------------------------------------------------------
C                              TELL SSCANS
c--------------------------------------------------------------------
   50 CONTINUE
c
      if (iounit(3) .le. 0) then
	write(cpuf,159) 'Save file does not exist'
	call pwrite(ipuf,n80)
      else
	call getlist(scans, jmax, iactual2, iounit(3), ciounit(3), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
        numscans = iactual2
        ldoblank = .true.
        prefix = 'S'
        istype = 0
        call tellout(scans, numscans, ldoblank, prefix, istype, numout)
c           
	if (numout .eq. 0) then
	  write(cpuf,159) ciounit(3)(1:lastblnk(ciounit(3))),
     . 			  ' (Save) file is empty'
	  call pwrite(ipuf,n80)
        endif
c
      endif
c
      GO TO 99
c--------------------------------------------------------------------
C                              TELL DISK
c--------------------------------------------------------------------
   60 CONTINUE
c
      if (online) then
c
#ifdef CONTINUUM
        call gcontlist2(iactual, imax, scans, irtn)
#else
        call gscanlist2(iactual, imax, scans, irtn)
#endif
c
        if (irtn .eq. 0) then
	  write(cpuf,9611) 'online','data',iactual
9611      format('The ', a, 1x, a, ' file contains ', i6, ' scans')
	else
	  write(cpuf,159) 'The on-line data file is Not Available'
        endif
        call pwrite(ipuf,n80)
#ifdef CONTINUUM
c		gains are line only
#else
        if (onlinesite() .eq. 1) then
           call ggscanlist2(iactual, imax, scans, irtn)
           if (irtn .eq. 0) then
              write(cpuf, 9611) 'online','gains',iactual
           else
              write(cpuf,159) 
     +           'The on-line gains file is Not Available'
           endif
           call pwrite(ipuf,n80)
        endif
#endif
      else
	write(cpuf,159) 'The on-line data file is Not Available'
        call pwrite(ipuf,n80)
      endif
c     Get number of scans from On-Line data file
c 
      if (iounit(1) .le. 0) then
	write(cpuf,159) 'Offline Data file does not exist'
      else
	call getlist(scans, jmax, iactual2, iounit(1), ciounit(1), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
        inonzero = 0
        do 360 i1 = 1, iactual2
           if (scans(i1).ne.0) inonzero = inonzero + 1
 360       continue
	write(cpuf,9611) ciounit(1)(1:lastblnk(ciounit(1))),
     . 			  '(Offline Data)', inonzero
      endif
      call pwrite(ipuf,n80)
c     Get num. for off-line file
c
      if (iounit(2) .le. 0) then
	write(cpuf,159) 'Keep file does not exist'
      else
	call getlist(scans, jmax, iactual2, iounit(2), ciounit(2), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
        inonzero = 0
        do 361 i1 = 1, iactual2
           if (scans(i1).ne.0) inonzero = inonzero + 1
 361       continue
      	write(cpuf,9611) ciounit(2)(1:lastblnk(ciounit(2))),
     . 			 '(Keep)', inonzero
      endif
      call pwrite(ipuf,n80)
c     Get num. for KEEP file
c
      if (iounit(3) .le. 0) then
	write(cpuf,159) 'Save file does not exist'
      else
	call getlist(scans, jmax, iactual2, iounit(3), ciounit(3), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
        inonzero = 0
        do 362 i1 = 1, iactual2
           if (scans(i1).ne.0) inonzero = inonzero + 1
 362       continue
	write(cpuf,9611) ciounit(3)(1:lastblnk(ciounit(3))),
     . 			 '(Save)', inonzero
      endif
      call pwrite(ipuf,n80)
c     Get num. for SAVE file
c
#ifdef CONTINUUM
#else
c	for off-line gains shut up unless file is available
      if (onlinesite() .ne. 0 .and. iounit(4) .gt. 0) then
        call getlist(scans, jmax, iactual2, iounit(4), ciounit(4), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
        inonzero = 0
        do 363 i1 = 1, iactual2
           if (scans(i1).ne.0) inonzero = inonzero + 1
 363    continue
        write(cpuf,9611) ciounit(4)(1:lastblnk(ciounit(4))),
     . 			 '(Gains)', inonzero
        call pwrite(ipuf,n80)
      endif
c     Get num. for Gains file
c
c	for off-line records shut up unless file is available
      if (onlinesite() .ne. 1 .and. iounit(5) .gt. 0) then
        call getlist(scans, jmax, iactual2, iounit(5), ciounit(5), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
        inonzero = 0
        do 364 i1 = 1, iactual2
           if (scans(i1).ne.0) inonzero = inonzero + 1
 364    continue
c
        call rmextra(scans, iactual2, n1, inonrpt)
c
        write(cpuf,9615) ciounit(5)(1:lastblnk(ciounit(5))),
     . 			 '(Records)', inonzero, inonrpt
9615      format('The ', a, 1x, a, ' file contains ', i6, 
     .		' records and ', i6, ' scans')
        call pwrite(ipuf,n80)
      endif
c     Get num. for Records file
c
#endif
      goto 99
c--------------------------------------------------------------------
C                              TELL GSCANS  gains
c--------------------------------------------------------------------
   70 CONTINUE
c
#ifdef CONTINUUM
c		online gains are line only
#else
      if (online) then
         if (onlinesite() .ne. 1) then
            call oerror(n232, m2, 'TELL GSCANS')
            goto 99
         endif
c
        call ggscanlist2(iactual, imax, scans, irtn)
        if (irtn .eq. 0 .and. iactual .gt. 0) then
           numscans = min(iactual, imax)
           ldoblank = .false.
           prefix = "G"
           istype = 1
c		check to see if they really are integers
           scanno = scans(1)
           iscanno = ifix(scanno)
           isubsc = ifix((scanno - iscanno)*100.0 + 0.5)
           if (isubsc .ne. 0) istype = 0
c
           call tellout(scans,numscans,ldoblank,prefix,istype,numout)
           if (numout .eq. 0) then
              write(cpuf, 159) 'Online Gains file is empty'
              call pwrite(cpuf, n80)
           endif
        else
           write(cpuf,159) 
     +       'Online Gains file may not exist or is empty'
           call pwrite(ipuf,n80)
        endif
c		if online file, look at gains file if available
      else
	write(cpuf,159) 'Online gains file is not available'
	call pwrite(ipuf,n80)
      endif
#endif
c
      if (iounit(4) .le. 0) then
	write(cpuf,159) 'Offline gains file does not exist'
	call pwrite(ipuf,n80)
      else
	call getlist(scans, jmax, iactual2, iounit(4), ciounit(4), ier)
        if (ier .ne. 0) call oerror(ier, m2, 'TELLS')
	numscans = iactual2
        ldoblank = .false.
        prefix = 'g'
        istype = 0
        call tellout(scans, numscans, ldoblank, prefix, istype, numout)
c
	if (numout .eq. 0) then
	  write(cpuf,159) ciounit(4)(1:lastblnk(ciounit(4))),
     . 			  ' (Offline Gains) file is empty'
	  call pwrite(ipuf,n80)
        endif
c
      endif
c
      GO TO 99
c--------------------------------------------------------------------
C                              TELL STAT
c--------------------------------------------------------------------
  100 CONTINUE
      call tellstat(status)
      goto 99
c--------------------------------------------------------------------
c			inappropriate argument (6,7,8,9,10,11,12)
c--------------------------------------------------------------------
 80   continue
      call oerror(n112, m2, 'TELL')
      goto 99
C----------------------------------------------------------------------
   99 CONTINUE
      RETURN
C-----------------------------------------------------------------------
      END
c
      subroutine rmextra(list, n,  style, nrtn)
c
c     Removes redundent entries from LIST -- LIST is modified
c     Style = 1 if integer comparison; else float
c     N = Number of entrie to look through
c     NRTN = Number found that were different
c
      real list(*)
      integer*2 n, style, nrtn
c
      integer inonrpt, i1
      logical skip
c
      skip = .false.
c
      if (style .eq. 1) then
        inonrpt = 1
        do 751 i1 = 2, n
           if (nint(list(i1)). ne. nint(list(inonrpt))) then
		skip = .false.
	   else
		if ( nint(100.*(list(i1)-nint(list(inonrpt)))) .le. 1.) 
     .			skip = .true.
	   endif
	   if (.not. skip) then
     		inonrpt = inonrpt + 1
		list(inonrpt) = list(i1)
	   endif
	   if (i1 .gt. inonrpt) list(i1) = 0.
 751    continue
c
      else
        inonrpt = 1
        do 752 i1 = 2, n
           if (list(i1) .ne. list(inonrpt) ) skip = .false.
	   if (.not. skip) then
     		inonrpt = inonrpt + 1
		list(inonrpt) = nint(list(i1))
	   endif
	   if (i1 .gt. inonrpt) list(i1) = 0.
 752    continue
c
      endif
c
      nrtn = inonrpt
c
      return
      end
