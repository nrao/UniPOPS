      SUBROUTINE    AU1 (J)
C-------------------------------------------------------------------------------
C  @(#)au1.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     au1 - Copy, Move, Remove
c            On, Off, Get, Get[0-9], Recall, Save, Keep, Cget, Check
c            Tell, Summary, GGet, CGGet, Chngonline, Kget
c
C-------------------------------------------------------------------------------
C
      INCLUDE 'appl.inc'
      include 'core.inc'
      INCLUDE 'cio.inc'
      INCLUDE 'cform.inc'
      include 'stk.inc'
c
      integer*2 j, itemp, iwork, ihold, iptwh, jj, iarrayout, 
     .          iarrayin, isave, imagic, ifil, ier
      integer*2 m1, m2, m3, n80, n105, n112, n120, n224, n226,
     .          n232, n263, n367
      integer*4 onlinesite, idtype, iver
      real*4 fscan, scan, icur
      character*60 errstr
c
      logical*2 oversave, overkeep
C
      DATA  ITEMP/2/, IWORK/1/, ihold/3/
c
      data m1, m2, m3, n80, n105, n112, n120, n224, n226,
     .          n232, n263, n367 
     .     /-1, -2, -3, 80, 105, 112, 120, 224, 226, 232, 263, 367/
C
C=======================================================================
C
      IPTWH = 1
c
      JJ = J
      GO TO ( 10, 20, 30,110,120,140,123,124,125,126,127,128,129,
     1        150,160,200,210,240,260,70,280,281,290,300), JJ
      call oerror(n120, m3, 'AU1')
C-----------------------------------------------------------------------
C					      COPY MOVE REMOVE
C-----------------------------------------------------------------------
  10  continue
      if (sp .le. 1) call oerror(n112, m1, 'COPY: Two needed')
c
      iarrayout = nint(v(sp))
      iarrayin = nint(v(sp-1))
      sp = sp - 2
c
      if (iarrayin .lt. 0 .or. iarrayin .gt. 9 .or.
     1    iarrayout .lt. 0 .or. iarrayout .gt. 9) 
     2    call oerror(n105, m1, 'COPY')
      if (iarrayin .eq. iarrayout) call oerror(n263, m2, 'COPY')
c
      if (dtwh(c12ni,iarrayin + 1) .eq. 0) call oerror(n226, m2, 'COPY')
c     Needs a scan to be in array to be copied
c
      call copy2(HDU_FLOAT_SIZE*2, itwh(1,iarrayin+1), 
     .     itwh(1,iarrayout+1) )
      goto 99
c---------------------------------------------------------------------
  20  continue
      if (sp .le. 1) call oerror(n112, m1, 'MOVE: Two needed ')
c
      iarrayout = nint(v(sp))
      iarrayin = nint(v(sp-1))
      sp = sp - 2
c
      if (iarrayin .lt. 0 .or. iarrayin .gt. 9 .or.
     1    iarrayout .lt. 0 .or. iarrayout .gt. 9) 
     2    call oerror(n105, m1, 'MOVE')
      if (iarrayin .eq. iarrayout) call oerror(n263, m2, 'MOVE')
c
      if (dtwh(c12ni,iarrayin + 1) .eq. 0) call oerror(n226, m2, 'MOVE')
c     Needs a scan to be in array to be moved
c
      call copy2(HDU_FLOAT_SIZE*2, itwh(1,iarrayin+1), 
     .     itwh(1,iarrayout+1) )
      call raz(dtwh(1,iarrayin+1))
      goto 99
c-------------------------------------------------------------
  30  continue
      if (sp .le. 0) call oerror(n112, m1, 'REMOVE: One needed')
c
      iarrayin = nint(v(sp))
      sp = sp - 1
c
      if (iarrayin .lt. 0 .or. iarrayin .gt. 9) 
     1		call oerror(n105, m1, 'REMOVE')
c
      call raz(dtwh(1,iarrayin+1))
      goto 99
C--------------------------------------------------------------------
C							  GET, GET0, ON
C---------------------------------------------------------------------
 110  IPTWH = IWORK
      goto 130
c
C---------------------------------------------------------------------
C							  OFF, GET1
C---------------------------------------------------------------------
 120  IPTWH = ITEMP
      goto 130
c
c--------------------------------------------------------------------
c     							GET2
c--------------------------------------------------------------------
 140  iptwh = ihold
      goto 130
c--------------------------------------------------------------------
c     							GET3
c--------------------------------------------------------------------
 123  iptwh = 4
      goto 130
c--------------------------------------------------------------------
c     							GET4
c--------------------------------------------------------------------
 124  iptwh = 5
      goto 130
c--------------------------------------------------------------------
c     							GET5
c--------------------------------------------------------------------
 125  iptwh = 6
      goto 130
c--------------------------------------------------------------------
c     							GET6
c--------------------------------------------------------------------
 126  iptwh = 7
      goto 130
c--------------------------------------------------------------------
c     							GET7
c--------------------------------------------------------------------
 127  iptwh = 8
      goto 130
c--------------------------------------------------------------------
c     							GET8
c--------------------------------------------------------------------
 128  iptwh = 9
      goto 130
c--------------------------------------------------------------------
c     							GET9
c--------------------------------------------------------------------
 129  iptwh = 10
C
 130  if (sp .le. 0) call oerror(n112, m1, 'GET0/9: One needed')
c
      fscan = v(sp)
      sp = sp - 1
      call getscan(fscan,iptwh,ier)
      if (ier .lt. 0) then
         call oerror(-ier, m1, 'GET0/9')
      else if (ier .gt. 0) then
         if (ier .eq. 362) then
            write(errstr, 1310) fscan
 1310       format('SCAN ',f10.2,' : GET0/9')
         else if (ier .eq. 371) then
            ier = 357
            write(errstr, 1320) fscan
 1320       format('SCAN ',f10.2,
     .             ' is not a valid scan number : GET0/9')
         else
            write(errstr, 1330)
 1330       format('GET0/9')
         endif
         call oerror(ier, m2, errstr)
      endif
      goto 99
C---------------------------------------------------------------------
C							  RECALL
C---------------------------------------------------------------------
 150  CONTINUE
      ISAVE=nint(NSAVE)
      call getbyloc(dtwh(1,iptwh),isave,imagic,iounit(3),ciounit(3),ier)
      if (ier .lt. 0) then
         call oerror(-ier, m1, 'RECALL')
      else if (ier .gt. 0) then
         if (ier .eq. 368 .or. ier .eq. 371) then
            write(errstr, 1510) isave
 1510       format('NSAVE ',i,' : RECALL')
         else
            write(errstr, 1520)
 1520       format('RECALL')
         endif
         call oerror(ier, m2, errstr)
      endif
      GO TO 99
C---------------------------------------------------------------------
C							  SAVE
C---------------------------------------------------------------------
 160  isave = nint(nsave)
c
      if (itwh(1,iwork) .eq. 0) call oerror(n224, m2, 'SAVE')
c     Needs a scan to be in array to be saved
c
      if (sprotect .le. 0) then
	oversave = .true.
      else
	oversave = .false.
      endif
      imagic = 2
      call savebyloc(dtwh(1,iptwh),isave,imagic,iounit(3),ciounit(3),
     1               oversave,ier)
      if (ier .lt. 0) then
         ier = -ier
         if (ier .eq. 355) then
            write(errstr, 1610)
 1610       format('SAVE file is full')
         else
            write(errstr, 1620)
 1620       format('SAVE')
         endif
         call oerror(ier, m1, errstr)
      else if (ier .gt. 0) then
         if (ier .eq. 355) then
            write(errstr, 1630) isave
 1630       format('Data exists at ',i,
     .             ' .... can not overwrite! : SAVE')
         else
            write(errstr, 1620)
         endif
         call oerror(ier, m2, errstr)
      endif
      goto 99
c----------------------------------------------------------------------
c    TELL
c----------------------------------------------------------------------
 200  continue
      call tells
      goto 99
C---------------------------------------------------------------------
C						      CGET
C---------------------------------------------------------------------
  210 CONTINUE
      IPTWH = IWORK
C
      if (sp .le. 0) call oerror(n112, m1, 'CGET: One needed')
c
      IFIL=nint(V(SP))
      SP=SP-1
      call getcurrent(ifil, iptwh,ier)
      if (ier .ne. 0) call oerror(ier, m2, 'CGET')
      GO TO 99
C-------------------------------------------------------------------------
C                                     DATA SUMMARY
C-------------------------------------------------------------------------
 240  CONTINUE
      call summary
      GO TO 99
C
C------------------------------------------------------------------------
C					 KEEP
C----------------------------------------------------------------------
  260 CONTINUE
c
      if (itwh(1,iwork) .eq. 0) then
	call oerror(n224, m2, 'KEEP')
	goto 99
      endif
c     Needs a scan to be in array to be kept
c
      if (kprotect .le. 0) then
	overkeep = .true.
      else
	overkeep = .false.
      endif
      imagic = 2
      call savebynum(dtwh(1,iptwh),dtwh(c1sno,iptwh),imagic,iounit(2), 
     1               ciounit(2), overkeep,ier)
      if (ier .lt. 0) then
         ier = -ier
         if (ier .eq. 355) then
            write(errstr, 2601)
 2601       format('KEEP file is full')
         else
            write(errstr, 2602)
 2602       format('KEEP')
         endif
         call oerror(ier, m1, errstr)
      else if (ier. gt. 0) then
         if (ier .eq. 355) then
            write(errstr, 2603) dtwh(c1sno,iptwh)
 2603       format('Scan ',f10.2,
     .             ' exists ... can not overwrite! : KEEP')
         else
            write(errstr, 2602)
         endif
         call oerror(ier, m2, errstr)
      endif
      GO TO 99
C---------------------------------------------------------------------
C					  CHECK
C---------------------------------------------------------------------
 70   ISAVE=nint(NSAVE)
      call checkbyloc(isave, iounit(3), ciounit(3), scan, ier)
      if (ier .lt. 0) then
	call oerror(-ier, m1, 'CHECK')
      else if (ier .gt. 0) then
	call oerror(ier, m2, 'CHECK')
      else 
	if (scan .gt. 0) then
	   WRITE (CPUF,73,IOSTAT=IER) ISAVE, SCAN
        else
           WRITE (CPUF,74,IOSTAT=IER) ISAVE
	endif
      endif
      CALL PWRITE (IPUF,n80)
 73   FORMAT(' NSAVE = ',I4, ' contains scan ',F8.2)
 74   FORMAT(' NSAVE = ',I4, ' is empty.')
      GO TO 99
C----------------------------------------------------------------------
C                                        GGET 
C-----------------------------------------------------------------------
 280  continue
      icur = 0
      iptwh = ihold
c                       gains are loaded into ihold
      if (sp.le.0) call oerror(n112, m1, 'GGET: One needed.')
c                       GGET needs a scan number
      fscan = v(sp)
      sp = sp - 1
c
      call gget(fscan, iptwh, icur, ier)
      if (ier .lt. 0) then
         call oerror(-ier, m1, 'GGET')
      else if (ier .gt. 0) then
         if (ier .eq. 362) then
            write(errstr, 2801) fscan
 2801       format('SCAN ',f10.2,' : GGET')
         else if (ier .eq. 371) then
            ier = 357
            write(errstr, 2802) fscan
 2802       format('SCAN ',f10.2,
     .             ' is not a valid scan number : GGET')
         else
            write(errstr, 2803)
 2803       format('GGET')
         endif
         call oerror(ier, m2, errstr)
      endif
      goto 99
C----------------------------------------------------------------------
C                                        CGGET 
C-----------------------------------------------------------------------
 281  continue
      icur = 1
c			signal that current gain scan is wanted
      iptwh = ihold
c			gains are loaded into ihold 
      if (sp.le.0) call oerror(n112, m1, 'CGGET: One needed.')
c			CGGET needs a rec/subscan number
      fscan = v(sp)
      sp = sp - 1
c
      call gget(fscan, iptwh, icur, ier)
      if (ier .lt. 0) then
         call oerror(-ier, m1, 'CGGET')
      else if (ier .gt. 0) then
         if (ier .eq. 362) then
            write(errstr, 2811) nint(fscan)
 2811       format('Subscan ',i,' : CGGET')
         else if (ier .eq. 371) then
            ier = 357
            write(errstr, 2812) nint(fscan)
 2812       format('Subscan ',i,
     .             ' is not a valid subscan number : CGGET')
         else
            write(errstr, 2813)
 2813       format('CGGET')
         endif
         call oerror(ier, m2, errstr)
      endif
      goto 99
C-----------------------------------------------------------------------
C					CHNGONLINE
C-----------------------------------------------------------------------
  290 continue
      if (sp .le. 1) call oerror(n112, m1, 'CHNGONLINE: Two needed.')
C
      if (online) then
         if (onlinesite() .eq. 1) then
            idtype = v(sp-1) 
            iver = v(sp)
            sp = sp - 2
            call chngol(idtype, iver)
         else
            call oerror(n232, m2, 'CHNGONLINE.')
         endif
      else
         call oerror(n367, m2, 'CHNGONLINE.')
      endif
      goto 99
C-----------------------------------------------------------------------
C					KGET
C-----------------------------------------------------------------------
 300  continue
      if (sp .le. 0) call oerror(n112, m1, 'KGET: One needed.')
c
      fscan = v(sp)
      sp = sp - 1
      iptwh = 1
      call getbynum(dtwh(1,iptwh),fscan,imagic,iounit(2),ciounit(2),ier)
      if (ier .lt. 0) then
         call oerror(-ier, m1, 'KGET')
      else if (ier .gt. 0) then
         if (ier .eq. 362) then
            write(errstr, 3001) fscan
 3001       format('SCAN ',f10.2,' : KGET')
         else if (ier .eq. 371) then
            ier = 357
            write(errstr, 3002) fscan
 3002       format('SCAN ',f10.2,' is not a valid scan number : KGET')
         else
            write(errstr, 3003)
 3003       format('KGET')
         endif
         call oerror(ier, m2, errstr)
      endif
      goto 99
C-----------------------------------------------------------------------
  99  CONTINUE
      RETURN
      END

