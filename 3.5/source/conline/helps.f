      SUBROUTINE HELPS (J)
C-------------------------------------------------------------------------------
C  @(#)helps.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C	 HELPS contains	the user assistance facility.
C---------------------------------------------------------------------
      INTEGER*2 VMSG(40), VMSG1(40), VMSG2(40), istch(80),
     .		j, jblank, lastblnk, length, isbjct(80), itopics(5,200),
     .		n0, n1, n2, n9, n17, n72, n80, n120, m1, m3,
     .		n28, n29, lngth
      logical*2 inquirefile, nmem
      integer*4 jj, ilen, istat, i, kbp, ll, mm, ii, kbc, ilen2, ier,
     .          ij, lp, irtn, ik
      character*160 sbjct, stch, ucsbjct, filename
      character*10 topics(200)
      integer*4 n10, kbpm1, system
      CHARACTER*80 CVMSG1, CVMSG, CVMSG2
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
c
      equivalence (sbjct,isbjct), (itopics, topics), (istch, stch)
      EQUIVALENCE  (VMSG, CVMSG), (VMSG1, CVMSG1), (VMSG2, CVMSG2)
c
      external compar
c
      include 'tags.inc'
c
      data n1/1/, n2/2/, n17/17/, n72/72/, n80/80/,
     .	   n120/120/, m1/-1/, m3/-3/, n10/10/, n0/0/, n9/9/,
     .     n28 /28/, n29/29/
c
      data jblank/'  '/
      DATA CVMSG/'List of defined '/
      DATA CVMSG1/
     .'For a detailed explanation use EXPLAIN <name>.  For a summary'/
      data cvmsg2/
     .'explanation, use HELP <name>; <name> = item of interest'/
C
C=======================================================================
C
      if (mode .ne. 0) call oerror(n17, m1, ' ')
c
      JJ = J
      GO TO (100,9909,9909,200,220), JJ
      call oerror(n120, m3, 'HELPS')
c
9909  call oerror(n2, m1, 'Type HELP HELP for help on HELP')
c
C---------------------------------------------------------------------
C	     H	  E    L    P
C---------------------------------------------------------------------
 100  continue
c
      CALL GETFLD
c
      IF (nkar .eq. 0 .or. tag .eq. ihelps) then
	irtn = system('help.exe Help')
        if (irtn .ne. 0) call oerror(n9,m1,'HELP')
	GO TO 99
      endif
c     For HELP HELP or just HELP
c
      if (type .eq. 11 .or. type .eq. 14) call oerror(n29, m1, 'HELP')
c
      ilen = lastblnk(cvmsg)
      type = 0
c
      IF (TAG .EQ. iverbs) THEN
         write(stch,101,iostat=istat) cvmsg(1:ilen), 'VERBS'
101	 format(a,1x,a)
         TYPE=4
      else if (tag .eq. iadverbs) then
	 write(stch,101,iostat=istat) cvmsg(1:ilen), 'ADVERBS'
	 type = -1
      else IF (TAG .EQ. iprocedure) THEN
	 write(stch,101,iostat=istat) cvmsg(1:ilen), 'PROCEDURES'
         TYPE=3
      else IF (TAG .EQ. iarray) THEN
	 write(stch,101,iostat=istat) cvmsg(1:ilen), 'ARRAY adverbs'
         TYPE=2
      else IF (tag .eq. iscalar) then
	 write(stch,101,iostat=istat) cvmsg(1:ilen), 'SCALAR adverbs'
	 type = 1
      else IF (tag .eq. istring) then
	 write(stch,101,iostat=istat) cvmsg(1:ilen), 'STRING adverbs'
	 type = 7
      else IF (tag .eq. ipointer) then
	 write(stch,101,iostat=istat) cvmsg(1:ilen), 
     .			'POINTER/KEYWORD adverbs'
	 type = 8
      else IF (tag .eq. ialias) then
	 write(stch,101,iostat=istat) cvmsg(1:ilen),'ALIASES'
	 type = 6
      endif
c
      if (type .ne. 0) then
c     (i.e., PROC, SCALAR, ADVERBS, ARRAYS, etc. was specified.)
c
	call pwrite(jblank,n1)
        call pwrite(istch,n80)
        call pwrite(cvmsg1,n80)
        call pwrite(cvmsg2,n80)
	call pwrite(jblank,n1)
c
        l = 1
	kbp = 1
c       Reset L, which has been changed by GETFLD above, so that the search
c       will begin from the start of core.
c
 110    L=K(L)
        IF (L.ne.0) then
          LL=K(L+1)/16
          MM=K(L+1)-16*LL
          if((type.eq.-1 .and. mm.ne.1 .and. mm.ne.2 .and. mm.ne.7 .and.
     .		mm.ne.8)
     1       .or. (type.ne.-1 .and. type.ne.mm)) goto 110 
c         1, 2, 7, and 8 are the TYPES for ADVERBS.  This is for the case that
c         ADVERBS are desired
c
C					   Copy	name.
	  LP=L+4
	  topics(kbp) = ' '
          DO 111 II = 1,min(LL,5)
            itopics(ii,kbp)=K(LP+II-1)
  111       CONTINUE
	  KBP=KBP+1
	  goto 110
c
        else if (kbp .gt. 1) then
c
	  kbpm1 = kbp - 1
          call qsort(topics, kbpm1, n10, compar)
	  do 6230 ii = 1, kbp-1, 6
	     sbjct = ' '
	     do 6231 kbc = ii, min(ii+5,kbp-1)
	       sbjct( (kbc-ii)*12+1: (kbc-ii)*12+12) = topics(kbc)
6231	       continue
             CALL PWRITE (isbjct,n72)
6230	     continue
c
	endif
c
        IF (TYPE.ne.4) then
	    call pwrite(jblank,n1)
	    goto 99
        else
            type = 5
            l = 1
	    kbp = 1
	    call pwrite(jblank,n1)
	    write(stch,101,iostat=istat) 'Pseudo Verbs:'
	    call pwrite(istch,n80)
	    goto 110
        endif
c       Once type 4 verbs are done with, start on PSEUDO operators until they
c	are all used up
c
      else
c     (i.e., NOT scalar, adverb, proc, array)
c
        ilen2 = lastblnk(dirprefix(1))
        sbjct = ' '
	if (tag .gt. 0) then
	  length = k(l+1)/16
	  call copy(length,k(l+4),isbjct)
	else
	  length=(nkar+1)/2
	  call copy(length,kpak,isbjct)
	endif
        filename = dirprefix(1)(1:ilen2) // 'help/' // sbjct
c       If the user has typed HELP <name>, where name is not SCALAR, PROC,
c       ARRAY, or VERB, but <name> is one of the entries in POPSDAT, then
c       'filename' contains POPS commands which describe <name> and prints
c       out ADVERB values, if needed.
c
        ier = -1
        if(inquirefile(filename) ) open(unit=ishelpin,status='old',
     1		iostat=ier,file=filename)
        if (ier .eq. 0) rewind(ishelpin,iostat=ier)
c
        if(ier.eq.0) then
          if (iinptr .ge. 50) then
c			iinlist is FULL, OUCH
             close(unit=ishelpin, iostat=ier)
             call oerror(n28, m1, 'HELP')
          else
             iinptr = iinptr + 1
             iinlist(iinptr, 1) = ishelpin
             iinlist(iinptr, 2) = ishelpin
             cinlist(iinptr) = filename
	     iinunit = iinlist(iinptr, 1)
	     iintype = iinlist(iinptr, 2)
          endif
	else
	  call oerror(n2, m1, 'No documentation on ' // 
     .		sbjct(1:lastblnk(sbjct)))
        endif
        goto 99
c       Opens help file if it exists.  Help file consists of POPS commands
c       which will generate the necessary information for the user
c
      endif
c
c------------------------------------------------------------------------
c     E X P L A I N 
c------------------------------------------------------------------------
  200 CONTINUE
      call nextfield(sbjct)
      ilen = lastblnk(sbjct)
      call uppercase(sbjct,ucsbjct)
      irtn = system('explain2.exe ' // ucsbjct)
      if (irtn .ne. 0) call oerror(n9,m1,'EXPLAIN')
      goto 99
c------------------------------------------------------------------------
c    H I S T O R Y
c------------------------------------------------------------------------
 220  continue
c
      call getfld
      if (nkar .eq. 0 .or. type.ne.11) then
	nmem = maxhist
      else
        nmem= max(1,min(maxhist,1+nint(x(1))))
      endif
c
      do 221 i = max(1, inhist-nmem+2), inhist 
	ij = mod(i-1,maxhist) + 1
	lngth = lastblnk(hist(ij))
	if (lngth .le. 0) goto 221
	do 2209 ik = 1, lngth, linelngth-6
	   write(stch,2205,iostat=istat) i, hist(ij)(ik:min(lngth,ik+linelngth-7))
2205       format(i5,':',a)
	   if (ik .ne. 1) stch(1:6) = "      "
	   call pwrite(istch,linelngth)
2209       continue
221     continue
      goto 99
   99 CONTINUE
      RETURN
      END
c
