      subroutine chngver(iftype)
c-----------------------------------------------------------------------
c @(#)chngver.F	5.1 06/22/94
c
c  Fortran front end to changever on-line function
c  Currently this only works for TUCSON data, but the check for that happens
c  in stores.  This routine exists only to check that iftype is appropriate
c  for the program currently being used (line or condar).
c------------------------------------------------------------------------
c
      integer*2 m1, n300, ier
      integer*4 iftype, iver
#ifdef CONTINUUM
#else
      character*80 cans, uans
#endif
      character*2 fb, hc
c
      include 'cio.inc'
      include 'core.inc'
c
      data m1, n300 /-1, 300/
      data fb /'FB'/, hc /'HC'/
c
#ifdef CONTINUUM
      if (iftype .eq. -1) iftype = 3
      if (iftype .ne. 3) call oerror(n300, m1, 'CHNGVER')
#else
      if (iftype .eq. -1) then
 100     write(istdout,1000)
 1000    format(' Type of on-line file ',
     .          '(FB=filterbank, HC=hybrid correlator) : ',$)
         read(istdin, 1010) cans
 1010    format(a80)
         call uppercase(cans, uans)
         if (uans(1:2) .eq. fb) then
            iftype = 2
         else if (uans(1:2) .eq. hc) then
            iftype = 1
         else
            write(istdout, 1020) cans(1:2)
 1020       format(' Unrecognized file type : ',a2)
            goto 100
         endif
      endif
c
      if (iftype .ne. 1 .and. iftype .ne. 2) 
     .   call oerror(n300, m1, 'CHNGVER')
#endif
c
      call changever(iftype, iver, projcode, ier)
      if (ier .ne. 0) call oerror(ier, m1, 'CHNGVER')
c				set apropriate pointer adverb
      if (iftype .eq. 1) hcver = iver
      if (iftype .eq. 2) fbver = iver
      if (iftype .eq. 3) conver = iver
c
      return
      end
