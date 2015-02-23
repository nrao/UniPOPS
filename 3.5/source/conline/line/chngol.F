      subroutine chngol(iftype, iver)
c------------------------------------------------------------------------
c @(#)chngol.F	5.1 06/22/94
c
c     This subroutine is the fortran frontend to the on-line function
c     chngonline, which changes both the online file type and the version.
c     The validity of the version is checked in chngonline but the
c     appropriateness of the file type is checked here.  
c------------------------------------------------------------------------
c
      integer*4 iftype, iver
      integer*2 m1, m2, n300, ier
c
      include 'cio.inc'
      include 'core.inc'
c
      data m1 /-1/ , m2 /-2/, n300 /300/
c
#ifdef CONTINUUM
      if (iftype .ne. 3) call oerror(n300, m1, 'CHNGONLINE')
#else
      if (iftype .ne. 1 .and. iftype .ne. 2) 
     .   call oerror(n300, m1, 'CHNGONLINE')
#endif
c
      call chngonline(iftype, iver, projcode, ier)
      if (ier .ne. 0) call oerror(ier, m2, 'CHNGONLINE')
c                   set pointer appropriate pointer adverb
      if (iftype .eq. 1) hcver = iver
      if (iftype .eq. 2) fbver = iver
      if (iftype .eq. 3) conver = iver
c
      return
      end
