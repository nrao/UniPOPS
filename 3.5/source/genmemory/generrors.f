
C-------------------------------------------------------------------------------
C  @(#)generrors.f	5.1 06/22/94
C-------------------------------------------------------------------------------
      SUBROUTINE oerror (iecode, ierr, routine)
C---------------------------------------------------------------------
C Modified 890105 [PPM] includes to .inc, lowercase
C          8903   [RJM] see CHANGES.DOC
C---------------------------------------------------------------------
      integer*2 ibuffer(40), istat, k, iecode, ierr
      integer*2 n1, n10, n80
      real*4 c
      character*80 cbuffer
      character*(*) routine
c
      INCLUDE 'smstuf.inc'
c
      equivalence (ibuffer, cbuffer)
c
      data n1, n10, n80 /1, 10, 80/
C
C=======================================================================
C
      WRITE (cbuffer,2005,iostat=istat) IECODE, ierr, routine
      if (istat .eq. 0) call pwrite(ibuffer, n80)
      WRITE (cbuffer,2010,iostat=istat) KPAK
      if (istat .eq. 0) call pwrite(ibuffer, n80)
      CALL KDUMP (n1,n10,K,C)
      RETURN
C---------------------------------------------------------
 2005 FORMAT (1X, 'ERROR=', I5, i5, a)
 2010 FORMAT (' KPAK:', 30A2)
 2000 FORMAT (' ERROR',I3,' FROM: ',A4)
      END


