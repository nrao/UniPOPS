      subroutine checkbyloc(iloc,iunit, filename, scanno, ierr)
C-------------------------------------------------------------------------------
C  @(#)checkbyloc.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Checks whether a scan exists at a certain location within a data file
c
c     filename is ignored
c
      character*(*) filename
      integer*2 iloc, iunit, ierr
      integer*4 lloc, lunit, lrtn
      real scanno
c
      scanno = -1
      lunit = iunit
      lloc = iloc
      ierr = 0
c
c		get the index at iloc
c
      call scanloc(lunit, lloc, scanno, lrtn)
c
      if (lrtn .ne. 0) then
         if (lrtn .eq. 1) ierr = -370
         if (lrtn .eq. 2) ierr = -357
         if (lrtn .eq. 3) ierr = -359
         if (lrtn .eq. 4) ierr = 371
         scanno = -1
      endif
c	retrieves the scan number 
c
99    continue
c
      return
      end
c
