      subroutine getlist(scans, max, number, iunit, filename, ierr)
C-------------------------------------------------------------------------------
C  @(#)getlist.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Returns in SCANS the scan numbers in file FILENAME attached to
c     unit IUNIT.  NUMBER is the number of scans in the file.  Only MAX
c     number of scans will be loaded into SCANS.
c
c     Filename is now ignored, file must already be open via the c sdd routines
c      
      character*(*) filename
      real scans(*)
      integer*2 number, iunit, max, ierr
      integer*4 lunit, lmax, lnumber, lrtn
c
      number = 0
      ierr = 0
c
      lunit = iunit
      lmax = max
      lnumber = 0
c
      call getslist(lunit, scans, lnumber, lmax, lrtn)
      number = lnumber
c
      if (lrtn .eq. 1) ierr = 370
      if (lrtn .eq. 2) ierr = 357
      if (lrtn .eq. 3) ierr = 359
c
 99   continue
      return
      end
