      subroutine getbyloc(array, iloc, imagic, iunit, filename, ierr)
C-------------------------------------------------------------------------------
C  @(#)getbyloc.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     Retrieves the array ARRAY from the ILOC location in file FILENAME
c     which is attached to IO unit IUNIT.  
c
c     All of the work is now done by getascan.
c     The filename is ignored.
c
      include 'params.inc'
c
      integer*4 lunit, flag, maxsize, lrtn
c
      parameter (maxsize = HDU_FLOAT_SIZE/2)
      parameter (flag = 0)
      character*(*) filename
      character*80 stch
      double precision array(*), xdata(maxsize)
      integer*2 iloc, iunit, ierr, imagic, ier
      integer*2 n80, n120, m3
c
      real location
c
      data n80, n120, m3 /80, 120, -3/
c
      ierr = 0
c
      lunit = iunit
      location = iloc
      call getascan(lunit,location,flag,xdata,maxsize,lrtn)
      if (lrtn .eq. 0) then
         call raz(array)
         call conversion2(xdata, array, ier)
         if (ier .ne. 0) ierr = 360
         imagic = 2
      else
         imagic = -1
         if (lrtn .eq. 1) then
            ierr = -370
         else if (lrtn .eq. 2) then
            ierr = -357
         else if (lrtn .eq. 3) then
            ierr = -359
         else if (lrtn .eq. 4) then
            ierr = 368
         else if (lrtn .eq. 5) then
            ierr = 371
         else
c				unknown error, a real bug
            write(stch, 20) lrtn
            call oerror(n120, m3, stch)
         endif
      endif
c
      return
c
 20   format('Unknown disk error code ', i)
c
      end
c
