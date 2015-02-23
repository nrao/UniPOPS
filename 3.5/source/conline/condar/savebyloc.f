      subroutine savebyloc (array, iloc, imagic, iunit, filename, 
     1                      overwrite, ierr)
C-------------------------------------------------------------------------------
C  @(#)savebyloc.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Saves the array ARRAY into the ILOC location in file FILENAME
c     which is attached to IO unit IUNIT.  
c     If data exists at the specified location, then, if OVERWRITE is
c     true, the old data is overwritten by the new.
c
c     All of the work is now done in saveascan.
c     The filename is ignored.
c
      integer*4 flag
      parameter (flag=0)
      character*(*) filename
      character*80 stch
      character*1 cobsmode(8)
      logical*2 overwrite
      integer*2 imagic, iunit, ierr, n0, n80, n120, n355, m3, ier, iloc
      integer*4 lunit, loverwrite, lrtn, j
      double precision array(*), location, obsmode, ydata(2786)
c
      equivalence (obsmode, cobsmode)
c
      data n0, n80, n120, n355, m3 /0, 80, 120, 355, -3/
c
      include 'cform.inc'
c
      lunit = iunit
      location = iloc
      loverwrite = 0
      if (overwrite) loverwrite = 1
      ierr = 0
c			make sure data_length is large enough
      if (array(c1dln).lt.(array(c12ni) + array(c12spn) - 1) * 4.0) then
         array(c1dln) = (array(c12ni) + array(c12spn) - 1) * 4.0
      endif
c			convert, pass address of array and ydata to saveascan
      call conversion3(array, ydata, ier)
      if (ier .lt. 0) then
         ierr = 360
         goto 99
      endif
c
      call saveascan(lunit, location, flag, array, ydata, 
     .               loverwrite, lrtn)
      if (lrtn .ne. 0) then
         if (lrtn .eq. 1) then
            ierr = -370
         else if (lrtn .eq. 2) then
            ierr = -357
         else if (lrtn .eq. 3) then
            ierr = -359
         else if (lrtn .eq. 4) then
            ierr = -355
         else if (lrtn .eq. 5) then
            ierr = 371
         else if (lrtn .eq. 6) then
            ierr = 355
         else if (lrtn .eq. 7) then
            obsmode = array(c1stc)
            write(stch, 40) (cobsmode(j),j=5,8)
            call pwrite(stch, n80)
         else if (lrtn .eq. 8) then
            ierr = -363
         else if (lrtn .eq. 9) then
            write(stch, 60) iloc
            call pwrite(stch, n80)
         else if (lrtn .eq. 10) then
            write(stch, 60) iloc
            call pwrite(stch, n80)
            obsmode = array(c1stc)
            write(stch, 40) (cobsmode(j),j=5,8)
            call pwrite(stch, n80)
c			errno 10 means errors 7 and 9 both occured
c			only these 2 warning messages will occur
c			at the same time
         else
c				an internal error, should never happen 
            write(stch, 50) lrtn
            call oerror(n120, m3, stch)
         endif
      endif
c
 99   return
c
 40   format('Unrecognized observing mode ',4a1)
 50   format('Unknown error code ', i)
 60   format('Data exists at ', i,' .... Overwriting!')
c
      end
