      SUBROUTINE WRITMS(LUN,IBLCK,NBLCK,LBUF,IER)
C-------------------------------------------------------------------------------
C  @(#)writms.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c     Modified 8903 [RJM] See CHANGES.DOC
c********************************************************
      integer*2 ier, LUN, LBUF(*)
      INTEGER*4  IBLCK, nblck, fd, blksize, unitno, ier4, getfd
c
      include 'cio.inc'
c
      unitno = LUN
      fd = getfd(unitno)
      blksize = irecsiz * 2
c
      call cwrite(fd, iblck, nblck, lbuf, blksize, ier4)
      ier = ier4
c
      return
      end

