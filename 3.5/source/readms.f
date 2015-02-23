      SUBROUTINE READMS(LUN,IBLCK,NBLCK,LBUF,IER)
C-------------------------------------------------------------------------------
C  @(#)readms.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c     Modified 8903 [RJM] See CHANGES.DOC
c*****************************************************
      INTEGER*2 LUN, LBUF(*), ier
      INTEGER*4  nblck, IBLCK, fd, blksize, unitno, ier4, getfd
c
      include 'cio.inc'
c
      unitno = lun
      fd = getfd(unitno)
      blksize = irecsiz * 2
c
      call cread(fd, iblck, nblck, lbuf, blksize, ier4)
      ier = ier4
c
      return
      end

