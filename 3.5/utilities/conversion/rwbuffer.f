      subroutine readbuff(ierr, ibytes, buffer)
c
c     @(#)rwbuffer.f	5.1 06/22/94
c
c     Reads in a buffer from standard input
c
      character*(*) buffer
      integer*4 ierr, ibytes, iinbytes, iin
c
      iinbytes = ibytes
      ierr= 0
c
10    continue
         call readb(iin, iinbytes, buffer(ierr+1:) )
	 if(iin .le. 0) then
	    ierr = iin
         else
	    iinbytes = iinbytes - iin
	    ierr = ierr + iin
	    if (iinbytes .gt. 0) goto 10
	 endif
c
      return
      end
c
      subroutine writebuff(ierr, ibytes, buffer)
c
c     Writes out a buffer to standard output
c
      character*(*) buffer
      integer*4 ierr, ibytes, iout, ioutbytes, iin
c
      ioutbytes = ibytes
      ierr= 0
c
10    continue
         call writeb(iout, ioutbytes, buffer(ierr+1:) )
	 if(iout .le. 0) then
	    ierr = iin
         else
	    ioutbytes = ioutbytes - iout
	    ierr = ierr + iout
	    if (ioutbytes .gt. 0) goto 10
	 endif
c
      return
      end
c
