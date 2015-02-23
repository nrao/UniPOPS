      subroutine donemenu
c
c     @(#)donemenu.f	5.1 06/22/94
c
c     Closes down menus
c
      integer*2 istat, irtn2, irtn3, irtn4
      integer*2 m1, n351
      integer*4 istat4, unlink, long
c
      include 'cio.inc'
c
      data n351, m1 /351, -1/
c
      if (iintype .eq. imenuin) then
c
	write(imenuout,fmt="(1x,a)",iostat=istat) 'DONEMENU'
c       Send 'donemenu' message to menus's program
c
	call closefifo(long(iinunit),istat4)
	close(imenuout,iostat=irtn2)
      	irtn3 = unlink(cmenuin)
      	irtn4 = unlink(cmenuout)
C
        iinptr = max(1, iinptr - 1)
        iinunit = iinlist(iinptr, 1)
        iintype = iinlist(iinptr, 2)
c
c       Return the input pointer etc to what they were before MENUS was called
c
     	 if (istat4 .ne. 0 .or. irtn2 .ne. 0 .or. irtn3 .ne. 0
     .      .or. irtn4 .ne. 0 .or. istat .ne. 0) 
     .	    call oerror(n351,m1, 'DONEMENU')
      endif
c
      return
c
      end
c
