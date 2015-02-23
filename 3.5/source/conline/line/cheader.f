      subroutine cheader
c
c     @(#)cheader.f	5.2 05/04/98
c
c     Lists out the contents of the header of the current cube
c
      integer*2 istch(40), ierr, n, iinitp, n1
      integer*2 n80
      integer*4 n24, nchar
      character*80 stch
      character*24 sdate
c
      include 'cio.inc'
      include 'mappl.inc'
      include 'mform.inc'
c
      equivalence(istch, stch)
c
      data n24, n80 /24, 80/
c
      write(stch,10)
10    format(72('-'))
      call pwrite(istch,n80)
c
      stch = 'Filename: ' // ccube 
      call pwrite(istch,n80)
c
      stch = 'Object: ' // cchead(cobject) //  
     .                     cchead(cobject+1)
      call pwrite(istch,n80)
      call fromdate(n24, nchar, chead(cdate), sdate)
      stch = 'Date: ' // sdate //  
     .	     '       Origin: ' //cchead(corigin) 
      call pwrite(istch,n80)
c
      write(stch,12)
12    format(' ')
      call pwrite(istch,n80)
c
      write(stch,15)
15    format('Axis',2x,'Num pixels',3x,'Units',6x,'Ref. value',10x,
     .		'Delta',5x,'Ref. pixel')
      call pwrite(istch,n80)
c
      write(stch,20,iostat=ierr) ' X: ', nint(chead(cnaxis1)), 
     .		cchead(ctype1), chead(crval1), chead(cdelt1), chead(cpix1)
20    format(a4, 7x, i5, 3x, a8, 2x, 1p3g15.6)
      call pwrite(istch,n80)
c
      write(stch,20,iostat=ierr) ' Y: ', nint(chead(cnaxis2)), 
     .		cchead(ctype2), chead(crval2), chead(cdelt2), chead(cpix2) 
      call pwrite(istch,n80)
c
      write(stch,20,iostat=ierr) ' Z: ', nint(chead(cnaxis3)), 
     .		cchead(ctype3), chead(crval3), chead(cdelt3), chead(cpix3) 
      call pwrite(istch,n80)
c     X, Y and Z axis parameters
c
      write(stch,12)
      call pwrite(istch,n80)
c			display equinox if non-zero
      if (chead(cequinox) .ne. 0) then
         write(stch,21,iostat=ierr) chead(cequinox)
 21      format('Equinox: ',1pg15.6)
         call pwrite(istch,n80)
         write(stch,12)
         call pwrite(istch,n80)
      endif
c
      write(stch,25,iostat=ierr) cchead(cbunit)
25    format('Units of data values: ', a8)
      call pwrite(istch,n80)
c
      write(stch,35,iostat=ierr) chead(ctmin), cchead(ctmax)
35    format('Min/Max values: ',1p2g15.6)
      call pwrite(istch,n80)
c
      stch = 'Comment:  '
      n = 4*min(7,(cheadsize-ccomment+1))
      n1 = iinitp(ccomment)
      call copy(n, chead(ccomment), istch(6))
      call pwrite(istch,n80)
c     Takes care of comments      
c
      write(stch,10)
      call pwrite(istch,n80)
c
      return
      end
c
