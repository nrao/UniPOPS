      subroutine mheader(iptwh)
c
c     @(#)mheader.f	5.2 05/04/98
c
c     Lists out the contents of the header of the IPTWH matrix
c
      integer*2 iptwh, istch(40), ierr, n, iinitp, n1
      integer*2 m2, n80, n112
      integer*4 n24, nchar
      character*80 stch
      character*24 sdate
c
      include 'mappl.inc'
      include 'mform.inc'
c
      equivalence(istch, stch)
c
      data m2, n24, n80, n112 /-2, 24, 80, 112/
c
      if (iptwh .lt. 1 .or. iptwh .gt. mnumarrays) 
     .		call oerror(n112, m2, 'MHEADER')
c     Check that the user has specified a legitimate matrix
c
      write(stch,10)
10    format(72('-'))
      call pwrite(istch,n80)
c
      stch = 'Object: ' // cmhead(mobject, iptwh) // 
     +                     cmhead(mobject+1,iptwh)
      call pwrite(istch,n80)
      call fromdate(n24, nchar, mhead(mdate, iptwh), sdate)
      stch = 'Date: ' // sdate //
     +       '       Origin: ' // cmhead(morigin, iptwh)
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
      write(stch,20,iostat=ierr) ' X: ', nint(mhead(mnaxis1,iptwh)), 
     .		cmhead(mtype1,iptwh), mhead(mrval1,iptwh), 
     .		mhead(mdelt1,iptwh), mhead(mpix1,iptwh)
20    format(a4, 7x, i5, 3x, a8, 2x, 1p3g15.6)
      call pwrite(istch,n80)
c
      write(stch,20,iostat=ierr) ' Y: ', nint(mhead(mnaxis2,iptwh)), 
     .		cmhead(mtype2,iptwh), mhead(mrval2,iptwh), 
     .		mhead(mdelt2,iptwh), mhead(mpix2,iptwh) 
      call pwrite(istch,n80)
c     X and Y axis parameters
c
      write(stch,12)
      call pwrite(istch,n80)
c			Equinox if appropriate
      if (cmhead(mtype1,iptwh)(1:4) .eq. 'RA--' .or.
     .    cmhead(mtype2,iptwh)(1:4) .eq. 'RA--' .or.
     .    cmhead(mtype1,iptwh)(1:4) .eq. 'HA--' .or.
     .    cmhead(mtype2,iptwh)(1:4) .eq. 'HA--' .or.
     .    cmhead(mtype1,iptwh)(1:4) .eq. 'DEC-' .or.
     .    cmhead(mtype2,iptwh)(1:4) .eq. 'DEC-') then
         if (mhead(mequinox,iptwh) .eq. 0) mhead(mequinox,iptwh)=1950.0
         write(stch,21,iostat=ierr) mhead(mequinox,iptwh)
 21      format('Equinox: ',1pg15.6)
         call pwrite(istch,n80)
         write(stch,12)
         call pwrite(istch,n80)
      endif
c
      write(stch,25,iostat=ierr) cmhead(mbunit,iptwh)
25    format('Units of data values: ',a8)
      call pwrite(istch,n80)
c
      stch = 'Comment:  '
      n = 4*min(7,(mheadsize-mcomment+1))
      n1 = iinitp(mcomment)
      call copy(n, mhead(mcomment,iptwh), istch(6))
      call pwrite(istch,n80)
c     Takes care of comments      
c
      write(stch,10)
      call pwrite(istch,n80)
c
      return
      end
c
