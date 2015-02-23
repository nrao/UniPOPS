      subroutine packer(ixrw, iyrw, c4out)
C-------------------------------------------------------------------------------
C  @(#)packer.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c Code used to pack TEKTRONIX like graphics commands
c***********************************************
c 8904 [RJM]
c***********************************************
c
      character*4 c4out
      integer*2 iout(4), ixrw, iyrw, mask1, mask2, mxyl, myr, mxr, ixl,
     .          ixr, iyl, iyr
      character*1 c1out(8)
c
      equivalence (c1out, iout)
c
      data mask1/992/, mask2/31/, mxyl/32/, myr/96/, mxr/64/
c     mask1 = 03d0; mask2 = 001e; mxyl = 0020; myr = 0060; mxr = 0040
c
       ixl = and(ixrw, mask1)
       ixl = ixl / 32
       iout(3) = or(ixl, mxyl)
       ixr = and(ixrw, mask2)
       iout(4) = or(ixr,mxr)
       iyl = and(iyrw, mask1)
       iyl = iyl / 32
       iout(1) = or(iyl,mxyl)
       iyr = and(iyrw,mask2)
       iout(2) = or(iyr,myr)
c
       c4out = c1out(2) // c1out(4) // c1out(6) // c1out(8)
c
       return
c
       end
c
c*********************************
c
