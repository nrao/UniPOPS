      subroutine upack(ixraw, iyraw, ich, iout)
C-------------------------------------------------------------------------------
C  @(#)upack.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Unpacks TEXTRONIX like command strings
c**********************************
c 8904 [RJM]
c**********************************
c
      character*1 iout(*)
      character*2 cxl, cxu, cyl, cyu, cch
      integer*2 iich, ixl, ixu, iyl, iyu, ixraw, iyraw, ich, 
     .          mask1, mask2
c
      equivalence (cxl,ixl),(cxu, ixu),(cyl,iyl),(cyu,iyu)
      equivalence (cch,iich)
c
      data mask1/31/, mask2/63/
c     mask1 = 001e, mask2 = 003e
c
      cxl(2:2) = iout(3)
      ixl = and(ixl, mask1)
      cxu(2:2) = iout(2)
      ixu = and(ixu, mask1)
      ixraw = ixu*32 + ixl
      cyl(2:2) = iout(5)
      iyl = and(iyl, mask1)
      cyu(2:2) = iout(4)
      iyu = and(iyu, mask1)
      iyraw = iyu*32 + iyl
      cch(2:2) = iout(1)
      iich = and(iich, mask2)
      ich = iich
c
      return
      end
c
