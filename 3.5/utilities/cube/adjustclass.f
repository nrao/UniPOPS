      subroutine adjustclass
c
c      @(#)adjustclass.f	5.1 06/22/94
c
c     Alters the header pointers to the values which correctly correspond to
c     the contents of the scan in the TWH array.
c
      integer class(15), numclass, i
c
      include 'cube.inc'
      include 'dform.inc'
c
      numclass = min(15, itwh(1)+1)
      do 10 i = 1, numclass
	class(i) = itwh(i+1)
10	continue
c     CLASS contains the location in the scan of the starting R*8 words
c     for each header class
c
      c1sno = d1sno + class(1)
      c1tel = d1tel + class(1)
      c1ona = d1ona + class(1)
      c1stc = d1stc + class(1)
      c3dat = d3dat + class(3)
      c4epo = d4epo + class(4)
      c4sx = d4sx + class(4)
      c4sy = d4sy + class(4)
      c4era = d4era + class(4)
      c4edc = d4edc + class(4)
      c4gl = d4gl + class(4)
      c4gb = d4gb + class(4)
      c4az = d4az + class(4)
      c4el = d4el + class(4)
      c4ix = d4ix + class(4)
      c4iy = d4iy + class(4)
      c4csc = d4csc + class(4)
      c7osn = d7osn + class(7)
      c12spn = d12spn + class(12)
      c12ni = d12ni + class(12)
      c12x0 = d12x0 + class(12)
      c12dx = d12dx + class(12)
      c12rp = d12rp + class(12)
c     Find out where each header parameter is using CLASS to find each class in
c     the header and the offsets D1... from the start of each class for each
c     parameter
c
      idatoff = 2*(class(14)-1)
c     Find the location of the end of the header (and the start of the data).
c
      return
c
      end
c
