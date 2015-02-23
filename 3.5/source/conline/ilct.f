      integer*4 function ilct(i, j, iptwh)
c
c     @(#)ilct.f	5.1 06/22/94
c
c     Finds where in matrix IPTWH the elements (I,J) are stored.
c
      integer*2 iptwh, i, j
      integer*2 fshort
      integer*4 long, istart
c
      include 'mappl.inc'
      include 'mform.inc'
c
      istart = long(fshort(iptwh-1))*mdatasize/mnumarrays
      ilct = long(i) + mhead(mnaxis1,iptwh)*long(fshort(j-1)) + istart
c
      return
      end
c
