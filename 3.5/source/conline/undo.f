      subroutine savestatus
C-------------------------------------------------------------------------------
C  @(#)undo.f	5.2 09/10/98
C-------------------------------------------------------------------------------
c
c     Stores common blocks APPL and CORE into arrays for a later
c     UNDO
c
      include 'appl.inc'
      include 'core.inc'
      include 'cio.inc'
c
      integer*4 StoreRappl1(RAPPLSIZE), StoreRappl2(RAPPLSIZE)
      integer*2 StoreK1(32767), StoreK2(32767)
      integer*2 StoreKx1(32767), StoreKx2(32767)
      integer*2 n32, m1
      integer*4 i4
      integer*2 snext
      logical*2 saved(2)
c
      save StoreRappl1, StoreRappl2
      save StoreK1, StoreK2
      save StoreKx1, StoreKx2
      save saved, snext
c
      data saved/.false., .false./, snext/1/
      data n32, m1 /32, -1/
c
      if (.not. undoon) then
	snext = 1
      else
        if (snext .eq. 1) then
           do 101 i4 = 1, numappl
	     StoreRappl1(i4) = Rappl(i4)
101          continue
           do 201 i4 = 1, k(3)
	     StoreK1(i4) =  k(i4)
201          continue
           do 301 i4 = 1, kx(3)
	     StoreKx1(i4) =  kx(i4)
301          continue
        else
           do 102 i4 = 1, numappl
	     StoreRappl2(i4) = Rappl(i4)
102          continue
           do 202 i4 = 1, k(3)
	     StoreK2(i4) =  k(i4)
202          continue
           do 302 i4 = 1, kx(3)
	     StoreKx2(i4) =  kx(i4)
302          continue
        endif
c
        saved(snext) = .true.
        snext = mod(snext,2) + 1
c
      endif
      return
c
      entry undo
c
      if (.not. undoon) call oerror(n32, m1, 'UNDOOFF in affect')
c
      if (snext .eq. 1 .and. saved(2)) then
        do 112 i4 = 1, numappl
	  Rappl(i4) = StoreRappl2(i4)
112       continue
        do 212 i4 = 1, StoreK2(3)
	  k(i4) = StoreK2(i4)
212	  continue
        do 312 i4 = 1, StoreKx2(3)
	  kx(i4) = StoreKx2(i4)
312	  continue
      else if(snext .eq. 2 .and. saved(1)) then
        do 111 i4 = 1, numappl
	  Rappl(i4) = StoreRappl1(i4)
111       continue
        do 211 i4 = 1, StoreK1(3)
	  k(i4) = StoreK1(i4)
211	  continue
        do 311 i4 = 1, StoreKx1(3)
	  kx(i4) = StoreKx1(i4)
311	  continue
      else
	call oerror(n32, m1, ' ')
      endif
c
      snext = mod(snext,2) + 1
      saved(snext) = .false.
c
      call togtline
c
      return
      end
