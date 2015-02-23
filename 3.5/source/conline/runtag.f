      subroutine runtag(numi2, itag)
c
c     @(#)runtag.f	5.1 06/22/94
c
c     Returns a location in the core array that would be safe to use
c     at run time for storing a constant.  NUMI2 is the number of
c     I*2 words that are to be taken up by the word to be stored.
c     ITAG is the location in the C array at which the constant can 
c     be stored.
c
      integer*2 numi2, itag, ktag
      integer*2 n131, m1
c
      integer*2 kxoffset, koffset, safety, numkleft, numkxleft,
     .		irealp
c
      include 'core.inc'
c
      save koffset, kxoffset
c
      data koffset/0/, kxoffset/0/, safety/6/
c     KOFFSET, KXOFFSET = Number of I*2 words already placed
c	 into core arrays.
c     SAFETY = Safety margin between end of static core usage and run-ime
c	core usage.
c
      data n131, m1 /131, -1/
c
      numkleft = k(5)-k(3)-koffset-2*safety
      numkxleft = kx(5)-kx(3)-kxoffset-2*safety
c     Number of i*2 words left in each part of core arrays; give ourselves  
c	a safety margin both right after the end of static core and before end
c	of array.  Probably safety could be 1.
c
      if (numkleft .gt. numi2) then
	itag = irealp(k(3) + safety + koffset)
	koffset = koffset + numi2
      else if (numkxleft .gt. numi2) then
c		irealp must be used in two parts here because
c               the arg to irealp is an I*2 and the full sum of
c               kx(3) + safety + kxoffset + kxorg will exceed 32767
	itag = irealp(kx(3) + safety + kxoffset) + irealp(kxorg)
	kxoffset = kxoffset + numi2
      else 
	call oerror(n131, m1, ' ')
      endif
c
      return
c
c-------------------------
c
      entry runktag(numi2, ktag)
c
c	Returns a location in the K array that would be safe to use
c	at run time for storing a program chunk.  NUMI2 is the number of
c	I*2 words that are to be taken up by the word to be stored.
c	KTAG is the location in the K array at which the program chunk can 
c	be stored.
c
      numkleft = k(5) - k(3) - koffset - 2*safety
      if (numkleft .gt. numi2) then
         ktag = k(3) + safety + koffset
         koffset = koffset + numi2
      else
         call oerror(n131, m1, ' ')
      endif
c
      return
c
c-------------------------
c
      entry clrruntag
c
c     Clears all run-time tags; should be called occasionally
c
      kxoffset = 0
      koffset = 0
c
      return
      end
c
