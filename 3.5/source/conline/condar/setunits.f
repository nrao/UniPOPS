      subroutine setunits(freq, funits, flimit, ifreq, litchanged)
c--------------------------------------------------------------------------
c @(#)setunits.f	5.1 06/22/94
c
c        this is a really bad way to get the units in the header to agree
c        with the units that the frequency is in
c
c        freq = the frequency found in the header - this is assumed to be MHz
c        funits = 3 element array which is just the other units (GHz, MHz and
c                 kHz in terms of MHz, i.e. 1000, 1, and .001)
c        flimit = limit used in test.  If freq/funits(ifreq) is larger than
c                 or equal to flimit, then ifreq is the correct units to use.
c        ifreq = current value of the pointer to funits which indicates
c                what was last printed out in the header
c        litchanged = a logical which is set to true of the new ifreq
c                     that is appropriate to freq is different from the
c                     the ifreq that the program was called with.
c                     i.e. if litchanged is true, then a new header needs to
c                     be printed.
c
c---------------------------------------------------------------------------
      real*8 freq, funits(3), ftest, flimit
      integer*4 ifreq, nfreq
      logical litchanged
c
      litchanged = .false.
c
c                this assumes first that funits(3) goes from a large number
c                to a small number and second that the desired units are
c                reached either when the resulting freq is greater than flimit 
c		 or the choices are exhausted.
c
      if (freq .eq. 0) then
         nfreq = 2
         ftest = 0.0
      else
         do 100 nfreq = 1, 3
            ftest = abs(freq / funits(nfreq))
            if (ftest .ge. flimit) go to 200
 100     continue
c
         nfreq = 3
c
 200     continue
      end if
c
      if (ifreq .ne. nfreq) litchanged = .true.
      ifreq = nfreq
      freq = freq / funits(ifreq)
      return
      end
