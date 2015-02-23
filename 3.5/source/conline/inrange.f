      integer*4 function inrange(min, max, value, default)
c
c @(#)inrange.f	5.1 06/22/94
c
c      checks that value is in the range specified by min and max.
c      If either min or max is default, value is assumed to fit within that
c      end of the range.
c
c      returns 0 for false and 1 for true
c
c      min and max are NOT checked to see that min <= max
c
      real*4 min, max, value, default
c
      logical*2 lupper, llower
c
      lupper = .true.
      llower = .true.
      inrange = 0
c
      if (min .ne. default) lupper = (value .ge. min)
      if (max .ne. default) llower = (value .le. max)
c
      if (lupper .and. llower) inrange = 1
c
      return
      end
c
      integer*4 function inrange8 (min, max, value, default)
c
c	same as inrange above but for double precision value
c	since the values for min and max come from pops adverbs,
c	they only have r*4 accuracy (they are r*8 arguments here so
c	that the c code that gets these values from the params structure
c	call call this with no conversion.  Also, if we ever go to an R*8
c	interpreter, only the down conversion to r*4 here will need to
c	be change for those routines using inrange 8.
c
      real*8 min, max, default, value
      real*4 rmin, rmax, rval, rdef
c
      logical*2 lupper, llower
c
      lupper = .true.
      llower = .true.
      inrange8 = 0
c		down convert to r*4
      rmin = min
      rmax = max
      rval = value
      rdef = default
c
      if (rmin .ne. rdef) lupper = (rval .ge. rmin)
      if (rmax .ne. rdef) llower = (rval .le. rmax)
c
      if (lupper .and. llower) inrange8 = 1
c
      return
      end
