      subroutine setflags
c
c     @(#)setflags.f	5.1 06/22/94
c
c     Sets up values for STATUS array-adverb depending upon
c     values of internal flags which the user may need to know.
c
c     DON'T ALTER WITHOUT ALSO CHANGING DOCUMENTATION AND TELLSTAT ROUTINE
c
      integer*2 curcur
c
      include 'appl.inc'
      include 'errors.inc'
      include 'cio.inc'
      include 'core.inc'
      include 'stk.inc'
c
      status(1) = lxa
      status(2) = uxa
      status(3) = splt
      status(4) = rplt
      status(5) = idx
      status(6) = ltype
      status(7) = sclchar*11
      status(8) = naccum
      status(9) = numplots
      if (pagefull) then
	status(10) = 1
      else
	status(10) = -1
      endif
      status(11) = igraphtype
      status(12) = iprinttype
      status(13) = iintype
      status(14) = iout
      if (online) then
	status(15) = 1
      else
	status(15) = -1
      endif
      if (dolog) then
	status(16) = 1
      else
	status(16) = -1
      endif
      if (graphon) then
	status(17) = 1
      else
	status(17) = -1
      endif
      if (erron) then
	status(19) = 1
      else
	status(19) = -1
      endif
      status(20) = idebug
      status(21) = showplot(curcur())
      if (undoon) then
	status(22) = 1
      else
	status(22) = -1
      endif

      status(23) = xmax0
      status(24) = ymax0
      status(25) = xorg(curcur())
      status(26) = yorg(curcur())
      status(27) = xmax(curcur())
      status(28) = ymax(curcur())
      status(29) = linelngth
      status(30) = numline
      status(31) = iglinex
      status(32) = igliney
c
      return
      end

