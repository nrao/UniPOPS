      subroutine fphandler(sig, code, sigcontext)
c
c     @(#)fphandler.f	5.1 06/22/94
c
c     Floating Point handler
c
      integer*4 sig, code, sigcontext(5), loc
      integer*2 short
c
      include 'errors.inc'
c
      if (loc(code) .eq. 208) then
	numinvalid = numinvalid + 1
      else if (loc(code) .eq. 200) then
	numdivision = numdivision + 1
      else if (loc(code) .eq. 212) then
	numoverflow = numoverflow + 1
      else if (loc(code) .eq. 204)then
	 numunderflow = numunderflow + 1
      else
	numothererr = numothererr + 1
	loclasterr = short(loc(code))
      endif
c
      return
      end
