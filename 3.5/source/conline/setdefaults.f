      subroutine setdefaults(itag, value)
c
c     @(#)setdefaults.f	5.1 06/22/94
c
c     Sets VALUE to the default value of the ADVERB located at tag ITAG
c
      integer*2 itag, i
      real*8 value, dinfinity, dinfinit
c
      include 'defaults.inc'
c
      dinfinit= dinfinity()
c
      if (value .ne. dinfinit) return
c
      do 100 i = 1, numtags-1
	if (defvalues(i) .ne.  dinfinit) then
	  if (itag .ge. offsets(i) .and. itag .lt. offsets(i+1)) then
		value = defvalues(i)
		return
	  endif
	endif
100	continue
c
      return
      end
c
