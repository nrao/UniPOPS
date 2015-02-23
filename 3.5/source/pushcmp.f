      logical function pushcmp(stack, top, maxstck, item)
c
c     @(#)pushcmp.f	5.1  06/22/94
c
c     Pushes an integer ITEM onto the STACK which has MAXSTCK indices,
c     of which TOP items have been used.
c
      integer*2 stack(*), top, maxstck, item
c
      if (top .eq. maxstck) then
	pushcmp = .false.
      else
	pushcmp = .true.
      	top = top + 1
      	stack(top) = item
      endif
c
      return
c
      end
c
c-------------------------------------
c
      logical function popcmp(stack, top, item)
c
c     Pops an integer ITEM from the TOP of the STACK.
c
      integer*2 stack(*), top, item
c
      if (top .eq. 0) then
	popcmp = .false.
      else
      	item = stack(top)
	popcmp = .true.
      	top = top - 1
      endif
c
      return
c
      end
c
