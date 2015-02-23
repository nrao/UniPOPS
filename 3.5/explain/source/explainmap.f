      program explainmap
c
c     @(#)explainmap.f	5.1 06/22/94
c
c     Gives a map of all the nodes and links in an info_file
c
c     Syntax:  info_file
c
      character*64 infofile
      integer*4 iargc, long, lnblnk, loc
c
      include 'explainmap.inc'
c
      integer*4 findchar
c
      open(unit=ISTDERR, form="print")
      open(unit=ISTDOUT, form="print")
c
      if (iargc() .lt. 1) then
	write(ISTDERR,*) 'Syntax: explainmap info_file'
	call exit(long(1))
      endif
c		Get the info file and open it
      call getarg(long(1), infofile)
      call readinfo(infofile)
c
      call initarrays
c
      write(ISTDOUT,*) 'Explain file : ',infofile(1:lnblnk(infofile))
c			start at the root explain file, "."
      loc = findchar(nodes, next, start, ".")
      call follownodes(loc, .true.)
c			do the same for things not hit the first time
      write(ISTDOUT,*) ' '
      write(ISTDOUT,*) 'The following were not hit the first time:'
      write(ISTDOUT,*) ' '
      loc = start
      call follownodes(loc, .false.)
c			and print out the hit count statistics
      loc = start
      write(ISTDOUT,1000) 
      do 10 while (loc .gt. 0)
         write(ISTDOUT,1010) nodes(loc), hardhit(loc), softhit(loc)
         loc = next(loc)
 10   continue
c
 1000 format(x,'Statistics:',/,4x,'Node Name',19x,'# Hard',x,'# Soft')
 1010 format(x,a32,x,2i4)
c
      stop
      end

c
      subroutine follownodes(loc, first)
c			follow the hard links from loc, displaying info
      integer*4 loc, lnblnk, links, findchar
      logical first
c
      include 'explainmap.inc'
c
      character*32 tmpnodes(NUMNODES)
      integer*4 toptmp, i, tmpint
c
      toptmp = 0
c
      do 10 while (loc .gt. 0)
         if ((.not. first .and. hardhit(loc) .eq. 0) .or.
     .       (first)) then
            hardhit(loc) = hardhit(loc) + 1
c                          display the info
            write(ISTDOUT,*) nodes(loc)(1:lnblnk(nodes(loc))), ' ',
     .             filenames(loc)(1:lnblnk(filenames(loc)))
	    call listedges2(nodes, dest, link, adj(loc), 
     .                    '      ',ISTDOUT)
c		descend down the hard list if loc has not been hit before
            if (hardhit(loc) .eq. 1) then
c			if nothing is on hlinks yet, put this one there
               if (tophlink .eq. 0) 
     .            call push(hlinks, tophlink, NUMNODES, loc)
               links = adj(loc)
c			look for hard links
               do 20 while (links .gt. 0) 
                  if (dest(links) .lt. 0) then
                     toptmp = toptmp + 1
                     tmpnodes(toptmp) = nodes(abs(dest(links)))
                  else
c			record a soft hit
                     softhit(dest(links)) = softhit(dest(links)) + 1
                  endif
                  links = link(links)
 20            continue
c			Sort tmpnodes into alphabetical order
               if (toptmp .gt. 0) then
                  call heapsort(tmpnodes, toptmp)
c			translate the node names back into ints and
c			add to the stack, in reverse order
                  do 30 i = toptmp, 1, -1
                     tmpint = findchar(nodes, next, start, tmpnodes(i))
                     call push(hlinks, tophlink, NUMNODES, tmpint)
 30               continue
                  toptmp = 0
               endif
            endif
         endif
c			see if there is a hard hit waiting to be dealt with
         if (tophlink .gt. 1) then
            call pop(hlinks, tophlink, loc)
         else
c			If there is only one thing left on the stack and
c			first is .true., this is where we came in, exit.
c                       Otherwise we're trying to step through the entire
c			list looking for nodes not hit from the root explain
c			node, in that case, go to the next node in the list
            if (first) then
               tophlink = 0
               loc = 0
            else
c			get where we were from last/first entry in hlinks
               if (tophlink .eq. 1) call pop(hlinks, tophlink, loc)
c			go to the next link from here
               loc = next(loc)
            endif
         endif
c
 10   continue
c
      return
      end
c
      subroutine initarrays
c			zero out hit count arrays and stack pointer
      integer*4 i
      include 'explainmap.inc'
c
      do 100 i = 1, NUMNODES
         hardhit(i) = 0
         softhit(i) = 0
 100  continue
      tophlink = 0
c
      return
      end
