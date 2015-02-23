      subroutine getprel(line,printer,ptype,pcmd,tfmt,pcmnt)
c-----------------------------------------------------------------------------
c @(#)getprel.f	5.1 06/22/94
c
c   get the printer elements from line
c-----------------------------------------------------------------------------
      character*(*) line, printer, ptype, pcmd, tfmt, pcmnt
      integer*4 llen, i, lplace
c
      if (line(1:1) .eq. '#') then
c			ignore this line, return all blanks
         do 100 i = 1, len(printer)
 100        printer(i:i) = ' '
         do 105 i = 1, len(ptype)
 105        ptype(i:i) = ' '
         do 110 i = 1, len(pcmd)
 110        ptype(i:i) = ' '
         do 115 i = 1, len(tfmt)
 115        tfmt(i:i) = ' '
         do 120 i = 1, len(pcmd)
 120        pcmd(i:i) = ' '
c
      else
         lplace = 1
         call decodeit(line,printer,lplace)
         call decodeit(line,ptype,lplace)
         call decodeit(line,pcmd,lplace)
         call decodeit(line,tfmt,lplace)
c			Everything at this point is a comment
         llen = len(line)
         do 200 i = 1, len(pcmnt)
            if (lplace+i-1 .gt. llen) then
               pcmnt(i:i) = ' '
            else
               pcmnt(i:i) = line(lplace+i-1:lplace+i-1)
            endif
 200     continue
c
      endif
c
      return
      end
c
      subroutine decodeit(bigstr,element,place)
c
      character*(*) bigstr, element
      character*1 colon
      integer*4 place, i, biglen
      logical foundcol
c
      data colon /':'/
c
      biglen = len(bigstr)
      foundcol = .false.
      do 100 i = 1, len(element)
         if (foundcol .or. place .gt. biglen) then
            element(i:i) = ' '
         else
            if (bigstr(place:place) .eq. colon) then
               foundcol = .true.
               element(i:i) = ' '
               place = place + 1
            else
               element(i:i) = bigstr(place:place)
               place = place + 1
            endif
         endif
 100  continue
c
      if (.not.foundcol .and. place .le. biglen) then
         do 110 i = place, biglen
            if (bigstr(i:i) .eq. colon) goto 120
 110     continue
 120     place = i + 1
      endif
c
      return
      end
