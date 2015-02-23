      subroutine posstring (pos, iflag, string)
c---------------------------------------------------------------
c @(#)posstring.f	5.1 06/22/94
c
c             Writes position POS assumed to be
c             decimal degrees in string STRING according to IFLAG.
c
c        IFLAG:
c               1    RA
c               2    DEC
c               3    galactic az-ell
c----------------------------------------------------------------
      integer*2 iflag, idd, imm, iss
      real*4 pos, dd, ss, mm
      character*12 string
      character*1 minus
      data minus /'-'/
c
c				Types 1 and 2, pull out mins and seconds
      if (iflag .lt. 3) then
         dd = abs(pos)
         if (iflag .eq. 1) dd = dd / 15.0
c
         idd = ifix(dd)
         mm = (dd - float(idd)) * 60.0
         imm = ifix(mm)
         ss = (mm - float(imm)) * 60.0
         iss = nint(ss)
         if (iflag .eq. 1 .and. ss .ge. 59.95) then
            ss = 0.0
            imm = imm + 1
         else if (iflag .eq. 2 .and. iss .ge. 60) then
            iss = 0
            imm = imm + 1
         end if
         if (imm .ge. 60) then
            imm = 0
            idd = idd + 1
         end if
c				write it out
         if (iflag .eq. 1) then
            if (ss .lt. 9.95) write(string,1000) idd, imm, ss
            if (ss .ge. 9.95) write(string,1005) idd, imm, ss
         else
            write(string,1010) idd, imm, iss
         end if
         if (pos .lt. 0) write(string(2:2), 1020) minus
      else
c				galactic and az-el, just write them out
         write(string,1030) pos
      end if
c
      return
C---------------------------------------------------------------
 1000 format(2x,i2.2,x,i2.2,x,'0',f3.1)
 1005 format(2x,i2.2,x,i2.2,x,f4.1)
 1010 format(2x,i2.2,x,i2.2,x,i2.2,2x)
 1020 format(a1)
 1030 format(x,f8.4,3x)
      end
