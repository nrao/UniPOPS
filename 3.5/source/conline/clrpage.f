      subroutine clrpage
C-------------------------------------------------------------------------------
C  @(#)clrpage.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Clears Graphics screen
c*************************
c 8904 [RJM]
c 8911 [RJM]
c*************************
c
      integer*2 istat
      integer*4 unlink
      logical*2 opened, inquirefile
      include 'cio.inc'
c
c
      if (igraphtype .eq. 4) then
	call v102clrpage
	inline = 1
	inlast = 1
      else if (igraphtype .eq. 3) then
	call tekclrpage
	inline = 1
	inlast = 1
      else if (igraphtype .eq. 2) then
	call coreclrpage
      else 
	call nogclrpage
      endif
c
      if (opened(coutgrph)) close(ioutgrph,status='delete',iostat=istat)
      if (istat .ne. 0) then
         write(istderr,*) ' Problem with CLRPAGE during attempted close'
      else 
         if (inquirefile(coutgrph)) istat = unlink(coutgrph)
         if (istat .ne. 0) then
            write(istderr,*) ' Problem with CLRPAGE during unlink'
         else 
            open(unit=ioutgrph,file=coutgrph, status='new',
     +	                 iostat=istat)
            if (istat .ne. 0) then
               write(istderr,*) ' Problem with CLRPAGE during open'
               write(istderr,*) ' istat = ', istat
               write(istderr,*) ' ioutgrph = ', ioutgrph
               write(istderr,*) ' coutgrph = ', coutgrph
            else 
               rewind(ioutgrph,iostat=istat)
               if (istat .ne. 0) 
     +           write(istderr,*) ' Problem with CLRPAGE during rewind'
            end if
         end if
      end if
c
      iglinex = 1
      igliney = 780
c     Reset graphics screen text line pointer.  
c
      return
      end
c
