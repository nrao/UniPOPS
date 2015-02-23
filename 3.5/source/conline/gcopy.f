      subroutine gcopy
C-------------------------------------------------------------------------------
C  @(#)gcopy.f	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Makes a hard copy of the graphics screen
c
      integer*2 n129, n0
      integer*4 system, istat, irtn
      character*90 output
c
      include 'cio.inc'
c
      data n129, n0 /129, 0/
c
      if (iprinttype .le. 0) then
	call oerror(n129, n0, 'GCOPY')
	return
      endif
c
      close(ioutgrph,status='keep',iostat=istat)
c
      output = 'printit ' // printer // ' ' // 
     .          printtype // ' ' // coutgrph
c
      irtn = system(output)
      if (istat .ne. 0) write(istderr,*) 'Internal problem with GCOPY'
      open(unit=ioutgrph,file=coutgrph,status='unknown',fileopt='eof',
     .     iostat=istat)
      if (istat .ne. 0) write(istderr,*) 'Internal problem with GCOPY'
c
      return
      end
c
