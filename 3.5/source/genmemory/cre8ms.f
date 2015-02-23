      SUBROUTINE CRE8MS(LUN,NAME,ISIZE,IER)
C-------------------------------------------------------------------------------
C  @(#)cre8ms.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C  To simulate the POPS PDP11 routine.
C  See MSIO.MAC for specifications.
c  Modified 8903   [RJM] See CHANGES.DOC
c*****************************************************
      INTEGER*2 LUN, name(*), isize, ier, inunit, i, istat
      INTEGER*2 IRECLWORD
      logical*2 inquirefile
      integer*2 iname(32)
      character*64 cname
c
      include 'cio.inc'
c
      equivalence (cname, iname(1))
c
      IRECLWORD=IRECSIZ/2            
c***  !Record size in Long-words
      inunit=LUN
C                                    First copy NAME into the local array
      do 10 i=1, 32
	iname(i) = name(i)
10	continue
c
      if (inquirefile(cname)) then
	ier = -500
      else
        open(unit=inunit,file=cname,recl=ireclword*4,form='unformatted',
     1     access='direct', status='new', iostat=istat)
	if (istat .eq. 0) close(inunit,iostat=istat)
	ier = istat
      endif
c
      RETURN
      END

