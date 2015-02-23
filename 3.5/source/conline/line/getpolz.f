      subroutine getpolz(scanno, record, iptwh, ierr)
C-------------------------------------------------------------------------------
C  @(#)getpolz.f	1.2 09/10/98
c
c  Gets polz data for a particular scan, and record combination.
c
C-------------------------------------------------------------------------------
C
      include 'appl.inc'
      include 'cio.inc'
      include 'cform.inc'
c
      integer*4 iptwh
c
      integer*4 onlinesite, is4, if4, irtn, lrec, nrec
      integer*2 ierr, record, irtn2
      real*4 scanno, ascanno, bscanno
      real*8 xtemp(HDU_FLOAT_SIZE/2)
c
      if (onlinesite() .ne. 1) then
c	Only for 12-m data
	ierr = -246
	goto 99
      endif
c
      ierr = 0
      ascanno = abs(scanno)
      is4 = int(ascanno+0.001)
      if4 = mod(100.*ascanno+0.5,100.)
      bscanno = is4 + float(if4) / 100.0
      lrec = record
c     Find the abs value of scan number (IS4) and feed number (IF4)
c
      irtn = -1
c     Set up irtn with a 'scan not found' flag in case online file or
c     ofline file doesn't exist.  IRTN will only be cleared when a scan
c     is found and correctly converted to SDD.
c
c       Only search on-line file
c
      if (online) then
c
         call gpzrec(is4, if4, lrec, nrec, xtemp, irtn)
c				nrec is set by gpzrec
         if (irtn .eq. 0) then
            call raz(dtwh(1,iptwh))
            call conversion2(xtemp, dtwh(1,iptwh), irtn2)
            if (irtn2 .ne. 0) then
               ierr = -360
               goto 99
            endif
c			set 2 additional header words
            dtwh(c1norec, iptwh) = nrec
            dtwh(c1recid, iptwh) = lrec
         else
c			gpzrec had a problem, irtn is appropriate code
            ierr = irtn
            goto 99
         endif
      endif
c       At this point, irtn = 0 if scan was found and the scan is in SDD
c       format.  Otherwise, online was false.
c
c
      if (irtn .ne. 0) ierr = 367
c
99    return
c
      end
c
