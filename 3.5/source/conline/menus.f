      subroutine menus(ier)
c---------------------------------------------
c  @(#)menus.f	5.1 06/22/94
c---------------------------------------------
c     Supplies the values of variables for the graphical user interface.
c
      integer*2 lptr, ii, itype, numi2, ilen, lastblnk, ioutstrng(256),
     1          iblank, isize, itag, iarryptr, ilast, iwpr, ier,
     2		il2, iwpc
      integer*2 fshort
      integer*4 long
      character*512 coutstrng, ctempstrng
      character*2 cblank
c
      include 'cio.inc'
      include 'appl.inc'
      include 'core.inc'
c
      equivalence(coutstrng, ioutstrng), (iblank,cblank)
c
      data cblank/'  '/
c
c     First, go through the core array to find the values of things the GUI
c     will need.
c
      ier = 0
      rewind(imenuout,iostat=ier)
      if (ier .ne. 0) goto 99
c
      lptr = 1
c
10    lptr = k(lptr)
c
      if (lptr .ne. 0) then
c
	numi2 = k(lptr+1)/16
	itype = k(lptr+1) - 16*numi2
c
	if (itype .eq. 1 .or. itype .eq. 2 .or. itype .eq. 3 .or.
     1      itype .eq. 7) then
	    coutstrng = ' '
	    ctempstrng = ' '
	    call copy(numi2, k(lptr+4), ioutstrng(1) )
	    ilen = lastblnk(coutstrng)
c
c           Get the name if it is a scalar, array, procedure, or literal and
c	    store it in coutbuf
c
c
	   if (itype .eq. 3) then
	      coutstrng = ':procedure:' // coutstrng(1:ilen)
c	      For procedures, just print out its name with a starting 'procedure'
c	      designator.
c
c
	   else if (itype .eq. 1) then
	      itag = k(lptr+2)
	      write(ctempstrng,22) coutstrng(1:ilen), c(itag)
22	      format(':',a,':',1x,1p32g14.6, )
	      coutstrng = ctempstrng
c	      For scalars, print out name and its value
c
c
	   else if (itype .eq. 2) then
	      iarryptr = k(lptr+3)
	      itag = k(lptr+2)
	      isize = min(32, k(iarryptr) )
	      ilast = isize + itag - 1
	      write(ctempstrng,22) coutstrng(1:ilen),(c(ii),ii=itag,ilast)
	      coutstrng = ctempstrng
c	      For arrays, print out names and array values
c
c
	   else if (itype .eq. 7) then
	      iarryptr = k(lptr+3)
	      itag = k(lptr+2)
	      isize = iwpr(k(iarryptr))
	      coutstrng = ':' // coutstrng(1:ilen) // ':'
	      il2 = iwpc(fshort(ilen+2))
	      call copy(min(isize, 79-ilen), c(itag), ioutstrng(il2+1) )
c	      For character strings, print out name and the string
c
	   endif
c
	   if (itype .eq. 1 .or. itype .eq. 2) then
	      il2 = lastblnk(coutstrng)
	      if (coutstrng(il2:il2).eq.'0') coutstrng(il2:il2) = ' '
	      do 239 ii = il2-1, ilen, -1
		if (coutstrng(ii:ii+1).eq.'0 ') coutstrng(ii:ii+1) = '  '
239		continue
c	      Removes trailing zeros if scalar or arrays variable.
c
2391	      ii = index(coutstrng,  '  0.Inf ')
	      if (ii .ne. 0) then
		coutstrng(ii:ii+7) = 'DEFAULT '
		goto 2391
	      endif
	      ii = index(coutstrng,  ' Inf    ')
	      if (ii .ne. 0) then
		coutstrng(ii:ii+7) = ' DEFAULT'
		goto 2391
	      endif
c	      Prints out DEFAULT (instead of Infinity).
c
	   endif
c
	   if (itype .ne. 7) then
		call cmprsstrng(coutstrng, ctempstrng)
	        ilen = lastblnk(ctempstrng)
	   else
		ctempstrng = coutstrng
		ilen = ilen+2+2*min(isize, 79-ilen)
	   endif
c	   Remove all spaces except in the case of strings; output is now
c	   ctempstrng.
c
	   write(imenuout,21,iostat=ier) ctempstrng(1:ilen)
21	   format(a)
	   if (ier .ne. 0) goto 99
c	   Write it out to the menuout file
c
	endif
	goto 10
      endif
c
99    call flush(long(imenuout))
      return
c
      end
c
