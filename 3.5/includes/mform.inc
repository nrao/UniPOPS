C-------------------------------------------------------------------------------
C  @(#)mform.inc	5.2 08/30/94
C-------------------------------------------------------------------------------
c
c        If you change any of the data values here, check that the size
c        of the header arrays in mappl.inc are large enough.  In particular,
c        there should be room for 56 characters in mcomment and ccomment
c        (7 R*8's).
C
      INTEGER*2 mnaxis1, mnaxis2, mtype1, mtype2, mrval1, mrval2,
     .		mdelt1, mdelt2, mpix1, mpix2, mcomment, mblank, mbunit, 
     .		mobject, morigin, mdate, mstrings, mequinox, mbitpix
      INTEGER*2 cnaxis1, cnaxis2, ctype1, ctype2, crval1, crval2,
     .		cdelt1, cdelt2, cpix1, cpix2, ccomment, cblank, cbunit,
     .		cobject, corigin, cdate, cstrings, cnaxis3, ctype3,
     .		crval3, cdelt3, cpix3, ctmin, ctmax, ctscale, ctzero,
     .          cequinox, cbitpix
c
      data mnaxis1/1/, mnaxis2/2/, mrval1/3/, mrval2/4/, mdelt1/5/,
     .	   mdelt2/6/, mpix1/7/, mpix2/8/, mblank/9/, mequinox/10/,
     .     mbitpix/11/
      data mtype1/20/, mtype2/21/, mbunit/22/, mobject/23/, morigin/25/,
     .     mdate/26/, mcomment/27/
c     Positions in MHEAD where header parameters can be found
c
      data mstrings/20/
c     First location in MHEAD where string items can be found
c
      data cnaxis1/1/, cnaxis2/2/, crval1/3/, crval2/4/, cdelt1/5/,
     .	   cdelt2/6/, cpix1/7/, cpix2/8/, cblank/9/, cnaxis3/10/,
     .     crval3/11/, cdelt3/12/, cpix3/13/ ctmin/14/, ctmax/15/,
     .     ctscale/16/, ctzero/17/, cequinox/18/, cbitpix/19/
      data ctype1/20/, ctype2/21/, cbunit/22/, cobject/23/, corigin/25/,
     .     cdate/26/, ctype3/27/, ccomment/28/
c     Positions in CHEAD where header parameters can be found
c
      data cstrings/20/
c     First location in CHEAD where string items can be found
c
