      subroutine initfperror
c
c     @(#)initfperror.F	5.1 06/22/94
c
c     Initializes floating Point error handler
c
      include 'errors.inc'
c
      external fphandler
c
#ifdef sun4
      call ieee_handler('set','common',fphandler)
#endif
c
      erron = .true.
      amath = .false.
      numdivision = 0
      numunderflow = 0
      numoverflow = 0
      numinvalid = 0
      numothererr = 0
      loclasterr = 0
c
      return
      end
