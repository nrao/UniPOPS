      subroutine tellstat(status)
c
c     @(#)tellstat.F	5.1 06/22/94
c
c     Relates the meaning of the contents of the status array.
c
      real status(*)
c
      character*24 status1(11), status3(0:2), status11(5), 
     .		   status13(6), status21(-1:5)
      integer*2 istat13(6), istch(40), ierr, iblue, igreen, ired, i,
     .		lnblnk
      character*80 stch
c
      include 'cio.inc'
c
      equivalence (stch, istch)
c
# ifdef CONTINUUM
      data status1 /    "Channels",
     .			"Time Offset",
     .			"Position Offset",
     .			"X-Position Offset",
     .			"Y-Position Offset",
     .			"X-Position",
     .			"Y-Position",
     .			"LST",
     .			"UT",
     .			2*" "/
# else
      data status1 /    "Channels",
     .			"Frequency Offset",
     .			"Velocity (Header Def.)",
     .			"Radio Velocity",
     .			"Optical Velocity",
     .			"Relativistic Velocity",
     .			"Linear Velocity Scale",
     .			"Rest Frequency",
     .			"Sky Frequency",
     .			"Image Rest Frequency",
     .			"Image Sky Frequency"/
#endif
      data status3 /	"Line",
     .			"Histogram",
     .			"Points"/
      data status11/	"No graphics",
     .			"SunView",
     .			"Tek 4010",
     .			"Retrographics",
     .			"X-Window"/
      data status13/	"Unknown",
     .			"Keyboard",
     .			"Batch File",
     .			"Setup File",
     .			"Help File",
     .			"Menus"/
      data istat13/0, 5, 7, 12, 13, 15/
      data status21/	"None",
     .			"SHOW",
     .			"PLOT",
     .			"HLFTNE",
     .			"RASTER",
     .			"CONTOUR/QCONTOUR",
     .			"PLOTDOTS"/
c
      write(stch,20)
20    format(72('-'))
      call pwrite(istch, 72)
c
      ired = ifix(status(5)/(16.*16.))
      igreen = ifix( (status(5)-ired*16.*16.)/16.)
      iblue = ifix(status(5)-ired*16*16 -igreen*16) 
      write(stch,"(a, 3i4)") "Color (RGB): ", -(ired+1),-(igreen+1),
     .				-(iblue+1)
      write(stch(36:),fmt="(a, i5.3)",iostat=ierr) 
     .		"Linetype (Tick,Dash,Width): ", nint(status(6))
      call pwrite(istch, 72)
c
      write(stch,fmt="(a, f5.2)",iostat=ierr) "Character Size: ", 
     .					status(7)
      write(stch(36:),fmt="(a, i5)",iostat=ierr) 
     .		"# of scans in on-going average: ", nint(status(8))
      call pwrite(istch, 72)
c
      if (status(10) .ge. 1) then
	stch = "Scroll Lock: ON"
      else
	stch = "Scroll Lock: OFF"
      endif
      write(stch(36:),fmt="(a, i3)",iostat=ierr)
     .          "# of plot areas since last PAGE: ", nint(status(9))
      call pwrite(istch, 72)
c
      if ( nint(status(11)) .eq. 2 .and. termtype .eq. 'xwindow') then
         stch = "Graphics Screen: " // status11(5)
      else
         stch = "Graphics Screen: " // status11(nint(status(11)))
      endif
      do 100 i = 2, 6
	if (nint(status(13)) .eq. istat13(i)) goto 101
100	continue
      i = 1
101   stch = stch(1:35) // "Command Input: " // status13(i)
      call pwrite(istch, 72)
c
      if (status(16) .gt. 0) then
	stch = "Command Logging:  ON"
      else
	stch = "Command Logging: OFF"
      endif
      if (status(19) .gt. 0) then
	stch = stch(1:35) // "Error Trapping:  ON"
      else
	stch = stch(1:35) // "Error Trapping: OFF"
      endif
      call pwrite(istch, 72)
c
      if (status(15) .gt. 0) then
	stch = "On-line data: Available"
      else
	stch = "On-line data: Unavailable"
      endif
      if (status(22) .gt. 0) then
	stch = stch(1:35) // "Undo Facility:  ON"
      else
	stch = stch(1:35) // "Undo Facility: OFF"
      endif
      call pwrite(istch, 72)
c
      stch = "Last Plot was: " // status21(nint(status(21)))
      if (status(20) .gt. 0) then
	stch = stch(1:35) // "Debugging:  ON"
      else
	stch = stch(1:35) // "Debugging: OFF"
      endif
      call pwrite(istch, 72)
c
      stch = "PLOT/SHOW type: "  // status3(nint(status(3)))
      stch = stch(1:35) // "RESHOW type: "  // status3(nint(status(4)))
      call pwrite(istch, 72)
c
      stch = "SHOW axis (Lower/Upper): " // 
     .	status1(nint(status(1)))(1:lnblnk(status1(nint(status(1))))) //
     .	' / ' //  status1(nint(status(2)))
      call pwrite(istch, 72)
c
      if (status(17) .gt. 0) then
	stch = "Text output is going to: Graphics Screen"
      else
	if (status(14) .eq. 6) then
	    stch = "Text output is going to: Text Screen"
	else
	    stch = "Text output is going to: Printout File"
	endif
      endif
      call pwrite(istch, 72)
c
      write(stch,10,iostat=ierr) 
     .		"Graphics Screen Size: (", nint(status(23)), ",",
     .  	nint(status(24)), ")  Pixels"
10    format(a, 4(i5, a))
      call pwrite(istch, 72)
c
      write(stch,10,iostat=ierr) 
     .		"Current Graphics Page: (", nint(status(25)), ",",
     .  	nint(status(26)), ") to (", nint(status(27)), ",",
     .  	nint(status(28)), ") Pixels"
      call pwrite(istch, 72)
c
      write(stch,10,iostat=ierr) 
     .		"Location of Graphics Cursor: (", nint(status(31)), ",",
     .  	nint(status(32)), ") Pixels"
      call pwrite(istch, 72)
c
      write(stch,10,iostat=ierr) 
     .		"Text Screen Size: (", nint(status(29)), ",",
     .  	nint(status(30)), ") Characters"
      call pwrite(istch, 72)
c
      stch = "Printer: " // printer 
      stch = stch(1:lnblnk(stch)) // " (" // printtype 
      stch = stch(1:lnblnk(stch)) // ")"
      if (status(12) .lt. 0) then
         stch = stch(1:lnblnk(stch)) // " (Copy: OFF)"
      else
         stch = stch(1:lnblnk(stch)) // " (Copy:  ON)"
      endif
      call pwrite(istch, 72)
c
      write(stch,20)
      call pwrite(istch, 72)
c
      return
      end
