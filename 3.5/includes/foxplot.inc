c
c     @(#)foxplot.inc	5.1 06/22/94
c
c     Common block and information to use for drawing lines and characters.
c
      real XDEFAULTS, YDEFAULTS
      integer*2 NUMSETS, NUMFONTS, MAXNUMCHARS, MAXNUMVCTRS, SIGPENUP
c
      parameter (XDEFAULTS = 24.)
      parameter (YDEFAULTS = 26.)
      parameter (NUMSETS = 3)
      parameter (NUMFONTS = 15)
      parameter (MAXNUMCHARS = 256)
      parameter (MAXNUMVCTRS = 128)
      parameter (SIGPENUP = -64)
c
c     xdefaults, ydefaults = default width and heights of characters
c     numsets = max. number of loaded fonts
c     maxnumchars = max. number of chars in a font
c     maxnumvctrs = max. num. vectors to describe a character.
c     sigpenup = signal that a vector decription is a move to and not
c		a vector.
c
c---------------------------
c
      integer*2 nc(MAXNUMCHARS,NUMSETS), 
     .          vx(NUMSETS,MAXNUMCHARS,MAXNUMVCTRS),
     .	        vy(NUMSETS,MAXNUMCHARS,MAXNUMVCTRS), numc(NUMSETS), 
     .		numbold, usesets(NUMSETS), oldest, nset, xpos, ypos 
      logical spaces
      real*4 angle, csize, widthchar, hghtchar
c
      common/foxplot/nc, vx, vy, numc, numbold, usesets, oldest, nset,
     .		     xpos, ypos, spaces, angle, csize, widthchar, hghtchar
c
c---------------------
c
c     nc = arrays for the three loaded fonts that records the character
c	numbers described by VX and VY
c     vx, vy = description of the vectors to draw the character indicated
c	by NC.
c     numc = number of characters in the three fonts
c
c     Example:  If NUMC(2) = 64, then there are 64 characters in the font
c		loaded into slot 2.  The 1st 64 elements of NC, VX, and VY
c		are defined.
c
c		If NC(2, 12) = 40, then the 12th character in slot 2 
c		corresponds to the 40th character in the computer's
c		character numbering scheme -- for ASCII, 40 = '('.
c
c		The absolute value of VX(2,12,1) contains the suggested x 
c		distance offsets (in pixels) between the start of the
c		character and the center of the character.
c		VY(2,12,1) contains the suggested x distance from the center
c		of the current character to its end of the character.  Thus,
c		the character width is abs(VX(2,12,1)) + abs(VY(2,12,1)).
c
c		The VX(2,12,2 - 128) and VY(2,12,2-128) contain a description 
c		of the vectors used to draw the character.
c		If VX(2,12,i)=-64 and VY(2,12,i) = -64, then no more
c		   lines to draw in character and character is finished.
c		Else if VX(2,12,i) = -64 but VY(2,12,i) doesn't = -64,  
c		   then pick the pen up and the next specification is a move.
c		Else if i = 2 or, for last i, VX=-64 (a move to), move the
c		   pen to [ VX(2,12,i),VY(2,12,i) ].
c		Else (you've just done a move), draw a line to
c		   [ VX(2,12,i), VY(2,12,i) ] 
c
c     numbold = boldness designation (1 thru 3)
c     usesets = font numbers of loaded fonts.  If USESTE(2) = 13, then
c		font number 13 is loaded into slot 2.
c     oldest = index in usesets  of the oldest loaded font (and the location
c	where the next font will go into VX, VY, NC, NUMC)
c     nset = index in NC, VX, VY, NUMC indicating which of the loaded fonts
c	is desired.  If NSET = 2, then the font in slot 2 is the currently
c	desired one.
c     spaces = true if you want uniform-spaced characters
c     angle = angle of rotation of the character to be drawn.
c     csize = desired size of characters in units of xdefaults (1 = default
c	character size).
c     xpos, ypos = current pen position in pixels.
c     widthchar, hghtchar = width and height of last character drawn
c
c--------------------------------------
c
