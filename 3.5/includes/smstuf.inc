c					include smstuf
C-------------------------------------------------------------------------------
C  @(#)smstuf.inc	5.1 06/22/94
C-------------------------------------------------------------------------------
c 
      integer*2 type, tag, kpak(30), nkar, kbptr, level, lx, nextp,
     .		locsym
      real*4 x(15)
C
      COMMON /SMSTUF/  X, KPAK, NKAR, KBPTR, TYPE, TAG, LEVEL, LX, 
     .		       NEXTP, LOCSYM
C	
c     kpak = Packed string containing the next field of the line being 
c	processed;
c     nkar = number of characters in next field on line
c     kbptr = character position in input line which begins next field
c     type = type of operator: 4 = regular verb; 5 = pseudo verb; 
c	6 = alias; 7 = string variable; 1 = scalar; 8 = protected scalar;
c	3 = procedure; 2 = array variable; 11 = real literal
c	15 = logical literal; 14 = hollerith literal
c     tag = tag of operator being processed, if > 0, or operand (if < 0)
c     level = present precedence level
c     lx = Number of i*2 words that the unpacked string, representing the
c	current field being processed, takes up
c     nextp = precedence level for next operator
c     locsym = pointer in K array to next entry for a compiled procedure
c     x = array where literals are stored
c
