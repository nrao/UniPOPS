      SUBROUTINE VERBS (J)
C-------------------------------------------------------------------------------
C  @(#)verbs.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C---------------------------------------------------------------------
C     VERBS is a driver for all application verbs.
C---------------------------------------------------------------------
      INTEGER*2 VERB, j, i, ioverlay
      integer*2 n120, m3
c
      include 'tags.inc'
c
      data n120, m3 /120, -3/
C---------------------------------------------------------------------
C					Search list for	proper overlay.
C---------------------------------------------------------------------
c
 20   DO 22 I=1,20
        if (iaubegin(i) .le. 0) goto 22
	if (j .ge. iaubegin(i) ) then
	    verb = j - iaubegin(i) + 1
	    ioverlay = i
	 endif
   22    CONTINUE
c
      GO TO (1,2,3,4,5,6,7,8,9,10,11), ioverlay
      call oerror(n120, m3, 'VERBS')
c
    1 CONTINUE
      CALL AU1(VERB)
      GO TO 99
    2 CONTINUE
      CALL AU2(VERB)
      GO TO 99
    3 CONTINUE
      CALL AU3(VERB)
      GO TO 99
    4 CONTINUE
      CALL AU4(VERB)
      GO TO 99
    5 CONTINUE
      CALL AU5(VERB)
      GO TO 99
    6 CONTINUE
      CALL AU6(VERB)
      GO TO 99
    7 CONTINUE
      CALL AU7(VERB)
      GO TO 99
    8 CONTINUE
      CALL AU8(VERB)
      GO TO 99
    9 CONTINUE
      CALL AU9(VERB)
      GO TO 99
   10 CONTINUE
      CALL AUA(VERB)
      GO TO 99
   11 CONTINUE
      CALL AUB(VERB)
      GO TO 99
C
   99 RETURN
      END

