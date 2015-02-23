PROCEDURE FIVELINE(CSCAN, FB)
#   Procedure for fitting spectral line five-points (both BS and TP)
#   Data must be taken in the order N-S-C-E-W
#   CSCAN is the scan number of the C, Center, scan
#   CSCAN is assumed to be an integer without a subscan number
#   Furthermore, scans with 128 or 768 channels are assumed to have originally
#   come from 2 receiver data (CSCAN.01 and CSCAN.02 or CSCAN.11 and CSCAN.12).
#   FB indicates the filter bank to be used.  
#      FB=1 -> CSCAN.01 (and .02 if 128 ch.)
#      FB=2 -> CSCAN.03 (and .04 if 128 ch.)
#      FB=3 -> CSCAN.11 and CSCAN.12
#   CSCAN.01 and CSCAN.03 will be averaged (if 256 channels and the user
#      answers "1" (YES)) independant of the value of FB.
#   Internally arrays 5-9 are used.
#   MODE = 0 ==> BS five point; MODE = 1 ==> TP five point
#         Last update 08/07/96 (JGM)
SCALAR DO2, GETBL, I, XSCAN, DSPL, ANS, CSCAN, mode
SCALAR TS3, TS4, TYINCR, TYMIN, TSLABEL, subscn, xsgn
string*16 objnam
ARRAY TINT(5), PEAKTA(5)
#	check value of FB
if (fb ~= 1 & fb ~= 2 & fb ~= 3) then
   print 'Unrecognized value for FB argument, fb = ',fb
   return
end
subscn = 1
if (fb = 2) then;subscn = 3;end
if (fb = 3) then;subscn = 11;end
#	save some values so we can reset what we've done
TS3 = STATUS(3); TS4 = STATUS(4)
TYINCR = YINCR; TYMIN = YMIN; TSLABEL = SLABEL
mode = 0
#
RHIST; HISTOGRAM; SLABEL=1
DO2 = 0; GETBL = 0
CSCAN = newfeed(CSCAN,subscn)
GET (CSCAN)
#
if (compare(h0(obsmode),'LINETPON')); then
  mode = 1
end
#	set object name for testing later
objnam = h0(object)
IF (H0(NOINT) = 256) THEN
   PRINT 'AVERAGE THE TWO ''RECEIVERS? ''(1=YES, 0=NO)'
   READ DO2
END
PRINT 'USE OLD BASELINE AND INTEGRATION REGIONS (1=YES, 0=NO)'
READ GETBL
#
if (do2 = 1) then
   subscn = 2
   if (fb = 2) then; subscn = -2; end
end
xsgn = 1
if (xscan < 0) then; xsgn = -1; end
FOR I = 1 TO 5
   XSCAN = CSCAN + (I - 3)*xsgn
   if (mode = 1) then
      tonns(xscan)
   else
      GET XSCAN
   end
   if ~(compare(objnam,h0(object))) then
      print 'OBJECT NAMES DO NOT MATCH...QUITTING'
      return
   end
   replace
   IF (DO2 = 1) THEN
      ACCUM
      if (mode = 1) then
         tonns(addfeed(xscan,subscn))
      else
         GET(addfeed(XSCAN,subscn))
      end
      replace; ACCUM; AVE
   END
   IF (H0(NOINT) = 128 | h0(noint) = 768) THEN
      ACCUM
      if (mode = 1) then
         tonns(addfeed(xscan,1))
      else
         GET(addfeed(XSCAN,1))
      end
      replace; ACCUM; AVE
# 	This assumes that the 2 subscan numbers overlap with no shifting
   END
   HANNING
   COPY(0,I+4)
END
#
IF GETBL = 0;THEN
   COPY(7,0);PAGE;SHOW;
   PRINT 'ENTER THE 2 BASELINE REGIONS'
   NREGION=0; NREGION(1) = CCUR; NREGION(2) = CCUR;
   NREGION(3) = CCUR; NREGION(4) = CCUR;
   BASELINE; ZLINE = 1; PAGE; SHOW; ZLINE = 0
   PRINT 'ENTER INTEGRATION REGION'
   BMOMENT = CCUR
   EMOMENT = CCUR
   BDROP = BMOMENT - 1
   EDROP = H0(NOINT) - EMOMENT
END
#
FOR I = 1 to 5
   COPY(I+4, 0); BASELINE; COPY(0,I+4); MOMENT; PEAK
   TINT(I) = ABS(SIZE(1)); PEAKTA(I) = HEIGHT(1)
END
#
BDROP = DEFAULT; EDROP = DEFAULT
ANS = 1
PRINT 'DISPLAY EACH SCAN ? (1 = YES, 0 = NO)'
READ DSPL
IF (DSPL = 1) THEN
   YRANGE(0, 7*PEAKTA(3))
   COPY(5,0); FACT=PEAKTA(3); BIAS; PAGE; SHOW
   FOR I = 1 TO 5
      SLABEL = 2
      COPY(I+4,0); FACT=I*PEAKTA(3); BIAS; RESHOW
   END
   PRINT '   MAKE A COPY ? (1 = YES, 0 = NO)'
   READ ANS
   IF (ANS = 1) THEN; GCOPY; END
   PRINT '   TYPE 1 TO CONTINUE ANYTHING ELSE TO STOP'
   READ ANS
END
#
IF (ANS = 1) THEN
   COPY(7,0);H0(NOINT)=10
   FOR I = 1 TO 5
      D0(I) = TINT(I)
      D0(I+5) = PEAKTA(I)
   END
#
   PAGE; PDOC 2;
END
#
STATUS(3) = TS3; STATUS(4) = TS4
YINCR = TYINCR; YMIN = TYMIN; SLABEL = TSLABEL
#
RETURN
FINISH
