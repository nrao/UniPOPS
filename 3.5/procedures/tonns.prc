PROCEDURE TONNS(XSCAN)
#  Takes a total power spectrum and it's offscan reference,
#  creates (ON-OFF)/OFF , and multiplies by the calibration
#  GAINS scan.
scalar gainscan, onscan, offsc, subsc
  GET XSCAN
#	make sure, for comparisons, that onscan is what we just got
  onscan = h0(scan) 
  subsc = ifix((h0(scan) - ifix(h0(scan)))*100 + 0.5) 
  OFFSC=ifix(H0(OFFSCAN)) + subsc / 100.
  IF ~(OFFSC=onscan | OFFSC=0.) THEN
    GAINSCAN=ifix(H0(GAINS)) + subsc/100.; 
#	make sure that sign of scan numbers matches XSCAN
    if (xscan < 0) then; offsc = -offsc; gainscan = -gainscan; end
    OFF OFFSC; GGET GAINSCAN
    TEMP
  ELSE
    PRINT ' Scan ',onscan,' is an OFF scan. No analysis possible.'
  END
  RETURN
FINISH
