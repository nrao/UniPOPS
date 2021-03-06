procedure gfbmode(answer)
#	returns the mode (1 or 2) for the filter-bank scan in array 2 (gains)
#	1 = series, single scan to display, 2 = parallel, 2 to display
#       answer = 1 for non-filter-bank data (subscan <= 4)
#
scalar subscan
#
answer = 1
subscan = ifix((h2(scan) - ifix(h2(scan))) * 100 + 0.5)
if (subscan <= 4 & subscan > 0) then
#		mode = 2 if 128 channels and NOT the 30 kHz FB)
   if (h2(noint) = 128 & nint(abs(h2(freqres))*1000) ~= 30) then
      answer = 2
   end
end
#
return
finish
