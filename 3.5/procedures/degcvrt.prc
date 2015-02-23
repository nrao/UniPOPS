proc ddtoddmmss(din)
# Converts an angle from DD.dddd format to DDMMSS.s
scalar absdin, ideg, rmin, imin, sec, pos
#
absdin = abs(din); ideg = ifix(absdin)
rmin = 60*(absdin - ideg); imin = ifix(rmin); sec = 60*(rmin - imin)
if abs(sec-60) < 0.001 then
	sec = 0; imin = imin + 1
end
if (imin >= 60) then
	imin = imin - 60; ideg = ideg + 1
end
pos = 10000*ideg+ 100*imin+ sec; pos = sign(pos, din)
return pos
finish

proc ddmmsstodd(din)
# Converts an angle from DDMMSS.s format to DD.dddd
scalar pos, absdin, ideg, rmin, imin, sec
#
absdin = abs(din); sec = mod(absdin, 100.)
if sec>60 then; print 'Error in format (SEC)'; return default; end
imin = mod(absdin-sec, 10000)/100
if imin>60 then; print 'Error in format (MIN)'; return default; end
ideg = (absdin-100*imin-sec)/10000
pos = ideg + imin/60 + sec/3600; pos = sign(pos, din)
return pos
finish

