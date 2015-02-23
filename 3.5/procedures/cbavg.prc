procedure cbavg(fb)
#	average all data from filter bank fb
#	scan number are presumed to be in ASTACK
global scalar spectype
scalar nchan, fscan
sclear
if acount < 1; then
   print "CBAVG: No scan number in astack."; return
end
if spectype = hctype; then
   pstack(1,fb)
   nchan = 1
else
   fscan = newfeed(astack(1),(1 + (fb-1)*2))
   get(fscan)
   fbmode(nchan)
   pstack(fb, 1)
end
ave
if nchan = 2; then
   copy(0,3)
   pstack(fb,2)
   ave
   copy(0,4)
   cboth
else
   all page show
end
return
finish
#		some useful shorthands
procedure c1
cbavg(1)
return
finish
#
procedure c2
cbavg(2)
return
finish
