procedure disp12m(xscan,fb)
#  Display the data from filter bank fb for scan xscan
global scalar spectype
scalar fscan, nchan
if spectype = hctype; then
   fscan = newfeed(xscan,(11 + (fb-1)))
   get(fscan)
   nchan = 1
else
   fscan = newfeed(xscan,(1 + (fb-1)*2))
   get(fscan)
   fbmode(nchan)
end
if nchan = 2; then
   copy(0,3)
   fscan = addfeed(fscan,1)
   get4(fscan)
   cboth
else
   page; show
end
return
finish
#		Some short hand for the above
#	s1(xscan) is equiv to disp12m(xscan, 1)
procedure s1(xscan)
disp12m(xscan, 1)
return
finish
#       s2(xscan) is equiv to disp12m(xscan, 2)
procedure s2(xscan)
disp12m(xscan, 2)
return
finish
#	old reliables aliased to these
alias f s1
alias s s2
