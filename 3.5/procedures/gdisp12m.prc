procedure gdisp12m(xscan, fb)
#  display the GAIN scan, xscan, from filter bank fb
global scalar spectype
scalar fscan, nchan
if spectype = hctype; then
   fscan = newfeed(xscan,(11 + (fb-1)))
   gget(fscan)
   nchan = 1
else
   fscan = newfeed(xscan,(1 + (fb-1)*2))
   gget(fscan)
   gfbmode(nchan)
end
if nchan = 2; then
   copy(2,3)
   gget(addfeed(fscan,1))
   copy(2,4)
   cboth
else
   copy(2,0)
   page show
end
return
finish
#		some useful shorthands for the above
procedure gget1(xscan)
gdisp12m(xscan, 1)
return
finish
#
procedure gget2(xscan)
gdisp12m(xscan, 2)
return
finish
#		some old standbys as aliases
alias ggets gdisp12m
alias gf gget1
alias gs gget2
