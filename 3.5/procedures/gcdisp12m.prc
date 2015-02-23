procedure gcdisp12m(fb)
# Display the most recent GAIN scan for filter bank fb.
global scalar spectype
scalar nchan, fscan
if spectype = hctype; then
   cgget(11+(fb-1))
   nchan = 1
else
   fscan = 1 + (fb-1)*2
   cgget(fscan)
   gfbmode(nchan)
end
if nchan = 2; then
   copy(2,3)
   cgget(fscan+1)
   copy(2,4)
   cboth
else
   copy(2,0)
   page show
end
return
finish
#		some useful shorthands
procedure g1
gcdisp12m(1)
return
finish
#
procedure g2
gcdisp12m(2)
return
finish
