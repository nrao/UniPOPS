procedure pstack(fb,polz)
# accums the data; fb is the filter bank or IF in use.
# polz is the polarization channel.
# scan numbers in astack
global scalar spectype
scalar i, xscan, fb, polz, feed_no
if spectype = hctype; then
   feed_no = 10
else
   feed_no = 0
end
if (mod(fb,2) = 0) then; feed_no = 2 + feed_no; end
feed_no = feed_no + polz
for i = 1 to acount
#	Use newfeed to construct the true scan number
   xscan = newfeed(astack(i),feed_no)
   get (xscan)
#	All the work happens in prcstack
   prcstack
end
return
finish
