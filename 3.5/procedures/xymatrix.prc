procedure xymatrix(dx, dy)
# Sets up array 0 to put a number associated with the each of the first 
# MAXNSAVE scans in the current save file onto an X-Y position grid
# using the 0 matrix.  Any contents of matrix 0 will be overwritten/lost.
#
# dx and dy are the increments in the X and Y directions between
# pixels on the sky (in arc seconds).
#
scalar xmin, ymin, xmax, ymax, dxtmp, dytmp, dxcor, ycent, slen
global scalar maxnsave
string*4 type
string*52 cmmt
#
if (maxnsave < 1) then
   print "xymatrix: no scans indicated.  MAXNSAVE = ", maxnsave
   return
end
nsave=1; recall
mremove(0)
mh0(mobject)=h0(object)
mh0(mpix1)=1; mh0(mpix2) = 1
mh0(mtype1) = "X-AXIS"
mh0(mtype2) = "Y-AXIS"
if (compare(h0(coordcd),"GALACTIC")) then
   mh0(mtype1) = "GLON"
   mh0(mtype2) = "GLAT"
else
   slen = lnblnk(h0(coordcd))
   if (slen > 6) then
      type = substr(h0(coordcd),(slen-3),slen)
      if (compare(type,"RADC")) then
         mh0(mtype1) = "RA"
         mh0(mtype2) = "DEC"
      end
   end
end
mh0(morigin) = h0(telescop)
sprint(cmmt,"a,i4,a,a") "xymatrix: using ",maxnsave," scans from ",fname(sscans)
mh0(mcomment) = cmmt
xmin=h0(xsource); ymin=h0(ysource)
xmax=xmin; ymax=ymin
if (maxnsave < 2) then; return; end
for nsave = 2 to maxnsave
   recall
   xmin = min(xmin, h0(xsource))
   xmax = max(xmax, h0(xsource))
   ymin = min(ymin, h0(ysource))
   ymax = max(ymax, h0(ysource))
end
ycent = (ymax + ymin) / 2
dxcor = dx / cos(ycent / 57.2957795)
dxtmp = dxcor / 3600.0
dytmp = dy / 3600.0
mh0(mdelt1) = dxtmp; mh0(mdelt2) = dytmp
if (dx > 1) then
  mh0(mrval1) = xmin
else
  mh0(mrval1) = xmax
end
if (dy > 0) then
  mh0(mrval2) = ymin
else
  mh0(mrval2) = ymax
end
mh0(mnaxis1) = nint((xmax-xmin)/abs(dxtmp)+1)
mh0(mnaxis2) = nint((ymax-ymin)/abs(dytmp)+1)
mheader
#
return
finish
