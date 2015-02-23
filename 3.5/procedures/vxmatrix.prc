procedure vxmatrix(dx)
# Sets up array 0 to put a number associated with the each of the first 
# MAXNSAVE scans in the current save file onto a vel-x position grid
# using the 0 matrix.  Any contents of matrix 0 will be overwritten/lost.
#
# dx is the increment in the X direction between pixels on the sky
# (in arc seconds).  The velocity increment is gleaned from the first scan.
#
scalar xmin, vmin, xmax, vmax, dxtmp, dv, xlunit, xuunit, slen, dxcor
scalar ycent, ymin, ymax
global scalar maxnsave
string*4 type
string*52 cmmt
#
if (maxnsave < 1) then
   print "vxmatrix: no scans indicated.  MAXNSAVE = ",maxnsave
   return
end
nsave=1; recall
mremove(0)
mh0(mobject)=h0(object)
mh0(mpix1)=1; mh0(mpix2) = 1
mh0(morigin) = h0(telescop)
mh0(mtype1) = "VELOCITY"
mh0(mtype2) = "X-AXIS"
if (compare(h0(coordcd),"GALACTIC")) then
   mh0(mtype2) = "GLON"
else
   slen = lnblnk(h0(coordcd))
   if (slen > 6) then
      type = substr(h0(coordcd),(slen-3),slen)
      if (compare(type,"RADC")) then
         mh0(mtype2) = "RA"
      end
   end
end
sprint(cmmt,"a,i4,a,a") "vxmatrix: using ",maxnsave," scans from ",fname(sscans)
mh0(mcomment) = cmmt
xmin=h0(xsource); xmax = xmin
ymin=h0(ysource); ymax = ymin
xlunit = status(1)
xuunit = status(2)
saxis(velhead, xuunit)
#			ensure that chantox returns a velocity
dv = h0(deltax)
if (dv > 0) then
   vmin=chantox(1); vmax=chantox(h0(noint))
else
   vmin=chantox(h0(noint)); vmax=chantox(1)
end
if (maxnsave < 2) then; return; end
for nsave = 2 to maxnsave
   recall
   xmin = min(xmin, h0(xsource))
   xmax = max(xmax, h0(xsource))
   ymin = min(ymin, h0(ysource))
   ymax = max(ymax, h0(ysource))
   dv = min(dv, h0(deltax))
   if (dv > 0) then
      vmin = min(vmin, chantox(1)); vmax=max(vmax, chantox(h0(noint)))
   else
      vmin = min(vmin, chantox(h0(noint))); vmax=max(vmax, chantox(1))
   end
end
ycent = (ymax + ymin) / 2
dxcor = dx / cos(ycent / 57.2957795)
dxtmp = dxcor / 3600.0
mh0(mdelt2) = dxtmp
saxis(xlunit, xuunit)
if (dx > 0) then
  mh0(mrval2) = xmin
else
  mh0(mrval2) = xmax
end
if (dv > 0) then
  mh0(mrval1) = vmin
else
  mh0(mrval1) = vmax
end
mh0(mdelt1) = dv
mh0(mnaxis2) = nint((xmax-xmin)/abs(dxtmp)+1)
mh0(mnaxis1) = nint((vmax-vmin)/abs(dv)+1)
mheader
#
return
finish
