procedure vymatrix(dy)
# Sets up array 0 to put a number associated with the each of the first 
# MAXNSAVE scans in the current save file onto a vel-y position grid
# using the 0 matrix.  Any contents of matrix 0 will be overwritten/lost.
#
# dy is the increment in the Y direction between pixels (in arc seconds).
# (the velocity increment is gleaned from the first scan)
#
scalar ymin, vmin, ymax, vmax, dytmp, dv, xlunit, xuunit, slen
global scalar maxnsave
string*52 cmmt
string*4 type
#
if (maxnsave < 1) then
   print "vymatrix: no scans indicated.  MAXNSAVE = ",maxnsave
   return
end
nsave=1; recall
mremove(0)
mh0(mobject)=h0(object)
mh0(mpix1)=1; mh0(mpix2) = 1
mh0(morigin) = h0(telescop)
mh0(mtype1) = "VELOCITY"
mh0(mtype2) = "Y-AXIS"
if (compare(h0(coordcd),"GALACTIC")) then
   mh0(mtype2) = "GLAT"
else
   slen = lnblnk(h0(coordcd))
   if (slen > 6) then
      type = substr(h0(coordcd),(slen-3),slen)
      if (compare(type,"RADC")) then
         mh0(mtype2) = "DEC"
      end
   end
end
sprint(cmmt,"a,i4,a,a") "vymatrix: using ",maxnsave," scans from ",fname(sscans)
mh0(mcomment) = cmmt
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
   ymin = min(ymin, h0(ysource))
   ymax = max(ymax, h0(ysource))
   dv = min(dv, h0(deltax))
   if (dv > 0) then
      vmin = min(vmin, chantox(1)); vmax=max(vmax, chantox(h0(noint)))
   else
      vmin = min(vmin, chantox(h0(noint))); vmax=max(vmax, chantox(1))
   end
end
saxis(xlunit, xuunit)
dytmp = dy / 3600.0
mh0(mdelt2) = dytmp
if (dy > 0) then
  mh0(mrval2) = ymin
else
  mh0(mrval2) = ymax
end
if (dv > 0) then
  mh0(mrval1) = vmin
else
  mh0(mrval1) = vmax
end
mh0(mdelt1) = dv
mh0(mnaxis2) = nint((ymax-ymin)/abs(dytmp)+1)
mh0(mnaxis1) = nint((vmax-vmin)/abs(dv)+1)
mheader
#
return
finish
