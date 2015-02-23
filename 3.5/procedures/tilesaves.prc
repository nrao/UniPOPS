procedure tilesaves(dx, dy)
#  uses tileprep to set up global values and tileplace to place the
#  first MAXNSAVE scans from the current save file tiled on the graphics screen
#  dx and dy are the angular distances between scans in the X and Y 
#  directions in arcseconds.  This value will be corrected to the
#  observed coordinate increment (using the cos or the average Y value).
global scalar maxnsave
scalar xmin, ymin, xmax, ymax, nx, ny, xcent, ycent, dxtmp, dytmp, dxcor
scalar zmax, nzmax
pointer rad 57.2957795
#  loop through the scans first to get the limits for tileprep
if (maxnsave < 1) then
   print "tilesaves: MAXNSAVE = ", maxnsave
   return
end
if (maxnsave = 1) then
   page;show 
   return
end
nsave=1; recall
xmin=h0(xsource); ymin=h0(ysource)
xmax=xmin; ymax=ymin; peak; zmax = height(1); nzmax = 1
for nsave = 2 to maxnsave
   recall
   xmin = min(xmin, h0(xsource)); xmax = max(xmax, h0(xsource))
   ymin = min(ymin, h0(ysource)); ymax = max(ymax, h0(ysource))
   peak; if (height(1) > zmax) then; zmax = height(1); nzmax = nsave; end
end
xcent = (xmax + xmin) / 2
ycent = (ymax + ymin) / 2
dxcor = dx / cos(ycent/rad)
dxtmp = abs(dxcor) / 3600.
dytmp = abs(dy) / 3600.
nx = nint((xmax - xmin)/dxtmp + 1)
ny = nint((ymax - ymin)/dytmp + 1)
tileprep(dxcor, nx, dy, ny, xcent, ycent)
#
page; slabel=2
nsave=nzmax;recall
graphics; place(1,10); title; crt
tileplace; holdy; slabel=0
for nsave = 1 to maxnsave
   if (nsave ~= nzmax) then; recall; tileplace; end
end
slabel=1; freey
return
finish
