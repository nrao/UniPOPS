procedure xygrid
# grid values produced by prc2dim (one value per scan) for the first
# MAXNSAVE scans stored in the save file.  The grid must have already
# been created.
# There is no attempt to check for overlap, overlapping scans mearly
# overwrite the previous value in the grid.
scalar x, y, on, refx, refy, delx, dely, nxpix, nypix
global scalar maxnsave
if (nsave < 1) then
   print "xygrid: no scans indicated.  MAXNSAVE = ", maxnsave
   return
end
refx = mh0(mrval1); refy = mh0(mrval2)
delx = mh0(mdelt1); dely = mh0(mdelt2)
nxpix = mh0(mnaxis1); nypix = mh0(mnaxis2)
for nsave = 1 to maxnsave
   recall
   on = true
   x = nint((h0(xsource)-refx)/delx + 1)
   y = nint((h0(ysource)-refy)/dely + 1)
   if (x < 1 | x > nxpix) then
      print "Scan ",h0(scan)," is off the grid with x = ", x
      on = false
   end
   if (y < 1 | y > nypix) then
      print "Scan ",h0(scan)," is off the grid with y = ", y
      on = false
   end
   if (on) then; md0(x,y) = prc2dim; end
end
return
finish
