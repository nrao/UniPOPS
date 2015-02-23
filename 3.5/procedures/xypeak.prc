procedure xypeak
# grid the peak intensity of each of the first MAXNSAVE scans stored in
# the save file.
# There is no checking for overlapping scans.  Overlapping scans mearly
# overwrite any previous values stored in the matrix.
#
scalar x, y, on, refx, refy, delx, dely, nxpix, nypix
global scalar maxnsave
if (nsave < 1) then
   print "xypeak: no scans indicated.  MAXNSAVE = ", maxnsave
   return
end
refx = mh0(mrval1); refy = mh0(mrval2)
delx = mh0(mdelt1); dely = mh0(mdelt2)
nxpix = mh0(mnaxis1); nypix = mh0(mnaxis2)
mh0(mbunit) = "K"
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
   if (on) then; md0(x,y) = peak2dim; end
end
return
finish
