procedure vygrid
# produce a vel-yposition grid.  Summing all values in the X direction.
# All x values have equal weight.
# array 3 is used to keep track of the number of X positions at each
# point for averaging.  Any existing contents of array 3 are therefore lost.
# The first MAXNSAVE save scans will be used.
# The grid must have already been created.
scalar y, v, on, xlunit, xuunit, i, j
scalar ydim, vdim, y0, ydelt, mv0, mvdelt, zundef
scalar vl, vr, vref, vdelt, l, r, iref, vchan
global scalar maxnsave
if (nsave < 1) then
   print "vygrid: no scans indicated.  MAXNSAVE = ", maxnsave
   return
end 
xlunit = status(1)
xuunit = status(2)
saxis(velhead, xuunit)
#		ensure that chantox returns velocity
iref = h0(refpt)
l = iref - 1; r = iref + 1
vl = chantox(l); vr = chantox(r); vref = chantox(iref)
vdelt = (vr - vl) / 2
saxis(xlunit, xuunit)
#		reset axis labels
#		set up array 3
mremove 3
ydim = mh0(mnaxis2); vdim = mh0(mnaxis1)
y0 = mh0(mrval2); ydelt = mh0(mdelt2)
mv0 = mh0(mrval1); mvdelt = mh0(mdelt1)
zundef = mh0(mundef)
mh3(mnaxis2) = ydim; mh3(mnaxis1) = vdim
mblank(3)
#
for nsave = 1 to maxnsave
   recall
   on = true
   y = nint((h0(ysource)-y0)/ydelt + 1)
   if (y < 1 | y > ydim) then
      print "Scan ",h0(scan)," is off the grid with y = ", y
      on = false
   end
   if (on) then
      for i = 1 to vdim
         vchan = (i - iref) * vdelt + vref
         v = nint((vchan - mv0)/mvdelt + 1)
         if (v < 1 | v > vdim) then
            print "Scan ", h0(scan)," is off the grid with v = ", v
            on = false
         end
         if (on) then
            if (md0(v,y) = zundef) then
               md0(v,y) = d0(i)
               md3(v,y) = 1
            else
               md0(v,y) = md0(v,y) + d0(i)
               md3(v,y) = md3(v,y) + 1
            end
         end
      end
   end
end
for i = 1 to vdim
   for j = 1 to ydim
      if (md3(i,j) ~= zundef) then
         md0(i,j) = md0(i,j) / md3(i,j)
      else
         md0(i,j) = zundef
      end
   end
end
return
finish
