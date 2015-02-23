procedure vxgrid
# produce a vel-xposition grid.  Summing all values in the Y direction.
# All y values have equal weight.
# array 3 is used to keep track of the number of Y positions at each
# point for averaging.  Any existing contents of array 3 are therefore lost.
# The first MAXNSAVE save scans will be used.
# The grid must have already been created.
scalar x, v, on, xlunit, xuunit, i, j
scalar xdim, vdim, x0, xdelt, mv0, mvdelt, zundef
scalar vl, vr, vref, vdelt, l, r, iref, vchan
global scalar maxnsave
if (nsave < 1) then
   print "vxgrid: no scans indicated.  MAXNSAVE = ", maxnsave
   return
end
xlunit = status(1)
xuunit = status(2)
saxis(velhead, xuunit)
#		ensure that chantox returns velocity
iref = h0(refpt)
l = iref -1; r = iref + 1
vl = chantox(l); vr = chantox(r); vref = chantox(iref)
vdelt = (vr - vl) / 2
saxis(xlunit, xuunit)
#		reset axis labels 
#		set up array 3
mremove 3
xdim = mh0(mnaxis2); vdim = mh0(mnaxis1)
x0 = mh0(mrval2); xdelt = mh0(mdelt2)
mv0 = mh0(mrval1); mvdelt = mh0(mdelt1)
zundef = mh0(mundef)
mh3(mnaxis2) = xdim; mh3(mnaxis1) = vdim
mblank(3)
#
for nsave = 1 to maxnsave
   recall
   on = true
   x = nint((h0(xsource)-x0)/xdelt + 1)
   if (x < 1 | x > xdim) then
      print "Scan ",h0(scan)," is off the grid with x = ", x
      on = false
   end
   if (on) then
      for i = 1 to vdim
         vchan = (i - iref) * vdelt + vref
         v = nint((vchan - mv0)/mvdelt + 1)
         if (v < 1 | v > vdim) then
            print "Scan ",h0(scan)," is off the grid with v = ", v
            on = false
         end
         if (on) then
            if (md0(v,x) = zundef) then
               md0(v,x) = d0(i)
               md3(v,x) = 1
            else
               md0(v,x) = md0(v,x) + d0(i)
               md3(v,x) = md3(v,x) + 1
            end
         end
      end
   end
end
for i = 1 to vdim
   for j = 1 to xdim
      if (md3(i,j) ~= zundef) then
         md0(i,j) = md0(i,j) / md3(i,j)
      else
         md0(i,j) = zundef
      end
   end
end
return
finish
