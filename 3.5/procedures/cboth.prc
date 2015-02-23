procedure sminmax(dval, dmin, dmax)
# compares the current min and max, dmin, dmax, against a data value, dval
# if dval=default (Inf), dmin and dmax are not change
# if dmin or dmax is Inf, they take the value of dval
# otherwise the standard min and max is used
#
scalar dval, dmin, dmax
#
if (dval ~= default) then
   if (dmin ~= default) then
      dmin = min(dval, dmin)
   else
      dmin = dval
   end
   if (dmax ~= default) then
      dmax = max(dval, dmax)
   else
      dmax = dval
   end
end
return
finish

procedure cboth
#  Displays two spectra side by side.  Assumes first spectrum is in
#  data array D3, second spectrum in D4. Uses same Y scale.
scalar i, tamin, tamax, tadiff
all page
slabel=1
#
# find min and max, ignoring any default (Inf) values
tamin = d3(1)
tamax = d3(1)
{ sminmax(d3@, tamin, tamax); }
{ sminmax(d4@, tamin, tamax); }
#
# if either tamax or tamin are default (Inf) at this point, then they
# both must be and nothing is available to plot
if (tamin = default) then
   print 'There are no valid data values to plot.'
   return
end
#
tadiff = tamax - tamin
tamin = tamin - .05 * tadiff
tamax = tamax + .05 * tadiff
yrange(tamin, tamax)
#
copy(3,0)
setpage(24,500,150,700)
charsize(9)
show
slabel = 2
copy(4,0)
setpage(500,1000,150,700)
charsize(9)
show
rstpage
freey; slabel=1
return
finish
