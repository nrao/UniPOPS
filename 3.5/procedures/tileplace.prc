procedure tileplace
#	places scan in array 0 onto the graphics screen.
#       used after a tileprep will place according to position
scalar xpix, ypix, xlow, xhi, ylow, yhi, ocsize, xm, ym, ncsize
string*8 cscan
#
xpix = (h0(xsource) - tile_xc) * tile_delx + tile_xpixc
ypix = (h0(ysource) - tile_yc) * tile_dely + tile_ypixc
xpix = nint(xpix); ypix = nint(ypix)
xlow = ifix(xpix - tile_xsize/2 + 1); xhi = xlow + tile_xsize - 1
ylow = ifix(ypix - tile_ysize/2 + 1); yhi = ylow + tile_ysize - 1
setpage(xlow, xhi, ylow, yhi); show
#
xm = xlow + tile_xsize/8; ym = yhi - tile_ysize/5
ocsize = status(7); sprint(cscan,'f8.2') h0(scan);
ncsize = min(ocsize * 3.0, 45)
charsize(ncsize); place(xm, ym); char(cscan)
rstpage
return
finish
