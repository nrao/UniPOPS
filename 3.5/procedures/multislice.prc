proc multislice(axis, first, dx, nmapx, nmapy)
# Produces multiple raster maps of slices through a cube on a single page
# axis = axis along which slice are to be taken (1, 2, or 3)
# first = cell number at which to start the slicing
# dx = width of slice (>= 1)
# nmapx, nmapy = number of maps on page in x and y direction; total
#	number of maps produced = nmapx*nmapy
# Must set up LEVS and CLUT arrays before calling; also set values
#	for MRATIO, MXMIN, MXMAX, MYMIN, and MYMAX beforehand as well.
scalar mapx, mapy, i, j, nmap, tmpxmx, tmpxmn, tmpymx, tmpymn
#
nmap = first; mapx = ifix(1024/nint(nmapx)); mapy=ifix(800/nint(nmapy))
tmpxmx=mxmax; tmpxmn=mxmin; tmpymx=mymax; tmpymn=mymin
page
for i = 1 to nmapx
   for j = 1 to nmapy
	setpage(1+(i-1)*mapx,i*mapx,1+(j-1)*mapy,j*mapy)
	slice(axis, nmap, nmap+dx-1)
	if nint(axis)=1 then; rotate(true, true, false); end 
	mxmax=tmpxmx; mxmin=tmpxmn; mymax=tmpymx; mymin=tmpymn
	raster label
	nmap = nmap + dx
end; end
rstpage
return
finish


