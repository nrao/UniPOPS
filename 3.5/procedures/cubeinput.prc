procedure cubeinput(dx, dy)
#
# This writes appropriate inputs to cube.exe to translate the first
# MAXNSAVE scans in the current save file to a FITS cube.  
# These inputs are written to a file named "cube.input" in the current 
# directory (which will be overwritten each time this procedure is used).  
# After exectuting this procedure, you can type the following from 
# withing UniPOPS: "system cube.exe < cube.input" or simply :
# "cube.exe < cube.input" at a unix prompt.
#
# dx and dy are the increments in the x and y directions between
# pixels on the sky (in seconds of arc).  The GLS projection is used (COSV
# correction) unless DOCAR=TRUE (in which case a simple cartesian map is made).
# Only abs(dx) and abs(dy) are used.  Velocity always increases with increasing
# pixel value in the cube.  If X-coord is RA it always increases 
# with decreasing pixel value in the cube.  The Y-coord always increases with
# increasing pixel value in the cube.
#
# requires the dms procedure
#
scalar xmin, ymin, xmax, ymax, zmin, zmax, tmin, tmax, dz
scalar unit, nx, ny, nz, xlunit, xuunit, dxtmp, dytmp
scalar ctype, hourdeg, i, ideg, imin, secs, sgn, ralike
scalar xcent, yatxmin, yatxmax, xtmp, ytmp, origfact
pointer pideg 57.2957795
string*60 filename
string*8 coordcode
string*4 coordtype, csgn
global scalar maxnsave, docar
#
if (maxnsave < 1) then
   print "cubeinput: no scans indicated.  MAXNSAVE = ", maxnsave
   return
end
unit = fopen("cube.input",0)
frewind(unit)
#
fprint(unit," ") 'N'
#		No cube.exe debugging messages
filename = fname(sscans)
fprint(-1,"1x,a,a") "Input sdd file (save file) : ", filename
fprint(unit,"1x,a") filename
#		Use the current save file name as the input data file
fprint(-1,"1x,a,1x,$") "Enter name of cube file : "
fread(-1,"a") filename
fprint(unit,"1x,a") filename
#		The user chooses the output cube name
fprint(unit,"1x,i1") 2
#		Use OBJECT of first scan as OBJECT of cube
fprint(unit,"1x,i1") 2
#		Use UTDATE of first scan as DATE of cube
fprint(unit,"1x,i1") 2
#		Use TELESCOP as ORIGIN in cube header
fprint(unit,"1x,i1") 1
#		Assume units are K.
#		Find out the nitty gritty stuff about the scans
nsave=1; recall
xlunit = status(1)
xuunit = status(2)
saxis(velhead, xuunit)
#		ensure that chantox returns a velocity
xmin=h0(xsource); ymin=h0(ysource); zmin=chantox(1); yatxmin=ymin
xmax=xmin; ymax=ymin; zmax=chantox(h0(noint)); yatxmax=ymax
dz=h0(deltax)
if (dz > 0) then
  zmin = chantox(1); zmax = chantox(h0(noint))
else
  zmin = chantox(h0(noint)); zmax = chantox(1)
end
origfact = fact
peak; tmax=height(1); fact=-1;scale;peak;tmin=height(1)
if (maxnsave < 2) then; return; end
for nsave = 2 to maxnsave
   recall
   xtmp=h0(xsource);ytmp=h0(ysource)
   if (xtmp < xmin) then;xmin=xtmp;yatxmin=ytmp;end
   if (xtmp > xmax) then;xmax=xtmp;yatxmax=ytmp;end
   ymin = min(ymin, ytmp)
   ymax = max(ymax, ytmp)
   zmin = min(zmin, chantox(1))
   if (dz > 0) then
      zmax = max(zmax, chantox(h0(noint))); zmin = min(zmin, chantox(1))
   else
      zmax = max(zmax, chantox(1)); zmin = min(zmin, chantox(h0(noint)))
   end
   peak; tmax=max(tmax, height(1))
   fact=-1; scale; peak; tmin=max(tmin, height(1))
end
#	return fact to its original state
fact = origfact
#
saxis(xlunit,xuunit)
#			return the X axis to its original state
tmin = -tmin
fprint(unit," ") tmin
fprint(unit," ") tmax
#			range of values expected
ctype = 1
coordcode = h0(coordcd)
if (compare(coordcode,"EPOCRADC")) then; ctype = 2; end
if (compare(coordcode,"GALACTIC")) then; ctype = 3; end
if (compare(coordcode,"AZEL")) then; ctype = 4; end
if (compare(coordcode,"INDICATD")) then; ctype = 5; end
i = max((lnblnk(coordcode) - 3), 1)
coordtype = substr(coordcode, i, i+3)
hourdeg = compare(coordtype,"RADC") | compare(coordtype,"HADC")
ralike = compare(coordtype,"RADC")
fprint(unit,"1x,i1") ctype
#			Use observed coordinate system
if (docar) then
   fprint(unit," ") "N"
else 
   fprint(unit," ") "Y"
end
dxtmp = abs(dx)
dytmp = abs(dy)
#			correct coords to GLS if docar is false
if (~ docar) then
   yatxmin = yatxmin/pideg; yatxmax = yatxmax/pideg
   xcent = xmin * cos(yatxmin) + xmax * cos(yatxmax)
   xcent = xcent / (cos(yatxmin) + cos(yatxmax))
end
if (ralike) then; dxtmp = -dxtmp;end
if (hourdeg) then
#			different output formats depending on coordtype
   if (dxtmp > 0) then
      dms(xmin/15,sgn,ideg,imin,secs)
   else
      dms(xmax/15,sgn,ideg,imin,secs)
   end
   fprint(unit,"1x,i2,x,i2,x,f5.2") ideg, imin, secs
#		Value at 1st (reference) X pixel
   dms(ymin,sgn,ideg,imin,secs)
   csgn = "+"; if (sgn < 0) then; csgn = "-"; end
   fprint(unit,"1x,a1,i2,x,i2,x,f5.2") csgn, ideg, imin, secs
#		Value at 1st (reference) Y pixel
   fprint(unit," ") zmin
#		Value at 1st (reference) Z pixel
   dxtmp = dxtmp / 3600.0
   dytmp = dytmp / 3600.0
   dms(dxtmp/15,sgn,ideg,imin,secs)
   csgn = "+"; if (sgn < 0) then; csgn = "-"; end
   fprint(unit,"1x,a1,i2,x,i2,x,f5.2") csgn, ideg, imin, secs
#		X axis increment
   dms(dytmp,sgn,ideg,imin,secs)
   csgn = "+"
   fprint(unit,"1x,a1,i2,x,i2,x,f5.2") csgn, ideg, imin, secs
#		Y axis increment
   fprint(unit," ") abs(dz)
#		Z axis increment
   dms(dxtmp/30, sgn,ideg,imin,secs)
   fprint(unit,"1x,i2,x,i2,x,f5.2") ideg, imin, secs
#		Allowable error in X axis (dxtmp/2)
   dms(dytmp/2, sgn,ideg,imin,secs)
   fprint(unit,"1x,i2,x,i2,x,f5.2") ideg, imin, secs
#		Allowable error in Y axis (dy/2)
   fprint(unit," ") abs(dz/2)
#		Allowable error in Z axis (dz/2)
else
#		if not hourdeg, can not be ralike, all axis positive incr.
   fprint(unit,"1x,f10.5") xmin
#		Value at 1st (reference) X pixel
   fprint(unit,"1x,f9.5") ymin
#		Value at 1st (reference) Y pixel
   fprint(unit," ") zmin
#		Value at 1st (reference) Z pixel
   dxtmp = dxtmp / 3600.0
   dytmp = dytmp / 3600.0
   fprint(unit,"1x,f10.5") dxtmp
#		X axis increment
   fprint(unit,"1x,f9.5") dytmp
#		Y axis increment
   fprint(unit," ") dz
#		Z axis increment
   fprint(unit,"1x,f9.5") dxtmp/2
#		Allowable error in X axis (dxtmp/2)
   fprint(unit,"1x,f8.5") dytmp/2
#		Allowable error in Y axis (dy/2)
   fprint(unit," ") abs(dz/2)
#		Allowable error in Z axis (dz/2)
end
nx = nint((xmax-xmin)/abs(dxtmp) + 1)
ny = nint((ymax-ymin)/dytmp + 1)
nz = nint((zmax-zmin)/abs(dz) + 1)
fprint(unit,"1x,i") nx
fprint(unit,"1x,i") ny
fprint(unit,"1x,i") nz
#		Number of pixels on each axis
if (~ docar) then
   if (hourdeg) then
      dms(xcent/15,sgn,ideg,imin,secs)
      fprint(unit,"1x,i2,x,i2,x,f5.2") ideg, imin, secs
   else
      fprint(unit,"1x,f10.5") xcent
   end
end
#		The reference X position for the GLS projection
fprint(unit," ") " "
fprint(unit," ") " "
fprint(unit," ") " "
#			No comments
fprint(unit,"1x,i1") 0
fprint(unit,"1x,i1") 0
fprint(unit,"1x,i1") 0
fprint(unit,"1x,i1") 0
#			Full scan and feed number range
#
fclose(unit)
#
print " "
print "Inputs written to cube.input."
print "To use, type the following at the UniPOPS prompt :"
print "system cube.exe < cube.input"
#
return
finish
#		These lines are NOT part of the procedure, instead they
#		make sure docar is not 0, default to FALSE, when cubeinput
#		is batched in.
if (docar = 0) then; docar = false;end
