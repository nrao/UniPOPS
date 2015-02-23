procedure sortstk(x_tol, y_tol)
#
#  Sort scan numbers on stack into a disk file such that the scans are
#  in order of position.  Write the CoordCode, X-position, Y-position, and scan
#  number to the file.  The positions are rounded to the nearest multiple
#  of x_tol and y_tol.  This file could then be used to list the
#  scans that need to be averaged (multiple scans at the same position, to
#  within the tolerance), or to actually direct that averaging.
#
#  x_tol and y_tol are in arc seconds and can not be zero.
#
#  The output file name uses the value of the global string : F_SORTSTK
#  This string MUST be non-blank.  If F_SORTSTK exists, its contents
#  will be overwritten.
#
#  Scan numbers to be sorted should already be in the stack.  
#
#  Scans are fetched using "GET" and hence follow the same rules as 
#  the GET verb (positive scan numbers are fetched from the on-line 
#  data file, if it exists, and then from the off-line data file, 
#  if it was not found in the on-line data file, negative scan numbers are 
#  fetched from the off-line data file and then from the on-line data file).
#  
#  One scratch disk files is used: sortstk.temp.  It is removed when
#  the procedure finishes.
#
global string*60 f_sortstk
scalar i, unit, x, y, scanno, newx, newy, dx_tol, dy_tol
string*8 ccode, newccode
string*60 strout
#
if x_tol <= 0 | y_tol <= 0 then
   print "SORTSTK: x_tol <= 0 | y_tol <= 0"
   return
end
#
if lnblnk(f_sortstk) = 0 then
   print "SORTSTK: f_sortstk has no non-blank characters."
   return
end
if compare(f_sortstk,"sortstk.temp") then
   print "SORTSTK: f_sortstk = 'sortstk.temp'"
   print "Use a different name."
   return
end
#    open and close f_sortstk just to be sure we can, if we can't its better
#    to not waste time with the rest of this.  Rely on fopen to toss us out.
unit = fopen(f_sortstk, 0)
fclose(unit)
#
unit = fopen("sortstk.temp",0)
frewind(unit)
#    Create a file with columns:  Coord_code, X, Y, scan_no
if acount <= 1 then
   print "SORTSTK: acount <= 1"
   return
end
#
dx_tol = x_tol / 3600
dy_tol = y_tol / 3600
#
for i = 1 to acount
   get astack(i)
   x = dx_tol * nint(h0(xsource)/dx_tol)
   Y = dy_tol * nint(h0(ysource)/dy_tol)
#    Round to nearest multiple of dx_tol and dy_tol
   fprint(unit," ") h0(coordcd), x, y, h0(scan)
end
#    We can now sort the file. Use the UNIX sort utility since it does sorting
#    better than we ever could.
fclose(unit)
sprint(strout,'a,a,a') 'sort +0 -2 -o ',substr(f_sortstk,1,lnblnk(f_sortstk)),\
                       ' sortstk.temp'
z_system(strout)
z_system("/bin/rm sortstk.temp")
return
finish
