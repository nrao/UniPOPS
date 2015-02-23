procedure chkavrg(x_tol, y_tol)
#
#  Produce a list of scans (by scan number) taken at the same location.
#
#  x_tol and y_tol are pointing tolerances -- any two scans located within
#  these distances from one another are considered to be taken at the same
#  location.  
#
#  x_tol and y_tol are in arcseconds and cannot be zero.  
#
#  Uses sortstk, which expects that scan numbers are already in the stack.
#  See the comments in sortstk for additional details.
#
scalar i, unit, x, y, scanno, newx, newy
string*8 ccode, newccode
string*60 strout
#
f_sortstk = 'chkavrg.temp'
#
sortstk(x_tol, y_tol)
#	The file f_sortstk is now sorted, we can read it and check for
#       duplicate positions.
unit=fopen(f_sortstk,0)
x=1.e34; y=1.e34; newccode=" "
for i = 1 to acount
   fread(unit," ") ccode, newx, newy, scanno
   if newx = x & newy = y & compare(ccode, newccode) then
#       Identical positions in the same coordinate system; report it
      print scanno, newx, newy
   else
#       new position
      print "---------------"
      print scanno, newx, newy
   end
   newccode = ccode; x=newx; y=newy
end
fclose(unit)
sprint(strout,'a,a') '/bin/rm ',substr(f_sortstk,1,lnblnk(f_sortstk))
z_system(strout)
return
finish
