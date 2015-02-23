procedure prcstk(x_tol, y_tol)
# x_tol and y_tol are pointing tolerances -- any two scans located within
# these distances from one another are considered to be taken at the same
# location.  Both are in arcseconds and cannot be zero.  Scan numbers to
# be searched should be in the stack already.
#
# This uses sortstk.  Check the comments on sortstk for additional
# information and restrictions.
#
#   Uses PROCESS to process the individual scans.
#   Accumes scans at the same position.
#   Results are stored to the current save file starting with current value
#   of nsave.
#
#  Modified 4/9/94 by David Burrows.  Changes:
#	1) Allows user to reset maxnsave to overwrite save file
#		or to create new save file.
#	2) Prints out value of nsave for each average.
#
global scalar maxnsave
scalar i, unit, x, y, scanno, newx, newy, ichoice
string*8 ccode, newccode
string*60 savefile
string*60 strout
#
#  Open a new SAVE file
#
ichoice = 0
print " "
print "Current SAVE file contains ", maxnsave, "scans."
print "Select an option:"
print "     0: append new data to end of existing SAVE file"
print "     1: overwrite existing SAVE file with new data"
print "     2: create new SAVE file"
print "     3: exit routine"
print " "
read ichoice
if (ichoice = 0) then
	print "Appending new data to end of existing SAVE file ..."
else
    if (ichoice = 1) then
	maxnsave = 0
	print "Overwriting existing SAVE file ..."
    else
	if (ichoice = 2) then
	    print " "
	    print "Enter the new save file name: "
	    read savefile
	    z_chngfil("CREATE", 3, savefile)
	    maxnsave = 0
	    print "New save file is: ", savefile
	else
	    print "Exiting prcstk ..."
	    return
	end
    end
end
#
f_sortstk = "prcstk.temp"
#
print " "
print "Sorting the scans from the stack ..."
print " "
sortstk(x_tol, y_tol)
#		f_sortstk contains a list of scan numbers, sorted by
# 		position, use this to direct the processing and averaging
unit=fopen(f_sortstk,0)
x=1.e34; y=1.e34;  newccode=" "; nsave=maxnsave
for i = 1 to acount
   erroff
   fread(unit," ") ccode, newx, newy, scanno
   if errcode = 357 then
#		Watch out reading past EOF, this shouldn't happen
      print "End of file in ", f_sortstk
      print "Before expected end.  Only ",i,"records found."
      print "Doing AVE and SAVE to clean up."
      print "Double check contents of save file."
      fclose(unit)
      sprint(strout,'a,a') '/bin/rm ',substr(f_sortstk,lnblnk(f_sortstk))
      z_system(strout)
      maxnsave=nsave; ave; nsave=nsave+1; save; maxnsave = nsave
      return
   end
   erron
   if newx = x & newy = y & compare(ccode,newccode) then
#       Indentical positions in the same coordinate system; report it
#       process it, and average it in with the current average
      print scanno, newx, newy
      get scanno; process; accum
   else
#	New positions; average data from previous position and store away
#       starting a new averaging.
      if i ~= 1 then; ave; nsave=nsave+1; save; end
      print "------------------------------------- nsave = ", nsave+1
      print scanno, newx, newy
      get scanno; process; accum
   end
   newccode = ccode; x = newx; y = newy
end
#	clean up, finish last ave in progress
fclose(unit)
sprint(strout,'a,a') '/bin/rm ',substr(f_sortstk,1,lnblnk(f_sortstk))
z_system(strout)
maxnsave=nsave; ave; nsave=nsave+1; save; maxnsave = nsave
return
finish
