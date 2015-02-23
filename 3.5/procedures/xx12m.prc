proc xx
# Clears the screen and SHOWs the contents of Array (0)
# This version is for 12m use.
#
if (status(9) = 2) then
 if (compare(h3(telescop),"NRAO 12M") & compare(h4(telescop),"NRAO 12M")) then
#		assume a cboth is really wanted
      cboth
   else
#		punt and do a page;show
      page show
 end
else
   page show 
end
return
finish
