# @(#)tuc.nrao.edu.login	5.2 11/15/94
#
# tuc.nrao.edu specific UniPOPS .login file
#
#
setenv OPENWINHOME /usr/openwin
setenv popsprinter postscript
set pathtmp = ( $path )
#	path will depend on SunOS vs Solaris
if (`/usr/bin/uname -r | awk -F. '{ print $1 }'` >= 5) then
#	The OS is Solaris
#	This will likely need to be changed in the future
   set addToPath = "~/bin /usr/local /usr/local/bin /home/cactus/unix /usr/bin" 
   set addToPath = "$addToPath /usr/ccs/bin $OPENWINHOME/bin /opt/SUNWspro/bin"
   set addToPath = "$addToPath /usr/ucb"
else
#	This OS is SunOS, it should be fine.
   set addToPath = "~/bin /usr/local /usr/local/bin /home/cactus/unix /bin"
   set addToPath = "$addToPath /usr/ucb /usr/bin /usr/etc /home/bohemia/lang"
endif
#
#		prepend . if not there
#		Note that this may mistake `.' for elements beginning or ending
#		with a `.'.   This 3 step check should find it at the beginning,
#		middle or end.	
echo $pathtmp | fgrep -s ". " ; set stat1 = $status
echo $pathtmp | fgrep -s " . "; set stat2 = $status
echo $pathtmp | fgrep -s " ." ; set stat3 = $status

if ( $stat1 && $stat2 && $stat3 ) then
   set pathtmp = ( . $pathtmp )
endif
#		append $addToPath if not there
foreach thing ( $addToPath )
   if ( -d $thing ) then
#		unlike above, this is a check only for this item being in
#		the middle.  This may lead to duplication if the item is
#		first (but that is likely to be `.') or currently last
#		(but except for the first item in $addToPath added to
#		$pathtmp, this will be the most recent item in $addToPath
#		added to $pathtmp and hence assuming there is no duplication
#		in $addToPath, this isn't a problem).
      echo $pathtmp | fgrep -s " $thing "
      if ( $status ) then
         set pathtmp = ( $pathtmp $thing )
      endif
   endif
end
set path = ( $pathtmp )
#

