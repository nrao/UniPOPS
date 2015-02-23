#  @(#)cv.login	5.6 05/25/95
#
#  C'Ville specific UniPOPS .login file
#
#
setenv OPENWINHOME /usr/openwin
setenv popsprinter ps3
set pathtmp = ( $path )
#	PATH depends on Solaris vs SunOS
#	This is the check in $popsdir/isSolaris, but we can't rely on finding
#	that from here
if (`/usr/bin/uname -r | awk -F. '{ print $1 }'` < 5 ) then 
#	set SunOS specific PATH additions, X stuff happens separately
   set addToPath = "~/bin /usr/bin /local/bin /usr/local/bin /usr/lang"
   set addToPath = "$addToPath /usr/ucb /bin /usr/5bin /usr/gnu/bin" 
   set addToPath = "$addToPath /usr/gnu/bin /etc /usr/etc"
   set addXToPath = "/local/bin/X11 /usr/bin/X11 /usr/local/bin/X11"
   set addXToPath = "$addXToPath $OPENWINHOME/bin $OPENWINHOME/bin/xview"
else 
#	set Solaris specific PATH additions
   set addToPath = "~/bin /opt/local/bin /opt/local/X11R5/bin $OPENWINHOME/bin"
   set addToPath = "$addToPath /usr/ccs/bin /opt/SUNWspro/bin /usr/bin /usr/ucb"
   set addToPath = "$addToPath /opt/SUNWmotif/bin"
#
#       Solaris also needs LD_LIBRARY_PATH set to this, SunOS doesn't need it
   setenv LD_LIBRARY_PATH /opt/local/X11R5/lib:$OPENWINHOME/lib:/usr/ucblib
   setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/opt/SUNWmotif/bin
endif
#
foreach thing ( $addToPath )
   if ( -d $thing ) then
     echo $pathtmp | fgrep -s " $thing "
     if ( $status ) then
        set pathtmp = ( $pathtmp $thing )
     endif
   endif
end
#	prepend the X stuff
if ( $?addXToPath ) then
   set newstuff = ""
   foreach thing ( $addXToPath )
      if ( -d $thing ) then
         echo $pathtmp | fgrep -s " $thing "
         if ( $status ) then
            set newstuff = ( $newstuff $thing )
         endif
      endif
   end
   set pathtmp = ( $newstuff $pathtmp )
endif
#	prepend . to path
set path = ( . $pathtmp )
#
