#  @(#)cv.bash_profile	5.4 11/17/94
#
#  C'Ville specific UniPOPS .bash_profile file
#
#
export OPENWINHOME=${OPENWINHOME:=/usr/openwin}
export popsprinter=ps3
pathtmp=$PATH
#	PATH depends on Solaris vs SunOS
#	This is the check in $popsdir/isSolaris, but we can't rely on finding
#	that from here
if [ `/usr/bin/uname -r | awk -F. '{ print $1 }'` -lt 5 ]; then 
#	set SunOS specific PATH additions, X stuff happens separately
   addToPath=~/bin
   addToPath="$addToPath /usr/bin /local/bin /usr/local/bin /usr/lang"
   addToPath="$addToPath /usr/ucb /bin /usr/5bin /usr/gnu/bin /etc /usr/etc"
   addXToPath="$addToPath /local/bin/X11 /usr/bin/X11 /usr/local/bin/X11"
   addXToPath="$addXToPath $OPENWINHOME/bin $OPENWINHOME/bin/xview"
else 
   addToPath=~/bin
   addToPath="$addToPath /opt/local/bin /opt/local/X11R5/bin $OPENWINHOME/bin"
   addToPath="$addToPath /usr/ccs/bin /opt/SUNWspro/bin /usr/bin /usr/ucb"
   addToPath="$addToPath /opt/SUNWmotif/bin"
#
#       Solaris also needs LD_LIBRARY_PATH set to this, SunOS doesn't need it
   export LD_LIBRARY_PATH=/opt/local/X11R5/lib:$OPENWINHOME/lib:/usr/ucblib
   LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/opt/SUNWmotif/lib
fi
#
for dir in $addToPath
   do if [ -d $dir ]; then 
         if (echo $pathtmp | fgrep -v -s :${dir}:); then
            pathtmp=$pathtmp:$dir
         fi
      fi
   done
#	prepend the X stuff
newstuff=""
for dir in $addXToPath
   do if [ -d $dir ]; then
         if (echo $pathtmp | fgrep -v -s :${dir}:); then
            newstuff=$newstuff:$dir
         fi
      fi
   done
pathtmp=$newstuff:$pathtmp
#		prepend . to path
export PATH=.:$pathtmp
#
