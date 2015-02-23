#  @(#)kp.nrao.edu.bash_profile	5.2 11/15/94
#
#  kp.nrao.edu specific UniPOPS .bash_profile
#
#
export OPENWINHOME=/usr/openwin
export popsprinter=qms
pathtmp=$PATH
#	PATH will depend on SunOS vs Solaris
if [ `/usr/bin/uname -r | awk -F. '{ print $1 }'` -ge 5 ]; then
#	The OS is Solaris
#	This will likely need to be changed in the future
   addToPath=~/bin
   addToPath="$addToPath /usr/local /usr/local/bin /home/cactus/unix /usr/bin" 
   addToPath="$addToPath /usr/ccs/bin $OPENWINHOME/bin /opt/SUNWspro/bin"
   addToPath="$addToPath /usr/ucb"
else
#	This OS is SunOS, it should be fine.
   addToPath=~/bin
   addToPath="$addToPath /usr/local /usr/local/bin /home/cactus/unix /bin"
   addToPath="$addToPath /usr/ucb /usr/bin /usr/etc /home/bohemia/lang"
fi
#
#		prepend . if not there
#		Note that this may mistake `.' for elements beginning or ending
#		with a `.'.   This 3 step check should find it at the beginning,
#		middle or end.	
if ( echo $pathtmp | fgrep -v -s .: && \
     echo $pathtmp | fgrep -v -s :.: && \
     echo $pathtmp | fgrep -v -s :. ); then
   pathtmp=.:$pathtmp
fi
#
for dir in $addToPath
   do if [ -d $dir ] 
      then
#		unlike above, this is a check only for this item being in
#		the middle.  This may lead to duplication if the item is
#		first (but that is likely to be `.') or currently last
#		(but except for the first item in $addToPath added to
#		$pathtmp, this will be the most recent item in $addToPath
#		added to $pathtmp and hence assuming there is no duplication
#		in $addToPath, this isn't a problem).
        if ( echo $pathtmp | fgrep -v -s :${dir}: ); then
           pathtmp=$pathtmp:$dir
        fi
      fi
   done 
#
export PATH=$pathtmp
#

