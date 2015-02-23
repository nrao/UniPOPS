#  @(#)gb.nrao.bash_profile	5.5 05/03/96
#
#  Green bank specific  UniPops .bash_profile
# 
export OPENWINHOME=/usr/openwin
export OSTYPE=`/bin/uname`
export OSVERS=$OSTYPE`/bin/uname -r | /usr/bin/cut -f1 -d.`
export HSTNME=`/bin/uname -n`
#
pathtmp=$PATH
#
case $OSVERS in
	SunOS4	)
   		export LOCALDIR=/usr/local
   		export LANGHOME=/usr/lang
   		export SWINHOME=$LOCALDIR/SoftWindows
   		export OPENWINHOME=/usr/openwin
   		export WPHOME=$LOCALDIR/wp
   		export WPTERM51=wpxterm
   		export LD_LIBRARY_PATH=$LOCALDIR/lib:$OPENWINHOME/lib
   		comp_loc="$LANGHOME /usr/etc $SWINHOME/bin $WPHOME/wpbin $WPHOME/shbin" 
   		comp_man="$LANGHOME/man $SWINHOME/man";;
	SunOS5	)
   		export LOCALDIR=/opt/local
   		export LANGHOME=/opt/SUNWspro
   		export OPENWINHOME=/usr/openwin
   		export SWINHOME=$LOCALDIR/SoftPC/SoftWindows
   		export XKEYSYMDB=$OPENWINHOME/lib/XKeysymDB
   		export XFILESEARCHPATH=$OPENWINHOME/lib/locale/%L/%T/%N%S:$OPENWINHOME/lib/%L/%T/%N%S:$OPENWINHOME/lib/%l/%T/%N%S:$OPENWINHOME/lib/%T/%N%S:$LOCALDIR/lib/X11/%L/%T/%N%S:$LOCALDIR/lib/X11/%l/%T/%N%S:$LOCALDIR/lib/X11/%T/%N%S
   		export WPTERM51=xcolor
   		export WPTMP=/tmp/wptmp
   		export SHTMP=/tmp/shtmp
   		export HELPPATH=$OPENWINHOME/lib/help:$OPENWINHOME/lib/locale
   		export LD_LIBRARY_PATH=$LANGHOME/lib:$OPENWINHOME/lib:$LOCALDIR/lib:/usr/ucblib:$LOCALDIR/X11R5/lib
   		comp_loc="$LANGHOME/bin /usr/ccs/bin $SWINHOME/bin $LOCALDIR/wp60/wpbin $LOCALDIR/wp60/shbin $LOCALDIR/gnu/bin $LOCALDIR/X11R5/bin"
   		comp_man="$LANGHOME/man $SWINHOME/man $LOCALDIR/gnu/man $LOCALDIR/X11R5/man";;
	*	)
   		echo "OS type unknown" ;;
esac 
#
for dir in $comp_loc         
    do  if   [ -d $dir ]; then 
	  if ( echo $pathtmp | fgrep -v -s  :${dir}: ); then
		pathtmp=$pathtmp:$dir
	  fi
        fi
    done
export PATH=$pathtmp
#
pathtmp=$MANPATH
#
for dir in $comp_man        
    do  if   [ -d $dir ]; then
	  if ( echo $pathtmp | fgrep -v -s  :${dir}: ); then
		pathtmp=$pathtmp:$dir
	  fi
        fi
    done
export MANPATH=$pathtmp
#
echo " "
if [ -r /usr/local/messages/motd ]
   then
      cat /usr/local/messages/motd
fi
#
export popsprinter=postscript
#
if [ -z "$DT" ]; then
   if [ $HSTNME = "yed" -a `tty` = "/dev/console" ] 
      then stty erase ^H
   fi
   if [ $HSTNME = "nath" -a `tty` = "/dev/console" ] 
      then stty erase ^H
   fi
fi
set +x
#
