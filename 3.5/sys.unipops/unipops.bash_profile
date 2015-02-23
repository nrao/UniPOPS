#  @(#)unipops.bash_profile	5.3 05/03/96
#
#  Standard UniPops .bash_profile
# 
# setup SWITCH variables we'll use later on
#
export OSTYPE=`/bin/uname`
export OSVERS=$OSTYPE`/bin/uname -r | /usr/bin/cut -f1 -d.`
export HSTNME=`/bin/uname -n`
#
# Give a starting path
#
export PATH=/usr/ucb:/usr/bin
#
if [ -z "$DT" ]; then
   case $OSVERS in
	SunOS4	)
		case `tty` in 
			/dev/console    )	;;
        		/dev/tty[a-e]*  )       TERM=dialup;; 
        		/dev/tty[p-r]*  )       ;; 
			/dev/pts*       )	;;
        		               *)       TERM=dialup;;
		esac
   		;;
	SunOS5	)
		case `tty` in 
			/dev/console    )	;;
        		/dev/tty[a-e]*  )       TERM=dialup;; 
        		/dev/tty[p-r]*  )       ;; 
			/dev/pts*       )	;;
        		               *)       TERM=dialup;;
		esac
   		;;
	*	)
   		echo "OS type unknown";;
   esac
   eval `/usr/ucb/tset  -s -Q -m "dialup:?dumb" $TERM`
   stty erase "^?"
fi 
#
case $OSVERS in
	SunOS4	)
   		export LANGHOME=/usr/lang
   		export LOCALDIR=/usr/local
   		export OPENWINHOME=/usr/openwin;;
	SunOS5	)
   		export LOCALDIR=/opt/local
   		export LANGHOME=/opt/SUNWspro
   		export OPENWINHOME=/usr/openwin;;
esac 
#
if [ -r ~/.bashrc ]; then 
    source ~/.bashrc
fi
# 
echo " "
echo "Current Terminal type:" $TERM "    Hostname:" `uname -n`
echo " "
#
pathtmp=$PATH
for dir in ~/bin $LOCALDIR/bin $OPENWINHOME/bin $OPENWINHOME/xview $OPENWINHOME/bin/xview:/usr/sbin       
    do  if   [ -d $dir ]; then 
	  if (echo $pathtmp | fgrep -v -s :${dir}: ); then
		pathtmp=$dir:$pathtmp
	  fi
        fi
    done
#
export PATH=$pathtmp
#
pathtmp=$MANPATH
for dir in /usr/man $LOCALDIR/man $OPENWINHOME/man $OPENWINHOME/share/man         
    do  if   [ -d $dir ]; then
	  if (echo $pathtmp | fgrep -v -s  :${dir}: ); then
		pathtmp=$dir:$pathtmp
	  fi
        fi
    done
unset dir
export MANPATH=$pathtmp
#
if [ -z "$popsversion" ]; then
   export popsversion=3.5
fi
isrc2=~unipops/$popsversion/sys.unipops/`domainname`.bash_profile
if [ -r $isrc2 ]; then
     source $isrc2
fi
unset isrc2
# This is for site specific stuff
#
if [ -z "$OBSINIT" ]; then
   export OBSINIT="."
fi
#
if [ -z "$popsprog" ]; then
     if [ -r ~/$OBSINIT/.unipops_bash ]; then
        source ~/$OBSINIT/.unipops_bash
     fi
     if [ ! -r ~/$OBSINIT/.unipops_bash -o -z "$popswindow" ]; then
	if [ -r ~/$OBSINIT/.unipops_bash ]; then
	   echo "Your .unipops_bash file is out of date -- creating a new one"
	   echo "The existing file has been renamed .unipops_bash.old"
	   mv -f ~/$OBSINIT/.unipops_bash ~/$OBSINIT/.unipops_bash.old
	fi
	(builtin cd ~/$OBSINIT ; ~unipops/$popsversion/sys.unipops/prepare.unipops)
        if [ -r ~/$OBSINIT/.unipops_bash ]; then
      	   source ~/$OBSINIT/.unipops_bash
        fi
    fi
fi
#
# Make sure path is set right for the chosen windowing environment
#
if [ -z "$DT" ]; then
   if [ $TERM = "sun" -a `tty` = "/dev/console" -a -n "$popswindow" ]; then
      echo " "
      echo "Hit Ctrl-C if you do not want windows to start up."
      echo "You have 5 seconds."
      if [ $popswindow = openlook ]; then
          sleep 5
	  export WINDOW_MGR=olwm
	  if [ ! -r ~/.openwin-init ]; then
	       cp ${popsdir}sys.unipops/.openwin-init .
	  fi
	  openwin
      elif [ $popswindow = sunview ]; then
	  echo Sunview is no longer supported....
	  echo See the local UniPops guru for help
      elif [ $popswindow = mwm ]; then
           sleep 5
	   export WINDOW_MGR=$popswindow
	   if [ ! -r ~/.xinitrc -a -z "$XINITRC" ]; then
		export XINITRC=${popsdir}sys.unipops/unipops.xinitrc
	   fi
	   if [ ! -r ~/.mwmrc ]; then
	        cp ${popsdir}sys.unipops/.mwmrc .
	   fi
	   xinit ; clear
      else
	sleep 5
	export WINDOW_MGR=$popswindow
	if [ ! -r ~/.xinitrc -a -z "$XINITRC" ]; then
	     export XINITRC=${popsdir}sys.unipops/unipops.xinitrc
	fi
	xinit ; clear
      fi
      echo " "; echo "Please log out by typing: bye"; echo " "
   fi
fi
# Start up windows; don't add any lines to be executed after this point
# since they will not be executed until after windows starts OR if this is 
# a non-console or other non-windows login session
#
#
