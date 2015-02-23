#  @(#)unipops.xinitrc	5.3 03/07/96
#
if [ -z "$WINDOW_MGR" ]; then 
   echo "No window manager set"
   exit 1
fi

if [ -z "$OPENWINHOME" ]; then
   OPENWINHOME=/usr/openwin	        # set default OPENWINHOME if not set
fi

if [ -f $HOME/.Xdefaults ]; then
    xrdb $HOME/.Xdefaults &		# Load Users X11 resource database
else
    xrdb $OPENWINHOME/lib/Xdefaults &	# Load Default X11 resource database
fi

xhost + > /dev/null 2>&1 &

$OPENWINHOME/lib/openwin-sys  #OpenWindows system initialization

if [ -f $OPENWINHOME/bin/locale_env ]; then
   eval `locale_env -env` 	#new under Solaris
fi

eval `svenv -env`	#enable the display of old SunView applications
 
if [ $WINDOW_MGR = "olwm" ]; then
   sleep 15 & pid=$!		# gets a pid for use below, see "man olwm"
   olwm -syncpid $pid & wmpid=$!  # remember the window manager pid
   wait $pid			# this pauses until olwm has inited
else
#			alternative window managers
   xsetroot -def		# clear root window
#			set the appropriate wm command
   if [ $WINDOW_MGR = "twm" -o $WINDOW_MGR = "tvtwm" ]; then
      if [ -f $HOME/.${WINDOW_MGR}rc ]; then
         WMCMD="$WINDOW_MGR"
      elif [ -f ${popsdir}sys.unipops/unipops.${WINDOW_MGR}rc ]; then
         WMCMD="$WINDOW_MGR -f ${popsdir}sys.unipops/unipops.${WINDOW_MGR}rc"
      else
         WMCMD="$WINDOW_MGR" 
      fi
   else
      WMCMD="$WINDOW_MGR"
   fi
#			and start it, remembering the wmpid
   $WMCMD & wmpid=$!
fi
 
if [ "$WINDOW_MGR" = "olwm" ]; then
   if [ $HOME/.openwin-init ]; then
      $HOME/.openwin-init
   else
      $OPENWINHOME/lib/openwin-init
   fi
else
   xterm -C -sb -sl 900 -g 72x12+0+0 -font 9x15bold -title Console -n Console &
   xterm -sb -sl 900 -g 72x36+0-100 -font 9x15bold &
   xclock -update 1 -geometry 80x80-250-0 &
   xterm -sb -sl 900 -g 72x24-0-200 -font 9x15bold -iconic &
   xcalc -g 200x300+400-0 -iconic &
fi

wait $wmpid		# wait for wm (key client) to exit

