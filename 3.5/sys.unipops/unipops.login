#  @(#)unipops.login	5.3 05/03/96 
#
#  Standard UniPOPS .login file
#
#
# setup SWITCH variables we'll use later on
#
setenv OSTYPE  `/bin/uname`
setenv OSVERS $OSTYPE`/bin/uname -r | /usr/bin/cut -f1 -d.`
setenv HSTNME `/bin/uname -n`
#
# Give a starting path
#
set path = (/usr/ucb /usr/bin)
#
if (! $?DT) then
  switch ($OSVERS) 
   case SunOS4:
     switch ( `tty` )
       case /dev/console:
	breaksw
       case /dev/ttyy0*:
	setenv TERM v102; breaksw
       case /dev/tty[a-e]:
	setenv TERM dialup; breaksw
       case /dev/tty[p-r]*:
	breaksw
       default:
	setenv TERM dialup; breaksw
       endsw
      breaksw
   case SunOS5:
     switch ( `tty` )
       case /dev/console:
	breaksw
       case /dev/tty[a-e]:
	setenv TERM dialup; breaksw
       case /dev/tty[p-r]*:
	breaksw
       case /dev/pts*:
	breaksw
       default:
	setenv TERM dialup; breaksw
       endsw
      breaksw
   default:
     echo "OS type unknown"  
     breaksw
   endsw
#
   set noglob
   eval `/usr/ucb/tset -s -Q -m "dialup:?dumb" $TERM`
   unset noglob
   stty erase "^?"
endif
#
switch ($OSVERS) 
 case SunOS4:
   setenv LANGHOME  /usr/lang
   setenv LOCALDIR /usr/local
   setenv OPENWINHOME /usr/openwin
  breaksw
 case SunOS5:
   setenv LOCALDIR /opt/local
   setenv LANGHOME /opt/SUNWspro
   setenv OPENWINHOME /usr/openwin
  breaksw
 default:
   echo "OS type unknown"  
   breaksw
 endsw
#
echo " "
echo "Current Terminal type:" $TERM "    Hostname:" `uname -n`
echo " "
#
set pathtmp = ( $path )
foreach thing ( ~/bin $LOCALDIR/bin $OPENWINHOME/bin $OPENWINHOME/xview \
		       $OPENWINHOME/bin/xview /usr/sbin )
   if ( -d $thing ) then
      echo $pathtmp | fgrep -s " $thing "
      if ( $status ) then
         set pathtmp = ( $thing $pathtmp )
      endif
   endif
end
set path=( $pathtmp )
#
if ( ! $?MANPATH) setenv MANPATH
set pathtmp = $MANPATH
foreach thing ( /usr/man $LOCALDIR/man $OPENWINHOME/man $OPENWINHOME/share/man )
   if ( -d $thing ) then
      echo $pathtmp | fgrep -s :${thing}:
      if ( $status ) then
         set pathtmp = ${thing}:$pathtmp
      endif
   endif
end
setenv MANPATH $pathtmp
#
if ( ! $?popsversion ) setenv popsversion 3.5
if (-e ~unipops/$popsversion/sys.unipops/`domainname`.login) then
   source ~unipops/$popsversion/sys.unipops/`domainname`.login
endif
# This is for site specific stuff
#
if ( ! $?OBSINIT ) setenv OBSINIT "."
if (! $?popsprog) then
   if (-e ~/$OBSINIT/.unipops) source ~/$OBSINIT/.unipops
   if (! -e ~/$OBSINIT/.unipops || ! $?popswindow) then
	if (-e ~/$OBSINIT/.unipops) then
		echo "Your .unipops file is out of date -- creating a new one"
		echo "The existing file has been renamed .unipops.old"
		mv -f ~/$OBSINIT/.unipops ~/$OBSINIT/.unipops.old
	endif
	(chdir ~/$OBSINIT ; ~unipops/$popsversion/sys.unipops/prepare.unipops)
        if (-e ~/$OBSINIT/.unipops) source ~/$OBSINIT/.unipops
   endif
endif
#
if (! $?DT) then
   if ($TERM == "sun" && `tty` == "/dev/console" && $?popswindow) then
      echo " "
      echo "Hit Ctrl-C if you do not want windows to start up."
      echo "You have 5 seconds."
      if ( $popswindow == openlook) then
	sleep 5
	setenv WINDOW_MGR olwm
	if (! -e ~/.openwin-init) cp ${popsdir}sys.unipops/.openwin-init .
	openwin
      else if ( $popswindow == sunview) then
	echo Sunview is no longer supported....
	echo See the local UniPops guru for help
      else if ( $popswindow == mwm) then
	sleep 5
	setenv WINDOW_MGR $popswindow
	if (! -e ~/.xinitrc && ! $?XINITRC) setenv XINITRC ${popsdir}sys.unipops/unipops.xinitrc
	if (! -e ~/.mwmrc) cp ${popsdir}sys.unipops/.mwmrc .
	xinit ; clear
      else
	sleep 5
	setenv WINDOW_MGR $popswindow
	if (! -e ~/.xinitrc && ! $?XINITRC) setenv XINITRC ${popsdir}sys.unipops/unipops.xinitrc
	xinit ; clear
      endif
      echo " "; echo "Please log out by typing: bye"; echo " "
   endif
endif
# Start up windows; don't add any lines to be executed after this point
# since they will not be executed until after windows starts OR if this is 
# a non-console or other non-windows login session
#
