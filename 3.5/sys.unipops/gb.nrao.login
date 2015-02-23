#  @(#)gb.nrao.login	5.5 05/03/96
#
#  Green bank specific UniPOPS .login file
#
setenv OPENWINHOME /usr/openwin
setenv OSTYPE `/bin/uname`
setenv OSVERS $OSTYPE`/bin/uname -r | /usr/bin/cut -f1 -d.`
setenv HSTNME `/bin/uname -n`
#
set pathtmp = ( $path )
#
switch ($OSVERS) 
 case SunOS4:
   setenv LOCALDIR /usr/local
   setenv LANGHOME  /usr/lang
   setenv SWINHOME $LOCALDIR/SoftWindows
   setenv OPENWINHOME /usr/openwin
   setenv WPHOME $LOCALDIR/wp
   setenv WPTERM51 wpxterm
   setenv LD_LIBRARY_PATH $LOCALDIR/lib:$OPENWINHOME/lib
   set comp_loc = "$LANGHOME /usr/etc $SWINHOME/bin $WPHOME/wpbin $WPHOME/shbin" 
   set comp_man = "$LANGHOME/man $SWINHOME/man"
   breaksw
 case SunOS5:
   setenv LOCALDIR /opt/local
   setenv LANGHOME /opt/SUNWspro
   setenv OPENWINHOME /usr/openwin
   setenv SWINHOME $LOCALDIR/SoftPC/SoftWindows
   setenv XKEYSYMDB $OPENWINHOME/lib/XKeysymDB
   setenv XFILESEARCHPATH $OPENWINHOME/lib/locale/%L/%T/%N%S:$OPENWINHOME/lib/%L/%T/%N%S:$OPENWINHOME/lib/%l/%T/%N%S:$OPENWINHOME/lib/%T/%N%S:$LOCALDIR/lib/X11/%L/%T/%N%S:$LOCALDIR/lib/X11/%l/%T/%N%S:$LOCALDIR/lib/X11/%T/%N%S
   setenv WPTERM51 xcolor
   setenv WPTMP /tmp/wptmp
   setenv SHTMP /tmp/shtmp
   setenv HELPPATH $OPENWINHOME/lib/help:$OPENWINHOME/lib/locale
   setenv LD_LIBRARY_PATH $LANGHOME/lib:$OPENWINHOME/lib:$LOCALDIR/lib:/usr/ucblib:$LOCALDIR/X11R5/lib
   set comp_loc = "$LANGHOME/bin /usr/ccs/bin $SWINHOME/bin $LOCALDIR/wp60/wpbin $LOCALDIR/wp60/shbin $LOCALDIR/gnu/bin $LOCALDIR/X11R5/bin"
   set comp_man = "$LANGHOME/man $SWINHOME/man $LOCALDIR/gnu/man $LOCALDIR/X11R5/man"
   breaksw
 default:
   echo "OS type unknown"  
   breaksw
 endsw
#
foreach thing ($comp_loc .)
   if ( -d $thing ) then
      echo $pathtmp | fgrep -s " $thing "
      if ( $status ) then
         set pathtmp = ( $pathtmp $thing )
      endif
   endif
end
set path=( $pathtmp )
#
set pathtmp = $MANPATH
foreach thing ( $comp_man )
   if ( -d $thing ) then
      echo $pathtmp | fgrep -s :${thing}:
      if ( $status ) then
         set pathtmp = ${pathtmp}:$thing
      endif
   endif
end
setenv MANPATH $pathtmp
#
echo " "
if (-e /usr/local/messages/motd) then
    cat /usr/local/messages/motd
endif
#
setenv popsprinter postscript
#
if (! $?DT) then
   if ($HSTNME == "yed" && `tty` == "/dev/console") stty erase ^H
   if ($HSTNME == "nath" && `tty` == "/dev/console") stty erase ^H
endif
#
unset comp_loc
unset comp_man
unset thing
unset pathtmp
#
