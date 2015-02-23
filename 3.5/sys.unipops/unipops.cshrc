#  @(#)unipops.cshrc	5.3 05/03/96
#
#  Standard UniPops .cshrc file
#
limit coredumpsize 0k
if ($USER == 0 || $?prompt == 0) exit
#
echo " "
echo `date` '  ' `tty` '  ' $cwd
#
umask 022
#
setenv HSTNME `/bin/uname -n`
set prompt="${HSTNME}% "
set time=20 
set history=100 
set nonomatch
set savehist=40
set ignoreeof
set noclobber
set notify
setenv CP -im
set filec
set hardpaths
#
alias bye logout
alias pwd 'echo $cwd'
alias cd 'set old=$cwd; chdir \!*; echo $cwd'
alias back 'set back=$old; set old=$cwd; chdir $back; echo $cwd; unset back'
alias hist 'history \!* | head -39 | more'
alias mv 'mv -i'
alias cp 'cp -i'
alias rm 'rm -i'
alias more 'more -d'
alias gs 'xv_get_sel $*'
#
if ($?popsversion == 0) then
   setenv popsversion 3.5
endif
if (-e ~unipops/$popsversion/sys.unipops/`domainname`.cshrc) then
   source ~unipops/$popsversion/sys.unipops/`domainname`.cshrc
endif
# This is for site specific stuff
