#  @(#)unipops.bashrc	5.3 05/03/96
#
#  Standard UniPops .bashrc file
# 
ulimit -c 0
if [ /usr/ucb/whoami = "root" ]; then
      exit
fi
#
echo " "
echo `date` ' ' `tty` ' ' $PWD
#
umask 022
#
export HSTNME=`/bin/uname -n`
export PS1='$HSTNME> '
export PS2='...> '
export HISTSIZE=100
export HISTFILESIZE=40
export IGNOREEOF 
export noclobber
export notify
alias bye='logout'
alias pwd='echo $PWD'
function cd {
   builtin cd $*; echo $PWD
}
function back {
   builtin cd -; echo $PWD
}
function hist {
  history | head -39 | more
}
alias mv='mv -i'
alias cp='cp -i'
alias rm='rm -i'
alias more='more -d'
function gs {
  xv_get_sel $*
}

if [ -z "$popsversion" ]; then
   export popsversion=3.5
fi
isrc=~unipops/$popsversion/sys.unipops/`domainname`.bashrc
if [ -r $isrc ]; then
   source $isrc
fi
unset isrc
# This is for site specific stuff
