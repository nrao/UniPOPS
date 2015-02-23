#  @(#)gb.nrao.bashrc	5.5 02/29/96
#
#  Green Bank specific UniPops .bashrc file
#
export HSTNME=`/bin/uname -n`
#
#     Note: lpr requires /usr/ucb in PATH
function print {
   pr -f $* | lpr -Plp
}
#
function lprint {
   case `uname -n` in
       fahd)    pr -f -l65 $* | lpr -Pland;;
       vela)    pr -f -l65 $* | lpr -Pland;;
       nath)    pr -f -l65 $* | lpr -Pland;;
          *)    pr -f -l65 $* | $LOCALDIR/bin/qpr -ls -Pnet;;
   esac
}
#
function netprint {
    pr -f $* | lpr -Pnet
}
#
function lnetprint {
    pr -l65 -f $* | $LOCALDIR/bin/qpr -ls -Pnet
}
#
function duplex {
    pr -f $* | $LOCALDIR/bin/qpr -d -Pnet
}
#
function tduplex {
    pr -f $* | $LOCALDIR/bin/qpr -t -Pnet
}
#
function hp3p {
    pr -f $* | unix2dos | lpr -Php3p
}
#
alias Scans='/fahd/oper140/bin/Scans -Wp 0 190 -Ws 586 206 -WL Scans -Wl Scans +Wi'
alias ModScreen='/fahd/oper140/bin/ModScreen'
#







