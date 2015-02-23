#  @(#)gb.nrao.cshrc	5.5 02/29/96
#
#  Green Bank specific UniPops .cshrc file
#
setenv HSTNME `/bin/uname -n`
#
#   lpr requires /usr/ucb in path
alias print 'pr -f \!* | lpr -Plp'
switch ($HSTNME)
  case nath:
  case vela:
  case fahd:
     alias lprint 'pr -f -l65 \!* | lpr -Pland'
     breaksw
  default:
     alias lprint 'pr -f -l65 \!* | $LOCALDIR/bin/qpr -ls -Pnet'
     breaksw
  endsw
alias netprint 'pr -f \!* | lpr -Pnet'
alias lnetprint 'pr -f -l65 \!* | $LOCALDIR/bin/qpr -ls -Pnet'
alias duplex 'pr -f \!* | $LOCALDIR/bin/qpr -d -Pnet'
alias tduplex 'pr -f \!* | $LOCALDIR/bin/qpr -t -Pnet'
alias hp3p 'pr -f \!* | unix2dos | lpr -Php3p'
#
alias Scans '/fahd/oper140/bin/Scans -Wp 0 190 -Ws 586 206 -WL Scans -Wl Scans +Wi'
alias ModScreen '/fahd/oper140/bin/ModScreen'
#
