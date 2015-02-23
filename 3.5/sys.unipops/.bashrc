#
#  @(#).bashrc	5.2 05/03/96
#  Default UniPOPS .bashrc file
#
if [ -z $popsversion ]
  then  export popsversion=3.5
fi
#
source ~unipops/$popsversion/sys.unipops/unipops.bashrc
#
# For additional Unix security you may want to uncomment out 
# the following line:
# 
# umask 066
#
