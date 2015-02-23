#
#  @(#).bash_logout	5.1 06/22/94
#  Default UniPOPS .logout file
#
if [ -z $popsversion ]
   then export popsversion=3.5
fi
source ~unipops/$popsversion/sys.unipops/unipops.bash_logout
