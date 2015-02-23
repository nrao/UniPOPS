#
#   @(#).bash_profile	5.1 06/22/94
#   Default UniPOPS .bash_profile file
#
if [ -z $popsversion ]
   then export popsversion=3.5
fi
#
source ~unipops/$popsversion/sys.unipops/unipops.bash_profile
#
# Don't add any lines to be executed after this point; they probably will
# not be executed!!
#
   

