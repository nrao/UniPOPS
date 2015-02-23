#!/bin/csh
#
#  @(#).cshrc	5.2 05/03/96
#  Default UniPOPS .cshrc file
#
if ( ! $?popsversion ) setenv popsversion 3.5
#
source ~unipops/$popsversion/sys.unipops/unipops.cshrc
#
# For additional Unix security you may want to uncomment out 
# the following line:
# 
# umask 066
#
