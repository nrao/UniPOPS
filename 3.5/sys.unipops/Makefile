# @(#)Makefile	5.3 05/03/96
#
# Recreates Unipops sys.unipops directory
#
EXECLIST = unipops.sunview prepare.unipops .openwin-init

OTHERS = .cshrc .login .defaults .rootmenu .sunview unipops.login \
	  unipops.logout unipops.cshrc .logout .mailrc .dtprofile \
	  gb.nrao.login gb.nrao.cshrc gb.nrao.prepare.unipops \
	  cv.login cv.cshrc cv.prepare.unipops \
	  tuc.nrao.edu.login tuc.nrao.edu.cshrc tuc.nrao.edu.prepare.unipops \
	  kp.nrao.edu.login kp.nrao.edu.cshrc kp.nrao.edu.prepare.unipops \
	  .Xdefaults .bash_logout .bash_profile .bashrc gb.nrao.bash_profile \
	  gb.nrao.bashrc unipops.bash_logout unipops.bash_profile \
	  unipops.bashrc cv.bash_profile cv.bashrc kp.nrao.edu.bash_profile \
	  kp.nrao.edu.bashrc tuc.nrao.edu.bash_profile tuc.nrao.edu.bashrc \
	  unipops.xinitrc unipops.tvtwmrc unipops.twmrc unipops.mwmrc

all:	$(EXECLIST) $(OTHERS)

clean:
	-sccs clean
