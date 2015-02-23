/*    @(#)rundaemon.c	5.2 07/08/94     */

/*  Runs a process as a daemon  */

#include <stdio.h>
#include <fcntl.h>
#include <signal.h>
#ifdef SOLARIS
# include <sys/types.h>
#else
# include <sys/ioctl.h>
#endif

static int file2;

void sessdetach(init_flag)
int init_flag;
{
	int file0,file1, fd;	/* file descriptors */
/* If launched by init (process 1), there's no need to detach.
 * Note: this test is unreliable due to an unavoidable race
 * condition if the process is orphaned.
 */
	if (init_flag)
		goto out;
/* Ignore terminal stop signals */
#ifdef SIGTTOU
	signal(SIGTTOU, SIG_IGN);
#endif
#ifdef SIGTTIN
	signal(SIGTTIN, SIG_IGN);
#endif
#ifdef SIGTSTP
	signal(SIGTSTP, SIG_IGN);
#endif
/* Allow parent shell to continue.
 * Ensure the process is not a process group leader.
 */
	if (fork() != 0)
		exit(0);	/* parent terminates */
/* child code follows: */
/* Disassociate from controlling terminal and process group.
 *
 * Ensure daemon can't reacquire a new controlling terminal.
 *
*/

#ifdef SOLARIS
/*		This may work for both SOLARIS and SunOS */
/*		but for now, leave the old code as is    */
	setsid();
#else
	setpgrp(0, getpid());
	if ((fd = open("/dev/tty", O_RDWR)) >= 0) {
		ioctl(fd, TIOCNOTTY, 0);
		close(fd);
	}
#endif
out:
	/* close all files except for stderr, which now goes to the console */
	close(0);
	close(1);
	close(2);
	if ((file0 = open("/dev/console",O_WRONLY)) == -1) /* file number 0 */
		exit(2);
	if ((file1 = open("/dev/console",O_WRONLY)) == -1) /* file number 1 */
		exit(3);
	if ((file2 = open("/dev/console",O_WRONLY)) == -1) /* file number 2 */
		exit(4);
	close(0);
	close(1);
	return;		/* from sessdetach() call */
}


main(argc, argv)
int argc;
char *argv[];
{
	if (argc < 2) {
		fprintf(stderr, "Usage: rundaemon command [args]\n");
		exit(2);
	}

	if (access(argv[1], 1) == -1) {
		fprintf(stderr, "rundaemon: can't execute\n");
		perror(argv[1]);
		exit(2);
	}
	sessdetach(0);
	execv(argv[1], &argv[1]);
	exit(2);
}



