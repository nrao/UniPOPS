/* @(#)popstool.c	5.1 06/22/94 
* Creates a new tty window that wont die when QUIT is clicked
* and that will run a process specified on the command line
*/

#include <sys/wait.h>
#include <stdio.h>
#include <suntool/sunview.h>
#include <suntool/tty.h>
#include <ctype.h>

static Notify_value donot_destroy();
static Notify_value destroy();

static Frame frame;
static Tty tty;

int child_death, child_status;

static short ic_image[258] = {
#include <pops.icon>
};
mpr_static(gfxic_mpr, 64, 64, 1, ic_image);

static	struct icon icon = {64, 64, (struct pixrect *)NULL, 0, 0, 64, 64,
	    &gfxic_mpr, 0, 0, 0, 0, NULL, (struct pixfont *)NULL,
	    ICON_BKGRDCLR};

main(argc, argv)
	int argc;
	char **argv;
{
	static	char *label_default = "PopsTool";
	int child_pid, j;

	child_death = 0;
	child_status = 0;

	/*
	 * Create tool window
	 */
	frame = window_create(NULL, FRAME,
	    FRAME_LABEL,		label_default,
	    FRAME_ICON,			&icon,
	    FRAME_ARGC_PTR_ARGV,	&argc, argv,
/*	    WIN_ROWS,			25,
	    WIN_COLUMNS,		80, */
	    WIN_ERROR_MSG,		"Cannot create frame",
	    0);

 	/*
	 * Create tty tool subwindow
	 */
	tty = window_create(frame, TTY,
	    TTY_QUIT_ON_CHILD_DEATH,	FALSE,
	    TTY_ARGV,			&argv[1],
	    0);

	child_pid = (int)window_get(tty, TTY_PID);
	(void) notify_interpose_wait3_func(tty, destroy, child_pid);
	(void) notify_interpose_destroy_func(frame, donot_destroy);
	window_main_loop(frame);
	exit(child_status);

}


static Notify_value
donot_destroy(frame, status)

Frame frame;
Destroy_status status;

{
	if (status == DESTROY_CHECKING) 
	{
		if (child_death == 0)
		{
		   (void) notify_veto_destroy(frame);
		    return(NOTIFY_DONE);
		}
		else
		{
		    window_set(frame, FRAME_NO_CONFIRM, TRUE, 0);
		}
	}
	else
	{
		window_set(frame, FRAME_NO_CONFIRM, TRUE, 0);
	} 
	return (notify_next_destroy_func(frame, status) );
}

static Notify_value
destroy(tty, pid, status, rusage)

Tty tty;
int pid;
union wait *status;
struct rusage *rusage;

{
	if ((*status).w_termsig) 
	    child_status = (*status).w_termsig;
	else
	    child_status = (*status).w_retcode;
	child_death = -1;
	window_done(tty);
	return(NOTIFY_DONE);
}

