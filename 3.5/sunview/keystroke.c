
/*   @(#)keystroke.c	5.1 06/22/94      */

#include <termio.h>
#include <stdio.h>

static struct termio tbufsave;

void setraw_()  /* sets up raw terminal  */

{

	struct termio tbuf;

	if (ioctl(0, TCGETA, &tbuf) == -1)
		exit(-1);

	tbufsave = tbuf;
	tbuf.c_lflag &= ~ICANON;
	tbuf.c_cc[4] = 4;
	tbuf.c_cc[5] = 2;
	if (ioctl(0, TCSETAF, &tbuf) == -1)
		exit(-2);
}

void keystroke_(ierr, result, length)  /* gets character from terminal  */

char *result;
int *ierr, length;

{
	*ierr = read(0, result, 1);

}

void restore_()  /* Restore terminal flags */
{
	if (ioctl(0, TCSETAF, &tbufsave) == -1)
		exit(-5);
}

