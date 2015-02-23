/* @(#)zignal.c	1.1 07/08/94 */
/*  set up signal handling function */
/*  the use of signal() is probably deprecated. */
/*  sigaction is the POSIX way */

#include <signal.h>

void (*zignal (signo, func))()
int signo;
void (*func)();

/*  signo is the signal number */
/*  func is the function to call, which should return nothing */
/*    func takes one argument, an int, the signal number that called it */
/*  returns the previous signal handler function */

{

   struct sigaction act, oact;

   act.sa_handler = func;
/*		make sure we don't block other signals */
   sigemptyset(&act.sa_mask);
/*		turn off all flags */
   act.sa_flags = 0;
#ifdef SA_RESTART
/*		Solaris uses this, SunOS default is essentially this */
      act.sa_flags = SA_RESTART;
#endif
   if (sigaction(signo, &act, &oact) < 0)
      return (SIG_ERR);
   else
      return (oact.sa_handler);
}

