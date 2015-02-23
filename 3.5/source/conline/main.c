
/* @(#)main.c	5.5 05/04/98 */

#include <signal.h>
#include <setjmp.h>
jmp_buf env, env2;
int zero, six, one;
sigset_t sset;
void (*iterm)();
void (*ipipe)();
void (*ihup)();
void (*iint)();
void onintr(), closeonintr(), closeonterm(), brokenpipe();
void (*zignal())();
int datestyle_();

#ifdef VS2_0_1
	main(argc, argv)
#else
	MAIN_(argc, argv)
#endif
     int argc;
     char **argv;

#ifndef NULL
#define NULL 0
#endif

{
    int iret, n1;
    n1 = 1;

    /* look for the undocumented y2k flag */
    if (argc == 2 && strcmp(argv[1],"-y2k") == 0) datestyle_(&n1);
    iterm = zignal(SIGTERM, SIG_IGN);
    if (iterm != SIG_IGN) iterm = zignal (SIGTERM, closeonterm);
/*  Initializes a KILL trap */

    ihup = zignal(SIGHUP, SIG_IGN);
    if (ihup != SIG_IGN) ihup = zignal (SIGHUP, closeonterm);
/*  Initializes a HUP trap, exiting windows can do this */
/*  Its essentially the same as a SIGTERM */

    iint = zignal(SIGINT, SIG_IGN);
    if (iint != SIG_IGN) iint = zignal (SIGINT, closeonintr);
/*  Initializes temporarily a CTRL-C trap */

    ipipe = zignal(SIGPIPE, SIG_IGN);
    if (ipipe != SIG_IGN) ipipe = zignal (SIGPIPE, brokenpipe);
/*  Initializes a broken pipe trap */

    initfperror_();
    initcio_(); 
    initlock_();
    initmess_();
    setjmp(env2);
    initappl_();
    recover_(&iret);
    gphint_();  
    initonline_();
    initlog_();
    initcomm_();
    one = 1;
    initfiles_(&one); 
    if (iret != 1) initsetup_();  

/*  Initializes IO variables, graphics; checks for existence of necessary files
    locks the directory from use by others, and initializes K array. */

    iint = zignal(SIGINT, SIG_IGN);
    if (iint != SIG_IGN) iint = zignal(SIGINT, onintr);
    sigemptyset(&sset);
    sigaddset(&sset, SIGINT);
    setjmp(env);
    /* make sure SIGINT is not blocked */
    sigprocmask(SIG_UNBLOCK, &sset, NULL);

/*  Sets up permanent CTRL-C trap */

    gtline_();
    zero = 0;
    exitpops_(&zero);
}

void closeonintr(signo)
int signo;

{
    zignal(SIGINT, SIG_IGN);
/*		ignore any more interrupts, just exit */
    printf("\nCTRL-C Interrupt -- Terminating\n");
    six = 6;
    exitpops_(&six);
}

void closeonterm(signo)
int signo;

{
    
    ihup = zignal(SIGHUP, SIG_IGN);
    iterm = zignal(SIGTERM, SIG_IGN);
/*		ignore things that can get here so this doesn't happen */
/*		more than once */
    printf("\nProccess has been KILLED\n");
    six = 6;
    exitpops_(&six);
}

void brokenpipe(signo)
int signo;

{
    printf("\nCannot communicate with child process -- Continuing\n");
}

catchtrlc_()
{
     iint = zignal(SIGINT, onintr);
}

void onintr(signo)
int signo;
{
    ctrlcintr_();
    longjmp(env, 1);
}

wipe_()
{
     initcio_();
     longjmp(env2, 1);
}
togtline_()
{
     longjmp(env, 1);
}
ignorectrlc_()
{
     iint = zignal(SIGINT, SIG_IGN);
}
resetctrlc_()
{
     iint = zignal(SIGINT, iint);
}
int signo;
