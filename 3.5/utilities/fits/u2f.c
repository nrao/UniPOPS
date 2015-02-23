/*  @(#)u2f.c	5.3 04/23/98	*/
/********************************************************************
 * u2f  - Main program for conversion of UNIPOPS IEEE format        *
 *       to FITS binary table.                                      *
 *  If u2f is typed with no command line arguments, or followed by  *
 *     "?" or "help", a USAGE message is written on stderr.         * 
 *  Command line parameters are:                                    *
 *     -L, -C          (required)  select line or continuum.        *
 *     -s scan1 scan2  (optional)  select scan number range.        *
 *     -n maxrecs      (optional)  maximum records per table.       *
 *     -t maxWscan     (optional)  #points at which to truncate scan*
 *     -v, -f          (optional) velocity or freq of 1st axis(line)*
 *     -prev           (optional) preview SDD file, output to stdout*
 *                                consists of maxrecs and maxWscan  *
 *     -y2k            (optional) force write of Y2K compliant FITS *
 *                                dates before 1999, useful for     *
 *                                debugging only                    *
 ******************************************************************** 
 *  The input file is standard input, a UNIPOPS format file.        *
 *  The output file is standard output, a FITS binary table.        *
 *  A log file is appended, "u2f.log"                               *
 ********************************************************************
 *  Note that if the "-n maxrecs" option is NOT used,  the binary   *
 *    FITS table is first written to a scratch file.  When the end  *
 *    of the input is reached, the program writes the headers and   *
 *    binary table to standard output.                              * 
 *  If the "-n maxrecs" option IS used, the output file is generated*
 *    directly onto standard output; no scratch file is written.    *
 *    Each table has maxrecs records (except for the last one, which*
 *    may be shorter.  Each header will indicate that there are     *
 *    maxrecs records, however.                                     *  
 ********************************************************************
 * Modifications:                                                   *
 *   Aug 30, 1991, ver 1.0 - adjust header columns in which "=" sign*
 *    goes, have all variables end in col. 30,                      *
 *    Put BLOCKED after EXTEND in main header. (in U2f_trans)       *   
 *   Feb 17, 1992. ver 1.1 - fix in utrans.c so that width of table *
 *    row (maxTwid) is the size of the row with the largest scan,   *
 *    and not any larger.                                           *
 ********************************************************************/
#include <math.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>
#include <stdio.h>

#ifdef THINK_C
 #include <stdlib.h>
 #include <console.h>
#endif

#include "unipops.h"
#include "u2fglob.h"      /* load global declarations */

char Origin[40]  = {"UniPOPS u2f/1.5    "};
char version[40] = {"u2f version 1.5 (22Apr98)"};

/* User option parameters  */

double scan1, scan2;     /*  scan number range                    */
int qcont;               /*  qcont=0 for line, =1 for continuum   */
int maxrecs;             /*  max allowed records in a file        */
int maxWscan;            /*  truncate scan to maxWscan points     */
int qvof;                /*  axis options:  qvof=0 to use velocity axis; 
                                 =1 for frequency.                */
int prev;                /* =1 if preview mode, else 0            */
int y2k;                 /* =1 to force y2k FITS dates before 1999 */
/*  log file  */

char logname[16]  = {"u2f.log"};   /* u2f.log file records log data */
FILE *logfile;        
int logflag;    

/* temp file */
char tmpname[80];
int tmpfileflag, tmpfid;   

/* binary table temp buffer pointers */
char  *tbuf;             /* buffer for FITS binary table rows      */
int    tbufflag;
int    ircvr;           /* for compatibility with rfits */

/* header buffer pointers */
int hbufflag, ttflag;             /* =1 when buffers are allocated  */
int Ntrans, Twidth, Tfields;
int ScanWid, ScanMult;            /* describe scan data matrix      */
int maxTwid;
int MHnlines, THnlines, THtotal;
TRANS_TABLE *tt;                  /* translation table              */
HEAD_TABLE  *mh;                  /* structure for main file header */
HEAD_TABLE  *th;                  /* structure for table header     */

/* space for UNIPOPS buffer */
char *ubuf;
long upnt[16];
int ubufflag;
UPOPS up;

/* global vars that need to be declared here, but are not actually   */
/* used by u2f, just f2u */
double freqres, restfreq, dsf, nt;

/*  today's date  */
  time_t *caltime, cltime;
  char *today, rundate[16];
  struct tm *ltime;             /* local time structure */

/*********************************************************************
 * MAIN starts here                                                  *
 *********************************************************************/
main(argc,argv)
int argc;
char *argv[];
{
  /* get present date, time.                  */
    
    cltime = time(caltime);  today=ctime(&cltime);

    fprintf(stderr,"\n%s,  run: %s",version,today);
  
   /* conditional compilation if MAC THINK-C version  */
#ifdef THINK_C
  argc = ccommand(&argv);      /*  use this for MAC Think-C version */ 
#endif
          
    logflag  = tmpfileflag = 0;
    hbufflag = 0;
    tbufflag = 0;
    ubufflag = 0;
    
    /*  get parameters from command line.  */       
  
    getparams(argc,argv); 

    /* translate the local time into a form for use in FITS */
    /* use the new form if after 1998, unless the y2k flag is set */
    /* in which case always use the new form */
    ltime = localtime(&cltime);
    if (y2k || ltime->tm_year > 98) {
      /* set the y2k flag here no matter what - used elsewhere to 
       * determine the form of other DATE outputs */
      y2k = 1;
      sprintf(rundate,"%04d-%02d-%02d",(ltime->tm_year+1900), (ltime->tm_mon+1),
	      ltime->tm_mday);
    } else {
      sprintf(rundate,"%02d/%02d/%02d",ltime->tm_mday,(ltime->tm_mon+1),
	      ltime->tm_year);
    }

    /*  create log file and type greetings */
    
    openlog();

    /*  initialize translation table                              */
    
    if (prev != 1) inittrans(qcont, "U2f", 0);
    
    /*   read and select data, send FITS file to stdout.          */
    
    getdata();


 /* close files and free up memory buffers. */
     closem();  /* close all other files and free up memory */

    if(logflag==1)
    {  fprintf(logfile,"\nwfits ends ok.\n");
       fclose(logfile);
    }
    
    return(0);
}
/*********************************************************************
 * END OF MAIN                                                       *
 *********************************************************************/



/***************************************************************
 * ERREXIT - print error message and exit.                     *
 ***************************************************************/
errexit(s)
char *s;
{
    perror(s);  /* print message and UNIX error code, if any */

 /* close files, if open */
     closem();  /* close all other files and free up memory */

 /* put goodbye message in the log file and close it. */
    if(logflag==1)
    {  fprintf(logfile,"\n%s\n u2f bombs!\n",s);
       fclose(logfile);
     }
     exit(0);  /* quit wfits, go back to the UNIX shell */
}

/*****************************************************************
 * CLOSEM - close all open files and release memory space.       *
 *    this is done by main program on normal exit, or by         *
 *    errexit if abnormal exit.                                  *
 *****************************************************************/
closem()
{
    char st[36];
    int sysreturn;

    if(tmpfileflag==1)
    {   close(tmpfid);
#ifndef THINK_C
       sprintf(st,"rm -f %s",tmpname);
        sysreturn=system(st);  /* delete temporary file */
        fprintf(logfile,"\nSystem: %s, status=%d",
              st,sysreturn);
#endif
    }
    
    if(tbufflag==1)  free(tbuf);

    if(hbufflag==1)    /* these were allocated in inittrans */
    {   free(&tt);
        free(&mh);
        free(&th);
    }
    if(ubufflag==1) free(ubuf);
}


/*****************************************************************
 * GETPARAMS - get parameter from command line, or print         *
 *   usage message if no parameters, or word "help"              *
 *****************************************************************/
getparams(argc,argv)
int argc;
char *argv[];
{
    int i, noprob, jprob;
    
   noprob=0;  jprob=0;
   
   while(noprob==0)    /*  give usage message if error, or no params */
   {
     if((argc<2)||(strcmp(argv[1],"help")==0)
                ||(strcmp(argv[1],"HELP")==0)
                ||(strcmp(argv[1],"Help")==0)
                ||(strcmp(argv[1],"?")==0)
                ||(jprob==1))
     {
        fprintf(stderr,"u2f: convert a UniPOPS file from std input to\n");
        fprintf(stderr,"     a FITS binary table on std output.\n");
        fprintf(stderr,"Usage:  u2f -L|C [-n max] [-t npts] [-v|-f]");
        fprintf(stderr," [-s s1 s2] -y2k\n");
        fprintf(stderr,"            -L -C    : select line, continuum.\n");
        fprintf(stderr,"            -n max   : maximum records per FITS");
        fprintf(stderr,"                       table.\n");
        fprintf(stderr,"            -t npts  : truncate scan to length npts.\n");
        fprintf(stderr,"            -s s1 s2 : scan number range.\n");
        fprintf(stderr,"            -v : 1st axis is veloc; -f for frequency (line only).\n");
	fprintf(stderr,"            -prev : preview mode.\n");
	fprintf(stderr,"            -y2k : force y2k compliant FITS dates.\n");
	fprintf(stderr,"                   This happens automatically after 1998.\n");
        exit(0);
      }     

     /*  set default parameters  */
     
     jprob=1;
     scan1 = scan2 = 0;  
 qcont = 0;          /* qcont=0 for line; =1 for continuum                */
 maxrecs = 0;        /* maxrecs=0 means put all records in one FITS table */
 maxWscan=0;         /* optional scan length truncation.                  */
     qvof=1;         /* axis options  - default is FREQ */
     prev=0;         /* preview mode is off by default */
     y2k=0;          /* y2k mode is off by default */
   
     /*  inspect command line parameters  */ 
    
     for(i=1; i<argc; i++)
     {   if(strcmp(argv[i],"-C")==0)  { qcont=1;  qvof=-1; noprob=1;  }
         if(strcmp(argv[i],"-c")==0)  { qcont=1;  qvof=-1; noprob=1;  }
         if(strcmp(argv[i],"-L")==0)  { qcont=0;  noprob=1;  }
         if(strcmp(argv[i],"-l")==0)  { qcont=0;  noprob=1;  }
         if(strcmp(argv[i],"-f")==0)  { qvof=1;   noprob=1;  }
         if(strcmp(argv[i],"-v")==0)  { qvof=0;   noprob=1;  }
         if(strcmp(argv[i],"-n")==0)  
         {  i++;
            if(i>=argc) { noprob=0; i=argc; break; } /* check parm missing */
            maxrecs = atoi(argv[i]);
         }
         if(strcmp(argv[i],"-t")==0)  
         {  i++;
            if(i>=argc) { noprob=0; i=argc; break; }
            maxWscan = atoi(argv[i]);
         }
         if(strcmp(argv[i],"-s")==0)
         {  i++;
            if(i>=argc) { noprob=0; i=argc; break; } 
             scan1 = atof(argv[i]);
            i++;
            if(i>=argc) { noprob=0; i=argc; break; }
             scan2 = atof(argv[i]);
         }
         if (strcmp(argv[i],"-prev")==0) { prev=1; noprob=1;}
	 if (strcmp(argv[i],"-y2k")==0) { y2k=1;}
     }
   }    /*  end of while  */

   /* simple error checks */
   if(qcont!=0 && qvof!=-1) 
   {   fprintf(stderr, "Warning: Continuum Data is incompatible with");
       fprintf(stderr, "   frequency or velocity as the first axis.\n");
   }
   /*      exit(0);       */
}
/*********end of getparams****************************************************/


/*****************************************************************************
 * OPENLOG - open log file and write initial log messages.                   *
 *****************************************************************************/
 openlog()
 {
    char st[80];
    
    if((logfile=fopen(logname,"a"))==NULL)     /*  append to log file  */
    {  sprintf(st,"u2f: error opening logfile: %s ",logname);
       fflush(stdout);
       perror(st);
       exit(0);
    }
    logflag=1;
  

fprintf(logfile,"\n*********************************************************");
    fprintf(logfile,"\n%s,  run: %s\n",version,today);
    fprintf(logfile," u2f parameters: qcont=%d, maxrecs=%d, qvof=%d,",
            qcont,maxrecs, qvof);
    fprintf(logfile," scan1=%.0f, scan2=%.0f", scan1, scan2);
    fprintf(logfile,"\n  maxWscan=%d ", maxWscan);
    fprintf(logfile,"\n  Rundate: %s, Origin: %s ",rundate, Origin);
    if (prev==1)
	fprintf(logfile,"\n Preview mode, no FITS output will be generated.");
}
/************end of openlog**************************************************/

