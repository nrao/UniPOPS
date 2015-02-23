/*  @(#)f2u.c	5.3 04/23/98	*/
/*********************************************************************
 * F2U - FITS reader for Green Bank Single Dish data.                *
 *  This is a general FITS reader - will read and list any FITS      *
 *  file.  If it is a Single-dish binary table, can convert to       *
 *  UNIPOPS file.                                                    *
 *********************************************************************/
#include <stdio.h>
#include <math.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>

#ifdef THINK_C
 #include <console.h>
 #include <stdlib.h>
#endif

#include "f2u.h"  
#include "setkw.h"
#include "u2fglob.h"
#include "unipops.h"

char version[40] = {"f2u version 1.5 (23Apr98)"};

/*  log file  */
char logname[16]  = {"f2u.log"};   /* f2u.log file records log data */
FILE *logfile;        
int logflag;    

/* globals for converting angles                         */
double rth,rtd;   /* radians to hours; radians to degrees */

/*  today's date  */
  time_t *caltime, cltime;
  char *today, rundate[16];
  struct tm *ltime;             /* local time structure */

/* user options and globals                                          */
int rcount;           /* input record counter                        */
int debug;            /*  =0 no debug printout; =1 some; =2 lots     */
int qsum;             /*  =0 for 1-line summary; =1 for full headers */
                      /*  =2 for header and data table listings.     */
int qscanc;           /*  =1 for scan count option.                  */
int qcont;            /*  =0 for line, =1 for continuum data         */  
int kpflag;           /*  =1 if Tucson data; =0 otherwise            */
int p11flag;          /*  =1 if prototype class 11, =0 otherwise     */
int qvof;             /* declared in u2fglob.h                       */
int prev;             /* declared in u2fglob.h, always =0 in f2u     */
int y2k;              /* declared in u2fglob.h, not used here        */
int ndata_start;      /*  tells which table row to start listing     */
int nrows;            /*  tells how many table rows to list          */
int jrows;            /*  counts table rows being listed.            */
int qkconv;           /*  =1 to convert to IEEE UNIPOPS format.      */ 
int qtinit;           /*  =0 if translation table not yet initialized*/
int ircvr;            /* counts # of receivers                       */
int uCount;           /* counts # of output records                  */
long rrec;            /* used in utrans                              */
double restfreq;      /* restfreq value to put in unipops record     */
double freqres;       /* freqres value to put in unipops record      */
double dsf;           /* dsf value to put in unipops record          */
double nt;            /* rentalue to put in unipops record           */

/* globals delcared in f2u.h */
int head, naxes, iaxes;
int maxes, maxisn[20];
int headlatch;
int Dwidth;                /* data width (bytes) in binary table        */
int iDrow;                 /* row count in data raster or binary table  */
int nDrow;                 /* total # rows in raster or table           */
int flag3D;                /* =1 if binary table or 3D  table           */
int flagGBSD;              /* =1 if GB Single Dish table                */

/* buffer for binary data tables */
char *tbuf;
long int siz_tbuf;
int num_tbuf;
int tbufflag, itbuf;

/* buffer for binary data column header info */
BTAB *bt;
int btflag, btoffs;
int Ntfields;              

/* buffer for UNIPOPS record */
int ubufflag;
char *ubuf;
UPOPS up;
long upnt[16];
UNIAUX auxbuf;

/* axis information structure */
/* note that we assume no more than 20 axes! (FITS standard allows up to 999)
   So watch out !                                                          */
AXDATA axd[20];
int Naxd;            /* = max num of axes in current map or table row.  */

/* buffer for header parameters */
KW_VAL *hk;
int nhk, hkflag, maxhk;

/* buffer for translation table */
TRANS_TABLE *tt;
int ttflag;
int hbufflag;
HEAD_TABLE *th,*mh;   /* define these because inittrans uses them */

int n_bitpix, naxisn[100];
long int n_datarecs, drec_count, drec_lim;
int ddone;

/*********************************************************************
 * MAIN starts here.                                                 *
 *********************************************************************/
main(argc,argv)
int argc;
char *argv[];
{
  /* init output buffer */
   uCount=0;
   nhk=hkflag=0;
   Naxd = 0;                 /* init axis structure */
   qcont = -1;       /* initialize contin/line flag */

  /* initialize globals for converting angles */       
   rth = 12./3.14159265358979;
   rtd = 180./3.14159265358979;
   
  /* get present date, time.                  */
    
    cltime = time(caltime);  today=ctime(&cltime);
    ltime = localtime(&cltime);
    fprintf(stderr,"\n%s,  run: %s",version,today);

#ifdef THINK_C
  argc = ccommand(&argv);   /*  use this for MAC Think-C version */ 
#endif

  getparams(argc,argv);    /* get command-line parameters      */
  openlog();               /* open log file and type greetings */

  initkw();                /*  initialize the keyword structure */
  
  fitread();               /* read standard input file        */
                           /* only one file per call of f2u   */
  
 /* close log file and free memory space  */ 
    closem();      /* free up buffers */

    if(logflag==1)
    {  fprintf(logfile,"\nf2u ends ok.\n");
       fclose(logfile);
    }
} 
/*********************************************************************
 * end of main                                                       *
 *********************************************************************/
 
 
/***************************************************************
 * ERREXIT - print error message and exit.                     *
 ***************************************************************/
errexit(s)
char *s;
{
    perror(s);  /* print message and UNIX error code, if any. */

 /* close memory allocations */
    closem();

 /* close files, if open */

    if(logflag==1)
    {  fprintf(logfile,"\n%s\n f2u bombs!\n",s);
       fclose(logfile);
     }
    exit(0);
} /****end of errexit**********************************************/


/*****************************************************************
 * CLOSEM -  release memory space, and flush stdout.             *
 *    this is done by main program on normal exit, or by         *
 *    errexit if abnormal exit.                                  *
 *****************************************************************/
closem()
{
    if(btflag==1)    free(bt);     /* binary table struct  */
    if(tbufflag==1)  free(tbuf);   /* binary table itself  */
    if(hkflag==1)    free(hk);     /* header keyword data  */

    if(ttflag==1)       /* translation table struct        */
    {   free(tt);       /* this was allocated in inittrans */
    }
    if(ubufflag==0) free(ubuf);
    
    btflag=tbufflag=ttflag=hkflag=0;
    ubufflag=0;
    
    fflush(stdout);
}


/*****************************************************************
 * GETPARAMS - get parameters from command line, or print        *
 *   usage message if no parameters, or word "help"              *
 *****************************************************************/
getparams(argc,argv)
int argc;
char *argv[];
{
    int i, noprob, jprob, jj;
    
   noprob=0;  jprob=0;
   
   while(noprob==0)    /*  give usage message if error, or no params */
   {
     if((argc<2)||(strcmp(argv[1],"help")==0)
                ||(strcmp(argv[1],"HELP")==0)
                ||(strcmp(argv[1],"Help")==0)
                ||(jprob==1))
     {
   fprintf(stderr,"f2u : read a FITS file from std input.\n");
   fprintf(stderr,"Usage: f2u -u|-sum|-head|-dump [n]\n");
   fprintf(stderr,"           -u | -U     : convert to UniPOPS format.\n");
   fprintf(stderr,"           -sum or -s  : 1-line summary to stdout.\n");
   fprintf(stderr,"           -head or -h : list full headers on stdout.\n");
   fprintf(stderr,"           -c          : #of scans in file to stdout.\n");
   fprintf(stderr,"           -dump or -d : list headers and 5 table rows,");
   fprintf(stderr," starting at n.\n");
       exit(0);
      }     

     /*  set default parameters  */
     
     jprob=1;
     debug=0;        /* debug=1 for some debug info, =2 for lots. */
     qsum=1;
     qscanc = 0;
     qkconv = 0;
     qtinit = 0;
     prev = 0;
     y2k = 0;
     ndata_start=0;
     nrows=NROWS;   jrows=0;
   
     /*  inspect command line parameters  */ 
    
     for(i=1; i<argc; i++)
     {   if((strcmp(argv[i],"-u")==0)     /* convert to IEEE UNIPOPS option */
           ||(strcmp(argv[i],"-U")==0))
         { qkconv=1;  noprob=1;
           qsum = -1; 
           i=argc;    break; 
         }
         if((strcmp(argv[i],"-d")==0)     /* data dump option */
              ||(strcmp(argv[i],"-dump")==0))
         {  qsum=2; 
            if((i+1)<argc)
            {  jj = atoi(argv[i+1]);    /* get starting record number */
               if(jj>0)  ndata_start = jj;
            }
            noprob=1;  i=argc;
            break;
         }
         if((strcmp(argv[i],"-s")==0)     /* brief summary option */
          ||(strcmp(argv[i],"-sum")==0)) 
         {  qsum = 0;
            noprob=1; i=argc;
            break; 
         }
         if((strcmp(argv[i],"-h")==0)      /* header summary option */
          ||(strcmp(argv[i],"-head")==0))
         {   qsum = 1;
             noprob=1; i=argc;
             break; 
         }
         if(strcmp(argv[i],"-c")==0)      /* Scan count to stdout  */
         {   qsum= -1;
             qscanc = 1;
             noprob=1; i=argc;
             break; 
         }
         if(strcmp(argv[i],"-dd")==0)     /* extra debug printing */
         {   debug=2;                     /* undocumented option  */
             noprob=1; i=argc;
             break; 
         }
     }
   }    /*  end of while  */
}
/*********end of getparams****************************************************/


/***************************************************************************
 * OPENLOG - open log file and write initial log messages.                  *
 ***************************************************************************/
 openlog()
 {
    char st[80];
    
    if((logfile=fopen(logname,"a"))==NULL)     /*  append to log file  */
    {  sprintf(st,"Wfits: error opening logfile: %s ",logname);
       perror(st);
       exit(0);
    }
    logflag=1;
  

    fprintf(logfile,
      "\n***********************************************************");
    fprintf(logfile,"\n%s,  run: %s\n",version,today);
    fprintf(logfile," f2u params: qsum=%d, qkconv=%d,  data_start=%d,",
            qsum,qkconv, ndata_start);
    fprintf(logfile,"  debug=%d",debug);
    fflush(logfile);
}
/************end of openlog************************************************/


/***************************************************************************
 * FITREAD - read one FITS file from standard input.                       *
 ***************************************************************************/
fitread()
{
  char inbuf[FITS_REC_LEN], tmpstr[64];
  int rcode, irec, ntoread, ipnt;

  rcount=0;
  headlatch=0;
  btflag=0;
  tbufflag=0;
  btoffs = 0;
  itbuf=0;
  flag3D=flagGBSD=0;

  /*  set up buffer for table header parameters */
   nhk=hkflag=0;
   maxhk = sizeof(kw)/sizeof(struct keyword);
   if((hk=(KW_VAL *)malloc((long)(maxhk*sizeof(KW_VAL))))==0)
       errexit("FITREAD: Cannot allocate space for hk table!");
   hkflag=1;

  head=1;    /* head is a global var =1 if expecting a header rec */
  irec=0;
 
  /* if -s option, put title lines on std out */
   if(qsum==0)
   { printf("\n%s,  run: %s\n",version,today);
     printf(" HdrType      Date    Telescope Observer ");
     printf("  Bpix Nax  Nx1  Nx2 Tflds Mx\n");
   }
   
  /*  loop thru all 2880-byte records in input file */

  rcode=FITS_REC_LEN;
  ntoread=FITS_REC_LEN;
  ipnt=0;
  while(rcode==FITS_REC_LEN)
  {  rcode=read(0,inbuf,FITS_REC_LEN);
     if((rcode>0)&&(rcode<ntoread))
     {  ipnt = rcode;
                                       /* wait for pipe to fill */
        while((ipnt<FITS_REC_LEN)&&(rcode>0))
        {  ntoread -= rcode;
           rcode = read(0,(inbuf+ipnt),ntoread);
           if(rcode<0)
           {  errexit("FITREAD PROBLEM"); }
           ipnt += rcode;
        }
        rcode=ipnt;
     }
     ntoread=FITS_REC_LEN;
     ipnt=0;

     irec++;
     rcount++;

     if(head) do_head(inbuf);  /* interpret header records */
     else    do_data(inbuf);  /* unpack data records      */
      
                                /* exit after adequate dumping */
     if((qsum==2)&&(jrows>=(nrows+ndata_start))) rcode=0; 
  }
  LOGPR"\nRead %d records, with remainder=%d, rows=%d\n",
                                          irec, rcode, jrows);
  LOGPR"Wrote %d UniPOPS records to stdout\n",uCount);
  fprintf(stderr,
    "Read %d FITS records, %d rows.  Wrote %d UniPOPS records.\n",
          irec, jrows, uCount);
}

/***********************************************************************
 * DO_HEAD - read one 2880 size buffer of FITS header info             *
 *    exits with head=0 after keyword END is found.                    *
 ***********************************************************************/
do_head(inbuf)
char *inbuf;
{
   int i, j, ikw, lcount, fretval;
   char lin[90], ermsg[80], cc;
   KW_VAL kval;

   lcount=0;
   ddone=0;
   qtinit=0;  /* reset init flag for translation table */
   
   /*  Check for standard header signature  */
   if(headlatch==0)
   {
     if((strncmp(inbuf,"SIMPLE",(long)6)!=0)
        && (strncmp(inbuf,"XTENSION",(long)8)!=0))
     {   sprintf(ermsg,"Rec#%d  not standard FITS header.",rcount);
         fprintf(logfile,"\n%s\n", ermsg);
         return(0);          /* assume end of file if  non-stand header */
     }
     nhk = 0;  /* reset hk table if new header */

     if(qsum>0) printf("\n******** Start of Header Record *********\n");
    }
   headlatch=1;  /* only check 1st record of header for FITS signature */

   for(i=0; i<FITS_REC_LEN; i+=HDR_LINE_WIDTH)
   {   strncpy(lin,inbuf+i,(long)HDR_LINE_WIDTH);
       (void)fixline(lin, HDR_LINE_WIDTH);  /* fix IRAM zero filled header */
       lcount++;
       if(qsum>0)
       {  cc = lin[HDR_LINE_WIDTH-1];  
          lin[HDR_LINE_WIDTH-1]='\n';
          printf("%s",lin);
          lin[HDR_LINE_WIDTH-1] = cc;  
       }
       j=parseln(lin,&kval);
       if((j<0) || (kval.nkws<=0))    
       {  sprintf(ermsg,"FITS syntax error, rec#%d, line %d:\n%s",
               rcount, lcount, lin);
          errexit(ermsg);
       }
       else
       {                      /* process if a kw was found */
          /* add to hk table if non-axis keyword */
          if(kval.naxi==0)
          {  *(hk+nhk) = kval;
             nhk++;
             if(nhk>maxhk)  errexit("DOHEAD: hk table overflows!!");
          }

          ikw = kval.pntr;
          if(ikw>=0)                    /* if kw not recognized, skip */
          {  if(*(kw[ikw].func) != 0)
             {  fretval= (*(kw[ikw].func)) (&kval); /*  execute function */
                if(fretval<0)
                {  sprintf(ermsg,"error processing keyword: %s",kw[ikw].name);
                   errexit(ermsg);
                }
             }

             if(strcmp(kw[ikw].name,"END")==0)   /*  if keyword=END  */
             {  
                drec_count=0;
                ddone=0;        /* reset data record counter and flag */
                jrows=0;
                break;                       /* jump out of for loop */
             }
          }
       }
    }
} /********end of do_head**************************************************/


/***********************************************************************
 * DO_DATA - take an input binary data record, put into tbuf;          *
 *    when tbuf fills up to siz_tbuf, either print the contents(qsum=2)*
 *    or convert to UNIPOPS format (qkconv=1)                          *
 ***********************************************************************/
do_data(inbuf)
char inbuf[];
{
   int i;

   if((tbufflag==1)&&(ddone==0))
   {
      for(i=0; ((i<FITS_REC_LEN)&&(tbufflag==1)); i++) 
      {  tbuf[itbuf++] = inbuf[i];
         if(itbuf>=siz_tbuf)
         {   if(flag3D==1)           /* if BINTABLE, process values */
             {  
              /* fprintf(logfile, "\nDo_Data, got row %d, i=%d, siz=%d ",
                      jrows, itbuf, siz_tbuf);
               fflush(logfile);

           /* convert to SUN UNIPOPS format if -u option */
                if((qkconv==1)||(qsum==0))  convert_row();
                else  
                {  if(qsum==2)
                   if(jrows>=ndata_start)  pr_tbuf(); /* dump table data */
                }
             }
            itbuf=0;
            jrows++;
            if(jrows >= num_tbuf)
            {  ddone=1;            /* done with data for this file */
               fprintf(logfile,"\nRow Limit: %d %d ", jrows, num_tbuf);
               break;
           }
         }
      }
   }
 fprintf(logfile, "\nDo_DataEnd, got row %d, i=%d, siz=%d, drec=%d n_data=%d ",
                   jrows, itbuf, siz_tbuf, drec_count, n_datarecs);
  fflush(logfile);

   drec_count++;
   if(drec_count>=n_datarecs)   /* switch back to header */
   {  head=1; 
      headlatch = 0;
      btoffs=0;
      if(qsum==0)  printf("\n");  /* space down for brief summary */
   } 
}  /*****end of do_data***********************************************/


/***********************************************************************
 * PR_TBUF - print contents of the binary table                        *
 *  This implements the -dump or -d option of f2u.                     *
 *  This routine is a generic dump of table data, one line of info per *
 *    table column.                                                    *
 ***********************************************************************/
pr_tbuf()
{
   int i,j, mm,nff, kk, tnmult, maxis1, vf;
   char cbf[128], *tb;
   short *sbf;
   float *fbf, flt,  ieee_to_real();
   double *dbf, dub, ieee_to_dub();

   sbf = (short *)cbf;
   fbf = (float *)cbf;
   dbf = (double *)cbf;
   tb  = tbuf;

   maxis1 = 0;
   j=0;
   if(flag3D==1)
   {
       for(i=1; i<Ntfields+1; i++)  /* loop thru all table columns */
                                    /* print name of column */
       {  printf("\n %5d%12s ", j, (bt+i)->ttype);
          if((bt+i)->form=='A')
          {   strncpy(cbf, tb, (long)(bt+i)->tmul);
              j += (bt+i)->tmul;
              tb += (bt+i)->tmul;
              cbf[(bt+i)->tmul]=0;
              printf(" %s", cbf);            /* print ascii data here */
              strcpy(cbf,"                                        ");
          }
          else
          {  /* if this col is the matrix, use maxis instead of
                 multiplicity.                                      */
             if(((bt+i)->matflg==1)&&(maxis1>10))  tnmult = maxis1;
              else  tnmult = (bt+i)->tmul;
              
             for(mm=0;mm<tnmult; mm++)
             {  if(mm%4==3)printf("\n ");
                if(tnmult<=1)  printf("     ");
                else
                     printf(" (%4d)",mm);  /* print array index, if any */
             
                switch ((bt+i)->form)
                {   case 'I' :  nff=2;
                          for(kk=0; kk<nff; kk++, j++)  
                          {  cbf[kk] = *tb;
                             tb++;
                          }
                          printf("%10d", sbf[0]);  /* print integer datum */
                          
                        /* for case of MAXIS1, get axis length */
                         if(strcmp((bt+i)->ttype, "MAXIS1")==0)
                         {  maxis1 = sbf[0];
                         }
                           break;
                    case 'E' : nff = 4;
                          for(kk=0; kk<nff; kk++, j++) 
                          {  cbf[kk] = *tb;
                             tb++;
                          }
                          flt = ieee_to_real(fbf,&vf);
                                                   /* print floating datum */
                          printf("%12.6E", flt);  
                           break;
       
                    case 'D' : nff = 8;
                          for(kk=0; kk<nff; kk++, j++) 
                          {  cbf[kk] = *tb;
                             tb++;
                          }
                          dub = ieee_to_dub(dbf,&vf);
                                        /* print double precision datum */
                          printf("%14.8E", dub);  
                           break;
                 }       
              }
          }
       }
   }
   itbuf=0;
} /********end of pr_tbuf********************************************/


/********************************************************************
 * FIXLINE - convert all control characters or null characters      *
 *           to blanks.   Fixes problem with zero fill in headers.  *
 *           n=length of line.                                      *
 ********************************************************************/
fixline(line, n)
char *line;
int n;
{
   int i;

   for(i=0; i<n; i++)  if(line[i]<' ')  line[i] = ' ';
   line[n] = 0;
}
