/*  @(#)f2u.h	5.1 06/22/94	*/
/* INCLUDE FILE FOR FITS READER   (F2U)                                  */
/*   by F. Ghigo  (January, 1990)                                        */
/*   last update June 14, 1991                                           */

#define HDR_LINE_WIDTH  80  /* # bytes per line in FITS header           */
#define HLIM             4  /* max levels of keyword Heirarchy           */
#define MAXKWLEN        16  /* max characters in keyword string          */
#define KWMAXCHAR        8  /* FITS standard max kw size                 */

typedef
struct keyword              /* store basic data about each kw            */
{  char name[MAXKWLEN];     /* keyword name                              */
   int status;              /* =1 if found in input file, else =0        */
   int type;                /* 0=logical, 1=integer, 2=real,             */
                            /*   3=character string                      */
   int maxi;                /* Max allowed axis index (for NAXISn        */
                            /*   type words                              */
   int no;                  /* no. of values expected. normally=1        */
                            /* =2 for a complex int or real              */
   int (*func)();           /* pointer to function to call when          */
                            /*   keyword is encountered.                 */ 
} KEYWORD;
extern KEYWORD kw[];

typedef 
struct keyword_values          /* store kw values found in input          */
{  char names[HLIM][MAXKWLEN]; /* may have up to HLIM keywords on         */
                               /*   any line (before the "=")             */
   int nkws;                   /* no. of keywords found on a line.        */
   int pntr;                   /* pointer to "keyword" array              */
   int tpntr;                  /* pointer to binary table column          */
   int status;                 /* maybe this will have a use later        */
   int type;                   /* =-1 if unknown type, otherwise          */
                               /*  type is as in "keyword" struct         */
   int naxi;                   /* keyword axis number, e.g, if its        */
                               /* NAXISn, n=naxi                          */
   int no;                     /* no.of values found for kw               */
                               /*   tells size of ival or dval            */
   int logic;                  /* value of keyword, if logical            */
                               /* logic=0 for False, =1 for True          */
   long int ival[2];           /* value(s) of keyword, if integer         */
   double dval[2];             /* value(s) of keyword, if real.           */
   char cval[80];              /* value of keyword, if string.            */
} KW_VAL;
extern KW_VAL *hk;             /* holds values of kws from header         */
extern int nhk, hkflag;        /* nhk counts # keywords in hk             */
extern int maxhk;              /* number of keywords in kw buffer         */

typedef struct btable_struct   /* store info for table column headings    */
{   char ttype[16];            /* type of quantity                        */
    char tunit[16];            /* units of quantity                       */
    int  tmul;                 /* multiplicity                            */
    char form;                 /* format type (A,I,E,D)                   */
    int  offs;                 /* offset into binary table                */
    int  tpntr;                /* pointer to translation table struct     */
    int  matflg;               /* =1 if this column contains a matrix     */
 } BTAB;
extern BTAB *bt;               /* table structure info structure          */
extern int btflag;             /* =1 if bt has space allocated.           */
extern int Ntfields;           /* number of table fields = items in bt    */
extern int btoffs;             /* offset counter for bt table             */

extern int head, naxes, iaxes;
extern int headlatch;
extern int Dwidth;             /* data width (bytes) in binary table      */
extern int iDrow;              /* row count in data raster or binary table*/
extern int nDrow;              /* total # rows in raster or table         */
extern int flag3D;             /* =1 if binary table or  3D table         */
extern int flagGBSD;           /* =1 if GB Single Dish table              */
extern int qcont;              /* =0 for line data, =1 for continuum      */

/* buffer for binary data tables */
extern char *tbuf;
extern long int siz_tbuf;    /* width of table row found in input.        */
extern int num_tbuf;         /* number of table rows found in input file. */
extern int tbufflag, itbuf;

extern int n_bitpix, naxisn[100];
extern long int n_datarecs, drec_count, drec_lim;
extern int maxes, maxisn[20];  /* table axis lengths                      */

/* Data for axes as read from header or table                             */
typedef struct ax_data
{  int no;                   /* axis number                               */
   int maxis;                /* number of pixels on this axis             */
   double crpix;             /* reference pixel number                    */
   char ctype[30];           /* name of axis                              */
   double cdelt;             /* axis increment                            */
   double crval;             /* value at reference pixel                  */
   double crota;             /* rotation of axis                          */
}  AXDATA;
extern AXDATA axd[];
extern int Naxd;        /* number of axes in current map or table row  */

/* header summary structure for use by -s options                         */
typedef struct header_summary
{  char type[10], date[16];
   char tele[10], obsr[22], omode[10];
   int  nxis1, nxis2, mxis, tflds;
} HSUM;
extern HSUM hsum;
   
#define NROWS 5         /* number of table rows to list if dump option    */
#define LOGPR  fprintf(logfile,  /* shorthand for printing to log file. ) */

/*  log file  */
extern FILE *logfile;        
extern int logflag;    

/*  today's date  */
#include <sys/types.h>
extern  time_t *caltime, cltime;
extern  char *today, rundate[16];
extern struct tm *ltime;             /* local time structure                */

/* user options and globals                                                 */
extern int rcount;           /* input record counter                        */
extern int debug;            /*  =0 no debug printout; =1 some; =2 lots     */
extern int qsum;             /*  =0 for 1-line summary; =1 for full headers */
                             /*  =2 for header and data table listings.     */
extern int ndata_start;      /*  tells which table row to start listing     */
extern int nrows;            /*  tells how many table rows to list          */
extern int jrows;            /*  counts table rows being listed.            */
extern int quconv;           /*  =1 to convert to UNIPOPS format.           */ 
extern int qtinit;           /*  =0 if translation table not yet initialized*/
extern int ircvr;            /* counts # of receivers                       */
extern int uCount;           /* counts # of output records                  */

/* restfreq and freqres globals to deal with overloaded header words */
extern double restfreq, freqres, dsf, nt;

