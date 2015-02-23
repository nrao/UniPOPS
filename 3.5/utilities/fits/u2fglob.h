/* Global variables for UNIPOPS IEEE to FITS translation program */

/*   @(#)u2fglob.h	5.3 04/23/98   */

/* FITS standard definitions */
#define FITS_REC_LEN 2880       /* standard FITS logical record size */

#define MAXTCOLS 70             /* max # columns in binary table     */
#define MHLINES  12             /* max # lines in main FITS header   */
#define THLINES 240             /* max # lines in FITS Table Header  */

/* User option parameters  */

extern double scan1, scan2;   /*  scan number range                    */
extern int qcont;             /*  qcont=0 for line, =1 for continuum   */
extern int maxrecs;           /*  max allowed records in a file        */
extern int maxWscan;          /*  truncate scan to maxWscan points     */
extern int qvof;              /*  qvof=0 if velocity is 1st axis, freq=1*/ 
extern int prev;              /*  prev=1 if preview desired, no FITS then */
extern int y2k;               /*  y2k=1 if force y2k compliant dates   */
               
/* operation flags */
extern int nthead;           /* =1 if its time to make a new table header */
extern int ftime;            /* =1 if first time thru                     */
extern int kpflag;           /* =1 if kitt-peak (NRAO 12M) data, else =0  */
extern int p11flag;          /* =1 if proto class 11 (PROTO12M), else = 0 */

/*  log file  */

extern FILE *logfile;        
extern int logflag;       

/* temporary file */
extern char tmpname[80];
extern int tmpfileflag, tmpfid;

/* temporary binary table output buffer */
extern char *tbuf;
extern int   tbufflag;

/*  today's date and version  */

extern char *today, rundate[16];
extern char Origin[];
extern struct tm *ltime;             /* local time structure */


/* Translation table structure */

typedef struct trans_table
{   char ttype[16];            /* FITS table column label                  */
    char tunit[12];            /* FITS unit label                          */
    int ncol;                  /* FITS table column number                 */
    char kat[4];               /* kategory of keyword                      */
    int opt;                   /* option: 0=always use, 1=freq ax; 2=vel.ax*/
                               /*         3=sample ax; 4=time ax;          */
                               /*         5=freq ax for cont data          */
    int axis;                  /* if non-zero, matrix axis number          */
    int mform;                 /* multiplicity of format                   */
    char fform;                /* FITS binary format designator (I,E,D,A)  */
    int foff;                  /* byte offset in FITS table row.           */
    int uoff;                  /* byte offset in unipops record class      */
    int ucl;                   /* Unipops class number                     */
    int minf;                  /* multiplicity of input format             */
    char inform;               /* input format designator                  */
    char cfact[12];            /* name of conversion factor                */
    double factor;             /* multiply by this to get FITS units       */
    double offset;             /* constant offset to convert to FITS units */
    int sflag;                 /* index of translation routine             */
                               /*  -1 -> Header only - do not put in table */
                               /*  -2 -> Header Comment line (not in table)*/
                               /*   0 -> standard processing               */
                               /*  >0 -> special routine for processing.   */
    int (*fun)();              /* translation function                     */
    char descr[66];            /* comment field of FITS header             */
 } TRANS_TABLE;
 
 extern TRANS_TABLE *tt;
 extern int ttflag;
 
 extern int Ntrans;              /* number of lines in table           */
 extern int Twidth;              /* width of FITS table row, in bytes  */
 extern int ScanWid;             /* bytes width of scan data matrix    */
 extern int ScanMult;            /* # points width scan data matrix    */
 extern int maxTwid;             /* min required table width for file  */
 extern int maxmaxis1;           /* max val of maxis1 for file         */
 extern float DATAmax, DATAmin;  /*  global max and min for scans      */
 
 extern init_k2f_fun(), init_f2k_fun(); /* tt->fun initializations */
 
/* Structure for construction of the FITS headers. */

typedef struct head_table
{   char htext[82];            /* the entire 80-col line of header       */
    int  icol[2];              /* col numbers at which to insert data.   */
                               /*   =0 if nothing to be inserted.        */
    int  iwid[2];              /* width of data to be inserted.          */
    char kw[10];               /* name of keyword                        */
}  HEAD_TABLE;
extern int hbufflag;

/* Main header lines */

 extern HEAD_TABLE *mh; 
 extern int MHnlines;         /* # lines in Main file header */
 
/* Table header lines */
 extern HEAD_TABLE *th;
 extern int THnlines;        /* # lines in Table header preamble.  */
 extern int THtotal;         /* total # lines in Table header      */
 
 extern int Tfields;         /* number of table fields             */

