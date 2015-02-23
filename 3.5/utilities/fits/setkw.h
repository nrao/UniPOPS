/*  @(#)setkw.h	5.1 06/22/94	*/
/* kw[i] holds the names and types of all keywords that this      */
/*  program recognizes.                                           */

/*  Keywords with no value, ie, "END" or "HIERARCH" are viewed in */
/*  this program as having a logical value of true if they appear.*/
/*  An axis name ending in "#" means an axis name, in which "#"   */
/*    may take values from 1-999.                                 */

/* status = 1 if found in input file, =0 otherwise.               */
/* type:  0=logical, 1=integer, 2=real, 3=characters              */
/* maxi = max allowed axis index                                  */
/* no   = number of values expected (=1 unless complex number)    */
/* func = pointer to function to call when keyword is encountered.*/

struct keyword kw[] = 
/*  keyword    status type maxi  no    func  */
{   "END",        0,    0,  0,    1,    0,   /* (0) fundamental words */
    "SIMPLE",     0,    0,  0,    1,    0,   /*  1                    */
    "BITPIX",     0,    1,  0,    1,    0,   /*  2                    */
    "NAXIS",      0,    1,  0,    1,    0,   /*  3                    */
    "NAXIS#",     0,    1, 99,    1,    0,   /*  4                    */   
    "BLOCKED",    0,    0,  0,    1,    0,   /*  5                    */   
    "BSCALE",     0,    2,  0,    1,    0,   /*  6                    */
    "BZERO",      0,    2,  0,    1,    0,   /*  7                    */
    "BUNIT",      0,    3,  0,    1,    0,   /*  8                    */
    "BLANK",      0,    1,  0,    1,    0,   /*  9                    */
    "OBJECT",     0,    3,  0,    1,    0,   /* 10                    */
    "DATE",       0,    3,  0,    1,    0,   /* 11                    */
    "DATE-OBS",   0,    3,  0,    1,    0,   /* 12                    */
    "ORIGIN",     0,    3,  0,    1,    0,   /* 13                    */
    "INSTRUME",   0,    3,  0,    1,    0,   /* 14                    */
    "TELESCOP",   0,    3,  0,    1,    0,   /* 15                    */
    "OBSERVER",   0,    3,  0,    1,    0,   /* 16                    */
    "COMMENT",    0,    3,  0,    1,    0,   /* 17                    */
    "HISTORY",    0,    3,  0,    1,    0,   /* 18                    */
    "        ",   0,    3,  0,    1,    0,   /* 19                    */
    "CRVAL#",     0,    2, 99,    1,    0,   /* (20)   axis descriptions */
    "CRPIX#",     0,    2, 99,    1,    0,   /* 21                    */
    "CDELT#",     0,    2, 99,    1,    0,   /* 22                    */
    "CTYPE#",     0,    3, 99,    1,    0,   /* 23                    */ 
    "CROTA#",     0,    2, 99,    1,    0,   /* 24                    */
    "DATAMIN",    0,    2,  0,    1,    0,   /* 25                    */
    "DATAMAX",    0,    2,  0,    1,    0,   /* 26                    */
    "EPOCH",      0,    2,  0,    1,    0,   /* 27                    */
    "GROUPS",     0,    0,  0,    1,    0,   /* (28)  Random Groups   */
    "PCOUNT",     0,    1,  0,    1,    0,   /* 29                    */ 
    "GCOUNT",     0,    1,  0,    1,    0,   /* 30                    */ 
    "PTYPE#",     0,    3, 99,    1,    0,   /*   Random axes      */
    "PSCAL#",     0,    2, 99,    1,    0,   /* 32                    */  
    "GZERO#",     0,    2, 99,    1,    0,   /* 33                    */  
    "EXTEND",     0,    0,  0,    1,    0,   /* (34) Tables extensions */
    "XTENSION",   0,    3,  0,    1,    0,   /* 35                    */  
    "EXTNAME",    0,    3,  0,    1,    0,   /* 36                    */  
    "EXTVER",     0,    1,  0,    1,    0,   /* 37                    */  
    "EXTLEVEL",   0,    1,  0,    1,    0,   /* 38                    */  
    "TFIELDS",    0,    1,  0,    1,    0,   /* 39                    */  
    "TFORM#",     0,    3,999,    1,    0,   /* 40                    */  
    "TTYPE#",     0,    3,999,    1,    0,   /* 41                    */  
    "TUNIT#",     0,    3,999,    1,    0,   /* 42                    */  
    "TNULL#",     0,    3,999,    1,    0,   /* 43                    */  
    "TBCOL#",     0,    1,999,    1,    0,   /* 44                    */  
    "TSCAL#",     0,    2,999,    1,    0,   /* 45                    */  
    "TZERO#",     0,    2,999,    1,    0,   /* 46                    */  
    "AUTHOR",     0,    3,  0,    1,    0,   /* 47                    */  
    "REFERENC",   0,    3,  0,    1,    0,   /* 48                    */  
    "NMATRIX",    0,    1,  0,    1,    0,   /* S.D. binary tables    */
    "MAXIS",      0,    1,  0,    1,    0,   /* 50                    */  
    "MAXIS#",     0,    1,999,    1,    0,   /* 51                    */  
    "TMATX#",     0,    0,999,    1,    0,   /* 52                    */  
    "TDIM#",      0,    3,999,    1,    0,   /* 53                    */  
    "HIERARCH",   0,    0,  0,    1,    0,   /* hierarchical classes  */
    "SINGLDSH",   0,    0,  0,    1,    0,   /* 55                    */  
    "TIME",       0,    2,  0,    1,    0,   /* SD core keywords      */
    "MOLECULE",   0,    3,  0,    1,    0,   /* 57                    */  
    "TRANSITI",   0,    3,  0,    1,    0,   /* 58                    */  
    "LINE",       0,    3,  0,    1,    0,   /* 59                    */
    "PROJID",     0,    3,  0,    1,    0,   /* SD shared keywords    */
    "OBSID",      0,    3,  0,    1,    0,   /* 61                    */  
    "SCAN",       0,    2,  0,    1,    0,   /*   scan number(real)   */
    "SCAN-NUM",   0,    2,  0,    1,    0,   /*   scan number(real)   */
    "DATE-RED",   0,    3,  0,    1,    0,
    "OBSMODE",    0,    3,  0,    1,    0,
    "TEMPSCAL",   0,    3,  0,    1,    0,
    "FRONTEND",   0,    3,  0,    1,    0,
    "BACKEND",    0,    3,  0,    1,    0,
    "TCAL",       0,    2,  0,    1,    0,
    "THOT",       0,    2,  0,    1,    0,
    "TRX",        0,    2,  0,    1,    0,
    "TSYS",       0,    2,  0,    1,    0,
    "VELDEF",     0,    3,  0,    1,    0,
    "RVSYS",      0,    2,  0,    1,    0,
    "VELO",       0,    2,  0,    1,    0,
    "VELOCITY",   0,    2,  0,    1,    0,
    "DELTAV",     0,    2,  0,    1,    0,
    "OBSFREQ",    0,    2,  0,    1,    0,
    "IMAGFREQ",   0,    2,  0,    1,    0,
    "FREQRES",    0,    2,  0,    1,    0,
    "RESTFREQ",   0,    2,  0,    1,    0,
    "BANDWIDT",   0,    2,  0,    1,    0,
    "LST",        0,    2,  0,    1,    0,
    "UT",         0,    2,  0,    1,    0,
    "OBSTIME",    0,    2,  0,    1,    0, /* integration time */
    "EXPOSURE",   0,    2,  0,    1,    0, /* integration time */
    "AZIMUTH",    0,    2,  0,    1,    0,
    "ELEVATIO",   0,    2,  0,    1,    0,
    "TAU",        0,    2,  0,    1,    0, /* atmos.opacity     */
    "TAU-ATM",    0,    2,  0,    1,    0, /* atmos.opacity     */
    "TAUIMAGE",   0,    2,  0,    1,    0,
    "TAUZENIT",   0,    2,  0,    1,    0,
    "HUMIDITY",   0,    2,  0,    1,    0, /* weather data      */
    "TAMBIENT",   0,    2,  0,    1,    0,
    "TOUTSIDE",   0,    2,  0,    1,    0,
    "PRESSURE",   0,    2,  0,    1,    0,
    "DEWPOINT",   0,    2,  0,    1,    0,
    "WINDSPEE",   0,    2,  0,    1,    0,
    "WINDDIRE",   0,    2,  0,    1,    0,
    "BEAMEFF",    0,    2,  0,    1,    0, /* tel.efficiencies  */
    "APEREFF",    0,    2,  0,    1,    0,
    "FORWEFF",    0,    2,  0,    1,    0,
    "ETAL",       0,    2,  0,    1,    0, /* rear spillover    */
    "ETAFSS",     0,    2,  0,    1,    0, /* forward spillover */
    "ANTGAIN",    0,    2,  0,    1,    0,
    "BEAMWIDT",   0,    2,  0,    1,    0, /* antenna beam width */
    "BMAJ",       0,    2,  0,    1,    0, /* tel. beam         */
    "BMIN",       0,    2,  0,    1,    0,
    "BPA",        0,    2,  0,    1,    0,
    "SITELONG",   0,    2,  0,    1,    0, /* geographic loc.   */
    "SITELAT",    0,    2,  0,    1,    0,
    "SITEELEV",   0,    2,  0,    1,    0,
    "EQUINOX",    0,    2,  0,    1,    0, /* coordinate equinox */
    "NPHASE",     0,    1,  0,    1,    0  /* number of obs phases */
} ;
