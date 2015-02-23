/*  @(#)unipops.h	5.2 09/10/98	*/
/*****************************************************************************
 * unipops.h defines the structure of the UNIPOPS spectral line format.      *
 *      Scan structure is determined by the 1st 16 words                     *
 *      Created Dec 13, 1990 by F. Ghigo;   Last modified: 20 Jun 1991       *
 *		23 Jan 92:  change size of cfform to 24, add effint          *
 * When header words are added, do not forget to change the fixed ustruct    *
 * values found in umake.c                                                   *
 *****************************************************************************/
#define SCANLEN 16384    /* max scan length supported by UNIPOPS */
#define SNAMLEN 16       /* # characters in source name    */
#define ONAMLEN 16       /* # characters in observers name */
#define PRCODE 8         /* # characters in project code   */

#ifdef THINK_C
#define DOUBLE  short double
#else
#define DOUBLE  double
#endif

/* Length of UNIPOPS record is  uniclass1.headlen + uniclass1.datalen(bytes)*/

/* UNIPOPS record is organized into up to 16 blocks, one for each class     */
/****************************************************************************/
/* Class zero contains pointers to each class (1-15) in input buffer.       */
/****************************************************************************/
typedef struct uniclass0     
{   short Nheadcls;             /* number of header classes                 */
    short head_ptr[15];         /* pointers to successive classes           */
                                /* pointer= # of doubles (8-byte units) from*/
                                /*  start of class 0                        */
}   UNICLASS0;                  /* defining word for this class             */

/****************************************************************************/
/*  Class 1 countains general information about the scan                    */
/****************************************************************************/
typedef struct uniclass1 /* description                        byte offset  */
{  DOUBLE headlen;      /* total bytes in header                      0     */
   DOUBLE datalen;      /* total bytes in data scan                   8     */
   DOUBLE scan;         /* scan number                               16     */
   char   obsid[8];     /* observer initials (up to 8 chars)         24     */
   char   observer[16]; /* observer name                             32     */
   char   telescop[8];  /* Telescope name - possible values are:     48     */
                        /*  NRAO12M   NRAO43M   NRAO93M   NRAO100M
                            MPI100M   IRAM30M   NRO45M    PMO14M
                            OSO20M    MASS14M   UTX5M     UK-D15M
                            IRAM15M                                         */
   char   projid[8];    /* project ID code                           56     */
   char   object[16];   /* source name                               64     */
   char   obsmode[8];   /* observing mode and type of data.          80     */
                        /*  1st 4 characters are types of data:
                              LINE  CONT  FSAM  PULS
                            last 4 characters are observing modes:
                              PSSW  FQSW  BMSW  TLPW  PLSW  SHSW  LDSW +more*/
                              
   char   frontend[8];  /* front end descriptor (KP only) values:    88     */
                        /*  2C3MMSIS  140CASS  2.7-1.2M  2C3MMSHM
                            21CM4CH   1.2-0.6M 2MM       11CM3CH
                            100-30CM  .8MM     6/25-6CM  25-20CM
                            BOLOMETER 23-17CM  6/2525CM  15-0.6CM           */
   char   backend[8];   /* back end descriptor.  possible values:    96     */
                        /*  DIGITAL    STD A/D   FABRITEK   384ACIII
                            1536HYSP   .03MHZFB  .1MHZFB    1024ACIV
                            .25MHZFB   .5MHZFB   1.MHZFB    2.5MHZFB        */
   char   precis[8];    /* data precision of series                 104     */
                        /*  precis may be one of the following:
                             I*4, R*4, R*8, R*16, C*8, C*16                 */
   DOUBLE savenum;      /* index number in UNIPOPS data base        112     */
   DOUBLE norecord;     /* total number of individual records       120 
                           associated with this scan number (GB only)       */
   DOUBLE recordid;     /* the record id number of this data (GB)   128     */
}  UNICLASS1;

/****************************************************************************/
/*  Class 2 contains pointing and focus info                                */
/****************************************************************************/
typedef struct uniclass2 /* description                        byte offset  */
{  DOUBLE xpoint;        /* total AZ/RA pointing  (KP only) (arcsecs)  0    */
   DOUBLE ypoint;        /* total EL/DEC pointing (KP only) (arcsecs)  8    */
   DOUBLE uxpnt;         /* user AZ/RA pointing correction (arcsecs)  16    */
   DOUBLE uypnt;         /* user EL/DEC pointing correction (arcsecs) 24    */
   DOUBLE ptcon[4];      /* secondary pointing constants              32    */
                         /*  GB: PVLS 1,2, and 3 in arcmins
                             KP: 4 values used.                             */
   DOUBLE orient;        /* orientation of receiver or reflector at   64                                                 the prime focus (degrees E of N)  
                             as seen looking down on the dish               */
   DOUBLE focusr;        /* radial focus  (mm)                        72    */
   DOUBLE focusv;        /* north-south focus (KP only) (mm)          80    */
   DOUBLE focush;        /* east-west focus (KP only) (mm)            88    */
   char pt_model[8];     /* pointing model (KP only)                  96    */
}  UNICLASS2;

/****************************************************************************/
/*  Class 3 contains time and observing parameters.                         */
/****************************************************************************/
typedef struct uniclass3 /* description                        byte offset  */
{  DOUBLE utdate;        /* UT date in format: YYYY.MMDD               0    */
   DOUBLE ut;            /* Universal Time (hours)                     8    */
   DOUBLE lst;           /* Local sidereal time  (hours)              16    */
   DOUBLE norchan;       /* no. polarizations or channel for this     24    */
                         /*   frontend.                                     */
   DOUBLE noswvar;       /* number of switching variables             32    */
   DOUBLE nophase;       /* number of phases per cycle                40    */
   DOUBLE cycllen;       /* length of cycle (seconds)                 48    */
   DOUBLE samprat;       /* length of sample                          56    */
                         /* (a sample may consist of multiple cycles)       */
   char cl11type[8];     /* class 11 type  (KP only)                  64    */
                         /* values are '        ' (original type) and       */
                         /*            'PROTO12M' (12m prototype)           */
   double phaseid;       /* The phase number of this data (GB)        72    */
}  UNICLASS3;

/****************************************************************************/
/*  Class 4:  positions and coordinates                                     */
/****************************************************************************/
typedef struct uniclass4 /* description                        byte offset  */
{  DOUBLE epoch;         /* epoch of coordinate equinox (in years)     0    */
   DOUBLE xsource;       /* commanded X position (degrees)             8    */
   DOUBLE ysource;       /* commanded Y position (degrees)            16    */
   DOUBLE xref;          /* commanded reference X (KP only) (degrees) 24    */
   DOUBLE yref;          /* commanded reference Y (KP only) (degrees) 32    */
   DOUBLE epocra;        /* R.A. at epoch   (degrees)                 40    */
   DOUBLE epocdec;       /* Dec. at epoch   (degrees)                 48    */
   DOUBLE gallong;       /* commanded galactic longitude (degrees)    56    */
   DOUBLE gallat;        /* commanded galactic latitude  (degrees)    64    */
   DOUBLE az;            /* commanded azimuth   - degrees (KP only)   72    */
   DOUBLE el;            /* commanded elevation - degrees (KP only)   80    */
   DOUBLE indx;          /* indicated X-position - degrees            88    */
   DOUBLE indy;          /* indicated Y-position - degrees            96    */
   DOUBLE desorg[3];     /* descriptive origin                       104    */
                         /*  (horizontal position, vertical position,
                              position angle)                               */
   char   coordcd[8];    /* coordinate system code - values:         128    */
                         /*  GALACTIC  1950RADC  EPOCRADC  MEANRADC
                             APPRADC   APPHADC   1950ECL   EPOCECL
                             MEANECL   APPECL    AZEL      USERDEF
                             2000RADC  INDRADC                              */
}  UNICLASS4;

/****************************************************************************/
/*  Class 5:  environmental data                                            */
/****************************************************************************/
typedef struct uniclass5 /* description                        byte offset  */
{  DOUBLE tamb;          /* ambient temperature (Celsius)              0    */
   DOUBLE pressure;      /* Bar. Pressure (cm-Hg) - GB only.           8    */
   DOUBLE humidity;      /* relative humidity (percent) - KP only     16    */
   DOUBLE refrac;        /* index of refraction (KP only)             24    */
   DOUBLE dewpt;         /* Dew Point in Celsius (GB only)            32    */
   DOUBLE mmh2o;         /* atmospheric H2O vapor (mm) - KP only      40    */
}  UNICLASS5;

/****************************************************************************/
/*  Class 6:  Mapping parameters                                            */
/****************************************************************************/
typedef struct uniclass6 /* description                        byte offset  */
{  DOUBLE scanang;       /* map scanning angle (degrees) - KP only     0    */
                         /*  orientation  wrt frame specified by
                             "frame" code. (alternative to DESORG)          */
   DOUBLE xzero;         /* X at map ref zero (degrees) (KP only)      8    */
   DOUBLE yzero;         /* Y at map ref zero (degrees) (KP only)     16    */
   DOUBLE deltax;        /* KP: X cell size (arcsecs)                 24    */
                         /*   KP,GB: horizontal slew rate (arcsec/sec)      */
   DOUBLE deltay;        /* KP: Y cell size (arcsecs)                 32    */
                         /*   KP,GB: horizontal slew rate (arcsec/sec)      */
   DOUBLE nopts;         /* total no.cells in map (KP only)           40    */
   DOUBLE noxpts;        /* number of grid points in X  (KP only)     48    */
   DOUBLE noypts;        /* number of grid points in Y  (KP only)     56    */
   DOUBLE xcell0;        /* starting X grid cell number (KP only)     64    */
   DOUBLE ycell0;        /* starting Y grid cell number (KP only)     72    */
   char   frame[8];      /* XY reference frame code (KP only)         80    */
                         /*  8-char code: 1st 4 tell whether the grid is
                             polar (POLR) or Cartesian (CART);
                             the 2nd 4 tell whether deltaxr and deltayr refer
                             to STEP sizes or SCANning rates.               */
                             
}  UNICLASS6;

/****************************************************************************/
/*  Class 7:  velocity calibration and other data parameters.               */
/****************************************************************************/
typedef struct uniclass7 /* description                        byte offset  */
{  DOUBLE bfwhm;         /* Beam FWHM (arcsec)  (KP only)              0    */
   DOUBLE offscan;       /* off scan number                            8    */
   DOUBLE badchv;        /* bad channel value (KP only)               16    */
                         /*  ant.temp. assigned to defective chans.         */
   DOUBLE rvsys;         /* velocity correction (km/sec)              24    */
                         /* = the Doppler correction for the EarthUs 
                              motion in the source direction wrt the
                              velocity reference frame.                     */
   DOUBLE velocity;      /* velocity wrt reference (km/sec)           32    */
   char   veldef[8];     /* velocity definition and reference         40    */
                         /*  1st 4 characters denotes velocity
                              definition:  RADI, OPTL, or RELV.
                             last 4 characters denotes vel.reference:
                                LSR, HELO, EART, BARI, OBS                  */
   char   typecal[8];    /* type of calibration                       48    */
}  UNICLASS7;

/****************************************************************************/
/*  Class 8:  antenna engineering data (Kitt Peak only).                    */
/****************************************************************************/
typedef struct uniclass8 /* description                        byte offset  */
{  DOUBLE appeff;        /* antenna aperture efficiency                0    */
                         /* (ratio of power detected to power 
                             incident on the antenna                        */
   DOUBLE beameff;       /* antenna main beam efficiency               8    */
   DOUBLE antgain;       /* antenna gain                              16    */
   DOUBLE etal;          /* rear spill & scattering efficiency        24    */
   DOUBLE etafss;        /* forward spill & scattering efficiency     32    */
}  UNICLASS8;

/****************************************************************************/
/*  Class 9G contains Green-Bank-Specific parameters  (140-ft)              */
/****************************************************************************/
typedef struct uniclass9g /* description                       byte offset  */
{  DOUBLE losynth[6];     /* L.O. frequencies (MHz)                    0    */
   DOUBLE loparms[4];     /* If frequencies (MHz)  (LA,LB,LC,LD)      48    */
   DOUBLE levcorr;        /* level correction  (volts)                80    */
   DOUBLE ptfudge[2];     /* pointing fudge (arcmin)                  88    */
   DOUBLE rho;            /* feed offset rotation (degrees)          104    */
   DOUBLE theta;          /* feed offset, lateral (degrees)          112    */
   char   cfform[24];     /* center frequency formulae               120    */
}  UNICLASS9G;

/****************************************************************************/
/*  Class 9K contains Kitt-Peak-Specific parameters   (12 meter)            */
/****************************************************************************/
typedef struct uniclass9k /* description                       byte offset  */
{  DOUBLE synfreq;        /* synthesizer frequency (MHz)               0    */
   DOUBLE lofact;         /* LO factor  (1,3,or 4)                     8    */
   DOUBLE harmonic;       /* Which harmonic of 2GHZ oscillator        16    */
   DOUBLE loif;           /* LO IF  (MHz)                             24    */
   DOUBLE firstif;        /* First IF  (KP only) (MHz)                32    */
   DOUBLE razoff;         /* reference azimuth offset (arcsecs)       40    */
   DOUBLE reloff;         /* reference elevation offset (arcsecs)     48    */
   DOUBLE bmthrow;        /* beam throw  (arcsecs)                    56    */
   DOUBLE bmorent;        /* beam orientation (degrees)               64    */
   DOUBLE baseoff;        /* baseline offset (Kelvin)                 72    */
   DOUBLE obstol;         /* observing tolerance  (arcsec)            80    */
   DOUBLE sideband;       /* Which sideband (2=USB, 3=LSB))           88    */
   DOUBLE wl;             /* Nothing to do with wavelength (mm)       96    */
   DOUBLE gains;          /* scan num of cal data to be applied      104    */
   DOUBLE pbeam[2];       /* Beam offset in plus direction (arcsec)  112    */
   DOUBLE mbeam[2];       /* Beam offset in minus direction (arcsec) 128    */
   DOUBLE sroff[4];       /* RA/Dec offsets  (arcsec)                144    */
   DOUBLE foffsig;        /* Freq. offset, signal (MHz)              176    */
   DOUBLE foffref[2];     /* Freq. offset, reference (MHz)           184    */
}  UNICLASS9K;

/****************************************************************************/
/*  Class 10:  Open parameters (data reduction)  Kitt Peak only             */
/****************************************************************************/
typedef struct uniclass10 /* description                        byte offset */
{  char   openpar[80];    /*                                           0    */
}  UNICLASS10;

/****************************************************************************/
/*  Class 11 is phase data (Kitt Peak only)                                 */
/****************************************************************************/
typedef struct uniclass11 /* description                        byte offset */
{  DOUBLE varvel;         /* variable value                            0    */
   char   vardes[8];      /* variable descriptor                       8    */
   char   phastb[8];      /* phase table                              16    */
}  UNICLASS11;

/****************************************************************************/
/* Prototype Class 11 is phase data (Kitt Peak Only)                        */
/****************************************************************************/
typedef struct uniclass11p /* description                       byte offset */
{  DOUBLE noswvarf;       /* Number of fast switching variables        0    */
   DOUBLE numcyc;         /* Number of slow cycles per scan            8    */
   DOUBLE numcycf;        /* Number of fast cycles per scan           16    */
   DOUBLE nophasef;       /* Number of fast phases per cycle          24    */
   DOUBLE cyclenf;        /* Length of fast cycle                     32    */
   DOUBLE samptimf;       /* Length of fast phase sample              40    */
   DOUBLE varval01;       /* variable value, phase 1                  48    */
   char   vardes01;       /* variable descriptor, phase 1             56    */
   char   phastb01[32];   /* phase table, phase 1                     64    */
   DOUBLE varval02;       /* variable value, phase 2                  96    */
   char   vardes02;       /* variable descriptor, phase 2            104    */
   char   phastb02[32];   /* phase table, phase 2                    112    */
   DOUBLE varval03;       /* variable value, phase 3                 144    */
   char   vardes03;       /* variable descriptor, phase 3            152    */
   char   phastb03[32];   /* phase table, phase 3                    160    */
   DOUBLE varval04;       /* variable value, phase 4                 192    */
   char   vardes04;       /* variable descriptor, phase 4            200    */
   char   phastb04[32];   /* phase table, phase 4                    208    */
   DOUBLE varval05;       /* variable value, phase 5                 240    */
   char   vardes05;       /* variable descriptor, phase 5            248    */
   char   phastb05[32];   /* phase table, phase 5                    256    */
} UNICLASS11P;

/****************************************************************************/
/*  Class 12:  Descriptor block for each receiver channel                   */
/****************************************************************************/
typedef struct uniclass12 /* description                        byte offset */
{  DOUBLE obsfreq;        /* observed frequency   (MHz)                0    */
   DOUBLE restfreq;       /* rest frequency (MHz)                      8    */
                          /* overloaded : 12m cont : data scale factor      */
   DOUBLE freqres;        /* frequency resolution(bw per chan.in MHz) 16    */
                          /* overloaded : 12m cont : noise tube flag        */
   DOUBLE bw;             /* total bandwidth of filter bank in MHz    24    */
   DOUBLE trx;            /* receiver temperature in K (KP only)      32    */
   DOUBLE tcal;           /* calibration temperature (Kelvin)         40    */
   DOUBLE stsys;          /* source system temperature (Kelvin)       48    */
   DOUBLE rtsys;          /* reference system temperature (Kelvin)    56    */
   DOUBLE tsource;        /* temp of radio source (K) - KP only       64    */
   DOUBLE trms;           /* rms of mean source temperature (K)       72    */
   DOUBLE refpt;          /* reference point number (center channel)  80    */
                          /*  NRAO convention: refpt=N/2 + 1, where 
                               N=# of channels                              */
   DOUBLE x0;             /* velocity at the ref. point (km/sec)      88    */
   DOUBLE deltax;         /* stepsize along x-axis                    96    */
                          /*  (usually velocity in km/sec)                  */
   DOUBLE inttime;        /* total scan duration (seconds)           104    */
   DOUBLE noint;          /* number of integrations                  112    */
   DOUBLE spn;            /* starting point number                   120    */
   DOUBLE tauh2o;         /* H2O opacity as computed by a model      128    */
   DOUBLE th2o;           /* H2O temperature (K) - KP only           136    */
   DOUBLE tauo2;          /* O2 opacity computed by a model(KP only) 144    */
   DOUBLE to2;            /* O2 temperature (K) - KP only            152    */
   char   polariz[8];     /* polarization  (KP only)                 160    */
                          /*  1st 2 or 3 chars give type of pol:
                               RC, LC, or LIN
                              last few chars give receiver angle to 
                              a tenth of a degree                           */
   DOUBLE effint;         /* Effective on-source integration time(s) 168    */
   char   rxinfo[16];     /* receiver info (KP only)                 176    */
}  UNICLASS12;

/****************************************************************************/
/*  Class 13:  Reduction parameters                                         */
/****************************************************************************/
typedef struct uniclass13 /* description                        byte offset */
{  DOUBLE nostac;         /* number of scans stacked (averaged)        0    */
   DOUBLE fscan;          /* first scan in stack                       8    */
   DOUBLE lscan;          /* last scan in stack                       16    */
   DOUBLE lamp;           /* line amplitude (KP only)                 24    */
   DOUBLE lwid;           /* line width  (KP only)                    32    */
   DOUBLE ili;            /* integrated line intensity (KP only)      40    */
   DOUBLE drms;           /* RMS noise level (KP only)                48    */
}  UNICLASS13;

/****************************************************************************/
/*  No Class:  Auxiliary parameters used by u2f software.                   */
/****************************************************************************/
typedef struct uniaux     /* description                        byte offset */
{  char   sourcename[20];     /*  [copy of sourcename]     OBJECT       0   */  
   char   obsname[20];        /*  [copy of observers name] OBSERV        */
   char   projcode[10];       /*  [copy of project code]   PROJID        */
   float  time;               /*  TIME   (= UT time of observatiosecs.)  */
   int    istop,istart;
   char   tele[10];           /*  TELESCOP                               */
   char   obsmode[10];        /*  OBSMODE                                */
   char   backend[10];        /*  BACKEND and INSTRUME                   */
   char   dateobs[14];        /*  DATE-OBS                               */
   char   velnam[10];         /*  velocity name = FELO-LSR, etc.         */
   char   veldef[10];         /*  velocity ref,def = RADILSR,etc         */
   int    nreceivers;         /*  no.of receivers in this input r        */
   int    nbeams;             /*  no.of beams for this experiment        */
   int    nchannels;          /*  no.of channels in series               */
   int    maxchan;            /*  max no.of channels for this fil        */
   float  datamax;            /*  DATAMAX (max in series   array)        */
   float  datamin;            /*  DATAMIN (min in series   array)        */
}  UNIAUX;

typedef struct upops          /*  pointers to all the structures        */
{  UNICLASS0  *u0;
   UNICLASS1  *u1;
   UNICLASS2  *u2;
   UNICLASS3  *u3;
   UNICLASS4  *u4;
   UNICLASS5  *u5;
   UNICLASS6  *u6;
   UNICLASS7  *u7;
   UNICLASS8  *u8;
   UNICLASS9G *u9g;
   UNICLASS9K *u9k;
   UNICLASS10 *u10;
   UNICLASS11 *u11;
   UNICLASS11P *u11p;
   UNICLASS12 *u12;
   UNICLASS13 *u13;
   float      *series;
   UNIAUX     *ux;
}  UPOPS;
extern UPOPS up;
extern long upnt[16];    /* pointers for UNIPOPS buffer          */
extern char *ubuf;       /*  buffer for UNIPOPS file             */
extern int ubufflag;     /* =1 if ubuf is allocated              */
extern int ustruct[16];  /* defines structure of UNIPOPS record  */
                         /* set in "ubuf_alloc" in file umake.c  */
                         /* Used to create output record when    */
                         /* translating from FITS to UNIPOPS     */
extern UNIAUX auxbuf;
