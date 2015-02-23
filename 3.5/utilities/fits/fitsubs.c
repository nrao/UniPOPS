/*  @(#)fitsubs.c	5.1 06/22/94	*/
/******************************************************************************/
/* FITSUBS - file with numeric conversion routines for FITS reader and writer */
/******************************************************************************/
#include <math.h>
#include <fcntl.h>
#include <stdio.h>
#include <time.h>

#ifdef THINK_C
 #include <stdlib.h>
 #include <unix.h>
#else 
#include <sys/types.h>
#include <sys/file.h>
#endif
extern FILE *logfile;

 /**************************************************************************
  * IEEE_TO_DUB - convert IEEE double (8-bytes in x) to local double       *
  **************************************************************************/
#define DMASK 0x7FF00000
 double ieee_to_dub(zx, iflg)
 char *zx;
 int *iflg;                    /* return 1 if NaN or infinity, else 0 */
 {
    unsigned long int ii,jj, x[2];
    char *cx,*cy;
    double xx,yy,zz, sign, *dd, pow();
    int i;

    for(i=0,cx=zx,cy=(char *)x; i<8; i++)  *cy++ = *cx++;

    *iflg = 0;
    if( (x[0]&DMASK)==DMASK) *iflg = 1; /* check for NaN */
    else
       if( (x[0]&DMASK)==0)             /* check for zero  */
       {  yy = 0.0;
          return(yy);
       }
    
  /* fprintf(logfile,"\nIEEE_2_DUB: %10lx %10lx, nan=%d ", x[0], x[1],(*iflg)); 
    */
    
#ifdef THINK_C
    sign = 1.0;
    ii = x[0] & 0x80000000;     /* get sign bit */
    if(ii!=0) sign = -1.0;
    
    ii = x[0] & 0x7FF00000;     /* get exponent  */
    xx = ii;
    xx /= 1048576.;
    zz = sign * pow((double)2.0, (xx-1023.));
    
    xx = x[1];                 /* low-order mantissa */
    ii = x[0] & 0xFFFFF;       /* high-order mantissa */
    yy = ii;
    
    yy *= pow((double)2.0, (double)32.0);  
    xx += yy;
    xx /= pow((double)2.0, (double)52.0);
    
    yy = zz * (1.0 + xx);

#else
    dd = (double *)x;
    yy = *dd;
#endif
    /* fprintf(logfile, " %8.4E", yy);  /* debug printout */
    return(yy);    
 }

/*********************************************************************
 * IEEE_TO_REAL - convert IEEE 4-byte real to local floating pt.     *
 *********************************************************************/
#define FLMASK 0x7F800000
 float ieee_to_real(x, iflg)
 char *x;
 int *iflg;       /* iflg=1 if NaN or infinity, else 0 */
 {
    float xx;
    long *ll, xl;
    int i;
    char *cb;

    for(i=0,cb=(char *)&xx; i<4; i++)  *cb++ = x[i];

    ll = (long *) &xx;
    *iflg = 0;
    xl = *ll;
    if( (xl&FLMASK)==FLMASK) 
    {  *iflg = 1;                  /* check for Inf or NaN */
       (*ll) = FLMASK;          /* set to +Inf   */
    }
    else  if ( (xl&FLMASK)==0)    /* check for IEEE zero */
          {  xx = 0.0;
             return(xx);
          }
    /* fprintf(logfile,"\nIEEE2REAL: %8.4E, %10x ",
            xx, xl); */
    return(xx);
 }

/*********************************************************************
 * REAL_TO_IEEE_ - convert local real to 4-byte IEEE.                *
 *********************************************************************/
 real_to_ieee(r,a, iflg)  /* put local real into IEEE form */
float r;
char *a;
int iflg;                  /* iflg=1 indicates invalid data */
{
   long *xx;
   char *cf;
   float xf;
   int i;

   xx = (long *)&xf;
   xf = r;

   if(iflg!=0)
       *xx = 0xFFFFFFFF;    /* set IEEE all bits on for invalid data */

   for(i=0, cf=(char *)xx; i<4; i++)  a[i] = *cf++;
}

/*********************************************************************
 * IEEE_TO_INT - convert IEEE 2-byte integer to local short integer. *
 *********************************************************************/
 short ieee_to_int(x, iflg)
 short *x;
 int *iflg;
 {
    *iflg = 0;
    if( (*x & 0x8000) == 0x8000)  *iflg = 1;
    return(*x);
 }

/*********************************************************************
 * INT_TO_IEEE_ - convert local integer to 2-byte IEEE.              *
 *********************************************************************/
 int_to_ieee(r,a, iflg)  /* put local real into IEEE form */
short r,*a;
int iflg;
{
   /* local integer is IEEE, so this is easy. */

   *a = r;

  /* if invalid, set to -32768 */
   if(iflg==1)   *a = 0x8000;
}

/*********************************************************************
 * DUB_TO_IEEE - convert local double to IEEE 8-byte double.         *
 *   d is the local double;  a is address where IEEE form is put.    *
 *   iflg=0 indicates d is valid number; iflg=1 means it is invalid. *
 *   If iflg=1, uof tells what to use as invalid value.              *
 *    uof=0 means set output value to Infinity (0x7FF00000, 00000000)*
 *    uof=1 means set output to -Nan (0xFFFFFFFF,FFFFFFFF)           *
 *   The UNIPOPS record uses Inf, FITS uses -NaN.                    *
 *  (Note: when compiling on the MacIntosh in Think-C, do not set    *
 *   the 68881 compiler switch.)                                     *
 *********************************************************************/
 dub_to_ieee(d,a, iflg, uof)
double d;
unsigned long a[];
int iflg, uof;            
{
     int iexp;
     unsigned long p1,p2, signbit, pexp, *uu;
     double xx,yy,z20,z52,zz;

#ifdef THINK_C   /* but not so much fun for MacIntoshes */
     if(d<0.0) {   signbit=0x80000000;  xx = -d; }
     else      {   signbit=0;           xx = d;  }
     
     yy = log(xx)/log((double)2.0);  /* get exponent */
     iexp = yy;
     zz = iexp;
     pexp = yy + 1023;
     pexp = pexp*1048576;
     pexp = (pexp & 0x7FF00000) | signbit;
     
     xx /= pow((double)2.0, zz);
     xx--;
     z20 = xx * pow((double)2.0, (double)20.0); /* hi-order 20 bits */
     p1  = z20;
     z52 = xx * pow((double)2.0, (double)52.0);
     zz  = z52 - p1*pow((double)2.0, (double)32.0); /* lo-order 32 bits */
     p2  = zz;
     
     a[0] = (p1&0x000FFFFF) | pexp;
     a[1] = p2;
     
#else
   /* for SUN and MASSCOMPS, this is easy.  */
    xx = d;
    uu = (unsigned long *)&xx;
    a[0] = uu[0];  a[1] = uu[1];
#endif
   if(iflg==1)              /* set invalid data values */
   {  if(uof==1)
      { a[0] = 0xFFFFFFFF;   /* set all bits on for FITS invalid data   */
        a[1] = 0xFFFFFFFF;
      }
      else
      { a[0] = 0x7FF00000;   /* set IEEE Infinity for UNIPOPS bad data  */
        a[1] = 0;
      }
   }
}  /*****end of dub_to_ieee************************************/


/***************************************************************/
form_size(c)  /* get width of format, depending on code letter */
char c;
{
    int wid;
    
    switch(c)
    {   case 'A' :  wid=1;  break;  /* ASCII string   */
        case 'I' :  wid=2;  break;  /* 16-bit integer */
        case 'E' :  wid=4;  break;  /* IEEE float     */
        case 'D' :  wid=8;  break;  /* IEEE double    */
        
        default : wid=0;
     }
     return(wid);
}

