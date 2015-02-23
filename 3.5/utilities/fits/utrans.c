/********************************************************************
 * UTRANS  - this file has all the functions for converting data    *
 *    from FITS to UNIPOPS or vice-versa.                           *
 *    Pointers to each function are put into the tt struct by       *
 *    either init_f2u_fun or init_u2f_fun (called by inittrans when *
 *    the tt struct is read in from the _trans table.               *
 ********************************************************************/
#include <stdio.h>
#include "f2u.h"
#include "unipops.h"
#include "u2fglob.h"

extern long rrec;          /* defined in udata.c */

/*  dmons array used below in mjd2day  */
int dmons[12] = {31,28,31,30,31,30,31,31,30,31,30,31};

/********************************************************************
 * INIT_u2f_FUN - initialize the Unipops-to-fits conversion functns.*
 ********************************************************************/
init_u2f_fun()
{
    int ii;
    int norm_u2f(), velax_u2f(), longproc_u2f(), latproc_u2f();
    int rxnum_u2f(), veldef_u2f(), utdate_u2f();
    int Lscan_u2f(), radecsys_u2f(), null_u2f();
    int dmax_u2f(),  dmin_u2f(), restfreq_u2f(), freqres_u2f();
    int dsf_u2f(), nt_u2f();
    
    fprintf(logfile,"\n Initializing u2f functions. ");
    
    for(ii=0; ii<Ntrans; ii++)
    {
        switch ((tt+ii)->sflag)
        {
             /* COORDCD, FREQRES, coordinates: normal conv in u2f */
          case  9 :
          case  0 : (tt+ii)->fun = norm_u2f;       break;
          case  1 : (tt+ii)->fun = velax_u2f;      break;
          case  2 : (tt+ii)->fun = longproc_u2f;   break;
          case  3 : (tt+ii)->fun = latproc_u2f;    break;  
          case  4 : (tt+ii)->fun = rxnum_u2f;      break;
          case  5 : (tt+ii)->fun = veldef_u2f;     break;
          case  6 : (tt+ii)->fun = radecsys_u2f;   break;
          case  7 : (tt+ii)->fun = utdate_u2f;     break;
          case  8 : (tt+ii)->fun = restfreq_u2f;   break;
          case 10 : (tt+ii)->fun = freqres_u2f;    break;
          case 11 : (tt+ii)->fun = dsf_u2f;        break;
          case 12 : (tt+ii)->fun = nt_u2f;         break;
          case 17 : (tt+ii)->fun = dmax_u2f;       break;
          case 18 : (tt+ii)->fun = dmin_u2f;       break;    
          case 20 : (tt+ii)->fun = Lscan_u2f;      break;
          default : (tt+ii)->fun = 0; 
        }
     }
}

/*********************************************************************
 * INIT_f2u_FUN - initializes the fits-to-unipops conversion functns.*
 *********************************************************************/
init_f2u_fun()
{
    int ii;
    int norm_f2u(), velax_f2u(), longproc_f2u(), latproc_f2u();
    int rxnum_f2u(), veldef_f2u(), utdate_f2u();
    int Lscan_f2u(), coords_f2u(), null_f2u();
    int dmax_f2u(),  dmin_f2u(), radecsys_f2u();
    int restfreq_f2u(), freqres_f2u(), dsf_f2u(), nt_f2u();
    
    fprintf(logfile,"\n Initializing f2u functions.");
    fflush(logfile);
    
    for(ii=0; ii<Ntrans; ii++)
    {  
       switch((tt+ii)->sflag)
       {
          case  1 : 
          case  0 : (tt+ii)->fun = norm_f2u;       break;
          case  2 : (tt+ii)->fun = longproc_f2u;   break;    
          case  3 : (tt+ii)->fun = latproc_f2u;    break; 
          case  4 : (tt+ii)->fun = rxnum_f2u;      break; 
          case  5 : (tt+ii)->fun = veldef_f2u;     break;
          case  6 : (tt+ii)->fun = radecsys_f2u;   break; 
          case  7 : (tt+ii)->fun = utdate_f2u;     break;
          case  8 : (tt+ii)->fun = restfreq_f2u;   break;
          case  9 : (tt+ii)->fun = coords_f2u;     break;
          case 10 : (tt+ii)->fun = freqres_f2u;    break;
          case 11 : (tt+ii)->fun = dsf_f2u;        break;
          case 12 : (tt+ii)->fun = nt_f2u;         break;
          case 17 : (tt+ii)->fun = dmax_f2u;       break;
          case 18 : (tt+ii)->fun = dmin_f2u;       break;
          case 20 : (tt+ii)->fun = Lscan_f2u;      break;
          default : (tt+ii)->fun = 0;
       }   
    }
    
} /****end of init_f2u_fun********************************************/


/***********************************************************************
 * norm_u2f  is the normal translation procedure, for parameters       *
 *   whose sflag is zero.                                              *
 *   Parameters types may be converted                                 *
 *         - I, E, or D may become  I, E, or D                         *
 *         - but A can only become A.                                  *
 ***********************************************************************/
norm_u2f(i,vf,cbuf,sht,flt,dub,intype,inmult, tbuf)
int i, inmult, *vf;
char *cbuf, *tbuf, intype;
short *sht;
float *flt;
double *dub;
{
     int j,jj,mm,nn, fwid, ffwid, nff;
     char tmpstr[16];
     
     fwid = form_size((tt+i)->fform);
     ffwid = (tt+i)->mform * fwid;
     nff=(tt+i)->foff;
     /* fprintf(logfile," foff=%d, vf=%d ", nff, vf[0]); */
     
     switch((tt+i)->fform)
     {  case 'A' :  
            for(j=0; j<ffwid; j++)   tbuf[j+nff] = cbuf[j];
            break;
        case 'I' :
            for(j=0; j<(tt+i)->mform; j++)
            {  int_to_ieee(sht[j],tmpstr, vf[j]);
               for(mm=0; mm<fwid; mm++)
                tbuf[(j*fwid)+nff+mm] = tmpstr[mm];
            }
          break;

        case 'E' :
            for(j=0; j<(tt+i)->mform; j++)
            {  real_to_ieee(flt[j],tmpstr, vf[j]);
               for(mm=0; mm<fwid; mm++)
                 tbuf[(j*fwid)+nff+mm] = tmpstr[mm];
            }
          break;
           
        case 'D' :
            for(j=0; j<(tt+i)->mform; j++)
            {  dub_to_ieee(dub[j],tmpstr, vf[j], 1);
               for(mm=0; mm<fwid; mm++)
                tbuf[(j*fwid)+nff+mm] = tmpstr[mm];
            }
     }
} /*****end of norm_u2f*************************************************/


/**********************************************************************
 * NORM_f2u - standard FITS to UNIPOPS conversion.                    *
 *   all items with sflag=0 use this function.                        *
 **********************************************************************/
norm_f2u(i,vf,cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult, *vf;
char   *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{
   char st[64], outtype, *ccb, *kkb, tmpstr[16];
   int inwid, outwid, outmult, mm, nn;
   
     inwid   = inmult * form_size(intype);
     outtype = (tt+i)->inform;
     outmult = (tt+i)->minf;
     outwid  = outmult * form_size(outtype);
     kkb = ubuf + (tt+i)->uoff + upnt[(tt+i)->ucl];
     
     switch(outtype)
     {  case 'A' :                                      /* Ascii data */
           for(mm=0,ccb=cb; mm<outwid; mm++)  *kkb++ = *ccb++;
           break;

        case 'I' : 
                for(mm=0; mm<outmult; mm++,kkb+=2)
                     int_to_ieee(sht[mm], kkb, vf[mm]); 
                break;
               
        case 'E' :
               for(mm=0; mm<outmult; mm++, kkb+=4)
                   real_to_ieee(flt[mm], kkb, vf[mm]);
               break;
        case 'D' :
               for(mm=0; mm<outmult; mm++, kkb+=8)
                   dub_to_ieee(dub[mm], kkb, vf[mm],0);
                
      } /*****end of switch******/
     
} /****end of norm_f2u***********************************************/


/**********************************************************************
 * NULL_u2f - UNIPOPS to FITS  - null case (ie, no conversion!)       *
 **********************************************************************/
null_u2f(i,vf, cb,sht,flt,dub,intype,inmult,tbuf)
int i, inmult, *vf;
char *cb, intype, *tbuf;
short  *sht;
float  *flt;
double *dub;
{
   /* fprintf(logfile,"\n Called NULL_u2f "); */
}

/**********************************************************************
 * NULL_f2u - FITS to UNIPOPS  - null case (ie, no conversion!)       *
 **********************************************************************/
null_f2u(i,vf, cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult, *vf;
char *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{
   /* fprintf(logfile,"\n Called NULL_f2u "); */
}


/**********************************************************************
 * LONGPROC_u2f - UNIPOPS to FITS  - Process long-like coordinates.   *
 *   sflag = 2                                                        *
 * The first call to longproc_u2f or latproc_u2f calculates all the   *
 *  FITS coordinate parameters for this scan.                         *
 *  One param is set for each call, depending on ttype or axis.       *
 **********************************************************************/
static int OldRecNum = -1;    /* calculate coordinate parameters only 
                                 if rrec (selected input rec #) changes */
static char ctype2[10], ctype3[10];
static char radecsys[10];
static double crval2, crval3, crpix2, crpix3, crota3;
static double cdelt2, cdelt3, equinox, azim, elev;
static double rxnum;

longproc_u2f(i,vf, cb,sht,flt,dub,intype,inmult,tbuf)
int i, inmult, *vf;
char *cb, intype, *tbuf;
short  *sht;
float  *flt;
double *dub;
{
   if(rrec != OldRecNum)
   {  do_coords();
      OldRecNum = rrec;
   }
   vf[0] = 0;       /* set valid flag to 0 (means valid) */
   
   switch ((tt+i)->axis)
   {
      case 22 :  strncpy(cb,ctype2, (long)8);     break;
      case 23 :  flt[0] = dub[0] = cdelt2;        break;
      case 24 :  flt[0] = dub[0] = crpix2;        break;
      case 25 :  flt[0] = dub[0] = crval2;        break;
      default :
                 if(strncmp((tt+i)->ttype, "EQUINOX", (long)7)==0)
                                          flt[0] = dub[0] = equinox;
                 else
                 if(strncmp((tt+i)->ttype, "RADECSYS", (long)8)==0)
                                   strncpy(cb, radecsys, (long)8);
                                              
                 else vf[0] = 1;  /* data invalid if none of the above */
   }
   /* let norm do the rest of the work ! */
   
   norm_u2f(i,vf, cb,sht,flt,dub,intype,inmult,tbuf);
}


/**********************************************************************
 * LATPROC_u2f - UNIPOPS to FITS  - process latitude coordinates      *
 *   sflag = 3                                                        *
 **********************************************************************/
latproc_u2f(i,vf, cb,sht,flt,dub,intype,inmult,tbuf)
int i, inmult, *vf;
char *cb, intype, *tbuf;
short  *sht;
float  *flt;
double *dub;
{
   /* fprintf(logfile,"\n Called latproc_u2f "); */
   if(rrec != OldRecNum)
   {  do_coords();
      OldRecNum = rrec;
   }
   vf[0] = 0;       /* set valid flag to 0 (means valid) */
   
   switch ((tt+i)->axis)
   {
      case 32 :  strncpy(cb,ctype3, (long)8);     break;
      case 33 :  flt[0] = dub[0] = cdelt3;        break;
      case 34 :  flt[0] = dub[0] = crpix3;        break;
      case 35 :  flt[0] = dub[0] = crval3;        break;
      case 36 :  flt[0] = dub[0] = crota3;        break;
      
      default :  vf[0] = 1;  /* data invalid if none of the above */
   }
   /* let norm do the rest of the work ! */
   
   norm_u2f(i,vf, cb,sht,flt,dub,intype,inmult,tbuf);

}


/****************************************************************************
 * do_coords - get all FITS sky coordinate parameters, given UNIPOPS record.*
 ****************************************************************************/
do_coords()
{
  char tmp[16];
  int vf[4];
  double ieee_to_dub();

  /*  set common defaults */
   strcpy(ctype2,   "RA---ARC");
   strcpy(ctype3,   "DEC--ARC");
   strcpy(radecsys, "FK4     ");
   crota3 = 0.0;
   crval2 = ieee_to_dub(&up.u4->xsource, vf);
   crval3 = ieee_to_dub(&up.u4->ysource, vf);
   crpix2 = crpix3 = 1.0;
   cdelt2 = cdelt3 = 0.0;  
   equinox = ieee_to_dub(&up.u4->epoch, vf);
   
  /* do special cases */
   if(strncmp(up.u4->coordcd, "APPRADC", (long)7)==0)
   {  strcpy(radecsys,"GAPPT");   return(0);
   }
   if((strncmp(up.u4->coordcd, "APPHADC", (long)7)==0)
    ||(strncmp(up.u4->coordcd, "INDRADC", (long)7)==0)
    ||(strncmp(up.u4->coordcd, "AZEL", (long)4)==0) )
   {  crval2 = ieee_to_dub(&up.u4->epocra, vf);
      crval3 = ieee_to_dub(&up.u4->epocdec, vf);
      return(0);
   }
   if(strncmp(up.u4->coordcd, "2000RADC", (long)8)==0)
   {  strcpy(radecsys, "FK5");
      return(0);
   }
   if((strncmp(up.u4->coordcd, "1950ECL", (long)7)==0)
    ||(strncmp(up.u4->coordcd, "EPOCECL", (long)7)==0)
    ||(strncmp(up.u4->coordcd, "MEANECL", (long)7)==0) )
    {  strcpy(ctype2, "ELON-ARC");
       strcpy(ctype3, "ELAT-ARC");
       return(0);
    }
    if(strncmp(up.u4->coordcd, "APPECL", (long)6)==0)
    {  strcpy(ctype2, "ELON-ARC");
       strcpy(ctype3, "ELAT-ARC");
       strcpy(radecsys, "GAPPT");
       return(0);
    }
    if(strncmp(up.u4->coordcd, "GALACTIC", (long)8)==0)
    {  equinox = 1950.0;
       strcpy(ctype2, "GLON-ARC");
       strcpy(ctype3, "GLAT-ARC");
       return(0);
    }
    if(strncmp(up.u4->coordcd, "USERDEF", (long)7)==0)
    {  equinox = 1950.0;
       crval2 = ieee_to_dub(&up.u4->desorg[0], vf);
       crval3 = ieee_to_dub(&up.u4->desorg[1], vf);
       crota3 = ieee_to_dub(&up.u4->desorg[2], vf);
       crpix2 = crpix3 = 0.0;
       cdelt2 = ieee_to_dub(&up.u4->xsource, vf);
       cdelt3 = ieee_to_dub(&up.u4->ysource, vf);
       fprintf(logfile,"\nDO_COORDS: USERDEF: desorg= %8.4e, %8.4e, %8.4e ",
             crval2, crval3, crota3);
       return(0);
    }
}

/**********************************************************************
 * LONGPROC_f2u - FITS to UNIPOPS  - Process long-like coordinates.   *
 *   sflag = 2 - convert CTYPE2, CRVAL2, CRPIX2, CDELT2, EQUINOX.     *
 **********************************************************************/
longproc_f2u(i,vf, cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult, *vf;
char *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{

  /* move equinox */
     equinox = dub[0];
     if(strncmp((tt+i)->ttype, "EQUINOX", (long)7)==0)
         dub_to_ieee(dub[0], &up.u4->epoch,0,0);
         
     if(strncmp((tt+i)->ttype, "CTYPE2", (long)6)==0)
         strncpy(ctype2, cb, (long)8);
     if(strncmp((tt+i)->ttype, "CRVAL2", (long)6)==0) crval2 = dub[0];
     if(strncmp((tt+i)->ttype, "CRPIX2", (long)6)==0) crpix2 = dub[0];
     if(strncmp((tt+i)->ttype, "CDELT2", (long)6)==0) cdelt2 = dub[0];
     
}

/**********************************************************************
 * LATPROC_f2u - FITS to UNIPOPS  - process latitude coordinates      *
 *  sflag = 3  - get CTYPE3,CRVAL3,CRPIX3,CDELT3,CROTA3               *
 **********************************************************************/
latproc_f2u(i,vf, cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult, *vf;
char *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{
     if(strncmp((tt+i)->ttype, "CTYPE3", (long)6)==0)
         strncpy(ctype3, cb, (long)8);
     if(strncmp((tt+i)->ttype, "CRVAL3", (long)6)==0) crval3 = dub[0];
     if(strncmp((tt+i)->ttype, "CRPIX3", (long)6)==0) crpix3 = dub[0];
     if(strncmp((tt+i)->ttype, "CDELT3", (long)6)==0) cdelt3 = dub[0];
     if(strncmp((tt+i)->ttype, "CROTA3", (long)6)==0) crota3 = dub[0];
}

/***********************************************************************
 * rxnum_u2f - insert the receiver index number in the output array.   *
 *   sflag = 4                                                         *
 ***********************************************************************/
rxnum_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, tbuf)
int i, inmult,*vf;
char *cbuf,*tbuf, intype;
short *sht;
float *flt;
double *dub;
{
    int j,jj, nff,mm;
    long ll;
    float rx;
    char tmpstr[8];
    
    ll = flt[0];
    ll = ll % 100;  /* get the hundredths from the scan number.*/
                    /* the scale factor of 100 has already been applied. */
    rx = ll;
    real_to_ieee(rx, tmpstr, vf[0]);
    nff = (tt+i)->foff;
    for(mm=0; mm<4; mm++)
           tbuf[nff+mm] = tmpstr[mm];
} 


/**********************************************************************
 * RXNUM_f2u - FITS to UNIPOPS conversion - get receiver number.      *
 *  sflag = 4                                                         *
 **********************************************************************/
rxnum_f2u(i,vf,cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult, *vf;
char   *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{
   rxnum = dub[0];
}


/**********************************************************************
 * velax_u2f - insert the velocity axis name                          *
 *   sflag = 1                                                        *
 **********************************************************************/
velax_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, tbuf)
int i, inmult,*vf;
char *cbuf,*tbuf, intype;
short *sht;
float *flt;
double *dub;
{
    int nff,mm, jj;
    char vname[10], tmp[4];
    
  /* label axis as fictitious velocity, i.e., "FELO-" */

    strcpy(vname,"FELO-");
    jj=4;
    if(( *(cbuf+jj) == ' ')       /* skip blank space or "-" if present */
       ||( *(cbuf+jj) == '-'))   jj=5;
    strncpy(tmp, (cbuf+jj), (long)3);
    tmp[3]=0;
  /* earth centered should become "GEO" */
    if(strcmp(tmp,"EAR")==0)  strcpy(tmp,"GEO");
    strcat(vname,tmp);
    
    nff = (tt+i)->foff;
    for(mm=0; mm<(tt+i)->mform; mm++)  tbuf[nff+mm] = vname[mm];
    return(0);
} 


/*************************************************************************
 * velax_f2u - Translate velocity axis name.                             *
 *************************************************************************/
velax_f2u(i,vf,cb, sht, flt, dub, intype, inmult, ubuf)
int i, inmult, *vf;
char *cb,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
    
} 


/*************************************************************************
 * VELDEF_u2f - insert the veldef in the output array.                   *
 *   sflag = 5                                                           *
 *************************************************************************/
veldef_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, tbuf)
int i, inmult,*vf;
char *cbuf,*tbuf, intype;
short *sht;
float *flt;
double *dub;
{
    int j,jj, nff,mm;
    char tmp[16], tmp2[16];
    
    strncpy(tmp,cbuf, (long)8);

  /* FITS form has hyphen in it. */
    jj=4;
    if( (tmp[jj]==' ')||(tmp[jj]=='-'))  jj=5;
    strncpy(tmp2, (tmp+jj), (long)3);
    tmp2[3]=0;
    tmp[4]=0;
    
    /* Earth centered case should become "GEO" */
    if(strcmp(tmp2,"EAR")==0) strcpy(tmp2, "GEO");
    strcat(tmp,"-");
    strcat(tmp,tmp2);

    fprintf(logfile,"\nVELdef: %s, %s ", tmp, tmp2);

    nff = (tt+i)->foff;
    for(mm=0; mm<8; mm++)
         tbuf[nff+mm] = tmp[mm]; 
} 



/**********************************************************************
 * VELDEF_f2u - FITS to UNIPOPS conversion - get velocity definition. *
 *   Converts from FITS form of veldef, e.g.,  RADI-LSR  or  OPTL-GEO *
 *   to UNIPOPS form:  RADILSR  or  OPTLEART                          *
 **********************************************************************/
veldef_f2u(i,vf,cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult,*vf;
char   *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{
    char tmp1[10],tmp2[8];
    int jj;
    
    jj=4;
    
    strncpy(tmp1,cb,(long)4);
    if((cb[4]=='-')||(cb[4]==' ')) jj=5;
    
    strncpy(tmp2, (cb+jj), (long)3);
    tmp2[3]=' ';
    tmp1[4]=tmp2[4]=0;
    
    switch(tmp2[0])
    {   case 'L' :  strcat(tmp1,"LSR ");  break;
        case 'H' :  strcat(tmp1,"HELO");  break;
        case 'G' :
        case 'E' :  strcat(tmp1,"EART");  break;
        case 'B' :  strcat(tmp1,"BARI");  break;
        case 'O' :  strcat(tmp1,"OBS ");  break;
        
        default :   strcat(tmp1, tmp2);
    }
    tmp1[8]=0;
    strcpy( up.u7->veldef, tmp1);
}

/**********************************************************************
 * COORDS_f2u - FITS to UNIPOPS conversion - get position data.       *
 *   sflag = 9                                                        *
 **********************************************************************/
coords_f2u(i,vf,cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult,*vf;
char   *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{
   norm_f2u(i,vf,cb,sht,flt,dub,intype,inmult,ubuf);
} 

/*************************************************************************
 * radecsys_u2f - convert UNIPOPS COORDCD to FITS RADECSYS.              *
 *  sflag = 6                                                            *
 * For definition of RADECSYS codes, see Hanisch and Wells.              *
 * Note this may depend on other coordinate parameters in the f2u case.  *
 *************************************************************************/
radecsys_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, tbuf)
int i, inmult,*vf;
char *cbuf,*tbuf, intype;
short *sht;
float *flt;
double *dub;
{
    int nff, mm;
    char tmp[10];
    double eqnx, ieee_to_dub();
    
    eqnx = ieee_to_dub(&up.u4->epoch, vf);

    strcpy(tmp, "FK4      ");   /* default coordinate system */
    
    if(strncmp(cbuf,"APP", (long)3)==0)  
                     strcpy(tmp, "GAPPT");   /* apparent place */
                     
    else
      if(strncmp(cbuf,"2000", (long)4)==0)
                     strcpy(tmp, "FK5");     /* J2000 system */
      else if(strncmp(cbuf,"EPOCRADC", (long)8)
              &&(eqnx == 2000.0))  strcpy(tmp, "FK5");
        
    nff = (tt+i)->foff;
    for(mm=0; mm<8; mm++)
         tbuf[nff+mm] = tmp[mm]; 
}

/*************************************************************************
 * radecsys_f2u - store FITS RADECSYS in local variable.                 *
 *  sflag = 6                                                            *
 * For definition of RADECSYS codes, see Hanisch and Wells.              *
 * Note this depends on other coordinate parameters in the f2u case.     *
 *************************************************************************/
radecsys_f2u(i,vf,cbuf, sht, flt, dub, intype, inmult, tbuf)
int i, inmult,*vf;
char *cbuf,*tbuf, intype;
short *sht;
float *flt;
double *dub;
{
}

/**********************************************************************
 * LSCAN_f2u - FITS to KEEP conversion - process the scan data.       *
 **********************************************************************/
Lscan_f2u(i,vf,cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult,*vf;
char   *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{
     long scanlen, ii;
     int maxlen;
     float *fb, ff, ieee_to_real();
     
   /* get length of scan  */
     for(ii=0, scanlen=1; ii<Naxd; ii++)  scanlen *= axd[ii].maxis;

   /* is it longer than SCANLEN (max allowable for UNIPOPS) */
    maxlen = SCANLEN;
    if(scanlen>maxlen)
    {  fprintf(logfile, "\nOutput scan (size %ld) truncated to %d ",
               scanlen, maxlen);
       scanlen = maxlen;
    }
    fb = (float *)cb;
    fprintf(logfile,"\nLSCAN_F2U: len=%ld f0=%8.5f f1=%8.5f ",
       scanlen, ieee_to_real(fb,vf), ieee_to_real(&fb[1],vf));
    fflush(logfile);
    
    for(ii=0; ii<scanlen; ii++)
    {  ff = ieee_to_real((fb+ii), vf);
       real_to_ieee( ff, (up.series+ii), vf[0]);
    }
}



/***********************************************************************
 * DMAX_F2U - FITS to UNIPOPS conversion - get DATAMAX and put it into *
 *   up.ux->datamax for use by the summary (-s) option                 *
 ***********************************************************************/
 dmax_f2u(i,vf,cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult,*vf;
char   *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{
   up.ux->datamax = dub[0];
}


/***********************************************************************
 * DMIN_F2U - FITS to UNIPOPS conversion - get DATAMIN and put it into *
 *   up.ux->datamin for use by the summary (-s) option                 *
 ***********************************************************************/
 dmin_f2u(i,vf,cb,sht,flt,dub,intype,inmult,ubuf)
int i, inmult,*vf;
char   *cb, intype, *ubuf;
short  *sht;
float  *flt;
double *dub;
{
   up.ux->datamin = dub[0];
}



/*************************************************************************
 * UTDATE_u2f - insert the Date in the output array.                     *
 *  Convert UNIPOPS date in form YYYY.MMDD (d.p.number)                  *
 *  FITS format DATE-OBS string:  yyyy-mm-dd                             *
 *   sflag = 7                                                           *
 *************************************************************************/
utdate_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, tbuf)
int i, inmult,*vf;
char *cbuf,*tbuf, intype;
short *sht;
float *flt;
double *dub;
{
    int nff,mm;
    long yr, day, mmdd, mo;
    double dd;
    char tmpstr[10];
    
    /* UNIPOPS UT date is in form:  YYYY.MMDD */
    yr = dub[0];    /* integer year */
    dd = dub[0] - yr;
    mmdd = (dd*10000.) + 0.48;
    mo = mmdd/100;
    day = mmdd - mo*100;
    if (y2k) {
      /* use the y2k compliant FITS date form */
      sprintf(tmpstr, "%04ld-%02ld-%02ld", yr, mo, day);
    } else {
      /* use the original FITS date form */
      sprintf(tmpstr, "%02ld/%02ld/%02ld", day, mo, (yr-1900));
    }
    
    nff = (tt+i)->foff;
    for(mm=0; mm<(tt+i)->mform; mm++)  tbuf[nff+mm] = tmpstr[mm];
} 


/*********************************************************************
 * UTDATE_f2u - get the Date from the FITS file.                     *
 *  sflag = 7                                                        *
 *********************************************************************/
utdate_f2u(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf)
int i, inmult,*vf;
char *cbuf,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
    int dd,mm,yy;
    double utd;
    

    /* examine the date, its either old, dd/mm/yy, or new yyyy-mm-dd */
    /* There's no simple way to handle the extra times possible */
    /* in the new format, so we'll leave that for the rewrite and */
    /* emit an warning message if we see any times like that */
    if (cbuf[2] == '/') {
      /* assume old format */
      sscanf(cbuf, "%2d/%2d/%2d", &dd, &mm, &yy);
      yy = yy + 1900;
    } else if (cbuf[4] == '-') {
      /* assume the new format */
      sscanf(cbuf, "%4d-%2d-%2d", &yy, &mm, &dd);
      /* warning message if there's a T at cbuf[10] */
      if (cbuf[10] == 'T') {
	fprintf(stderr, "\nUnable to digest times in DATE fields : %s", cbuf);
      }
    } else {
      /* unrecognized date format */
      fprintf(stderr, "\nUnrecognized date format : %s",cbuf);
    }
    utd = yy + (mm + dd*0.01)*0.01;
    vf[0] = 0;
    dub_to_ieee(utd, &up.u3->utdate, vf[0],0);      
} 

/*********************************************************************
 * restfreq_u2f - get RESTFREQ if appropriate                        *
 *    This routine sets vf to 1 if the data is NOT LINE and is from  *
 *    the 12m (where the overloading is the problem)                 *
 *  sflag = 8                                                        *
 *********************************************************************/
restfreq_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf)
int i, inmult,*vf;
char *cbuf,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
   int n;

   if (strncmp(up.u1->obsmode,"LINE",4) != 0 &&
        strncmp(up.u1->telescop,"NRAO 12M",8) == 0)
      for (n=0;n<inmult;n++) vf[n] = 1;

/*		norm_u2f does all the work                           */ 

   norm_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf);
}


/*********************************************************************
 * restfreq_f2u - get the restfreq from the FITS file                *
 * sflag = 8                                                         *
 *********************************************************************/
restfreq_f2u(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf)
int i, inmult,*vf;
char *cbuf,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
   dub_to_ieee(dub[0], &restfreq, vf[0], 0);
}

/*********************************************************************
 * freqres_u2f - get FREQRES if appropriate                          *
 *    This routine sets vf to 1 if the data is NOT LINE and is from  *
 *    the 12m (where the overloading is the problem)                 *
 *  sflag = 10                                                        *
 *********************************************************************/
freqres_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf)
int i, inmult,*vf;
char *cbuf,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
   int n;

   if (strncmp(up.u1->obsmode,"LINE",4) != 0 &&
        strncmp(up.u1->telescop,"NRAO 12M",8) == 0)
      for (n=0;n<inmult;n++) vf[n] = 1;

/*		norm_u2f does all the work                           */ 

   norm_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf);
}

/*********************************************************************
 * freqres_f2u - get the freqres from the FITS file                  *
 * sflag = 10                                                        *
 *********************************************************************/
freqres_f2u(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf)
int i, inmult,*vf;
char *cbuf,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
   dub_to_ieee(dub[0], &freqres, vf[0], 0);
}


/*********************************************************************
 * dsf_u2f - use DSF if appropriate                                  *
 *    This routine sets vf to 1 if the data is LINE or is not from   *
 *    the 12m (where the overloading is the problem)                 *
 *  sflag = 11                                                       *
 *********************************************************************/
dsf_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf)
int i, inmult,*vf;
char *cbuf,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
   int n;

   if (strncmp(up.u1->obsmode,"LINE",4) == 0 ||
        strncmp(up.u1->telescop,"NRAO 12M",8) != 0)
      for (n=0;n<inmult;n++) vf[n] = 1;

/*		norm_u2f does all the work                           */ 

   norm_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf);
}

/*********************************************************************
 * dsf_f2u - get the dsf from the FITS file                          *
 * sflag = 11                                                        *
 *********************************************************************/
dsf_f2u(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf)
int i, inmult,*vf;
char *cbuf,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
   dub_to_ieee(dub[0], &dsf, vf[0], 0);
}

/*********************************************************************
 * nt_u2f - use NT if appropriate                                    *
 *    This routine sets vf to 1 if the data is LINE or is not from   *
 *    the 12m (where the overloading is the problem)                 *
 *  sflag = 12                                                       *
 *********************************************************************/
nt_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf)
int i, inmult,*vf;
char *cbuf,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
   int n;

   if (strncmp(up.u1->obsmode,"LINE",4) == 0 ||
        strncmp(up.u1->telescop,"NRAO 12M",8) != 0)
      for (n=0;n<inmult;n++) vf[n] = 1;

/*		norm_u2f does all the work                           */ 

   norm_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf);
}


/*********************************************************************
 * nt_f2u - get the nt from the FITS file                            *
 * sflag = 12                                                        *
 *********************************************************************/
nt_f2u(i,vf,cbuf, sht, flt, dub, intype, inmult, ubuf)
int i, inmult,*vf;
char *cbuf,*ubuf, intype;
short *sht;
float *flt;
double *dub;
{
   dub_to_ieee(dub[0], &nt, vf[0], 0);
}

/*************************************************************************
 * DMAX_u2f - insert into the output buffer the value of DATAMAX, which  *
 *   was found earlier by L_pre_scan or C_pre_scan.                      *
 *   sflag = 17                                                          *
 *************************************************************************/
dmax_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, tbuf)
int i, inmult,*vf;
char *cbuf,*tbuf, intype;
short *sht;
float *flt;
double *dub;
{
   int nff, nfr, jrec, mm;
   char tmpstr[8];
   float dmax;
   
   
    nff = (tt+i)->foff;       /* offset position of start of scan  */
    
       dmax = up.ux->datamax;
        
       real_to_ieee(dmax, tmpstr,0);
        for(mm=0; mm<4; mm++)
           tbuf[nff+mm] = tmpstr[mm];  
}


/*************************************************************************
 * DMIN_u2f - insert into the output buffer the value of DATAMIN, which  *
 *   was found earlier by L_pre_scan or C_pre_scan.                      *
 *   sflag = 18                                                          *
 *************************************************************************/
dmin_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, tbuf)
int i, inmult,*vf;
char *cbuf,*tbuf, intype;
short *sht;
float *flt;
double *dub;
{
   int nff, nfr, jrec, mm;
   char tmpstr[8];
   float dmin;
   
    nff = (tt+i)->foff;       /* offset position of start of scan  */
    
       dmin = up.ux->datamin;
       real_to_ieee(dmin, tmpstr, 0);
       for(mm=0; mm<4; mm++)
           tbuf[nff+mm] = tmpstr[mm];  
}


/**************************************************************************
 * LSCAN_u2f  - process the series or continuum scan, a vector of up to   *
 *   SCANLEN points.                                                        *
 *   sflag = 20                                                           *
 **************************************************************************/
Lscan_u2f(i,vf,cbuf, sht, flt, dub, intype, inmult, tbuf)
int i, inmult,*vf;
char *cbuf,*tbuf, intype;
short *sht;
float *flt;
double *dub;
 {
    int jrec, nff, j, jj;
    char tmpstr[16];
    double ieee_to_dub();
    
    nff = (tt+i)->foff; /* offset position of start of series                */

    /* make sure noint doesn't exceed ScanMult! */
       jrec = ieee_to_dub(&up.u12->noint, vf);                     
       if(jrec>ScanMult)   jrec = ScanMult;
       
       for(j=0, jj=nff; j<jrec; j++, jj+=4)
           real_to_ieee(up.series[j], (tbuf+jj), 0);

       /* set minimum max table width */
       if(jj>maxTwid)  maxTwid = jj; 
        
      /* if ScanMult > jrec, fill rest of array with Nan's !! */
       if(ScanMult>jrec)
       { for(; j<ScanMult; j++, jj+=4)
             real_to_ieee(up.series[0], (tbuf+jj), 1);
       }
        fprintf(logfile,"\nLscan_u2f: jrec=%d, ScanMult=%d, jj=%d",
                            jrec,ScanMult,jj);
        fprintf(logfile,", maxTwid=%d, maxmaxis1=%d ",maxTwid,maxmaxis1);
        
        if(maxmaxis1< jrec)  maxmaxis1 = jrec;

 } /****end of Lscan_u2f**********************************************/


/**************************************************************************
 * L_PRE_SCAN  - process the series, a vector of up to SCANLEN points.      *
 *   Determines DATAMAX and DATAMIN for this vector and for the file.     *
 *   These are later put into the output file.           f                *
 **************************************************************************/
L_pre_scan(k)
UPOPS *k;
 {
    int j, mm, jj, mxoff, dmaxoff;
    int rpixoff, krpix, vf;
    double ieee_to_dub();
    
    /* make sure number of integrations doesn't exceed ScanMult! */
      mm = ieee_to_dub(&(k->u12->noint), &vf);
    /* preview mode, just remember max no. of data points */
      if (prev==1) if (maxmaxis1<mm) maxmaxis1 = mm;
      else {
         if(mm>ScanMult)
         {  fprintf(logfile,"\nL_pre_scan  %d points in scan exceeds %d ", 
                                         mm, ScanMult);
            mm = ScanMult;
            k->u12->noint = ScanMult;
         }
         for(j=0; j<mm; j++)
         {   
           if(k->series[j]>k->ux->datamax) 
                         k->ux->datamax = k->series[j];
           if(k->series[j]<k->ux->datamin)
                      k->ux->datamin = k->series[j];
           if(k->series[j]>DATAmax) 
                  DATAmax = k->series[j];
           if(k->series[j]<DATAmin) DATAmin = k->series[j];
         }
      }
 } /****end of L_pre_scan**********************************************/

