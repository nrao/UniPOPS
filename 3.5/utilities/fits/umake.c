/*  @(#)umake.c	5.1 06/22/94	*/
/**************************************************************
 * UMAKE  - make SUN UNIPOPS file for POPS from the FITS tape *
 **************************************************************/
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

/* globals for table translation */
#include "unipops.h"
#include "u2fglob.h"

#define ASIZE 64             /* max multiplicity excepting series */

extern double rth, rtd;

double headlen, datalen;   /* describes length of UNIPOPS output buffer */
long   lenhead, lendata;   /*  (ubuf)                                   */

/* axis values for continuum (globals in u2fglob.h) */
/*  float Cdelt1, Crval1, Crval2, Crota1, Crota2;
 int   feednum;
 char  Ctype1[10], Ctype2[10];
*/

int maxWscan, Ntrans, Twidth, ScanWid;
int ScanMult, MHnlines, THnlines, THtotal, Tfields;
int maxTwid, Maxis1, maxmaxis1;
float DATAmax,DATAmin;

int ismode, istel, isprot; /* points to "OBSMODE", "TELESCOP", and   */
                           /* "CL11TYPE" entries in the bt structure */
/************************************************************************
 * CONVERT_ROW - convert a binary table row to SUN UNIPOPS format.       *
 ************************************************************************/
convert_row()
{
   int i, itp;
   long scanlen, itmp;
   short *sb;
   char tmpstr[64], tmp2[64], tmp3[64];
 
  /* if the first time for this file, read in the translation table */
  /*    and set up pointers from table columns to tt array.         */
   if(qtinit==0) initconv();  
   qtinit=1;
    
  /* set "qcont" to 0 if line data, or =1 if continuum.            */
   if(strncmp((tbuf+(bt+ismode)->offs), "LINE", (long)4)==0)  qcont=0;
   else  qcont=1;
  /* set "kpflag" to 1 if Tucson data.                              */
   if(strncmp((tbuf+(bt+istel)->offs), "NRAO 12M",(long)8)==0) kpflag=1;
   else  kpflag=0; 
  /* set p11flag to 1 if prototype class 11 data.                   */
   if(strncmp((tbuf+(bt+isprot)->offs), "PROTO12M",(long)8)==0) p11flag=1;
   else p11flag=0;

   restfreq = -1.;
   freqres = -1.;
   dsf = -1.;
   nt = -1.;
   up.u12->refpt = 0.0;
   up.u12->x0 = 0.0;

   for(i=1; i<Ntfields+1; i++)     /* translate each parameter */
   {  if((bt+i)->tpntr >= 0)
      {   itp = (bt+i)->tpntr;
         /* if this parameter is an axis, get the axis data into axd */
          if( ((tt+itp)->axis > 0) && ((tt+itp)->ncol>0)) 
                   get_axis(i, (tt+itp)->axis, tbuf); 
  
         /* skip translation if class 9 data of wrong type           */
         /*  i.e., GB data ignored if it's a KP scan, and vice-versa */
         /* or if class 11 data is of wrong type                     */
         /*  i.e., original class 11 ignored if it's a prototype 11  */
         /*  scan and vice-versa                                     */ 
         if( (((tt+itp)->ucl!= 9) && ((tt+itp)->ucl!=11))
          || (((tt+itp)->ucl==9) && (kpflag==0)&&((tt+itp)->kat[0]=='g'))
          || (((tt+itp)->ucl==9) && (kpflag==1)&&((tt+itp)->kat[0]=='k')) 
          || (((tt+itp)->ucl==11) && (p11flag==0)&&((tt+itp)->kat[0]!='t'))
          || (((tt+itp)->ucl==11) && (p11flag==1)&&((tt+itp)->kat[0]=='t')) )
            /* translate the table item and store it in the UNIPOPS record */
               untranslate(i, tbuf);
      }
   }

  /* put appropriate axis data into the UNIPOPS record.            */
   do_axes();
   if (qcont==1 && kpflag ==1) {
      if (dsf != -1) up.u12->restfreq = dsf;
      if (nt != -1) up.u12->freqres = nt;
   } else {
      if (restfreq != -1) up.u12->restfreq = restfreq;
      if (freqres != -1) up.u12->freqres = freqres;
   }
  
   LOGPR"\nConverted row: qtinit=%d, Ntfields=%d", qtinit,Ntfields); 
   fflush(logfile);
   
   strncpy(tmpstr, up.u1->object, (long)16);
   tmpstr[8]=0;
   
   if(qsum==0)        /* if qsum==0 print summary line on stdout */
   {                  /* summarize all data here */
      {  strncpy(tmp2, up.u4->coordcd, (long)8);  tmp2[8]=0;
         strncpy(tmp3, up.u1->obsmode, (long)8);  tmp3[8]=0;
         printf("%3d%8.2f %-9s%-9s%8s %6.2f%7.2f%5.0f %4d %8.3f%9.3f\n",
            (jrows+1), up.u1->scan, tmpstr, tmp3, tmp2,
            up.u4->epocra/15., up.u4->epocdec, up.u12->inttime, 
            axd[0].maxis, up.ux->datamax, up.ux->datamin);
      }
   }
   else
   {
     /*   get actual length of data matrix */
        for(i=0, scanlen=1; i<maxes; i++)  scanlen *= axd[i].maxis;
       /* do not allow scanlen to exceed max for UNIPOPS */
        if(scanlen > SCANLEN)  scanlen=SCANLEN;
        scanlen *= 4;          /* convert to bytes */
        lendata = scanlen;
        datalen = scanlen;
        scanlen += lenhead;
       
     /* pad buffer length to next 512 byte block */
        if((scanlen%512)>0)
        {  itmp = scanlen/512;
           scanlen = 512*(itmp+1);
        }
     /* put data length into ubuf */
        dub_to_ieee(datalen, &up.u1->datalen, 0, 0);

     /* fill in the precision parameter (this never changes) */
        strncpy(up.u1->precis, "R*4     ", (long)8);

     /* If KP data, put obsolete parameter BASEOFF = 0.      */
        if(kpflag==1)
          dub_to_ieee(0.0, &up.u9k->baseoff, 0,0);

     /*   write POPS data file to stdout                     */
      
      if( fwrite(ubuf, (long)1, (long)scanlen, stdout)!= scanlen)
      {  sprintf(tmpstr, "Error writing UNIPOPS record %d to stdout",
              (uCount+1));
          errexit(tmpstr);
      }
        LOGPR"\nWrote rec# %d to stdout, %d bytes", uCount, scanlen);
        uCount++;
   }
    
} /****end of convert_row**********************************************/


/***********************************************************************
 * INITCONV - read in U2f_trans and initialize the conversion tables. *
 ***********************************************************************/
initconv()
{
    int i;
    char *tb;
    short *sbuf;
   
    ismode=istel=isprot= -1;

   /* first find out whether we are line or continuum */

       for(i=0; i<Ntfields; i++)
       {   if(strncmp((bt+i)->ttype, "OBSMODE", (long)7) == 0)
           {   tb= tbuf + (bt+i)->offs;
               if(strncmp(tb, "LINE", (long)4)==0) qcont=0;
               if(strncmp(tb, "CONT", (long)4)==0) qcont=1;
               ismode = i;      /* points to OBSMODE in table */
               fprintf(logfile,"\nInitConv: Found OBSMODE, %d, qcont=%d",
                       i,qcont);
               fflush(logfile); 
           }
           if(strncmp((bt+i)->ttype, "TELESCOP", (long)8) == 0)
           {   tb= tbuf + (bt+i)->offs;
               if(strncmp(tb, "NRAO 12M", (long)8)==0) kpflag=1;
               else                                    kpflag=0;
               istel = i;       /* points to TELESCOP in table */
               fprintf(logfile,"\nInitConv: Found TELESCOP , %d, kpflag=%d",
                       i,kpflag);
               fflush(logfile); 
           }
           if(strncmp((bt+i)->ttype, "CL11TYPE", (long)8) == 0)
           {   tb= tbuf + (bt+i)->offs;
               if(strncmp(tb, "PROTO12M", (long)8)==0) p11flag=1;
               else                                    p11flag=0;
               isprot = i;     /* points to CL11TYPE in table */
               fprintf(logfile,"\nInitConv: Found CL11TYPE, %d, p11flag=%d",
                       i,p11flag);
               fflush(logfile);
           }
           if((ismode>0) && (istel>1) && (isprot>1)) break;
        }
    if(qcont<0)
    {   qcont = 0;
        fprintf(logfile,"\nCannot find OBSMODE !!  Assuming LINE. ");
    }

   /* for line data, set qvof appropriately.                        */
   /*  qvof = 1 if a "FREQ" axis is present, else 0                 */
   qvof = 0;
   if (qcont == 0) 
   {  for (i=0; i<Naxd; i++)
      {  if (strncmp(axd[i].ctype,"FREQ",(long)4)==0) 
         {  qvof=1;
            break;
         }
      }
   } else {
      qvof = -1;
   }

   inittrans(qcont, "U2f", 1);   /* get the translation table */

   /* allocate space for the ubuf array */
    ubuf_alloc();
       
   /* find any header parameters that need to go in the UNIPOPS record */
   findhdparms(); 
    
   /* now identify each FITS table column with the correct UNIPOPS parameter.
      This will fill in the tpntr param in  the bt table.  
      tpntr points to the matching line in the tt table.                  */
    identab();
    
} /****end of initconv**************************************************/


/**************************************************************************
 * FINDHDPARMS - find which header parameters (in the hk array) are       *
 *    parameters that are wanted in the output UNIPOPS data record.        *
 *    Then insert these parameters into the UNIPOPS record.                *
 **************************************************************************/
findhdparms() 
{
    int i,j, nn,mm;
    
    fprintf(logfile,"\nFindHDParms: nhk=%d, Ntrans=%d",
           nhk, Ntrans);
    fflush(logfile);
    for(i=0; i<nhk; i++)
    {   mm=(hk+i)->nkws - 1;
        nn=strlen((hk+i)->names[mm]);
        for(j=0; j<Ntrans; j++)
        {   if(strncmp((tt+j)->ttype, (hk+i)->names[mm], (long)nn)==0) 
            {  /* fprintf(logfile,"\n FHP.t: i=%d, j=%d", i,j);
                  fflush(logfile); */
               headtouni(j, i);
            }
        }
    }
} /****end of findhdparms**************************************************/


 
 
/**************************************************************************
 * IDENTAB - match each column label found in the FITS header             *
 *     (these labels are in the bt array)  with the matching label in the *
 *     translation table (in the tt array).                               *
 *     The bt.tpntr pointer is set to point to the matching line in tt.   *
 **************************************************************************/
identab() 
{
   int i,j,mm,nn;
   
   for(i=0; i<Ntfields+1; i++)
   {   nn=strlen((bt+i)->ttype);
       if(nn>8) nn=8;
       (bt+i)->tpntr = -1;       /* tpntr is -1 if bt item is not identified */
       if(nn>0)
/*                          recognize old SPECTRUM as current SERIES */
       {  if (strcmp((bt+i)->ttype,"SPECTRUM")==0) strcpy((bt+i)->ttype,"SERIES");
          for(j=0; j<Ntrans; j++)
          {   if((strcmp((tt+j)->ttype, (bt+i)->ttype)==0) 
               &&((tt+j)->ncol>0) )
              {   (bt+i)->tpntr = j;
                   break;
              }
           }
       }
    }
} /******end of identab*************************************************/

   
/***********************************************************************
 * HEADTOUNI - convert an item from the FITS header to UNIPOPS form.   *
 *   i is index in translation table. (thats the tt table)             *
 *   hflag= index in hk table (header info)                            *
 ***********************************************************************/
headtouni(i, hflag)
int i, hflag;
{  
   char *cbuf, intype, *cb, st[80];
   short *sbuf, sht[ASIZE];
   double dbuf[ASIZE], dub[ASIZE];
   float  *fbuf, flt[ASIZE];
   int mm,nn, inmult, vf[ASIZE];
   
   sbuf = (short *)dbuf;
   cbuf = (char  *)dbuf;
   fbuf = (float *)dbuf;
   
   /*              hflag>=0 for data from header (going to k)          */
   /*                 i is index in tt table; hflag is index in hk.    */
   
   /* prepare header item for ubuf */
                                   /* (ignore if no conversion function) */
      if( (tt+i)->fun == 0) return(0); 
      
      inmult = (hk+hflag)->no;
      for(mm=0; mm<inmult; mm++) vf[mm]=0;  /* set valid flags to good */
      
      switch((hk+hflag)->type)
      {   case 0 :  sbuf[0] = (hk+hflag)->logic; /* logical value */
                    sht[0] = sbuf[0];
                    dub[0] = flt[0] = sht[0];
                     intype='I';  inmult=1;
                     break;
          case 1 :                             /* integer value */
                     intype='I'; 
                     for(mm=0; mm<inmult; mm++)
                     {  sbuf[mm] = (hk+hflag)->ival[mm];
                        dub[mm] = sbuf[mm];
                        if(strcmp((tt+i)->cfact,"-")!=0)
                        {  dub[mm] = dub[mm] - (tt+i)->offset;
                           dub[mm] /= (tt+i)->factor;
                           sbuf[mm]= dub[mm] + 0.49;
                        }
                        sht[mm]=sbuf[mm];
                        flt[mm] = dub[mm];
                     }
                     break;
          case 2 :                         /* floating or double value  */
                     intype = 'D';
                     for(mm=0; mm<inmult; mm++)
                     {  dub[mm] = (hk+hflag)->dval[mm];
                        if(strcmp((tt+i)->cfact,"-")!=0)
                          dub[mm] = (dub[mm]-(tt+i)->offset)/(tt+i)->factor;
                        flt[mm] = dub[mm];
                        sht[mm] = dub[mm] + 0.49;
                     }
                     break;
          case 3 :                        /* ascii value */
                     intype = 'A';
                     inmult = strlen((hk+hflag)->cval);
                     for(mm=0, cb=cbuf; mm<inmult; mm++, cb++)
                       *cb = (hk+hflag)->cval[mm];
                     break;
          default :  
                     sprintf(st,"unknown data type in header, kw=%s",
                         (hk+hflag)->names[(hk+hflag)->nkws-1]);
                     errexit(st);
          }
          /* now call the translation function */
          (*((tt+i)->fun)) (i, vf, cbuf, sht, flt, dub, intype, inmult, ubuf);
         
} /*****end of headtouni***************************************************/



/***************************************************************************
 * UNTRANSLATE - get parameter i from input tbuf (FITS binary table) and   *
 *   prepare it for translation function.                                  *
 ***************************************************************************/
untranslate(i,tbuf)
int i;
char *tbuf;
{
   char intype, *cbuf, st[80], *ccb, *ttb, *cx;
   short *sbuf, sht[ASIZE];
   double dbuf[ASIZE], dub[ASIZE], pi;
   float  *fbuf, flt[ASIZE];
   int mm,nn, inmult, itp, inwid,j;
   int vf[ASIZE];
   
   float ieee_to_real();
   double ieee_to_dub();
   short  ieee_to_int();
   
   pi = 3.14159265358979323;
   
   /*    i is index in bt table    */
   
   /* prepare item for ubuf */
                                   /* (ignore if no conversion function) */
      itp = (bt+i)->tpntr;
      if(itp<0)               return(0);
      if( (tt+itp)->fun == 0) return(0); 
      
      inmult = (bt+i)->tmul;
      intype = (bt+i)->form;
      inwid = inmult * form_size(intype);
      
      sbuf = (short *)dbuf;
      fbuf = (float *)dbuf;
      cbuf = (char  *)dbuf;
      ccb = cbuf;
      ttb = (bt+i)->offs + tbuf;

     if((bt+i)->matflg==1)         /* if series, skip to translation */
     {   cbuf = ttb; 
         cx = (char *)flt;
         for(j=0;j<32;j++) *cx++ = cbuf[j];

         fprintf(logfile,"\nSERIES: offs:%d, %d %8.4f", 
              (ttb-tbuf), (bt+i)->offs, flt[0], flt[1]);
         fflush(logfile);
         vf[0]=0;
         fprintf(logfile, " vf=%lx, vf[0]=%d ", vf, vf[0]);
         fflush(logfile);
     }
     else
     { for(mm=0; mm<inwid; mm++, ccb++, ttb++) 
         *ccb = *ttb;
      
      switch(intype)
      {   case 'A' :                                 /* ascii value */
                     /* if first byte is null, set string to blanks */
                     /* and set validity (vf) to invalid (=1)       */
                     if(cbuf[0]==0)
                     {  vf[0]=1;
                        for(mm=0; mm<inwid; mm++)  cbuf[mm]=' ';
                     }
                     else  vf[0]=0;
                     break;
          case 'I' :                             /* integer value */
                     for(mm=0; mm<inmult; mm++)
                     {  sht[mm] = ieee_to_int((sbuf+mm),(vf+mm));
                        dub[mm] = sht[mm];
                       if(strcmp((tt+itp)->cfact,"-")!=0)
                       { dub[mm] = (dub[mm]-(tt+itp)->offset)/(tt+itp)->factor;
                          if(dub[mm]<0.0)  sbuf[mm]= dub[mm]-0.5;
                           else            sbuf[mm]= dub[mm] + 0.5;
                        }
                        flt[mm] = dub[mm];
                     }
                     break;
          case 'E' :                         /* floating  value  */
                     for(mm=0; mm<inmult; mm++)
                     {  flt[mm] = ieee_to_real((fbuf+mm),(vf+mm));
                        dub[mm] = flt[mm];
                       if(strcmp((tt+itp)->cfact,"-")!=0)
                         dub[mm] = (dub[mm]-(tt+itp)->offset)/(tt+itp)->factor;
                         
                        flt[mm] = dub[mm];
                        if(dub[mm]<0.0)  sht[mm] = dub[mm]-0.5;
                         else            sht[mm] = dub[mm]+0.5;
                     }
                     break;
          case 'D' :                        /* double value */
                     for(mm=0; mm<inmult; mm++)
                     {  dub[mm] = ieee_to_dub((dbuf+mm), (vf+mm));
                       if(strcmp((tt+itp)->cfact, "-")!=0)
                         dub[mm] = (dub[mm]-(tt+itp)->offset)/(tt+itp)->factor;
                        
                        flt[mm] = dub[mm];
                        if(dub[mm]<0.0)  sht[mm] = dub[mm] -0.5;
                         else            sht[mm] = dub[mm] +0.5;
                     }
                     break;
          default :  
            sprintf(st,
               "Unknown data type in table, col %d, pntr=%d, type=%o, kw=%s ",
                i, itp, intype, (bt+i)->ttype);
            fprintf(logfile,"\n %s ", st);
            fflush(logfile);
            errexit(st);
          }
        }  /* end of if(...SERIES) */

       /* now call the translation function */
          
        (*((tt+itp)->fun))(itp,vf,cbuf,sht,flt,dub,intype,inmult,ubuf);

} /****end of untranslate***************************************************/


/***************************************************************************
 * GET_AXIS - get axis info from table columns which are marked as being   *
 *   axes in the tt (trans-table).                                         *
 ***************************************************************************/
get_axis(ii, axcode, tbuf)
int ii, axcode;
char *tbuf;
{
   int iax, nax,jax,jj, inwid, mm;
   char cbuf[16], *ccb, *ttb, msg[64];
   short *sbuf;
   float *fbuf;
   double *dbuf;

   inwid = (bt+ii)->tmul * form_size((bt+ii)->form);
   sbuf = (short *)cbuf;
   fbuf = (float *)cbuf;
   dbuf = (double *)cbuf;

   ccb = cbuf;
   ttb = (bt+ii)->offs + tbuf;
   
   for(mm=0; mm<inwid; mm++, ccb++, ttb++)
       *ccb = *ttb;

   iax = axcode%10;
   nax = axcode/10;
   jax = nax-1;
   if(nax>Naxd)
   {  sprintf(msg, "No.Axes found in table (%d) exceeds MAXIS (%d)!",
             nax,Naxd);
      errexit(msg);
   }
   axd[jax].no = nax;

   switch(iax)
   {   case 1 :   axd[jax].maxis = *sbuf;  break;     /* MAXIS */

       case 2 :  jj = (bt+ii)->tmul;                  /* CTYPE */
                 strncpy(axd[jax].ctype, cbuf, (long)jj);
                 axd[jax].ctype[jj] = 0;
                 break;
       case 3 :  axd[jax].cdelt = *fbuf;  break;      /* CDELT */
       case 4 :  axd[jax].crpix = *fbuf;  break;      /* CRPIX */
       case 5 :  axd[jax].crval = *dbuf;  break;      /* CRVAL */
       case 6 :  axd[jax].crota = *fbuf;  break;      /* CROTA */
   }
} /*****end of get_axis*********************************************/


/******************************************************************
 * DO_AXES - process axis data and store into the UNIPOPS record  *
 *   test ver - just print out the axis data.                     *
 ******************************************************************/
do_axes()
{
   int i, jmid;

   for(i=0; i<Naxd; i++)
   {  if(qcont==0)                      /* do the line case */
      /* check for velocity axis */
      {   if((strncmp(axd[i].ctype, "VELO", (long)4)==0)
                ||(strncmp(axd[i].ctype, "FELO", (long)4)==0))
          {  jmid = axd[i].maxis/2; 
             if(kpflag!=1)  jmid++;  /* note different convention for kp */
        /* set refpt if not already set */
             if (up.u12->refpt == 0) up.u12->refpt = jmid;
        /* if crpix is not = refpt, adjust V_center accordingly */
             up.u12->x0 = 0.001 *
                 (axd[i].crval + (up.u12->refpt-axd[i].crpix)*axd[i].cdelt);  
             up.u12->deltax = axd[i].cdelt * 0.001;
          }
          else
         /* check for frequency axis */
            if(strncmp(axd[i].ctype, "FREQ", (long)4)==0)
            {  jmid = axd[i].maxis/2;
               if(kpflag!=1)   jmid++;
         /* set these values if not already set */
               if (up.u12->refpt == 0) up.u12->refpt = jmid;
               if (up.u12->x0 == 0) up.u12->x0 = up.u7->velocity;
            }
      }
      else             /* do the continuum case */
      {
         up.u12->refpt = 1.0;
         up.u12->x0 = 0.0;
      }
   } /*****end of for(i=...) *********************/
} /*****end of do_axes********************************************/


/*******************************************************************
 * UBUF_ALLOC - allocate space for UNIPOPS buffer array            *
 *******************************************************************/
/* ustruct defines the structure for the output UNIPOPS record */
/*   This is the index that is to be put at the beginning of each UNIPOPS
     record.  ustruct[0]= # header classes;
              ustruct[j]= pointer to class j in units of R*8 s        
    The KP structure is used for both GB and KP records  (KP is larger) 
    and for both types of class 11 (PROTO12M is larger)                 */
int ustruct[16] = {13,5,22,35,45,62,68,79,86,91,116,126,162,186,193,0};
ubuf_alloc()
{
   long usize, itmp, i;
   char st[64];

   /* estimate max size needed for ubuf */
   /* header structure is fixed by pre-set array "ustruct"
      (see early part of f2u.c for definition.                 */
    i= ustruct[0];                 /* number of header classes */
    usize = (ustruct[i+1] - 1)*8;     /* length of header      */
    headlen = usize;
    lenhead = usize;

   /* add max size for scan length */
    usize += SCANLEN * 4;   /* assume series is R*4 size data */
   
   /* pad to the next 512 bytes */
    itmp = usize/512;
    usize = 512*(itmp+1);
    
   /* allocate the space */
    if(ubufflag==0) 
    {  if((ubuf=(char *)malloc(usize))==NULL)
       {  sprintf(st, "UMAKE: cannot allocate %ld bytes for ubuf!!",
                 usize);
          errexit(st);
       }
      /* clear the ubuf array */
       for(i=0; i<usize; i++)  ubuf[i] = 0;
    }
    ubufflag = 1;  /* set flag indicating space is allocated */

   /* set the index part of the ubuf record */
    up.u0  = (UNICLASS0 *)ubuf;
    up.u0->Nheadcls = ustruct[0];
    upnt[0] = ustruct[0];

    for(i=1; i<16; i++)   
    {  upnt[i] = (ustruct[i] - 1) * 8 ;
       up.u0->head_ptr[i-1] = ustruct[i];
    }

      /* set pointers for UPOPS structure */
        up.u1  = (UNICLASS1 *)(ubuf+upnt[1]);
        up.u2  = (UNICLASS2 *)(ubuf+upnt[2]);
        up.u3  = (UNICLASS3 *)(ubuf+upnt[3]);
        up.u4  = (UNICLASS4 *)(ubuf+upnt[4]);
        up.u5  = (UNICLASS5 *)(ubuf+upnt[5]);
        up.u6  = (UNICLASS6 *)(ubuf+upnt[6]);
        up.u7  = (UNICLASS7 *)(ubuf+upnt[7]);
        up.u8  = (UNICLASS8 *)(ubuf+upnt[8]);
        up.u9g = (UNICLASS9G *)(ubuf+upnt[9]);
        up.u9k = (UNICLASS9K *)(ubuf+upnt[9]);
        up.u10 = (UNICLASS10 *)(ubuf+upnt[10]);
        up.u11 = (UNICLASS11 *)(ubuf+upnt[11]);
        up.u11p = (UNICLASS11P *)(ubuf+upnt[11]);
        up.u12 = (UNICLASS12 *)(ubuf+upnt[12]);
        up.u13 = (UNICLASS13 *)(ubuf+upnt[13]);
        up.ux = &auxbuf;
        up.series = (float *)(ubuf+lenhead);

    /* set header length into ubuf */
     dub_to_ieee(headlen, &up.u1->headlen, 0,0);

} /****end of ubuf_alloc******************************************/
