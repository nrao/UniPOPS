/*  @(#)ureadhdr.c	5.1 06/22/94	*/
/*****************************************************************
 * UREADHDR - routines for parsing lines in the FITS header, and *
 *   taking appropriate action for each keyword.                 *
 *****************************************************************/
#include <stdio.h>
#include <math.h>
#include <fcntl.h>
#include <errno.h>
#include <time.h>

#ifdef THINK_C          /* includes for Macintosh THINK-C */
 #include <console.h>
 #include <stdlib.h>
#endif
#include "f2u.h"  

HSUM hsum;             /* header summary structure (see f2u.h) */
extern int qscanc;     /* flag for scan count option.  */

/************************************************************************
 * PARSELN - parse a FITS header line, find the keyword and the value.  *
 *   The input line is in 'lin', assumed to have no more than 80 chars. *
 *   The principal keyword, normally in the 1st 8 columns,              *
 *       goes into kval.names[0].                                       *
 *   If the principal kw is blank or COMMENT, cols 9-80 are             *
 *     taken to be a comment and put into cval.                         *
 *   Any other blank-delimited strings that occur before an "=" sign    *
 *      are assumed to be secondary keywords, and are put into          *
 *      kval.names[1], kval.names[2], etc (limit of HLIM).              *
 *      kval.nkws is set to the # of keywords found. It will be zero if *
 *       the whole line was blank.                                      *
 *   Anything following an "=" sign and before a "/" will be the value  *
 *     to be assigned to the keyword.                                   *
 *     If the single-quote (') is found, anything between quotes is a   *
 *     comment and put into kval.cval                                   *
 *     If "T" or "F" is found, the value is a logical true or false.    *
 *     If a decimal point, or letter "E" is found , the value is real.  *
 *     Otherwise, the value is integer.                                 *
 *  If a 2nd real or integer value is found, then it goes into ival[1]  *
 *     or dval[1].   (for the imaginary parts, if present)              *
 ************************************************************************
 *  Inputs: lin contains the input header line to be parsed.            *
 *  Outputs:  return value is 0 if everything is ok                     *
 *            return value is -1 if syntax error or unknown keyword.    *
 *           ermsg will contain an error message if return val is -1.   *
 ************************************************************************
 *  FITS rules of header syntax enforced by this program:               *
 *    The 1st keyword on the line must be left-justified in cols.1-8.   *
 *    Up to HLIM (4) keywords, blank delimited, may occur before the "="*
 *    If the 1st 8 columns are blank, the rest of the line is a comment.*
 *    Keywords must have at least 1 and no more than 8 characters.      *
 *    Keywords may not contain lower case letters.                      *
 *     (in this prog, chars with ascii code >='a' cause an error.)      *
 *    The Value to be assigned to a keyword follows an "=" sign.        *
 *    Good FITS practice is to delimit the "=" sign by blanks, but this *
 *      program does not require it.                                    *
 *    Anything following a slash (/), after at least one keyword and    *
 *      after column 8, is a comment and is ignored.                    *
 ************************************************************************
 *  parseln written by F. Ghigo, NRAO-Green Bank, Jan. 1990             *
 *      last update Apr 5, 1990.                                        *
 *      HISTORY is treated as a "heirarch": following kws are processed *
 ************************************************************************/
parseln(lin,kval)
KW_VAL *kval;
char *lin;
{
   int i,j,ii, jj, kk, ikw, nnax, trouble;
   char tmp[80], kwtmp[MAXKWLEN], *tmpp;

   /*  initialize results to non-disastrous values */
    trouble      =  0;
    kval->nkws   = -1;
    kval->pntr   = -1;
    kval->status =  0;
    kval->type   =  0;
    kval->naxi   =  0;
    kval->no     =  1;
    kval->logic  =  0;
    kval->ival[0] = kval->ival[1] = 0;
    kval->dval[0] = kval->dval[1] = 0.0;
    kval->cval[0] = 0;
    for(i=0; i<HLIM; i++)  kval->names[i][0] = 0;

    strcpy(tmp,"                   ");

   /* scan thru line, skip leading blanks before keyword  */
   for(i=0; ((i<HDR_LINE_WIDTH) && (lin[i]==' ')); i++);

   if(i>0)
   {  if(strncmp(lin,"        ",(long)8)==0)
      {  strcpy(kval->names[0],"        ");        /* keyword is blank */
         strncpy(kval->cval, (lin+8), (long)72);   /* rest of line is comment */
         kval->cval[72]=0;
         kval->nkws = 1;
         kval->status = 1;
         kval->type   = 3;
         goto idstep;          /* return for blank keyword */
       }
       else
       {   LOGPR"\nParseln:  1st keyword must start in col.1.");
           trouble=1;
           goto errorexit;
        }
    }

   /*  LOGPR"\n checkA - i,lin[i]: %d %c \n", i,lin[i]);  
       LOGPR"     line=%s::\n", lin);  
    */
   /* copy keyword into kval, stop if blank or equals sign */
   /* also stop if "/" or control character                */

   for(j=0;
          ((i<HDR_LINE_WIDTH) && (lin[i]>' ') && (lin[i]!='=')
            && (lin[i]!='/') );
            j++, i++)
   {   kval->names[0][j] = lin[i];

   /* check that characters in keyword are ordinary ascii */
       if((lin[i] <= ' ') || (lin[i]>='a'))  trouble=2;
       if(j>KWMAXCHAR)
       {   trouble=1;            /* keyword too long ? */
           kval->names[0][j]=0;
           LOGPR"\nParseln:  Keyword %s has >%d chars.", 
              kval->names[0], KWMAXCHAR);
           goto errorexit;
       }
   }
   kval->names[0][j] = 0;  /* terminate kw string */

   /*  LOGPR"\n checkB - i,j,lin[i]: %d %d %c", i,j,lin[i]); */  

   if(trouble==2)
   {   LOGPR"\nParseln: non-ascii char in keyword %s ",kval->names[0]);
       goto errorexit;         /* return if bad grammar */
   }
   
   kval->nkws = 1;                  /* a principal kw was found */

   /*  check for comment keywords  */
    if(strcmp(kval->names[0], "COMMENT")==0)
    {  strncpy(kval->cval, (lin+8), (long)72);  /* rest of line is comment */
       kval->cval[72]=0;
       kval->type=3;
       kval->status=1;
       goto idstep;      
    }

   if(lin[i]==' ')     /*  skip blanks  if any */
      for(j=0; ((i<HDR_LINE_WIDTH) && (lin[i]==' ')); i++);
   if(i>=HDR_LINE_WIDTH-1)  goto idstep;

   /*  advance to the equals sign  */

    /*  LOGPR"\n checkC - i,j,lin[i]: %d %d %c", i,j,lin[i]); */  

   for(j=0,ii=1; ((i<HDR_LINE_WIDTH) && (lin[i]!='=')); i++)
   {  
      if(lin[i]==' ')     /*  skip blanks  */
      {
         if(j>0)          /* keyword complete; check for errors */
         {  kval->names[ii][j]=0;
            j=0;  ii++;
            if(ii>HLIM)
            {  LOGPR"\nParseln: too many keywords on one line.");
               trouble=1;
               goto errorexit;
            }
            if(trouble==2)
            {  LOGPR"\nParseln: Keyword %s has illegal characters.",
                  kval->names[ii-1]);
               goto errorexit;
            }
            trouble=0;
          }
      }
      else
      {                    /* get multiple or hierarachical kws */
         kval->names[ii][j++] = lin[i];
        /* check that characters in keyword are ordinary ascii */
         if((lin[i] <= ' ') || (lin[i]>='a'))  trouble=2;

         if(j>MAXKWLEN)
         {   trouble=1;
             kval->names[ii][j]=0;
             LOGPR"\nParseln: keyword %s has >%d characters.",
                  kval->names[ii], MAXKWLEN);
             goto errorexit;
         }
      }
    }
  /* LOGPR"\n checkD - i,j,ii,lin[i]: %d %d %d %c", i,j,ii,lin[i]); */  

         if(j>0)          /* last keyword complete; check for errors */
         {  kval->names[ii][j]=0;
            j=0;  ii++;
            if(ii>HLIM)
            {  LOGPR"\nParseln: too many keywords on one line.");
               trouble=1;
               goto errorexit;
            }
            if(trouble==2)
            {  LOGPR"\nParseln: Keyword %s has illegal characters.",
                  kval->names[ii-1]);
               goto errorexit;
            }
            trouble=0;
          }

   kval->nkws = ii;


   i++;   /* get past the equal sign */

   if(i>=HDR_LINE_WIDTH-1) goto idstep;    /* if no '=' found */

   kval->type = 1;   /* value will be integer if no ' . or  E found */

   /* scan remaining characters after = sign; stop at slash (/)  */

   for(j=0, ii=0; ((i<HDR_LINE_WIDTH) && (lin[i]!='\n'));
             i++)
   { 
       if((lin[i]=='\'')&&(kval->type!=3))  
       {   kval->type = 3;            /* found 1st single quote */
           j=0;
       }
       if(kval->type==3)               /* get character value */
       {   tmp[j++] = lin[i];
           if((tmp[j-1]=='\'')&&(j>1)) /* found matching single quote? */
           {  tmp[j]=0;
              break;                   /* jump out of for loop */
           }
       }
       else
       {   if((lin[i]!=' ')&&(lin[i]!=',')) /* char is not blank or comma */
           {  if(lin[i]=='/')   break;      /* slash(/) ends value field */

              tmp[j++] = lin[i];
              if(lin[i]=='T')
              {   kval->type=0;          /* its a logical True */
                  kval->logic=1;
                  goto idstep;
              }
              if(lin[i]=='F')
              {   kval->type=0;          /* its a logical False */
                  kval->logic=0;
                  goto idstep;
              }
              if((lin[i]=='.')||(lin[i]=='E')||(lin[i]=='e'))
                 kval->type=2;          /*  its a real constant  */
           }
           else    /* blank or comma delim found */
           {   if((kval->type==2)&&(j>0))
               {   tmp[j]=0;      /* its a real string */
                   kval->dval[ii] = atof(tmp);
                   j=0;
                   ii++;
                   if(ii>2)
                   {  trouble=1;
                      LOGPR"\nParseln:  >2 reals in value.");
                      goto errorexit;
                   }
                }
               if((kval->type==1)&&(j>0))
               {   tmp[j]=0;      /* its an integer string */
                   kval->ival[ii] = atoi(tmp);
                   j=0;
                   ii++;
                   if(ii>2)
                   {  trouble=1;
                      LOGPR"\nParseln: >2 ints in value.");
                      goto errorexit;
                   }
                }
            }
       }
    }
   /*  if it was a string, remove trailing blanks */
    if(kval->type==3) 
    {  if(tmp[j-1]=='\'')
       {  for(i=j-2; ((i>0) && (tmp[i]==' ')); i--) ;
          tmp[i+1]=0;
          tmpp = tmp;
          if(tmp[0]=='\'') tmpp++;
          strcpy(kval->cval,tmpp);
       }
       else 
       {   trouble = 1;   /* no ending quote on the string */
           LOGPR"\nParseln:No end quote on string value.");
           goto errorexit;
       }
    }
   /* if an int or real, find value here */
      if((kval->type==2)&&(j>0))
       {   tmp[j]=0;      /* its a real string */
           kval->dval[ii] = atof(tmp);
           j=0;
           ii++;
           if(ii>2)
           {  trouble=1;
              LOGPR"\nParseln: >2 reals in value.");
              goto errorexit;
           }
        }
       if((kval->type==1)&&(j>0))
       {   tmp[j]=0;      /* its an integer string */
           kval->ival[ii] = atoi(tmp);
           j=0;
           ii++;
           if(ii>2)
           {  trouble=1;
              LOGPR"\nParseln: >2 ints in value.");
              goto errorexit;
           }
        }
        if((kval->type==1)||(kval->type==2))kval->no = ii; /* set # values */


idstep:

 /* check last (or only) keyword for axis number */
    strcpy(kwtmp," ");
    nnax=0;
    ii=0;
    i=kval->nkws-1;
    j=strlen(kval->names[i])-1;
    for( ;((j>0)&&(kval->names[i][j]>='0')&&(kval->names[i][j]<='9'));
            j--)  ii++;
    if(ii>0)
                                            /* get axis name and number */
    {   strncpy(kwtmp, kval->names[i], (long)(j+1));  
        kwtmp[j+1] = '#';
        /* strcat(kwtmp,"#"); */
        kwtmp[j+2] = 0;
        for(jj=0; jj<ii; jj++)  tmp[jj]=kval->names[i][jj+j+1];
        tmp[ii]=0;
        nnax = atoi(tmp);
    } 

 /* debug printout - results of each line parse */
   if(debug>1)
   { LOGPR"\nParseln: line:\n%s\n",lin);
     LOGPR"\nParseln:Found %d keywords:", kval->nkws);
     for(i=0; i<(kval->nkws); i++)  LOGPR" #%d:%s,  ",i, kval->names[i]);
     LOGPR"\nParseln: type:%d, no:%d, logic:%d, int:%d %d, real:%8.3f %8.3f",
          kval->type, kval->no, kval->logic, kval->ival[0], kval->ival[1],
          kval->dval[0], kval->dval[1]);
     LOGPR"\nParseln: kwtmp=%s,  nnax=%d", kwtmp,nnax);
     LOGPR"\nParseln: char:%s", kval->cval);
     fflush(logfile);
   }
  
 /*  Now try to identify the keywords */

  kk = kval->nkws;
  if(nnax>0)  kk--;  /* if last kw has an axis number, check it later */

  for(i=0; i<kk; i++) 
  {  
     if((ikw=findkw(kval->names[i]))<0)
     {   LOGPR"\nParseln: keyword %s not recognized.",
            kval->names[i]);
         goto normexit;            /* this line will be skipped */
     }
   }
   if(nnax>0)        /* now check last kw */
   {  /* first see if kw with number is recognized */
      if((ikw=findkw(kval->names[kk]))<0)
      {   if((ikw=findkw(kwtmp))<0)       /* if not, check axis kw */
          {  LOGPR"\nParseln: keyword %s not recognized.",kwtmp);
              goto normexit;         /* this line will be skipped */
           }
           else 
           {   if((nnax>kw[ikw].maxi)||(nnax<1))
               {  LOGPR"\nParseln: keyword %s axis %d out of range.",
                      kwtmp, nnax);
                  goto errorexit;
               }
               kval->naxi = nnax;
           }
        }
   }  

  /*  The last of a string of keywords has the value attached
      to it.
      It is assumed that all keywords are unique, regardless of
      what hierarchy they are in.  This code will have to change
      when this assumption becomes untrue.                        */

    kval->pntr = ikw;
    kw[ikw].status = 1;
    
  /*  Check that data type and "no" of keyword matches what was found */
    if(kw[ikw].type!=kval->type)
    {   LOGPR"\nParseln: Type of %s should be %d, but %d was found.",
           kw[ikw].name, kw[ikw].type, kval->type);
        goto normexit;
    }
    if(kw[ikw].no!=kval->no)
    {   LOGPR"\nParseln: Nvalues for %s should be %d, but %d was found.",
            kw[ikw].name, kw[ikw].no, kval->no);
        goto errorexit;
    }
  
normexit:
    return(0);

errorexit:   return(-1);
}
/****end of parseln****************************************************/


/**********************************************************************
 * FINDKW - looks for input keyword (kwd) in structure kw.            *
 *    If kwd is found, return index of keyword (ikw), else return -1  *
 **********************************************************************/
findkw(kwd)
char *kwd;
{
   int i,ikw,flag;

   flag=0;
   for(i=0; i<maxhk; i++)
     if(strcmp(kwd, kw[i].name)==0)
     {  flag=1;
        ikw=i;
        if(debug>1)
           LOGPR"\nFindKW: found %s is kw # %d\n", kwd,ikw);
        break;
     }
   if(flag) return(ikw);
   else 
   {  if(debug>1)
        LOGPR"\nFindKW: keyword %s not recognized.\n",kwd);
      return(-1);
   }
}



/*****************************************************************
 * INITKW - initialize the keyword function table                *
 *****************************************************************/
initkw()             /*  initialize the keyword structure */
{ 
   int i,j;
   int  simple(), naxis_proc(), process_end();
   int  bitpix(), naxisn_proc(), xtension_proc();
   int  tform_proc(), ttype_proc(), tunit_proc();
   int  extname_proc(), tfields_proc(), obsmode_proc();
   int  tmatx_proc(), maxis_proc(),maxisn_proc();
   int  date_proc(), dateobs_proc(), tele_proc(), pid_proc();
   int  obsr_proc(), ctype_proc(), crpix_proc(), cdelt_proc();
   int  crval_proc(), crota_proc(), tdim_proc();

  /* NOTE this program must have the right index number for 
     each keyword - if keyword table (kw) changes, the following
     may need to change also !   (refer to file "setkw.h")       */

   kw[0].func = process_end;        /* END   is kw # 0  */
   kw[1].func = simple;
   kw[2].func = bitpix;
   kw[3].func = naxis_proc;         /* NAXIS is kw # 3  */
   kw[4].func = naxisn_proc;
   kw[11].func = date_proc;
   kw[12].func = dateobs_proc;
   kw[15].func = tele_proc;         /* (15)  TELESCOP   */
   kw[16].func = obsr_proc;         /* (16)  OBSERVER   */
   kw[20].func = crval_proc;        /* (20)  CRVAL      */
   kw[21].func = crpix_proc;        /* (21)  CRPIX      */
   kw[22].func = cdelt_proc;        /* (22)  CDELT      */
   kw[23].func = ctype_proc;        /* (23)  CTYPE      */
   kw[24].func = crota_proc;        /* (24)  CROTA      */
   kw[35].func = xtension_proc;     /* (35)  XTENSION   */
   kw[36].func = extname_proc;      /* (36)  EXTNAME    */
   kw[39].func = tfields_proc;      /* (39)  TFIELDS    */
   kw[40].func = tform_proc;        /* (40)  TFORM      */
   kw[41].func = ttype_proc;        /* (41)  TTYPE      */
   kw[42].func = tunit_proc;        /* (42)  TUNIT      */
   kw[50].func = maxis_proc;        /* (50)  MAXIS      */ 
   kw[51].func = maxisn_proc;       /* (51)  MAXISnn    */
   kw[52].func = tmatx_proc;        /* (52)  TMATXnn    */
   kw[53].func = tdim_proc;         /* (53)  TDIMnn     */
   kw[60].func = pid_proc;          /* (60)  PROJID     */
   kw[65].func = obsmode_proc;      /* (65)  OBSMODE    */

/*  initialize the header summary structure */
   hsum.nxis1 = hsum.nxis2 = hsum.mxis = hsum.tflds = 0;
   hsum.type[0] = hsum.date[0] = hsum.tele[0] = 0;
   hsum.obsr[0] = hsum.omode[0] = 0;
}


/********************************************************************
 * PROCESS_END - evaluates header contents after END detected.      *
 *    If next record expected is data, set head to 0.               *
 ********************************************************************/
process_end(kval)
KW_VAL *kval;
{
    int i;
    long int lim_file;
    char ermsg[80];

    head = 0;
    headlatch=0;
    
    Dwidth = naxisn[0];  /* width of binary data row */
    nDrow  = naxisn[1];  /* number of rows           */
    iDrow  = 0;

    lim_file=1;          /* calculate size of file to follow */
    for(i=0; i<naxes; i++)
       lim_file *= naxisn[i];
    
    lim_file *= n_bitpix;

    n_datarecs = (lim_file + 23039)/23040;
    
    if((naxes==0)||(n_datarecs<1))  /* if no data expected */
    {  head=1;
       n_datarecs=0;
    }
    LOGPR"\nFound header END;  rec#%d ",rcount);
    LOGPR"  lim_file=%ld, n_bitpix=%d", lim_file, n_bitpix);
    LOGPR"\nExpecting %ld data records.", n_datarecs);
     
   if((flag3D==1)&&(debug>1))   /* debug print of table table */
   {   for(i=0; i<Ntfields+1; i++)
       LOGPR"\n %d, %10s %8s %5d %c", i,(bt+i)->ttype,
         (bt+i)->tunit, (bt+i)->tmul, (bt+i)->form);
    }
    siz_tbuf = n_bitpix/8;
    siz_tbuf *= naxisn[0];           /* width of table or raster row */
    num_tbuf = naxisn[1];            /* expected number of table rows */

    LOGPR"\nProcess_end: siz_tbuf=%ld, nax1=%d",siz_tbuf, naxisn[0]);
    LOGPR", num_tbuf=%d,  n_bitpix=%d\n", num_tbuf, n_bitpix);

   /* allocate a buffer for an input line from binary table or raster */
    if(siz_tbuf>16)  
    {  tbuf = (char *)malloc(siz_tbuf);
       if(tbuf==0) 
       {   sprintf(ermsg,"Can't allocate %ld bytes for tbuf!",siz_tbuf);
           errexit(ermsg);
       }
       tbufflag=1;            /* mark the tbuf buffer as open */
    }
    
   /* if -s option, write summary line for header on std out */
    if(qsum==0)
    {  printf(" %-9s  %-9s %-9s %-8s ", hsum.type, hsum.date,
         hsum.tele, hsum.obsr);
       printf("   %3d%3d%6d%5d%5d %3d\n",
         n_bitpix, naxes, hsum.nxis1, hsum.nxis2, Ntfields, maxes);
         
       /* if this is a 3D table, put title line for summary of table
          (summary lines will be generated in "convert_row")         */
        if((strcmp(hsum.type, "3DTABLE")==0)
         ||(strcmp(hsum.type, "A3DTABLE")==0)
         ||(strcmp(hsum.type, "BINTABLE")==0) )
        {  if(qcont==0)
           {  printf("\n     scan# rx object   obsmode    veloc   r.a. ");
              printf(" dec.   dur. ax1     max    min \n");
           }
           else
           {  printf("\n      scan# object   obsmode  coordcd ");
              printf("   r.a.   dec.");
              printf(" Tint  ax1     max      min \n");
           }
        }
    }  /* end of if(qsum=0)    */
    
   return(0);
}
/******end of process_end********************************************/


/********************************************************************
 * SIMPLE - process keyword "simple"  This merely checks to see if  *
 *   The value of SIMPLE is True.  If not, then the program exits.  *
 ********************************************************************/
simple(kval)
KW_VAL *kval;
{
    strcpy(hsum.type, "MAIN");
    
    if(kval->logic!= 1 )
    {  LOGPR"\n SIMPLE not = T means tape is not standard FITS !");
       return(-1);
    }
    else  return(0);
}

/*********************************************************************
 * BITPIX - process keyword "BITPIX".  Stores value in global var    *
 *    "n_bitpix"                                                     *
 *********************************************************************/
bitpix(kval)
KW_VAL *kval;
{
   n_bitpix = kval->ival[0];
   return(0);
}

/********************************************************************
 * NAXIS_PROC - get value of NAXIS from header.                     *
 ********************************************************************/
naxis_proc(kval)
KW_VAL *kval;
{
   naxes = kval->ival[0];
   Naxd = naxes;             /* set max num of axes for axd struct */
   iaxes = 0;
   
   hsum.nxis1 = hsum.nxis2 = 0;
   
   return(0);
}

/********************************************************************
 * NAXISN_PROC - get value of NAXISn from header.                   *
 ********************************************************************/
naxisn_proc(kval)
KW_VAL *kval;
{
   int i;
 
   i = kval->naxi - 1;
   naxisn[i] = kval->ival[0];
   axd[i].no = i+1;      /* put NAXISn value into axis descriptor struct */
   axd[i].maxis = kval->ival[0];
   
   if(i==0) hsum.nxis1 = naxisn[i];
   if(i==1) hsum.nxis2 = naxisn[i];

  /* scan count option - output # scans from NAXIS2 of BINTABLE header. */
   if((qscanc==1)&&(flag3D==1)&&(axd[i].no==2))
   {  printf("%d", naxisn[i]);
      exit(0);                       /* exit from program at this point */
   }
   
   return(0);
}

/********************************************************************
 * MAXIS_PROC - get value of MAXIS from header.                     *
 ********************************************************************/
maxis_proc(kval)
KW_VAL *kval;
{
   maxes = kval->ival[0];
   Naxd = maxes;             /* set max num of axes for axd struct */
   
   return(0);
}

/********************************************************************
 * MAXISN_PROC - get value of MAXISn from header.                   *
 ********************************************************************/
maxisn_proc(kval)
KW_VAL *kval;
{
   int i;
 
   i = kval->naxi - 1;
   maxisn[i] = kval->ival[0];
   axd[i].no = i+1;      /* put MAXISn value into axis descriptor struct */
   axd[i].maxis = kval->ival[0];
   return(0);
}

/********************************************************************
 * DATE_PROC - get value of DATE from header.                       *
 ********************************************************************/
date_proc(kval)
KW_VAL *kval;
{
   strcpy(hsum.date, kval->cval);

   return(0);
}

/********************************************************************
 * DATEOBS_PROC - get value of DATE-OBS from header.                *
 ********************************************************************/
dateobs_proc(kval)
KW_VAL *kval;
{
   strcpy(hsum.date, kval->cval);

   return(0);
}


/********************************************************************
 * XTENSION_PROC - get value of XTENSION from header.               *
 *     If type of xtension is BINTABLE, set flag3D.                 *
 ********************************************************************/
xtension_proc(kval)
KW_VAL *kval;
{
    strcpy(hsum.type, kval->cval);

   if((strcmp(kval->cval,"3DTABLE")==0)
     || (strcmp(kval->cval,"A3DTABLE")==0)
     || (strcmp(kval->cval,"BINTABLE")==0) )
         flag3D=1;
    return(0);
}

/********************************************************************
 * EXTNAME_PROC - get value of EXTNAME from header.                 *
 *     If type of extname if UNIPOPS SINGLE DISH, then              *
 *     set flagGBSD.                                                *
 ********************************************************************/
extname_proc(kval)
KW_VAL *kval;
{
    if(strcmp(kval->cval,"UNIPOPS SINGLE DISH")==0)
       flagGBSD=1;
    return(0);
}

/********************************************************************
 * TFIELDS_PROC - get value of TFIELDS from header.                 *
 *    Allocate space for bt table.                                  *
 ********************************************************************/
tfields_proc(kval)
KW_VAL *kval;
{
   char ermsg[64];
   int i;
   
   Ntfields = kval->ival[0];
   
   if((bt=(BTAB *)malloc((long)(Ntfields+2)*sizeof(BTAB)))
     ==0)
   {   sprintf(ermsg,"Cannot allocate space for btab %d",Ntfields);
       errexit(ermsg);
   }
   btflag=1;
   
   LOGPR"\n Ntfields=%d", Ntfields);
   
   /* set all matflg s to zero */
   for(i=0; i<=Ntfields; i++)
      (bt+i)->matflg = 0;   
   
    return(0);
}

/*********************************************************************
 * TFORM_PROC - decode the format letter (A,I,E,D) from the TFORM    *
 *   keyword and put them into the bt table.                         *
 *********************************************************************/
tform_proc(kval)
KW_VAL *kval;
{
    int i,j,jj;
    char tmp[16];
    
    i = kval->naxi;
    (bt+i)->offs = btoffs;  /* set offset into FITS binary table */
    
  /* skip leading quote, blanks, or control chars */
    for(j=0; ((j<32)&&
           ((kval->cval[j]=='\'')||(kval->cval[j]<=' ')) ); j++) ; 
    
    jj=0;
    for(; ((j<8)&&(kval->cval[j]>='0')&&(kval->cval[j]<='9')); j++,jj++)
       tmp[jj]=kval->cval[j];
    tmp[jj]=0;
    (bt+i)->tmul = atoi(tmp);
    (bt+i)->form = kval->cval[j];

    LOGPR"\nTFORM_PROC: %s, mul=%d, form=%c ",
           kval->names[0], (bt+i)->tmul, (bt+i)->form);

    btoffs += ((bt+i)->tmul) * (form_size((bt+i)->form)) ;
    return(0);
}

/********************************************************************
 * TTYPE_PROC - get value of TTYPE for current table column from    *
 *    header and put it into the bt structure.                      *
 ********************************************************************/
ttype_proc(kval)
KW_VAL *kval;
{
    int i;
    
    i = kval->naxi;
    strcpy((bt+i)->ttype, kval->cval);
    
    return(0);
}

/********************************************************************
 * TUNIT_PROC - get value of TUNIT for current table column from    *
 *    header and put it into the bt structure.                      *
 ********************************************************************/
tunit_proc(kval)
KW_VAL *kval;
{
    int i;
    
    i = kval->naxi;
    strcpy((bt+i)->tunit, kval->cval);
    
    return(0);
}


/********************************************************************
 * TMATX_PROC - if TMATX is true, then set flag in bt structure.    *
  ********************************************************************/
tmatx_proc(kval)
KW_VAL *kval;
{
    int i;
    
    i = kval->naxi;
    if(kval->logic>0)
      (bt+i)->matflg = 1;
          
    return(0);
}

/********************************************************************
 * TDIM_PROC - if TDIM is true, then set flag in bt structure.    *
  ********************************************************************/
tdim_proc(kval)
KW_VAL *kval;
{
    int i;
    
    i = kval->naxi;
      (bt+i)->matflg = 1;
          
    return(0);
}


/******************************************************************
 * OBSMODE_PROC - find whether obsmode is line or cont and set    *
 *    qcont to 0 for line or 1 for continuum.                     *
 ******************************************************************/
 obsmode_proc(kval)
 KW_VAL *kval;
 {
     qcont = -1;
     if(strncmp(kval->cval, "LINE", (long)4)==0) qcont=0;
     if(strncmp(kval->cval, "CONT", (long)4)==0) qcont=1;
     
     fprintf(logfile,"\nFound OBSMODE= %s, qcont=%d",
        kval->cval, qcont);
     fflush(logfile);
     
     strcpy(hsum.omode, kval->cval);
     
     return(0);
 }


/***************************************************************
 * TELE_PROC - store telescope name in hsum array.             *
 ***************************************************************/
 tele_proc(kval)
 KW_VAL *kval;
 {
    strcpy(hsum.tele, kval->cval);
    hsum.tele[8] = 0;
    return(0);
 }
 
 /**************************************************************
 * PID_PROC - store project ID name in hsum array.             *
 ***************************************************************/
 pid_proc(kval)
 KW_VAL *kval;
 {
    strcpy(hsum.obsr, kval->cval);
    return(0);
 }
 
 /***************************************************************
 * OBSR_PROC - store observer's name in hsum array.             *
 ***************************************************************/
 obsr_proc(kval)
 KW_VAL *kval;
 {
    strcpy(hsum.obsr, kval->cval);
    hsum.obsr[8]=0;
    return(0);
}

/*****************************************************************
 * CRVAL_PROC - process this axis descriptor (#20 in kw array)   *
 *****************************************************************/
crval_proc(kval)
KW_VAL *kval;
{   int i;
    i = kval->naxi - 1;
    axd[i].crval = kval->dval[0];
    return(0);
}

/*****************************************************************
 * CRPIX_PROC - process this axis descriptor (#21 in kw array)   *
 *****************************************************************/
crpix_proc(kval)
KW_VAL *kval;
{   int i;
    i = kval->naxi - 1;
    axd[i].crpix = kval->dval[0];
    return(0);
}

/*****************************************************************
 * CDELT_PROC - process this axis descriptor (#22 in kw array)   *
 *****************************************************************/
cdelt_proc(kval)
KW_VAL *kval;
{   int i;
    i = kval->naxi - 1;
    axd[i].cdelt = kval->dval[0];
    return(0);
}

/*****************************************************************
 * CTYPE_PROC - process this axis descriptor (#23 in kw array)   *
 *****************************************************************/
ctype_proc(kval)
KW_VAL *kval;
{   int i;
    i = kval->naxi - 1;
    strcpy(axd[i].ctype, kval->cval);
    return(0);
}

/*****************************************************************
 * CROTA_PROC - process this axis descriptor (#24 in kw array)   *
 *****************************************************************/
crota_proc(kval)
KW_VAL *kval;
{   int i;
    i = kval->naxi - 1;
    axd[i].crota = kval->dval[0];
    return(0);
}

