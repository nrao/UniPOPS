/*  @(#)uinit.c	5.2 11/22/94	*/
/************************************************************************
 * UINIT - read in translation table and initialize table in core.      *
 *   qcont=0 for line, =1 for continuum, = -1 for both.                 *
 *   pname is the table type:  "u2f" is used for both programs u2f and  *
 *      f2u.                                                            *
 *   direc=0 for translating UNIPOPS to FITS; =1 for FITS to UNIPOPS.   *
 *    (for the FITS to UNIPOPS version, the translation table is sorted *
 *      into order by ttype, so that the FITS reader can do a binary    *
 *      search to find the column name.                                 *
 ************************************************************************/
#include <stdio.h>
#include <errno.h>

#ifdef THINK_C
#include <stdlib.h>
#else
#define SEEK_SET 0
#endif

#include "u2fglob.h"
int ftime, nthead;           /* define these flags */

/***********************************************************************
 * procedure inittrans starts here.                                    *
 ***********************************************************************/
inittrans(qcont,pname, direc)
int qcont, direc;
char pname[8];
{
    char tfname[80], tc[8], st[140], tmp[16], *ppfx, *getenv();
    FILE *tfile;
    int i, iflg, nlines, jcol;
    int imhed, ithed, lenst;
    
    /* initialize flags */
     ftime = nthead = 1;
     iflg=0;
     i=imhed=ithed=0;
          
    strcpy(tc,pname);

    /* make name of translation file:  U2f_trans   */
    
    sprintf(tmp,"%s_trans",tc);
    fprintf(logfile,"\nINITTRANS: qcont=%d, pname=%s, direc=%d",
     qcont,pname,direc);
    fflush(logfile);
    fprintf(logfile,"\n Transfile= %s",tmp);
    fflush(logfile);
  
  /* look for translation file first in local directory, then in
     "popsdir" directory                                      */
     
    if((tfile=fopen(tmp,"r"))==NULL)
    {  
		/* clear the expected error from above */
       errno = 0;
       if((ppfx=getenv("popsdir"))==NULL)
           errexit("INITTRANS: Cannot find popsdir.");
       strcpy(tfname, ppfx);
       strcat(tfname, "utilities/");
       strcat(tfname, tmp);
       if((tfile=fopen(tfname,"r"))==NULL)
       {  sprintf(st, "Cannot open file %s", tfname);
          errexit(st);
       }
    }
    else  strcpy(tfname,tmp);
    
    fprintf(logfile,"\nInitTrans: opened file %s ", tfname);
      
    /* preview tfile, get Ntrans, MHnlines, THnlines */
     prev_tfile(st,tfile);

    /* allocate space for trans-table (tt) */
     if((tt=(TRANS_TABLE *)
            malloc((long)((Ntrans+1)*sizeof(TRANS_TABLE))))==0)
     {  sprintf(st,"INITTRANS: cannot allocate %d trans_tables!",Ntrans+1);
        errexit(st);
     }  
     fprintf(logfile,"\nAllocated for TRANS_TABLE %ld bytes, %d tables.",
          (long)((Ntrans+1)*sizeof(TRANS_TABLE)), (Ntrans+1));
     ttflag=1;

     if(direc==0)   /* only allocate header strucs if making FITS */
  
     {  alloc_tab(&mh,MHnlines+1);  /* allocate space for table structures */
     
       /* estimate space required for th table */
        THtotal = THnlines + 3*Ntrans + 5;  /* need more if >1 dim matrix */
        alloc_tab(&th,THtotal);
        hbufflag=1;
      }

      i=0;   jcol=1;
    /*************************************************************************
     * start of loop for reading trans file  table                           *
     *************************************************************************/
     while(fgets(st,140,tfile)!=NULL)
     {  if(strncmp(st,"END",(long)3)==0)  iflg++;  /* check for END keyword */
        else
        {
          if((iflg==0)&&(strncmp(st,"/*",(long)2)!=0)) /* get trans table */
          {  if(ptabline(st,(tt+i))==0)
             {   (tt+i)->ncol = 0;              /* initialize column #    */  
                                                /* set FITS column number */      
                if((tt+i)->sflag>= 0)  
                 {  if((tt+i)->opt==0)  (tt+i)->ncol = jcol++;    
                    else
                    {   if(((tt+i)->opt==1)&&(qvof==1))
                                      (tt+i)->ncol = jcol++;
                        if(((tt+i)->opt==2)&&(qvof==0))
                                      (tt+i)->ncol = jcol++;
                        if(((tt+i)->opt==3)&&(qvof==-1))
                                      (tt+i)->ncol = jcol++;
                    } 
                 }     
                 i++;
             }   
          }
          if(direc==0)
          {  if((iflg==1)&&(strncmp(st,"/*",(long)2)!=0))
             {   if(strlen(st)>8)    
                 {  ghdline(st, (mh+imhed));    /* get main header lines */
                    imhed++;
                 }
             }
             if((iflg==2)&&(strncmp(st,"/*",(long)2)!=0)) 
             {   if(strlen(st)>8)
                 {  ghdline(st, (th+ithed));    /* get table header lines */
                    ithed++;
                 }
             }
           }
        }   /* end of else */
     }   /* end of while loop */
          
     Ntrans = i;
     
     fprintf(logfile,"\nRead %d of trans, %d of main, %d of tab, from %s",
        Ntrans, MHnlines, THnlines, tfname);
     fflush(logfile);
     fclose(tfile);

     /* calc offsets in FITS binary table row, and total width, Twidth */

      getWidth(&Twidth);
      fprintf(logfile,"\n Twidth= %d", Twidth);
      fflush(logfile);
          
     /* check that multiplicities are the same for sflag=0 lines */
      for(i=0; i<Ntrans; i++)
      {  if((tt+i)->sflag==0)
         {  if((tt+i)->mform != (tt+i)->minf)
            { sprintf(st,"format multiplicities do not match, line %d %d %d %s",
                i, (tt+i)->mform, (tt+i)->minf, (tt+i)->descr);
               errexit(st);
             }
          }
       }

      /* zero out all the function pointers */
       for(i=0; i<Ntrans; i++)  (tt+i)->fun = NULL;
 
    /*  turned off debug print of table  
     fprintf(logfile,"\n       ttype      ff    tunit ncol kat/op axis");
     fprintf(logfile,"  uf foff ucl uoff");
     for(i=0; i<Ntrans; i++)        
     {  fprintf(logfile,"\n %12s, %4d%c %8s%4d %4s%2d%4d %4d%c %4d",  
           (tt+i)->ttype, (tt+i)->mform, (tt+i)->fform, (tt+i)->tunit,
           (tt+i)->ncol, (tt+i)->kat, (tt+i)->opt, (tt+i)->axis, 
           (tt+i)->minf, (tt+i)->inform, (tt+i)->foff);
         fprintf(logfile," %3d %4d %8s %4d",
           (tt+i)->ucl, (tt+i)->uoff, (tt+i)->cfact, (tt+i)->sflag); 
         fprintf(logfile," %12.5e %6.1f, %5d",
            (tt+i)->factor, (tt+i)->offset, (tt+i)->foff);
         fflush(logfile);
      }
     /*  initialize the function pointers in the tt structs */    
         if(direc==0)  init_u2f_fun();
         if(direc==1)  init_f2u_fun();

     /*  if translating to UNIPOPS from FITS, exit here  */
         if(direc==1) return(0);

     /*  add the table column descriptors to the table header structure */

       make_col_kwds();

    /* turned off debug print of main header table 
      for(i=0; i<MHnlines; i++)
      {  fprintf(logfile,"\nMainkw:%9s, col:%2d, wid:%2d ",
            (mh+i)->kw, (mh+i)->icol[0], (mh+i)->iwid[0]);
         fflush(logfile);
       }
    /* turned off debug print of secondary header table 
      for(i=0; i<THnlines; i++)
      {  fprintf(logfile,"\nTab.Head.kw:%9s, col:%2d, wid:%2d ",
            (th+i)->kw, (th+i)->icol[0], (th+i)->iwid[0]);
         strncpy(st,(th+i)->htext, (long)50);
         st[50]=0;
         fprintf(logfile,"::%s",st);
         fflush(logfile);
       }
     */
     return(0);
}
/**********************end of inittrans*************************************/



/*************************************************************************
 * PTABLINE - read in line from translation table                        *
 *************************************************************************/
 ptabline(st,table)
 TRANS_TABLE *table;
 char *st;
 {    
    int i,ii;
    double pi;
    char cc;
    
    pi = 3.14159265358979323;
    
    if(strlen(st)<8) return (-1);      /* skip if blank or line < 8 chars */
    if((strncmp(st,"        ",(long)8)==0))
       return(-1);
    if((strncmp(st,"/*",(long)2)==0))  /* skip if line starts with "/*" */ 
       return(-1);
       
    /* scan line - get all parameters but description */
    
    sscanf(st,"%11s%5d %1c %10s%2s %2d %4d %3d %4d %3d %1c %8s %d",
      table->ttype, &table->mform, &table->fform, table->tunit, table->kat,
      &table->opt, &table->axis, &table->ucl, &table->uoff, &table->minf, 
      &table->inform, table->cfact, &table->sflag);

     /* Make sure that if tunit starts with a ', i.e. is a string, that it */
     /* contains 8 chars between the quotes */

     if (table->tunit[0] == '\'') {
        int tlen = strlen(table->tunit);
        if (tlen < 2 || table->tunit[tlen-1] != '\'') {
		/* The last non-null char MUST be the second quote */
           sprintf(st,"Error in TUNIT field for TTYPE = %s, TUNIT = %s",
                      table->ttype, table->tunit);
           errexit(st);
        }
        for (i=tlen-1;i<9;i++) table->tunit[i] = ' ';
        table->tunit[9] = '\'';
        table->tunit[10] = '\0';
    }
        
                
     /* find slash, get description from last 40 cols */
     
     for(i=30; ((i<140)&&(st[i]!=0)); i++) 
       if(st[i]=='/') break;
     strcpy(table->descr, (st+i+2));
     
     /* remove trailing blanks */
     ii = strlen(table->descr);
     for(i=ii; i>1; i--)
     {   if((table->descr[i]!='\n')&&(table->descr[i]!=0)
            &&(table->descr[i]!=' '))
            break;
      }
      table->descr[i+1]=0;     /* truncate description at blank trailer */
     /* blank any control characters that may have been left in the line */
      for(i=0; i<strlen(table->descr); i++)
        if(table->descr[i]<' ')  table->descr[i]=' '; 
    /*  blank fill the description out to char # 64 */
      for(i=strlen(table->descr); i<64; i++)  table->descr[i]=' ';
      table->descr[64] = 0;
      
    /* fix up kat */
      for(i=0; i<strlen(table->kat); i++)
         if(table->kat[i]!=' ')  { cc=table->kat[i];  break; };
      table->kat[0] = cc;  table->kat[1]=0;
      
     /* fill in scale factor data */
      table->factor = 1.0;
      table->offset = 0.0;
      
      /* radians to degrees */
      if(strcmp(table->cfact,"RTD")==0)  table->factor = 57.2957795;

      /* radians to hours   */
      if(strcmp(table->cfact,"RTH")==0)  table->factor = 3.819718634;
      
      /* radians to seconds of time */
      if(strcmp(table->cfact,"RTS")==0)  table->factor = 13750.98708;

      /* hours to seconds of time */
      if(strcmp(table->cfact,"HTS")==0)  table->factor = 3600.;

      /* arcseconds to degrees */
      if(strcmp(table->cfact,"STD")==0)  table->factor = 1.0/3600.;

      /* arcminutes to degrees */
      if(strcmp(table->cfact,"MTD")==0)  table->factor = 1.0/60.;
      
      /* mm to meters */
      if(strcmp(table->cfact,"mmTM")==0) table->factor = 0.001;
      
      /* cm to meters */
      if(strcmp(table->cfact,"cmTM")==0) table->factor = 0.01;

      /* KM to meters */
      if(strcmp(table->cfact,"KTM")==0) table->factor = 1000.0;

      /* MHz to Hz */
      if(strcmp(table->cfact,"MHZTHZ")==0) table->factor = 1E6;
      
      /* Centigrade to Kelvin */
      if(strcmp(table->cfact,"CTK")==0)   table->offset = 273.15;

      /* times 100  */
      if(strcmp(table->cfact,"x100")==0)  table->factor= 100.0; 

      /* percent to fraction */
      if(strcmp(table->cfact,"%Tf")==0)  table->factor= 0.01;  

      /* cm of Hg to Pascals */
      if(strcmp(table->cfact,"cmHgTP")==0) table->factor = 1333.22;

    return(0);
 }


/**********************************************************************
 * GHDLINE - get header format info from transtable                   *
 **********************************************************************/
ghdline(st, hd)
char *st;
HEAD_TABLE *hd;
{
   int ii,j, jj;
   char str1[64],str2[64];
         
   /* put total header line into htext */
   ii=strlen(st);  if(ii>80)ii=80;
   strncpy(hd->htext, st, (long)ii);
   
   /* blank remainder of htext field */
   for(j=ii-1; j<81; j++)  hd->htext[j]=' ';
   hd->htext[80]=0;
   
   /* find keyword in st */
   sscanf(st,"%s %s", str1,str2);
   ii=strlen(str1);
   if(str1[ii-1]=='=')  str1[ii-1]=0;
   if(strcmp(str1,"HISTORY")==0)  strcpy(str1,str2);
   strcpy(hd->kw, str1);
   
   /* initialize icol,iwid,hform */
   for(ii=0; ii<2; ii++)
   {   hd->icol[ii]=0;
       hd->iwid[ii]=0;
    }
   /* find the % sign - it tells the format */
   j=0;
   for(ii=9; ((ii<80)&&(st[ii]!='\n')); ii++)
   {  if(st[ii]=='%')
      {  if(j<=2)
         {   hd->icol[j]=ii;                /* store the column number */
             ii++;
             jj=0;
             while((st[ii]>='0')&&(st[ii]<='9'))
             {  str2[jj] = st[ii];
                jj++;
                ii++;
              }
             str2[jj]=0;
             hd->iwid[j] = atoi(str2);      /* get the field width */
          }
          j++;
       }
    }
} /********end of ghdline**********************************************/


/***************************************************************
 * GETWIDTH - calc byte offsets of each FITS table column      *
 *     and store into tt[i].foff.                              *
 *     Also get total table width (=Twidth)                    *
 ***************************************************************/
getWidth(twidth)
int *twidth;
{
   char st[80];
   int tw, i, iw;

   tw = 0;
   *twidth = 0;
   
   for(i=0; i<Ntrans; i++)
   {   (tt+i)->foff = tw;       /* set byte offset for FITS column i */

       if( (iw=form_size((tt+i)->fform))==0)
       {  sprintf(st,"wfits: bad format type, %c, line %d of table.",
               (tt+i)->fform, i);
          errexit(st);
       }
       if((tt+i)->ncol>0)   /* ncol=0 means it does not go in table */
       {   iw *= (tt+i)->mform;
           tw += iw;                       /* increment byte offset */
       }
   }
   *twidth = tw;        /* Twidth is # bytes width of whole FITS table. */
   ScanWid = iw;        /* scan width (=#bytes per scan matrix)         */
                        /* is width of last table column                */
                        /* ScanMultiplicity is # points per scan matrix.*/
   ScanMult= (tt+Ntrans-1)->mform;
   maxTwid = tw-iw;     /* get min size for maxTwid =row width, sans matrix */
   fprintf(logfile,"\nGetWidth:  maxTwid=%d,  ScanWid=%d ",
       maxTwid, ScanWid);
       fflush(logfile);
   
   /* check whether scan length is to be truncated */
      
   if((maxWscan!=0)&&(maxWscan<ScanMult))  /* allow only truncation!! */
   {  ScanMult = maxWscan;
      ScanWid  = ScanMult * form_size((tt+Ntrans-1)->fform);
      *twidth = tw-iw + ScanWid;
   }
} /****end of getWidth*************************************************/


/**********************************************************************
 * PREV_TFILE - read thru tfile to find sizes for the 3 tables -      *
 *   Ntrans, MHnlines, THnlines                                       *
 **********************************************************************/
 prev_tfile(st,tfile)
 FILE *tfile;
 char *st;
 {
      int i, imhed, ithed, iflg;
      
      i=imhed=ithed=iflg=0;
      
      while(fgets(st,140,tfile)!=NULL)
     {  if(strncmp(st,"END",(long)3)==0)  iflg++;   /* check for END keyword */
        else
        {
          if((iflg==0)&&(strncmp(st,"/*",(long)2)!=0)) /* get trans table */
          {  if(strlen(st)>8)  i++;
          }
          if((iflg==1)&&(strncmp(st,"/*",(long)2)!=0))
          {   if(strlen(st)>8) imhed++;
           }
          if((iflg==2)&&(strncmp(st,"/*",(long)2)!=0)) 
          {   if(strlen(st)>8) ithed++;
              
           }
        }   /* end of else */
     }   /* end of while loop */
     
     Ntrans = i;
     MHnlines = imhed;
     THnlines = ithed;
     
     fprintf(logfile,"\nRead %d of trans, %d of main, %d of tab.",
        Ntrans, MHnlines, THnlines);
     fflush(logfile);
 
    /*  "rewind" tfile to the beginning */
    
    if( fseek(tfile, (long)0, SEEK_SET)!=0)
       errexit("Cannot rewind tfile!");
       
 } /****end of prev_tfile**********************************************/
 
 
/**********************************************************************
 * ALLOC_TAB - allocate space for table structure.                    *
 **********************************************************************/
 alloc_tab(hd,nn)
 int nn;
 HEAD_TABLE **hd;
 {
     char st[64];
     long int mm;
     
     mm = nn*(long)sizeof(HEAD_TABLE);
     
     if( (*hd = (HEAD_TABLE *)malloc(mm))==0)
     {   sprintf(st, "Alloc_tab:  Cannot allocate %ld bytes, %d tables.", mm, nn);
         errexit(st);
     }
     fprintf(logfile,"\nAllocTab: allocated %ld bytes, %d tables.", mm, nn);
     fflush(logfile);
 }
 
 
/*********************************************************************
 * MAKE_COL_KWDS - add all the FITS binary table column descriptors, *
 *   i.e, TTYPE, TFORM, TUNIT, etc, to the "th" structure, as        *
 *   directed by the translation table.                              *
 *********************************************************************/
make_col_kwds()
{
    int i, j, jt, mm, nax, tax, lastcoln;
    char st[80];
    
    jt = THnlines - 1;
    
    for(i=0; i<Ntrans; i++)
    {
        /* check axis descriptor = 10*axis number plus axis part */
        /*   n1 = axis MAXISn (dimension)
             n2 = axis CTYPE (name of quantity)
             n3 = axis CDELT (increment)
             n4 = axis CRPIX (reference pixel)
             n5 = axis CRVAL (value at reference pixel)
         */
         
        /* check options, maybe ignore line altogether */
         if( (((tt+i)->opt == 1)&&(qvof!=1)) ||
             (((tt+i)->opt == 2)&&(qvof!=0)) ||
             (((tt+i)->opt == 3)&&(qvof!=-1)))
           continue;

        jt++;
        new_th(jt);     /* initialize new Table Header line */
        
         /* for ncol=0, insert line in header, but it is not in the table */
         if( (tt+i)->ncol == 0)
         {                       /* the keyword is ttype, the value is tunit */
            strcpy((th+jt)->kw, (tt+i)->ttype);
            if((tt+i)->sflag >= -1)   /* check for comment line */
            {  mm = -1;
               for(j=0; j<strlen((tt+i)->tunit); j++)
               {  if((tt+i)->tunit[j]=='\'')       /* check for string value */
                  { sprintf( (th+jt)->htext, "%-8s= %-10s  ",
                               (tt+i)->ttype, (tt+i)->tunit);
                    mm=strlen((th+jt)->htext);
                    (th+jt)->htext[mm]=' ';
                    break;
                  }
               }
               if(mm== -1)  /* if numeric value, right justify to col.30 */
               {  mm=strlen((tt+i)->tunit);
                  for(j=mm-1; ((j>=0)&&((tt+i)->tunit[j]==' ')); j--) ; 
                  (tt+i)->tunit[j+1] = '\0';
                  sprintf( (th+jt)->htext, "%-8s= %20s ",
                                         (tt+i)->ttype, (tt+i)->tunit);
                  mm=strlen((th+jt)->htext);
                  (th+jt)->htext[mm]=' ';
               }
               (th+jt)->htext[36] = '/';
               mm=strlen((tt+i)->descr);
               if(mm>42)mm=42;
               for(j=0; j<mm; j++) (th+jt)->htext[j+38] = (tt+i)->descr[j];
             }
             else       /* comment line to be inserted in table header */
             {   strcpy( (th+jt)->htext, "            / ");
                 strncat( (th+jt)->htext, (tt+i)->descr, (long)64);
                 (th+jt)->htext[ strlen((th+jt)->htext)]=' ';
                 (th+jt)->htext[80]=0;
             }
         }

         if( (tt+i)->ncol > 0)          /* normal table entry */
         {
             sprintf((th+jt)->kw, "TFORM%d", (tt+i)->ncol);
             sprintf((th+jt)->htext,
                    "TFORM%-3d= '%d%c        '",
                    (tt+i)->ncol, (tt+i)->mform, (tt+i)->fform);
             mm=strlen((th+jt)->htext);
             (th+jt)->htext[mm]=' ';
             (th+jt)->htext[36] = '/';
             
             /* special case for TFORM for the matrix */
             if(i==(Ntrans-1))
                          /* place to insert new multiplicity later */
             {   (th+jt)->icol[0]=10; 
                 (th+jt)->iwid[0]=8;
             }
             
             jt++;
             new_th(jt);
             sprintf((th+jt)->kw, "TTYPE%d", (tt+i)->ncol);
             lastcoln = (tt+i)->ncol;
             sprintf((th+jt)->htext,
                    "TTYPE%-3d= '%-12s '",
                    (tt+i)->ncol, (tt+i)->ttype);
             mm=strlen((th+jt)->htext);
             (th+jt)->htext[mm]=' ';
             (th+jt)->htext[36] = '/';
             mm=strlen((tt+i)->descr);
             if(mm>42)mm=42;
             for(j=0; j<mm; j++)
                (th+jt)->htext[j+38] = (tt+i)->descr[j];
             
             if(strcmp((tt+i)->tunit,"-")!=0)
             {  jt++;
                new_th(jt);
                sprintf((th+jt)->kw, "TUNIT%d", (tt+i)->ncol);
                sprintf((th+jt)->htext,
                    "TUNIT%-3d= '%-12s '",
                    (tt+i)->ncol, (tt+i)->tunit);
                mm=strlen((th+jt)->htext);
                (th+jt)->htext[mm]=' ';
                (th+jt)->htext[36] = '/';
             }
         }
    }  /***end of for(i=...)*********************************/
    
    /* put final TDIMnnn line, to designate the matrix */
    jt++;
    new_th(jt);
    sprintf((th+jt)->kw, "TDIM%d", lastcoln);
    sprintf((th+jt)->htext, 
 "TDIM%-3d = '(%d,1,1,1,1)'         / Dimensions of Matrix.",
       lastcoln, ScanMult);
    mm=strlen((th+jt)->htext);
    (th+jt)->htext[mm]=' ';
    /* place to insert new multiplicity later for TDIM */
     {   (th+jt)->icol[0]=10; 
         (th+jt)->iwid[0]=17;
      }
    jt++;
    
    THnlines = jt;    
    Tfields = lastcoln;
} /****end of make_col_kwds*********************************************/


new_th(jt)      /* initialize new Table Header line */
int jt;
{        
    char st[64];
    int j;
    
    if(jt>THtotal)
    {   sprintf(st, "make_col_kwds: th exceeds allocation %d", jt);
        errexit(st);
    }
   /* zero out th struct, item #jt */
    for(j=0;j<81;j++)  (th+jt)->htext[j] = ' ';
    for(j=0;j<2; j++)
    {  (th+jt)->icol[j]=0;
       (th+jt)->iwid[j]=0;
    }
}
