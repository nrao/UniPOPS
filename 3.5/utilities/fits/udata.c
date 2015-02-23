/*  @(#)udata.c	5.4 04/23/98	*/
/**************************************************************************
 * The UDATA module reads the data file from standard input and           *
 *   reformats it as a FITS binary table, directed by a translation table.*
 **************************************************************************/
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

#include "unipops.h"
#include "u2fglob.h"

/* more globals (see u2fglob.h) */
long  siz_tbuf;          /* required size of binary table buffer   */
long  OutCount;          /* count of table lines                   */
int   maxmaxis1;         /* max val of maxis1 = fixed matrix wid   */
extern int maxWscan;     /* truncated value of maxis1, user option */
extern int ScanMult;
int feednum;             /* current feed number (hidden in SCAN)   */
int kpflag;              /* =1 if Kitt Peak data (NRAO 12M)        */
int p11flag;             /* =1 if prototype class 11 (PROTO12M)    */

float DATAmax, DATAmin;  /* max and min data values for whole file */

char fitsbuf[FITS_REC_LEN];   /* output buffer for FITS file       */
int  jfbuf;              /* pointer to char pos in fitsbuf         */
int  FitCount;           /* number of fits records written         */
long rrec;               /* selected record number - used in utrans */

int Naxd, *axd;          /* dummy defns for compatibility with f2u */

UNIAUX auxbuf;           /* buffer for auxiliary input data        */
long upsize[16];         /* actual size of unipops class in bytes  */
/*************start of getdata********************************************/
getdata()
{
    long jread, qread, ipnt, itmp, usize;
    int i;
    unsigned int ntoread;
    long recnum;
    long headlen, datalen, buflen;
    int ivf;                       /* floating validity flag */
    char st[80], tmpstr[64];
    float *fbf;
    short *sbf;
    double dtmp, ieee_to_dub();
    
    DATAmax = -1.0E30;  DATAmin = 1.0E30;
    OutCount = 0;
    FitCount = 0;
    maxmaxis1 = 0;

    up.ux = &auxbuf;   /* set pointer to unipops auxiliary buffer */
    
    /* make temp file for FITS table, if in non-real-time mode. */
    if(maxrecs==0 && prev==0) create_temp();
    /* create initial space for ubuf */
    usize = 4096;
    if((ubuf = (char *)malloc( usize ))==0)
       errexit("GetData: cannot allocate space for ubuf!");
    fprintf(logfile,"\nGetData: allocated for ubuf %ld bytes.", usize);
    fflush(logfile);
    ubufflag = 1;

    recnum=rrec=0;
    tbufflag=0;
    qread=1;
    ipnt=0;          /* pointer to next location in ubuf to be read */
  /*----------------------------------------------------------------*/
  /* read input UNIPOPS file from stdin until EOF                   */
  /* one UNIPOPS record per loop                                    */
  /*----------------------------------------------------------------*/
    while(qread == 1)     /* qread set to zero by read error or EOF */
    {
     /* first read the header portion of the record */
      if( (jread = read(0, ubuf, 32)) != 32)   { qread=0; continue; }
      ipnt = 32;
      sbf = (short *)ubuf;
      /* set pointers array  - upnt is in units of bytes */
       up.u0  = (UNICLASS0 *)ubuf;
      /* Do some sanity checks on the values in class 0 */
         if (up.u0->Nheadcls <= 0 || 
             up.u0->Nheadcls >= 15 ||
             up.u0->head_ptr[0] < 5) continue;
      /*         Its not an error, just skip to next record */
         upnt[0] = up.u0->Nheadcls;
         fprintf(logfile, "\nUDATA: rec=%ld, #cls=%ld, ptr=",
               recnum, upnt[0]);
         fflush(logfile);
         for(i=1; i<16; i++)   
         {  upnt[i] = (up.u0->head_ptr[i-1] - 1) * 8 ;
            fprintf(logfile, " %ld", upnt[i]);
         }
         fflush(logfile);

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

     /* get class 1 */
      itmp = upnt[2];                  /* pointer to start of class 2 */
      ntoread = itmp - ipnt;
      if( (jread = read(0, (ubuf+ipnt), ntoread)) != ntoread)  
      {  fprintf(logfile,"\nUdata: read problem1?  n= %d, read= %d ",
                   ntoread, jread);
         qread=0; continue;
      }
      ipnt += ntoread;
      itmp = upnt[1];                         /* pointer to HEADLEN */
      dtmp = ieee_to_dub( (ubuf+itmp), &ivf);    headlen = dtmp;
      fprintf(logfile,"\nUDATA: Headlen = %8.4E ", dtmp);
      dtmp = ieee_to_dub( (ubuf+itmp+8), &ivf);  datalen = dtmp;
      fprintf(logfile,"\nUDATA: Datalen = %8.4E ", dtmp);
      buflen = headlen + datalen;           /* total length of data record */
      
    /* buflen needs to be padded to the next multiple of 512 bytes */
       if((buflen%512)>0)
       {  itmp = buflen/512;
          buflen = 512*(itmp+1);
       }

    /* make sure that unpt[i] point to valid things and upsize[i] is set */
      if (upnt[15] <= 0) upnt[15] = headlen;
      upsize[15] = headlen - upnt[15];
      for (i=14;i>0;i--) {
         if (upnt[i] <= 0) upnt[i] = upnt[i+1];
         upsize[i] = upnt[i+1] - upnt[i];
      }

      up.series = (float *) (ubuf + headlen);  /* pntr to series data */
      fprintf(logfile,"\nUdata: hdlen=%ld, datlen=%ld, buflen=%ld",
               headlen, datalen, buflen);
      fflush(logfile);

     /* allocate more space if necessary */
      if(buflen > usize)
      {  if((ubuf = (char *)realloc(ubuf, buflen))==0)
             errexit("GetData: cannot allocate more space for ubuf!");
          fprintf(logfile,
               "\nGetData: re-allocated for ubuf %ld bytes.", buflen);
          usize = buflen;
     /* Now we need to reposition the pointers that depend on ubuf */
         sbf = (short *)ubuf;
         up.u0 = (UNICLASS0 *)ubuf;
         up.u1 = (UNICLASS1 *) (ubuf+upnt[1]);
         up.u2 = (UNICLASS2 *) (ubuf+upnt[2]);
         up.u3 = (UNICLASS3 *) (ubuf+upnt[3]);
         up.u4 = (UNICLASS4 *) (ubuf+upnt[4]);
         up.u5 = (UNICLASS5 *) (ubuf+upnt[5]);
         up.u6 = (UNICLASS6 *) (ubuf+upnt[6]);
         up.u7 = (UNICLASS7 *) (ubuf+upnt[7]);
         up.u8 = (UNICLASS8 *) (ubuf+upnt[8]);
         up.u9g = (UNICLASS9G *) (ubuf+upnt[9]);
         up.u9k = (UNICLASS9K *) (ubuf+upnt[9]);
         up.u10 = (UNICLASS10 *) (ubuf+upnt[10]);
         up.u11 = (UNICLASS11 *) (ubuf+upnt[11]);
         up.u11p = (UNICLASS11P *) (ubuf+upnt[11]);
         up.u12 = (UNICLASS12 *) (ubuf+upnt[12]);
         up.u13 = (UNICLASS13 *) (ubuf+upnt[13]);
         up.series = (float *) (ubuf + headlen);
      }
     /* now read the remainder of the record */
     /* allow repeated reads from std input to allow pipelines */
      while( (ntoread = buflen - ipnt) > 0)
      {  if( (jread = read(0, (ubuf+ipnt), ntoread)) <= 0)  
         {   sprintf(tmpstr, "Udata: read problem2?  n= %d, read= %d ",
                      ntoread, jread);
             qread=0;
             errexit(tmpstr);
          }
          ipnt += jread;
       }
       recnum++;
       if(dselect(&up,recnum)==1)    /* is it within selection range? */
       {  rrec++;
          get_hdr_dat(&up);  /* decode various parameters from the input rec */
   
          /* initialize the max and min values */
              up.ux->datamax = -1.0E30;   up.ux->datamin = 1.0E30; 
          
         L_pre_scan(&up);   /* pre-process scan, whether line or cont. */  

       /* if preview mode continue with while */
         if (prev==1) continue;
                       
      /* allocate and zero the tbuf  */

          siz_tbuf = Twidth ;
  
         if( (tbuf=(char *)calloc(siz_tbuf, (long)1) )==NULL) 
          {  sprintf(st,"Can't allocate tbuf, need %d bytes",siz_tbuf);
             errexit(st);
          } 
          tbufflag=1;
     
        /* loop thru all items in translation table  */
    
          for(i=0; i<Ntrans; i++)
             translate(i,ubuf,tbuf);          /* Lscan_u2f adjusts maxTwid */
           
         /*  fprintf(logfile,"\nmaxTwid=%d ",maxTwid); */
          
          if(ftime==1)  do_file_head();  /* fill and write file header */
          ftime=0;
          
          if(nthead==1)                   /* make table header */
          {   fill_Thead(); 
              nthead=0;
              
              /* if maxrecs>0, write header to std out */
              if(maxrecs>0) write_Thead();
           } 
          write_rec(tbuf);                /* write to temp file or std out */
 
          if(tbufflag==1)  {  free(tbuf);  tbufflag=0; }
       }
    }  /***** done reading input UNIPOPS file ****************************/
    
    fprintf(logfile,"\nRead %ld records from std in; %ld selected. ",
                   recnum,rrec);
    fflush(logfile);
    
    if(rrec<1) errexit("WARNING:  NO DATA SELECTED FROM INPUT FILE!!");
    
    if (prev==0) finish_output();             /* finish writing std out  */  
    
    fprintf(logfile,"\nWrote %ld table rows, %d fits records.",
       OutCount, FitCount);
    fflush(logfile);
    fprintf(stderr,"Wrote %ld scans out of %ld; %d fits records written. \n",
          OutCount, recnum, FitCount);
    /* if preview mode, put out final statistics */
    if (prev==1)
	fprintf(stdout,"maxLength(-t) %d maxRecords(-n) %d\n",maxmaxis1, rrec);
}   /*******end of getdata***********************************************/


/************************************************************************
 * CREATE_TEMP - makes a temporary file for the binary table, if not in *
 *   real-time mode.                                                    *
 ************************************************************************/
create_temp()
{
   char st[80];
   
   if(maxrecs==0)    /* create file only if maxrecs==0 */
   {   
       /* create temp file name */
       sprintf(tmpname,".%d%d%d%d.ftmp",ltime->tm_mday,ltime->tm_hour,
         ltime->tm_min,ltime->tm_sec);
       
       fprintf(logfile,"\nCreate_Temp: file= %s ",tmpname);
       
       /* open file name */
       if((tmpfid=open(tmpname,(O_RDWR|O_CREAT|O_TRUNC))) <2)
       {  sprintf(st,"Cannot create temp file: %s ",tmpname);
          errexit(st);
       }
       tmpfileflag = 1;
   }
}


/**********************************************************************
 * DSELECT - return 1 if scan selected, 0 if not.                     *
 *   Applies selection criteria specified on command line.            *
 *   checks if scan number is within range.                           *
 *  Line and continuum are allowed to be mixed if "-LC" option.       *
 *  Do NOT select LINEOTF scans                                       *
 **********************************************************************/
dselect(k,recnum)
UPOPS *k;
long recnum;
{
   char mode[12];
   
   strncpy(mode, k->u1->obsmode, (long)8);
   mode[8]=0;

   fprintf(logfile,"\nDSelect: rec=%ld, obsmode= %s, scan#= %11.2lf, qcont=%d ",
           recnum, mode, k->u1->scan, qcont);
   fflush(logfile);

   /* reject LINEOTF scans from NRAO 12M */
   if ((strncmp(k->u1->obsmode,"LINEOTF ",(long)8)==0) &&
       (strncmp(k->u1->telescop,"NRAO 12M",(long)8)==0)) {
      fprintf(logfile,"\nDSelect: unable to convert LINEOTF data at this time");
      return(0);
   }

   /*  reject non-line record if qcont==0  */
   if((qcont==0)&&(strncmp(k->u1->obsmode,"LINE",(long)4)!=0) )
      return(0);
      
   /*  reject non-continuum record if qcont==1  */
   if((qcont==1)&&(strncmp(k->u1->obsmode,"CONT",(long)4)!=0) )
       return(0);
          
   if((scan1!=0)||(scan2!=0))    /* check scan num range ? */
      if((k->u1->scan<scan1)
       ||(k->u1->scan>= (scan2+1) ))  return(0);
       
   return(1);
 }   /**********end of dselect*************************************/
 


/**********************************************************************
 * GET_HDR_DAT - get header items and other special parameters        *
 *    from the input structure.                                       *
 **********************************************************************/
get_hdr_dat(k)
UPOPS *k;
{
     long dd,mm,yy, mmdd;
     
     strncpy(k->ux->sourcename, k->u1->object, (long)SNAMLEN);
     k->ux->sourcename[SNAMLEN]=0;
     strncpy(k->ux->obsname,    k->u1->observer, (long)ONAMLEN);
     k->ux->obsname[ONAMLEN]=0;
     strncpy(k->ux->projcode,   k->u1->projid, (long)PRCODE);
     k->ux->projcode[PRCODE]=0;
     strncpy(k->ux->tele,       k->u1->telescop, (long)8);
     k->ux->tele[8] = 0;
     
   /* determine if kitt peak data or not */
     if((strncmp(k->ux->tele,"NRAO12M",(long)8)==0)
      ||(strncmp(k->ux->tele,"NRAO 12M",(long)8)==0))  kpflag=1;
     else  kpflag=0;

  /* determine if prototype class 11 or not */
     if (strncmp(k->u3->cl11type,"PROTO12M",(long)8)==0) p11flag=1;
     else p11flag=0;
          
  /*  change utdate to FITS-type DATE-OBS */
     yy = k->u3->utdate;
     mmdd = (k->u3->utdate - yy)*10000. + .48;
     mm = mmdd/100;
     dd = mmdd % 100;
     if (y2k) {
       /* use the new Y2K form */
       sprintf(k->ux->dateobs, "%04ld-%02ld-%02ld", yy,mm,dd);
     } else {
       /* use the original DATE form */
       yy -= 1900;
       sprintf(k->ux->dateobs, "%02ld/%02ld/%02ld",dd,mm,yy);
     }
     
     fflush(logfile);
     
}  /******end of get_hdr_dat*******************************************/



/***********************************************************************
 * DO_FILE_HEAD - make main file FITS header; write to std out.        *
 *   This gives standard header info, including                        *
 *      Origin, Date file written.                                     *
 ***********************************************************************/
 do_file_head()
 {
     int i;
     
     initfits(' ');
     
     puthead(mh,0, "ORIGIN", Origin,  MHnlines);
     puthead(mh,0, "DATE",   rundate, MHnlines);
     
     for(i=0; i<MHnlines; i++)
     {
        stuffits(80,(mh+i)->htext, ' ');
     }
    
     stuffits(3,"END", ' ');     /* add END card to main header    */
     
    /* now let's send it to standard output                        */
    flushfits(' ');
    
 } /****end of do_file_head******************************************/
 
 
 /*********************************************************************
  * STUFFITS - put n characters from st into fitsbuf                  *
  *********************************************************************/
 stuffits(n,st, ch)
 char *st, ch;
 int n;
 {     
     int i;
     
     for(i=0; i<n; i++, jfbuf++)
     {           
       /* check whether buffer is full */
         if(jfbuf>=FITS_REC_LEN)  flushfits(ch);
         
         fitsbuf[jfbuf] = st[i];
     }
 } /***end of stuff_fits*********************************************/
 
 
 /********************************************************************
  * FLUSHFITS - empty FITS output buffer and fill buffer with char c *
  ********************************************************************/
  flushfits(c)
  char c;
  {
      int j;
      
      if( write(1,fitsbuf, (long)FITS_REC_LEN) == -1)
         errexit("Error writing FITS record to stdout.");
         
       FitCount++;
   /*  fprintf(logfile,"\nWrote FITS rec# %d to stdout", FitCount); */
       initfits(c);
   }
   
   
 /********************************************************************
  * INITFITS - set FITS buffer to character c, set jfbuf=0           *
  ********************************************************************/
  initfits(c)
  char c;
  {
      int j;
      
      for(j=0; j<FITS_REC_LEN; j++)  fitsbuf[j] = c;
      jfbuf=0;
  }
      
   
/**********************************************************************
 * PUTHEAD - put string st into header line with kw="key"             *
 **********************************************************************/
 puthead(hd, mm, key, st, max)
 HEAD_TABLE *hd;
 char *key, *st;
 int max,mm;
 {
     int i, jj, len, j, nn;
     
     len = strlen(st);
     
     for(i=0; i<max; i++)
     {   if(strcmp((hd+i)->kw,key)==0)
         {   jj = (hd+i)->icol[mm];
             nn = (hd+i)->iwid[mm];
             for(j=0; j<nn; j++, jj++)
                 /* if st is empty, blank out the space anyway */
             {   if(j>=len)  (hd+i)->htext[jj] = ' ';
                 else        (hd+i)->htext[jj] = st[j];
             }
          }
      }
 }
 
    
 /********************************************************************
  * FILL_THEAD - build Table header in hbuf.                         *
  ********************************************************************/
 fill_Thead()
 {
     char st[80], snam[12];
     int maxis;
 
     sprintf(st,"%5d",Twidth);
     puthead(th,0, "NAXIS1",   st,           THnlines);
     sprintf(st,"%5d",maxrecs);
     puthead(th,0, "NAXIS2",   st,           THnlines);
     sprintf(st,"%5d", Tfields);
     puthead(th,0, "TFIELDS",  st,           THnlines);
     puthead(th,0, "TELESCOP", up.ux->tele,       THnlines);
     puthead(th,0, "OBJECT",   up.ux->sourcename, THnlines);
     puthead(th,0, "OBSERVER", up.ux->obsname,    THnlines);
     puthead(th,0, "DATE-OBS", up.ux->dateobs,    THnlines);
     sprintf(st,"%5.0f.",up.ux->time);
     puthead(th,0, "TIME",     st,           THnlines);
     
  /* set MAXIS = 5   (5th axis ("reciever number") is used) */
    maxis=5;
    sprintf(st, "%4d", maxis);
    puthead(th,0, "MAXIS", st, THnlines);

     sprintf(st,"%12.4E", up.ux->datamax);
     puthead(th,0, "DATAMAX",  st,           THnlines);
     puthead(th,1, "DATAMAX", "first scan only.", THnlines);
     
     sprintf(st,"%12.4E", up.ux->datamin);
     puthead(th,0, "DATAMIN",  st,           THnlines);
     puthead(th,1, "DATAMIN", "first scan only.", THnlines);
     
     if(maxWscan>0)  /* put ScanMult as TFORM for the last kw */
     {               /* only if -t option used by user.        */
         sprintf(snam,"TFORM%d", Tfields);
         sprintf(st,"'%dE      '  ", ScanMult);
         puthead(th,0, snam,  st,         THnlines); 

         /* set TDIM for the last kw to ScanMult */
         sprintf(snam,"TDIM%d", Tfields);
         sprintf(st, "'(%d,1,1,1,1)'"  , ScanMult);
         puthead(th,0, snam, st, THnlines);
     }
 } /***end of fill_Thead******************************************/
 
 
 /********************************************************************
  * UPDATE_THEAD - add ending data to table header in hbuf.          *
  ********************************************************************/
 update_Thead()
 {
     char st[80], snam[12];
     
     sprintf(st,"%5d",maxTwid);
     puthead(th,0, "NAXIS1",   st,           THnlines);
     sprintf(st,"%5ld",OutCount);
     puthead(th,0, "NAXIS2",   st,           THnlines);
     
     sprintf(st,"%12.4E", DATAmax);
     puthead(th,0, "DATAMAX",  st,           THnlines);
     puthead(th,1, "DATAMAX", "for whole file.  ", THnlines);
     
     sprintf(st,"%12.4E", DATAmin);
     puthead(th,0, "DATAMIN",  st,           THnlines);
     puthead(th,1, "DATAMIN", "for whole file.  ", THnlines);
     
     /* fill in the series data width = the TFORM for the last kw.
        value comes from maxmaxis1                            */
     sprintf(snam,"TFORM%d", Tfields);
     sprintf(st,"'%dE          '  ", maxmaxis1);
     puthead(th,0, snam,  st,         THnlines);

     /* set TDIM for the last kw to maxmaxis1 */
        sprintf(snam,"TDIM%d", Tfields);
        sprintf(st, "'(%d,1,1,1,1)'"  , maxmaxis1);
         puthead(th,0, snam, st, THnlines);
        fprintf(logfile,"\nUPDATE: debug: putted head, %s, %s ", snam,st);
  }   

/************************************************************************
 * WRITE_THEAD - send Table Header to standard output.                  *
 ************************************************************************/
 write_Thead()
 {
      int i;
     
     initfits(' ');
     
     for(i=0; i<THnlines; i++)
        stuffits(80,(th+i)->htext, ' ');
    
     stuffits(3,"END", 0);     /* add END card to table header      */
     
    /* now let's send it to standard output                           */
    if(jfbuf>0)
       flushfits('\0');  /* (initialize next buffer for binary data.) */
 }
 
 
/************************************************************************
 * WRITE_REC - write binary table lines to either std out or temp file. *
 ************************************************************************/
 write_rec(tbuf)
 char *tbuf;
 {
     int i,j,jj;
     
     if(maxrecs==0)
     {                /* write to temp file */
         if(write(tmpfid,tbuf,(long)siz_tbuf)<0)
            errexit("write_rec: error writing temp file");
         OutCount += (siz_tbuf+1)/Twidth;
            
     }
     else
     {               /* write to std out */
         for(i=0; i<siz_tbuf; i+=Twidth)
         {   if(OutCount>=maxrecs)
             {   flushfits(' ');    /* flush last fits buffer  */
                 fprintf(logfile,"\nFinish Table, #outrecs=%d, #FITS recs=%d",
                   OutCount, FitCount);
                 fflush(logfile);
                 fill_Thead();      /* update header info      */
                 write_Thead();     /* write header to std out */
                 OutCount=0;
             }
             stuffits(Twidth,(tbuf+i), 0);
             OutCount++;
          }
      }

 } /*****end of write_rec***************************************************/
 
 
/***************************************************************************
 * FINISH_OUTPUT - finish writing to std out.                              *
 *   If in psuedo-real-time mode (maxrecs>0), just dump last fits buffer.  *
 *   If not in pseudo-real-time mode (maxrecs=0),                          *
 *           1. update table header and write it out                       *
 *           2. rewind temp file and write to std out                      *
 ***************************************************************************/
 finish_output()
 {
     int i, ii;
     char *tmp;
     
     if(maxrecs==0)
     {                     /* omniscient mode       */
         update_Thead();
         write_Thead();
       
#ifdef THINK_C         
         lseek(tmpfid, (long)0, SEEK_SET);  /* reset temp file to beginning */
#else
         lseek(tmpfid, (long)0, L_SET); 
#endif
         
         tmp = (char *)malloc((long)Twidth);
         if(tmp==0) errexit("finish_output: cannot allocate space for tmp");
         
    fprintf(logfile,"\nFinishOutput: allocated for tmp %ld bytes.", Twidth);
    fflush(logfile);
         
         for(i=0; i<OutCount; i++)
         {   if(read(tmpfid,tmp, Twidth)!=Twidth)
                errexit("finish_output: error reading tempfile.");
             
             stuffits(maxTwid, tmp, 0);
         }
         free(tmp);
     }
                            /* in either mode, flush buffer at end */
         flushfits(0);
     
 } /****end of finish_output**************************************************/
 
 

 
/********************************************************************
 * TRANSLATE - use translation table to convert input buffer (ubuf) *
 *     to output buffer (tbuf).                                     *
 *    i = item number in translation table.                         *
 *  Ascii fields (A) are copied directly - no byte swap.            *
 *  I,E, or D fields may be converted to any of I,E, or D with      *
 *   function calls to convert between IEEE and local form.         *
 ********************************************************************/
#define ASIZE 24
 
 translate(i,ubuf,tbuf)
 int i;
 char *tbuf, *ubuf;
 {
    int iwid, nwid, fwid, ffwid, j, jj, inmult;
    char st[80];
    char   cbuf[SCANLEN], intype;
    double ieee_to_dub();
    int vf[ASIZE];              /* validity flag (=1 if invalid data) */
    double dub[ASIZE];          /* for temporary double arrays */
    float  ieee_to_real(), flt[ASIZE];
    short  ieee_to_int(),  sht[ASIZE];

   /* get size of table item */

    if( (iwid=form_size((tt+i)->inform))==0)
    {   sprintf(st,"Tranl: bad inp format %c, tab.line %d",
                      (tt+i)->inform,i);
        errexit(st);
     }
     nwid = (tt+i)->minf * iwid;   /* UNIPOPS size, in bytes */
     inmult = (tt+i)->minf;
     intype = (tt+i)->inform;

  /* debug printout 
    fprintf(logfile, "\nTRANS: %d, %s, %d %d ", i, (tt+i)->ttype,
               (tt+i)->ucl, (tt+i)->uoff);
    */
   /* skip to trans function, if series */
   if (strcmp((tt+i)->ttype,"SERIES")!=0)
   {
    /* get item into cbuf and scale, if necessary  */
    if((tt+i)->uoff>=0)
    /* make sure it is actually found in ubuff */
    {if ((tt+i)->uoff < upsize[(tt+i)->ucl]) 
     { jj = (tt+i)->uoff;
       jj += upnt[(tt+i)->ucl];   /* offset to class location in ubuf */
       for(j=0; j<nwid; j++, jj++)  cbuf[j] = ubuf[jj];

       switch((tt+i)->inform)
       {   case 'A' :  for(j=jj=0; j<nwid; j++)  if(cbuf[j]==' ') jj++;
                       /* If UNIPOPS string is all blank, set first byte
                          null to indicate invalid string in FITS */
                       if(jj==nwid)  { vf[0]=1;  cbuf[0]=0; }
                       else  vf[0]=0;
                      cbuf[nwid]=0;
                      /* fprintf(logfile, " %s", cbuf); */
                      break;
           case 'I' :  for(j=0; j<(tt+i)->minf; j++)
                       {  sht[j] = ieee_to_int(cbuf+2*j, (vf+j));
                       }
                      /* fprintf(logfile, " %d, (%d)", sht[0], vf[0]); */
                       for(j=0; j<(tt+i)->minf; j++)
                       {  dub[j] = sht[j] * (tt+i)->factor + (tt+i)->offset;
                          flt[j] = dub[j];
                          sht[j] = dub[j];
                       }
                      /* fprintf(logfile, " %8.4f", dub[0]); */
                       break;
           case 'E' :  for(j=0; j<(tt+i)->minf; j++)
                          flt[j] = ieee_to_real(cbuf+4*j, (vf+j));
                      /* fprintf(logfile, " %8.4f, (%d),", flt[0], vf[0]); */
                       for(j=0; j<(tt+i)->minf; j++)
                       {  dub[j] = flt[j] * (tt+i)->factor + (tt+i)->offset;
                          flt[j] = dub[j];
                          sht[j] = dub[j];
                        }
                      /* fprintf(logfile, " %8.4f", dub[0]); */
                       break;
           case 'D' :  for(j=0; j<(tt+i)->minf; j++)
                           dub[j] = ieee_to_dub(cbuf+8*j, (vf+j));
                       /* fprintf(logfile, " %8.4f, (%d),", dub[0], vf[0]); */
                         for(j=0; j<(tt+i)->minf; j++)
                         {  dub[j] = dub[j] * (tt+i)->factor + (tt+i)->offset;
                            flt[j] = dub[j];
                            sht[j] = dub[j];
                         }
                       /* fprintf(logfile, " %8.4f", dub[0]); */
       }
       /* if class=9, check the kpflag and the category of data       */
       /*  if this is not a KP scan, set all KP data items to invalid */
       /*  Do complementary thing for GB scan.                        */
        if((tt+i)->ucl==9)
        {   if( ((kpflag==1)&&((tt+i)->kat[0]=='g'))
              ||((kpflag==0)&&((tt+i)->kat[0]=='k')) )
            {  for(j=0; j<(tt+i)->minf; j++)  vf[j]=1;
            }
        }
        /* if class=11, check the p11flag and the category of data    */
        /* if this is not a protype class 11 scan, set all prototype  */
        /* class 11 items (t) to invalid.  Do complementary thing for */
        /* the original class 11 items                                */
        if ((tt+i)->ucl==11)
        {    if( ((p11flag==1)&&((tt+i)->kat[0]!='t'))
               ||((p11flag==0)&&((tt+i)->kat[0]=='t')) )
             {  for(j=0; j<(tt+i)->minf; j++)  vf[j]=1;
             }
        }
     }			/* end of if (tt+i)->uoff < upsize ... */
     else
     {
        for(j=0; j<(tt+i)->minf; j++) vf[j]=1;
     }
    }
   }  /* end of if (..."SERIES") */
     
     /* convert each parameter using its conversion function  */
     
     if(( (tt+i)->sflag>=0) && ((tt+i)->fun!=0) )
     {
        (*((tt+i)->fun)) (i, vf, cbuf, sht, flt, dub, intype, inmult, tbuf);
     }
}  /***********end of translate************************************/
