/* @(#)access.c	5.5 05/03/95 */

#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <malloc.h>

#include <tucaccess.h>
#include <sdd.h>

#define RETURN(N) \
   {\
      *return_code = N * 10000 + errno;\
      return;\
   }\


void openaccess_(anal_user_code, anal_user_name, anal_site_name, return_code)
char *anal_user_code, *anal_user_name, *anal_site_name;
int *return_code;
{
   short int ier;
   int n, len, iftype, ivers;
   void getinit(), make_user_code(), chngonline_(), progname_(), olfiles_();
   char prog[2];

   *return_code = 0;
 
/*		some initial values		 */
 
   for (n=0;n<3;n++) {
      strcpy(onfile[n].type, ftype[n].c);
      onfile[n].data_status = ST_UNAVAIL;
      onfile[n].gain_status = ST_UNAVAIL;
      onfile[n].datafile.fd = -1;
      onfile[n].datafile.index = 0;
      onfile[n].datafile.unit = -1;
      onfile[n].gainfile.fd = -1;
      onfile[n].gainfile.index = 0;
      onfile[n].gainfile.unit = -1;
      onfile[n].version = -1;
   }
 

/* 			get env values */
 
   getinit();

/*			initialize the files, etc */
 
   if (strlen(obsinit) != 0) {
      if (access(datadir, 4) != 0) {
         strcpy(datadir, ".");
      }
/*	if datadir can't be accessed, just substitude current directory */

      for (n=0;n<3;n++) {
         iftype = n+1;
         ivers = 0;
         chngonline_(&iftype, &ivers, anal_user_code, &ier);
      }

/*		set default initial file type */
/*		FB, unlist condar is running, then CON */

      filetype = FBTYPE;
      progname_(prog);
      if (*prog == 'c' || *prog == 'C') filetype = CONTYPE;

/*		finally, have olfiles display what we've just set up */

      olfiles_();
  
      make_user_code(anal_user_code);
   } else {
/*		if obsinit isn't set, there is NO way to access any data */
      RETURN(-1)
   }
}

void setformat_(fmtcode, return_code)
   int *fmtcode, *return_code;
{
/*			this only can return SDD format data */
/*			return an error (1) if any other format is requested */
   *return_code = 0;
   if (*fmtcode != SDDFMT) *return_code = 1;
}

void closeaccess_()
{
/*			close the files		*/
   int n;

   for (n=0;n<3;n++) {
      if (onfile[n].datafile.fd != -1) {
         close(onfile[n].datafile.fd);
         free(onfile[n].datafile.index);
         onfile[n].datafile.index = 0;
         onfile[n].datafile.fd = -1;
         onfile[n].data_status = ST_UNAVAIL;
      }
      if (onfile[n].gainfile.fd != -1) {
         close(onfile[n].gainfile.fd);
         free(onfile[n].gainfile.index);
         onfile[n].gainfile.index = 0;
         onfile[n].gainfile.fd = -1;
         onfile[n].gain_status = ST_UNAVAIL;
      }
   }
}

/*	allow user to change initials if current OBSINT env variable */
/*      is "sys" */

void chgprj_(anal_user_code, anal_user_name, anal_site_name, return_code)
char *anal_user_code, *anal_user_name, *anal_site_name;
int *return_code;
{
   char *topdata, *envinit;
   short int ier;
   int n, iftype, iver, oldftype;
   void make_user_code(), chngonline_();
   
   *return_code = 0;

   envinit = getenv("OBSINIT");
   if (envinit != NULL) {
      if (strcmp(envinit,"sys") == 0) {
/*			allow change of initials */
         if (strcmp(anal_user_code,"tucson") == 0) {
            strcpy(obsinit,"sys");
            strcpy(anal_user_code, "sys");
            iver = 0;
            for (n=0;n<3;n++) {
               iftype = n+1;
               chngonline_(&iftype, &iver, anal_user_code, &ier);
            }
         } else {
            strncpy(obsinit,anal_user_code,3);
            if (strchr(obsinit,' ') != NULL) *strchr(obsinit,' ') = '\0';
            topdata = getenv("TOPDATA");
            datadir[0] = '\0';
            if (topdata != NULL) {
               strcpy(datadir, topdata);
               strcat(datadir,"/");
               strcat(datadir,obsinit);
            }
            if (strlen(datadir) == 0) strcpy(datadir, ".");
            oldftype = filetype;
            for (n=0;n<3;n++) {
               iftype = n+1;
/*				set the version to -1 to force a reopen */
               onfile[n].version = -1;
               iver = 0;
               chngonline_(&iftype, &iver, anal_user_code, &ier);
            }
/*				restore original file type */
            filetype = oldftype;
         }
      }
   }
   make_user_code(anal_user_code);
}

/* get the list of LINE scans in the default file that belong to this project */

void gscanlist2_(actno, maxno, list, return_code)
int *actno;		/* actual number of scans sent back */
int *maxno;		/* the max number to send back */
float *list;		/* the list of scan numbers */
int *return_code;	/* not used */
{

   sddfile *file;
   void getlist();

   file = &(onfile[filetype-1].datafile);
   getlist(file, "LINE", actno, maxno, list);
   *return_code = 0;
}

/* get the list of gain scans in the default file that belong to this project */

void ggscanlist2_(actno, maxno, list, return_code)
int *actno;		/* actual number of scans sent back */
int *maxno;		/* the max number to send back */
float *list;		/* the list of scan numbers */
int *return_code;	/* not used */
{

   sddfile *file;
   void getlist();

   file = &(onfile[filetype-1].gainfile);
   getlist(file, "LINE", actno, maxno, list);
   *return_code = 0;
}

/* get the list of CONT scans in the default file that belong to this project */

void gcontlist2_(actno, maxno, list, return_code)
int *actno;		/* actual number of scans sent back */
int *maxno;		/* the max number to send back */
float *list;		/* the list of scan numbers */
int *return_code;	/* not used */
{

   sddfile *file;
   void getlist();

   file = &(onfile[filetype-1].datafile);
   getlist(file, "CONT", actno, maxno, list);
   *return_code = 0;
}

/*	this is what actually does the work for making lists of scans */

void getlist(file, mode, actno, maxno, list)
sddfile *file;
char *mode;
int *actno;
int *maxno;
float *list;
{

   int n, scan, numout, modecode, modetst;
   int getincbs();

   *actno = 0;

   if (getincbs(file) != 0) return;

/*		translate the mode into its appropriate code */
   modecode = 0;
   if (strncmp(mode, "CONT", 4) == 0) modecode = 1;
   if (strncmp(mode, "LINE", 4) == 0) modecode = 2;

   numout = 0;
   for (n=0;n<file->bs.num_entries_used;n++) {
      scan = file->index[n].scan_number;
      modetst = file->index[n].obsmode / 256;
      if ((scan != 0) &&  ((modecode == 0) || (modetst == modecode))) {
         *(list+numout) = (float) scan + 
                                (float) file->index[n].feed_number/100.0;
         numout++;
      }
      if (numout >= *maxno) break;
   }
   *actno = numout;
}

/* This function standardizes the rule on where to find a particular subscan number */

int filenum(subscan)
    int subscan;                 /* the subscan number */
{
    int fnum;
    if (subscan >= 11) {
	fnum = HCTYPE - 1;
    } else if (subscan < 11 && subscan != 0) {
	fnum = FBTYPE - 1;
    } else {
	/* use the default */
	fnum = filetype - 1;
    }
    return fnum;
}

/* get a particular LINE scan */

void gscan_(scan, sub, record, return_code)
int *scan;			/* scan number */
int *sub;			/* subscan number */
char *record;			/* buffer to fill */
int *return_code;
{

   sddfile *file;
   void getit();

   file = &(onfile[filenum(*sub)].datafile);

   getit(file, "LINE", scan, sub, record, return_code);
}

/* get the header for a particular LINE scan */

void gshead_(scan, sub, record, return_code)
int *scan;			/* scan number */
int *sub;			/* subscan number */
char *record;			/* buffer to fill */
int *return_code;
{
	gscan_(scan, sub, record, return_code);
}

/* get a particular gain scan */

void ggscan_(scan, sub, record, return_code)
int *scan;			/* scan number */
int *sub;			/* subscan number */
char *record;			/* buffer to fill */
int *return_code;
{

   sddfile *file;
   void getit();

   file = &(onfile[filenum(*sub)].gainfile);

   getit(file, "LINE", scan, sub, record, return_code);
}

/*
** Get the Spectral Processsor scan
*/
void gspscan_(scan_no,feed_no,record,return_code)
	int *scan_no;
	int *feed_no;
	float *record;
	int *return_code;
{
}

/*
** Get the Spectral Processsor individual record
*/
void gspir_(scan_no,feed_no,phase_no, rec_no,record,return_code)
	int *scan_no, phase_no, rec_no;
	int *feed_no;
	float *record;
	int *return_code;
{
}
/*
** Get the AC individual record
*/
void gacir_(scan_no,feed_no,phase_no, rec_no,record,return_code)
	int *scan_no, phase_no, rec_no;
	int *feed_no;
	float *record;
	int *return_code;
{
}

#define OTF 0
#define PZ 1

void getweird(type, scan_no, feed_no, rec_no, num_rec, record, return_code)
int *type;
int *scan_no, *feed_no, *rec_no, *num_rec;
char *record;
int *return_code;
{
    char *rptr, field[4];
    short id, irtn, *class_0;
    int curpos, toread, startat, i, bytperrec;
    int nrecs, hbytes, dbytes, n, indtype;
    float *fbuf, *fptr;
    double *headlen, *datalen, *noint, *dbuf;
    sddfile *file;
    void modefield_();
    int nextra = 5;

    /* first, guess which file is appropriate */
    /* use the default feed number if not already set */
    if (*feed_no == 0) {
	*feed_no = 1;
	if (filetype == HCTYPE -1) *feed_no = 11;
    }
    file = &(onfile[filenum(*feed_no)].datafile);
    if (file->fd <0 || getincbs(file) != 0) {
	*return_code = -370;
	return;
    }

    /* find a matching scan number */
    for (i=0;i<file->bs.num_entries_used;i++) {
      if ((file->index[i].scan_number == *scan_no) &&
          (file->index[i].feed_number == *feed_no)) break;
    }
    if (i >= file->bs.num_entries_used) {
	/* not found, see if it is in the other file */
	file = &(onfile[FBTYPE-1].datafile);
	if (file->fd >0 && getincbs(file) == 0) {
	    for (i=0;i<file->bs.num_entries_used;i++) {
		if ((file->index[i].scan_number == *scan_no) &&
		    (file->index[i].feed_number == *feed_no)) break;
	    }
	}
	if (i >= file->bs.num_entries_used) {
	    /* still not found, give up */
	   *return_code = 362;
	   return;
	}
    }
    /* finally, determine the type as shown in the index */
    id = file->index[i].obsmode - 512;
    if (id >= 0) {
	modefield_(&id, field, &irtn);
	if (strncmp(&field[2],"PZ",2) == 0) {
	    nextra = 0;
	} else if (strncmp(field,"OTF ",4) != 0) {
	    /* bad type */
	    *return_code = 362;
	    return;
	}
    } else {
	/* no id, we must assume its the wrong type */
	*return_code = 362;
	return;
    }
/* 		position it */
   bytperrec = file->bs.bytperrec;
   startat = file->index[i].start_rec * bytperrec;
   if (lseek(file->fd, startat, SEEK_SET) == -1) {
      *return_code = -357;
      return;
   }

/*		read the first record */
   rptr = record;
   if (read(file->fd, rptr, bytperrec) != bytperrec) {
      *return_code = -357;
      return;
   }
   rptr += bytperrec;
/*		some sanity checks */
   class_0 = (short *)record;
   if (class_0[0] < 12 || class_0[1] > bytperrec || class_0[1] < 5) {
      *return_code = -357;
      return;
   }
/*		read the rest of the header */
   dbuf = (double *)record;
   headlen = (dbuf + class_0[1] - 1);
   hbytes = *headlen;
   toread = hbytes - bytperrec;
   if (toread > 0) {
      if (read(file->fd, rptr, toread) != toread) {
         *return_code = -357;
         return;
      }
   }
   rptr += toread;
/*		set up the other header pointers, with sanity checks */
   if ((class_0[12] + 15) > hbytes || (class_0[12] <= 0)) {
      *return_code = -357;
      return;
   }

   datalen = (dbuf + class_0[1]);
   dbytes = *datalen;
   noint = (dbuf + class_0[12] + 13);
   if (*noint <= 0) {
      *return_code= -357;
      return;
   }

    nrecs = (dbytes / (*noint + nextra)) / sizeof(float);
    if (dbytes != nrecs * sizeof(float) * (*noint + nextra)) {
	*return_code = -357;
	return;
    }
   if (*rec_no == 0 || *rec_no > nrecs || *rec_no < -nextra) {
      *return_code = 362;
      return;
   }
   
/*		seek to and read the requested data */
/*		rec_no < 0 imply one of the last 5 arrays */
/*  The above check against nextra has verified that this should be safe in all cases */
   if (*rec_no < 0) {
      startat = *noint * sizeof(float) * nrecs;
      startat = startat + ((-1)*(*rec_no)-1)*nrecs*sizeof(float);
      toread = nrecs * sizeof(float);
   } else {
      startat = (*noint * sizeof(float) * (*rec_no - 1));
      toread = *noint * sizeof(float);
   }

   if (lseek(file->fd, startat, SEEK_CUR) == -1) {
      *return_code = -357;
      return;
   }
   curpos = startat;
   if (toread > ((MAX_DATA_POINTS - nextra)*sizeof(float))) 
            toread = (MAX_DATA_POINTS - nextra)*sizeof(float);
   if (read(file->fd, rptr, toread) != toread) {
      *return_code = -357;
      return;
   }
   rptr += toread;
   curpos = curpos + toread;
   *datalen = *noint * sizeof(float);

/*		seek to and read the 5 informational values for this rec */
/*		but only if *rec_no > 0 && nextra == 5 && this is an OTF request */
   if (*rec_no > 0) {
       if (nextra == 5 && type == OTF) {
	   startat = *noint * nrecs * sizeof(float);
	   toread = nrecs * sizeof(float);
	   if ((fbuf = (float *)malloc(toread)) == NULL) {
	       *return_code = -357;
	       return;
	   }
	   if (lseek(file->fd, (startat - curpos), SEEK_CUR) == -1) {
	       *return_code = -357;
	       free(fbuf);
	       return;
	   }
	   fptr = (float *)rptr;
	   for (n=0;n<5;n++) {
	       if (read(file->fd, fbuf, toread) != toread) {
		   *return_code = -357;
		   free(fbuf);
		   return;
	       }
	       *fptr = fbuf[*rec_no - 1];
	       fptr++;
	   }
/*		fbuf can be freed now */
	   free(fbuf);
/*		And finally, reset DATALEN and NOINT */
	   *noint = *noint + 5;
	   *datalen = *noint * sizeof(float);
       } else if (type == OTF) {
	   /* just assign 0's */
	   fptr = (float *)rptr;
	   for (i=n;n<5;n++,fptr++) *fptr = 0.0;
	   *noint = *noint + 5;
	   *datalen = *noint * sizeof(float);
       }
   } else {
      *noint = nrecs;
      *datalen = nrecs * sizeof(float);
   }
   *num_rec = nrecs;
   *return_code = 0;
}

void gotfrec_(scan_no, feed_no, rec_no, num_rec, record, return_code)
int *scan_no, *feed_no, *rec_no, *num_rec;
char *record;
int *return_code;
{

/*            everything now happens in getweird */

   getweird(OTF, scan_no, feed_no, rec_no, num_rec, record, return_code);
   return;
}


/*
** Get a polariz. record
*/
void gpzrec_(scan_no, feed_no, rec_no, num_rec, record, return_code)
int *scan_no, *feed_no, *rec_no, *num_rec;
char *record;
int *return_code;
{
   getweird(PZ, scan_no, feed_no, rec_no, num_rec, record, return_code);
   return;
}
/* get a particular CONT scan */

void gcont_(scan, sub, actualpoints, maxpoints, record, return_code)
int *scan;			/* scan number */
int *sub;			/* subscan number */
int *actualpoints;		/* not set */
int *maxpoints;			/* not checked */
char *record;			/* buffer to fill */
int *return_code;
{

   int fnum;
   sddfile *file;
   void getit();

/*		 	which file */

   fnum = CONTYPE - 1;

   file = &(onfile[fnum].datafile);

   getit(file, "CONT", scan, sub, record, return_code);
}

void gchead_(scan, sub, record, return_code)
int *scan;			/* scan number */
int *sub;			/* subscan number */
char *record;			/* buffer to fill */
int *return_code;
{
	int *actualpoints = 0;		/* not set */
	int *maxpoints = 0;		/* not checked */
	gcont_(scan, sub, actualpoints, maxpoints, record, return_code);
}

/*		this is what does the actual work when fetching scans */

void getit(file, mode, scan, sub, record, return_code)
sddfile *file;			/* the file to fetch it from */
char *mode;			/* To check agains the mode in the index */
int *scan;			/* the scan number to fetch */
int *sub;			/* the subscan number to fetch */
char *record;			/* where to return it */
int *return_code;		/* the error code */
{

   int i, j, offset, toread, getincbs(), tscan, modecode, modetst;
   int start_rec, bytperrec;

/*		find the scan in the the index, must match type and subscan */

   if (file->fd < 0 || getincbs(file) != 0) RETURN(5)

   if (*sub == 0) {
      *sub = 1;
      if (filetype == HCTYPE - 1) *sub = 11;
   }

/*		translate the mode into its appropriate code */
   modecode = 0;
   if (strncmp(mode, "CONT", 4) == 0) modecode = 1;
   if (strncmp(mode, "LINE", 4) == 0) modecode = 2;

/*		see if anything matches in the in index */
   for (i=0;i<file->bs.num_entries_used;i++) {
      modetst = file->index[i].obsmode / 256;
      if ((file->index[i].scan_number == *scan) &&
          (file->index[i].feed_number == *sub) &&
          ((modecode == 0) || (modetst == modecode))) break;
   }

   if (i >= file->bs.num_entries_used) RETURN(5)
            
/*		position it */
   start_rec = file->index[i].start_rec;
   bytperrec = file->bs.bytperrec;
   if (lseek(file->fd, (start_rec * bytperrec), SEEK_SET) == -1) RETURN(6)

/*		read it */
   toread = file->index[i].nb_rec * bytperrec;
   if (toread > sizeof(sddformat)) toread = sizeof(sddformat);

   if (read(file->fd, record, toread) != toread) RETURN(7)

   *return_code = 0;
}

/* Get the last completed LINE scan */

void gcscan_(record, sub, return_code)
char *record;			/* where to put it */
int *sub;			/* subscan number to find */
int *return_code;
{
   sddfile *file;
   int fnum;

   if (*sub >= 11) {
      fnum = HCTYPE - 1;
   } else if (*sub < 11 || *sub != 0) {
      fnum = FBTYPE - 1;
   } else {
/*			just use the current default */
      fnum = filetype - 1;
   }

   file = &(onfile[fnum].datafile);

   getlast(file, "LINE", &fnum, sub, record, return_code);
}

/* Get the last completed gain scan */

void glgscan_(record, sub, return_code)
char *record;			/* where to put it */
int *sub;			/* subscan number to find */
int *return_code;
{
   sddfile *file;
   int fnum;

   if (*sub >= 11) {
      fnum = HCTYPE - 1;
   } else if (*sub < 11 || *sub != 0) {
      fnum = FBTYPE - 1;
   } else {
/*			just use the current default */
      fnum = filetype - 1;
   }

   file = &(onfile[fnum].gainfile);

   getlast(file, "LINE", &fnum, sub, record, return_code);
}

/* Get the last completed CONT scan */

void glcont_(sub, actualpoints, maxpoints, record, return_code)
int *sub;			/* subscan number to find */
int *actualpoints, *maxpoints;  /* not used or set */
char *record;			/* where to put it */
int *return_code;
{
   sddfile *file;
   int fnum;

   fnum = CONTYPE - 1;

   file = &(onfile[fnum].datafile);

   getlast(file, "CONT", &fnum, sub, record, return_code);
}

/*	this is what really does it when fetching the last scan */

getlast(file, mode, fnum, sub, record, return_code)
sddfile *file;				/* the file to fetch it from */
char *mode;				/* the mode it must be */
int *fnum;				/* the file number */
int *sub;				/* the sub it must be */
char *record;				/* the place to put it */
int *return_code;
{

   int last, j, toread, getincbs(), modecode, modetst, bytperrec, start_rec;

   if (file->fd < 0 || getincbs(file) != 0) RETURN(5)

/*		translate the mode into its appropriate code */
   modecode = 0;
   if (strncmp(mode, "CONT", 4) == 0) modecode = 1;
   if (strncmp(mode, "LINE", 4) == 0) modecode = 2;

   last = file->bs.num_entries_used - 1;

   for (j=last;j>=0;j--) {
      modetst = file->index[j].obsmode / 256;
      if (file->index[j].feed_number == *sub &&
          ((modecode == 0) || (modetst == modecode))) break;
   }
   if (j < 0) RETURN(5)

/*			find it and read it */

   start_rec = file->index[j].start_rec;
   bytperrec = file->bs.bytperrec;
   if (lseek(file->fd, (start_rec * bytperrec), SEEK_SET) == -1) RETURN(6)
   toread = file->index[j].nb_rec * bytperrec;
   if (read(file->fd, record, toread) != toread) RETURN(6)
   *return_code = 0;
}

/*	this signals to either initonline or stores that this code does not */
/*      use project code information for security	*/
/*	eventually this needs to be moved to the server process, but, as that */
/*	require rethinking openenaccess and possibly chgprj on the gb server */
/*	that task is left for later	*/

int useprojcode_()
{
   return(0);
}

int onlinesite_()
{
   return(1);
}
/*	this function returns and integer appropriate for the online data */
/*      file that this is dealing with */
/*	for 12-m data this should return a 1 */

void chngonline_(iftype, iver, anal_user_code, ier)
int *iftype, *iver;
char *anal_user_code;
short int *ier;
{
   void make_user_code();
   char name1[256], name2[256];
   int vlist[100], maxlist=100, oldtype, file;
   int newver;

   *ier = 0;
   newver = 0;
   oldtype = filetype;

   if (*iftype == HCTYPE || *iftype == FBTYPE || *iftype == CONTYPE) {
      filetype = *iftype;
      file = filetype - 1;
      make_user_code(anal_user_code);
   } else {
      *ier = 112;
      return;
   }

   if (*iver == 0) {
/*					most recent version */
      verslist(filetype, vlist, maxlist);
      *iver = vlist[0];
      if (*iver != onfile[file].version) newver = 1;
   } else if (*iver < 0) {
/*					subtract from current version */
      *iver = onfile[file].version + *iver;
      newver = 1;
   } else {
/*					is it different from current value */
      if (*iver != onfile[file].version) newver = 1;
   }

/*					At this point, iver <= 0 is an error */
   if (newver) {
      if (*iver <= 0) {
         *ier = 375;
         filetype = oldtype;
         return;
      }
/*			version # has changes, can we access this version */
      if (filetype == HCTYPE) {
         sprintf(name1,"%s/sdd_hc.%s_%3.3i",datadir,obsinit,*iver);
         sprintf(name2,"%s/gsdd_hc.%s_%3.3i",datadir,obsinit,*iver);
      } else {
         sprintf(name1,"%s/sdd.%s_%3.3i",datadir,obsinit,*iver);
         sprintf(name2,"%s/gsdd.%s_%3.3i",datadir,obsinit,*iver);
      }
      if (access(name1, 4) != 0) {
         *ier = 375;
         filetype = oldtype;
         return;
      }
      if (onfile[file].datafile.fd != -1) {
         close(onfile[file].datafile.fd);
         free(onfile[file].datafile.index);
         onfile[file].datafile.index = 0;
         onfile[file].datafile.fd = -1;
         onfile[file].data_status = ST_UNAVAIL;
      }
      if (onfile[file].gainfile.fd != -1) {
         close(onfile[file].gainfile.fd);
         free(onfile[file].gainfile.index);
         onfile[file].gainfile.index = 0;
         onfile[file].gainfile.fd = -1;
         onfile[file].gain_status = ST_UNAVAIL;
      }
      onfile[file].datafile.name = strdup(name1);
      if (initfile(&(onfile[file].datafile)) != 0) {
         *ier = 375;
         filetype = oldtype;
         return;
      } 
      onfile[file].data_status = ST_OK;
      onfile[file].version = *iver;
      if (filetype != CONTYPE) {
         onfile[file].gainfile.name = strdup(name2);
/*		we don't care if gzfile can open successfully or not */
         onfile[file].gain_status = ST_OK;
         if (initfile(&(onfile[file].gainfile)) != 0) 
            onfile[file].gain_status=ST_UNAVAIL;
      }
   }
}

void changever_(iftype, iver, anal_user_code, ier)
int *iftype;
int *iver;
char *anal_user_code;
short int *ier;
{
   int vlist[100], maxlist=100, i, n, fcioget_(), doagain, file;
   char tmpstr[120];
   tmpstr[0] = '\0';

   *ier = 0;
   if (*iftype != HCTYPE && *iftype != FBTYPE && *iftype != CONTYPE) {
      *ier = 112;
   }

   file = *iftype - 1;
   verslist(*iftype, vlist, maxlist);
   if (vlist[0] == -1) {
/*				nothing found */
      printf("CHNGVER: no %s files are available.\n",ftype[file].c);
   } else {
      while(1) {
         printf("CHNGVER: the following versions of type %s are available:\n",
               ftype[file].c);
         for (i=0;i<maxlist;i++) {
            if (vlist[i] < 0) break;
            if (vlist[i] == onfile[file].version) {
               printf(" -> %3i <- Current version\n", vlist[i]);
               *iver = vlist[i];
            } else {
               printf("    %3i\n", vlist[i]);
            }
         }
         printf("Which version to use (<return> implies the current version): ");
         if ((n=fcioget_(tmpstr)) <= 0) break;
         if (sscanf(tmpstr, "%i", &n) == 1) {
            chngonline_(iftype, &n, anal_user_code, ier);
            if (*ier == 0) {
               *iver = n;
               break;
            }
         }
/*			if it makes it here, either the input was wrong or */
/*			there was a problem in chngonline, try again */
         printf("Invalid or unavailable version number, try again.\n");
      }
   }
}

/*		do selection using values in params struct */
void gselect_(params, actno, maxno, list, return_code)
sel_params *params;
int *actno, *maxno, *return_code;
float *list;
{
   short id, irtn;
   int modecode, modetst, iscan, getone, lrtn, entry;
#define NFINDEX 8
   int inrange8_(), i, n, nfindex = NFINDEX, curfindex = -1;
   double def_val, value, scan_min, scan_max, x_min, x_max;
   double y_min, y_max, lst_min, lst_max, ut_min, ut_max;
   double bw_min, bw_max, f_min, f_max, feed_min, feed_max; 
   double rate_min, rate_max, int_min, int_max;
   char field[4];
   char *object, *mode;
   double utdate, lst, samprat, sourcex, sourcey;
   double deltaxr, deltayr, restfreq, freqres, rate;
   
   sddfile *file;
   sindex *index;
   sdd_index_entry findex[NFINDEX], *findptr;
   int getincbs();
   void modefield_();
   void getnsddindex();

/*		set up for a bad return */
   *actno = 0;
   *return_code = -1;
   if (*maxno <= 0) {
      *return_code = 0;
      return;
   }
/*		get the parms into usable chunks */
   def_val = params->def_val;
   scan_min = params->scan_min;
   scan_max = params->scan_max;
   feed_min = params->feed_min;
   feed_max = params->feed_max;
   x_min = params->x_min;
   x_max = params->x_max;
   y_min = params->y_min;
   y_max = params->y_max;
   lst_min = params->lst_min;
   lst_max = params->lst_max;
   ut_min = params->ut_min;
   ut_max = params->ut_max;
   bw_min = params->bw_min;
   bw_max = params->bw_max;
   f_min = params->f_min;
   f_max = params->f_max;
   rate_min = params->rate_min;
   rate_max = params->rate_max;
   int_min = params->int_min;
   int_max = params->int_max;
/*		what type of scans (LINE or CONT) */	
   modecode = 0;
   if (strncmp(params->obs_mode,"CONT",4) == 0) modecode = 1;
   if (strncmp(params->obs_mode,"LINE",4) == 0) modecode = 2;

/*	loop through the index in memory for the current file */

   file = &(onfile[filetype-1].datafile);
   if (getincbs(file) != 0) return;

   index = file->index;
   if (index == 0) return;

   for (n=0;n<file->bs.num_entries_used;n++) {
/*		check the values in the index first */
/*		continuing with search if it misses */
      modetst = index[n].obsmode / 256;
      iscan = index[n].scan_number;
      if (iscan == 0 || ((modecode != 0) && (modetst != modecode))) continue;
      value = index[n].scan_number;
      if (inrange8_(&scan_min, &scan_max, &value, &def_val) == 0) continue;
      value = index[n].feed_number;
      if (inrange8_(&feed_min, &feed_max, &value, &def_val) == 0) continue;
/*	if we get here, we may need to get the full index */
/*	but only if something isn't = def_val or strings are non-blank */
      getone = 0;
/*		mode independant stuff */
      if (x_min != def_val || x_max != def_val ||
          y_min != def_val || y_max != def_val ||
          lst_min != def_val || lst_max != def_val ||
          ut_min != def_val || ut_max != def_val) getone = 1;
/*		mode dependant stuff */
      if (getone != 1 && modecode == 1) {
         if (rate_min != def_val || rate_max != def_val ||
             int_min != def_val || int_max != def_val) getone = 1;
      } else if (getone != 1) {
         if (bw_min != def_val || bw_max != def_val ||
             f_min != def_val || f_max != def_val) getone = 1;
      }
/*		strings */
      if (getone != 1 && 
          strncmp(params->source_name,"                ", 16) != 0)
              getone = 1;
      mode = (params->obs_mode) + 4;
      if (getone != 1 && 
          strncmp(mode,"    ", 4) != 0) getone = 1;
/*		Ok, if getone == 1 here, get the full index */
      if (getone == 1) {
/*		do we need to read in a set of NFINDEX or is it already in */
         entry = n+1;
         if (curfindex == -1 ||
             curfindex > entry || entry > (curfindex + NFINDEX - 1)) {
            getnsddindex(file, &entry, findex, &nfindex, &lrtn);
            if (lrtn != 0) RETURN(2);
            curfindex = entry;
         }
/*		set findptr to point to the desired findex */
         findptr = (findex + entry - curfindex);

/*		ok, first do the strings */
         object = findptr->source;
         if ((strncmp(object, params->source_name, 16) != 0) &&
             (strncmp(params->source_name,"                ", 16) != 0)) continue;
         id = findptr->obsmode % 256;
         modefield_(&id, field, &irtn);
         mode = (params->obs_mode) + 4;
         if ((strncmp(field, mode, 4) != 0) &&
             (strncmp(mode,"    ", 4) != 0)) continue;
/*		now the mode independant */
         utdate = findptr->UT;
         if (inrange8_(&ut_min, &ut_max, &utdate, &def_val) != 1) continue;
         lst = findptr->lst;
         if (inrange8_(&lst_min, &lst_max, &lst, &def_val) != 1) continue;
         sourcex = findptr->h_coord;
         if (inrange8_(&x_min, &x_max, &sourcex, &def_val) != 1) continue;
         sourcey = findptr->v_coord;
         if (inrange8_(&y_min, &y_max, &sourcey, &def_val) != 1) continue;
/*		and finally the mode dependant stuff */
         if (modecode != 1) {
            restfreq = findptr->rest_freq;
            if (inrange8_(&f_min, &f_max, &restfreq, &def_val) != 1) continue;
            freqres = findptr->freq_res;
            if (inrange8_(&bw_min, &bw_max, &freqres, &def_val) != 1) continue;
         } else {
            samprat = findptr->rest_freq;
            if (inrange8_(&int_min,&int_max,&samprat,&def_val) != 1) continue;
            rate = findptr->freq_res;
            if (inrange8_(&rate_min, &rate_max, &rate, &def_val) != 1) continue;
         }
      }
/*		if we've made it here, it belongs in the list */
      *list = index[n].scan_number + index[n].feed_number / 100.0;
      list++;
      (*actno)++;
      if (*actno >= *maxno) break;
   }
   *return_code = 0;
}


int deffeed_()
/*	returns the default feed number for the first feed */
{

   if (filetype == HCTYPE) return(11);
   return(1);
}

void make_user_code(anal_user_code)
/*		reconstructs the full user code	*/
char *anal_user_code;
{

   int len, n;
   char *firstsp;

   strncpy(anal_user_code, obsinit,8);

   len = strlen(anal_user_code);

   if (len < 8) {
      strcat(anal_user_code," ");
      if (len < 6) {
         if (filetype == FBTYPE) {
            strcat(anal_user_code,"FB");
         } else if (filetype == HCTYPE) {
            strcat(anal_user_code,"HC");
         } else {
            strcat(anal_user_code,"  ");
         }
         for (n = (len+3);n < 8;n++) strcat(anal_user_code," ");
      } else {
         strcat(anal_user_code," ");
      }
   }
}

int qvers_(iftype)
int *iftype;
{
   int vers;

   vers = -1;
   if ((*iftype == HCTYPE) || (*iftype == FBTYPE) || (*iftype == CONTYPE)) {
      vers = onfile[*iftype-1].version;
   }
   return(vers);
}
