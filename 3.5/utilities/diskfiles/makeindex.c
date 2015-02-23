
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <math.h>

/*	The following are defined in unistd.h in 4.1 and later but */
/*	are found in sys/file.h in 4.0.3, define them here if */
/*	they have no already been definied */
/*	If one has been definied, its probably safe to assume they all */
/*	have been definied */

#ifndef F_OK
#define F_OK 0
#define X_OK 1		/* not actually used here */
#define W_OK 2
#define R_OK 4
#endif

#define max(A,B) ((A) > (B)) ? (A) : (B)

#include <sdd.h>

/*		Keywords used in this program */
/*		They don't have to be the POPS keywords, but it makes it */
/*		easier to tell what is being used. */

enum keyword {HEADLEN=0, DATALEN, SCAN, OBJECT, OBSMODE, SAVENUM,
                 UTDATE, LST, SAMPRAT, XSOURCE, YSOURCE, COORDCD, 
                 DELTAXR, DELTAYR, RESTFREQ, FREQRES};
/*		This should be the last one in the above enumeration */
#define MAXHDR (FREQRES+1)
struct header {
   int coffset, hoffset, class, size;
   double **ptr;
} hdr[MAXHDR];

main(argc, argv)
   int argc;
   char *argv[];
{
   double *buf;
   char *cbuf;
   sdd_bs_block bsblock;
   old_sdd_bs_block oldbsblock;
   sdd_index_entry *indexed, *un_indexed, *index;
   old_sdd_index_entry oldindex;
   short *class_0;
   char *coord_sys_code, *source_name, *type_data;
   double *sourcex, *sourcey, *scan_number, *freq_resolution, *rest_freq;
   double *deltax_rate, *deltay_rate, *length_sample;
   double *lst, *ut_date, *save_bin;
   double *headlen, *datalen;

   char *fname, *pname, field[4];
   int acmode, fd, i, n, numindexperrec, maxindexent, offset, maxunindexed=0;
   int numbytes, numrecords, numhdr;
   int irec, irec1, irec2, sbin, numdata = 0, isold = 0, makeold = 0;
   int curpos, j, recindx=0, rec_num, phase_num, iscan,ifeed ;
   int oldscan = -32767, doagain, indsize, lastused, old_save_bin;
   double oldlst = 1.e34;
   short id, irtn;
   void modeid_(), modefield_(), copybytes(), sethdr();
   double temp, temp2;
   double zero = 0;
   char *blank8 = "        ", *blank16 = "                ";

   int maxhoff = 0;

#define MAXCTYPE 15
   char *ctype[MAXCTYPE];

/*   sethdr syntax: keyword, class#, class offset (R*8s), size (R*8s), &ptr */
/*		This could have been done outside of a function, but */
/*		it is MUCH easier to read (and get right) this way */

   sethdr(HEADLEN , 1, 0,1,&headlen);
   sethdr(DATALEN , 1, 1,1,&datalen);
   sethdr(SCAN    , 1, 2,1,&scan_number);
   sethdr(OBJECT  , 1, 8,2,(double **)&source_name);
   sethdr(OBSMODE , 1,10,1,(double **)&type_data);
   sethdr(SAVENUM , 1,14,1,&save_bin);
   sethdr(UTDATE  , 3, 0,1,&ut_date);
   sethdr(LST     , 3, 2,1,&lst);
   sethdr(SAMPRAT , 3, 7,1,&length_sample);
   sethdr(XSOURCE , 4, 1,1,&sourcex);
   sethdr(YSOURCE , 4, 2,1,&sourcey);
   sethdr(COORDCD , 4,16,1,(double **)&coord_sys_code);
   sethdr(DELTAXR , 6, 3,1,&deltax_rate);
   sethdr(DELTAYR , 6, 4,1,&deltay_rate);
   sethdr(RESTFREQ,12, 1,1,&rest_freq);
   sethdr(FREQRES ,12, 2,1,&freq_resolution);

   ctype[0] = "        ";   /*	not used, default is 2 */
   ctype[1] = "GALACTIC";
   ctype[2] = "1950RADC";
   ctype[3] = "EPOCRADC";
   ctype[4] = "MEANRADC";
   ctype[5] = "APPRADC ";
   ctype[6] = "APPHADC ";
   ctype[7] = "1950ECL ";
   ctype[8] = "EPOCECL ";
   ctype[9] = "MEANECL ";
   ctype[10]= "APPECL  ";
   ctype[11]= "AZEL    ";
   ctype[12]= "USERDEF ";
   ctype[13]= "2000RADC";
   ctype[14]= "INDRADC ";

   if (argc != 2) {
      printf("Usage : makeindex.exe filename\n");
      exit(1);
   }

   pname = *argv++;
   fname = *argv++;

   if (strncmp("makerecindex.exe",pname,16)==0) recindx = 1;
   if (strncmp("makeoldindex.exe",pname,16)==0) makeold = 1;
   if (strncmp("makeoldrecindex.exe",pname,19)==0) {
      recindx = 1;
      makeold = 1;
   }

/*		check for existance and access permissions */
   acmode = F_OK | R_OK | W_OK;
   if (access(fname, acmode) != 0) {
      printf("Filename, %s, does not exist or has the wrong permissions.\n", 
                 fname);
      exit(1);
   }

/*		open it for read and write */
   if ((fd = open(fname, O_RDWR, 0)) < 0) {
/*		hmm, couldn't open it after all, bizzare */
      perror("");
      printf("Filename, %s, could not be opened\n", fname);
   }

/*		Ok, now read in the bootstrap block */

   if (read(fd, &bsblock, SDDBUFSIZE) != SDDBUFSIZE) {
      printf("Error reading bootstrap block: too few bytes read.\n");
      printf("Cannot continue.\n");
      exit(1);
   }

/*		Is it an old type */
   if (bsblock.sddversion == 0) isold = 1;
   if (isold == 1) {
/*		reread the bs block into oldbsblock */
      lseek(fd, 0, SEEK_SET);
      if (read(fd, &oldbsblock, SDDBUFSIZE) != SDDBUFSIZE) {
         printf("Error reading bootstrap block: too few bytes read. \n");
         printf("Cannot continue.\n");
         exit(1);
      }
/*		copy values to new bs block */
      bsblock.num_index_rec = oldbsblock.num_index_rec;
      bsblock.num_data_rec = oldbsblock.num_data_rec;
      bsblock.bytperrec = oldbsblock.bytperrec;
      bsblock.bytperent = oldbsblock.bytperent;
      bsblock.num_entries_used = oldbsblock.num_entries_used;
      bsblock.counter = oldbsblock.counter;
      bsblock.typesdd = oldbsblock.typesdd;
   }


/*		Print out the contents of the bootstrap block */
   printf("Number of index records %i\n", bsblock.num_index_rec);
   printf("Number of data records %i\n", bsblock.num_data_rec);
   printf("Number of bytes per record %i\n", bsblock.bytperrec);
   printf("Number of bytes per index item %i\n", bsblock.bytperent);
   printf("Number of scans in file %i\n", bsblock.num_entries_used);
   printf("Counter %i will be reset to 0\n", bsblock.counter);
   if (recindx) {
      printf("Type SDD %i will be reset to 1\n", bsblock.typesdd);
   } else {
      printf("Type SDD %i\n", bsblock.typesdd);
   }
   if (isold == 1) {
      if (makeold == 1) {
        printf("Old I*2 index.\n");
      } else {
        printf("Old I*2 index, a new I*4 index will be generated.\n");
        printf("  The old I*2 index can be regenerated using \n");
        printf("  makeoldindex.exe or makeoldrecindex.exe\n");
      }
   } else {
      if (makeold == 1) {
         printf("Replacing I*4 index with old I*2 index.\n");
      }
   }

/*		At this point, set isold to value of makeold */
/*		isold now indicates the output format which may */
/*		not be the input format */
  isold = makeold;

/*		Sanity check on the values in the bootstrap block */

   if ((bsblock.num_index_rec <= 1) ||
       (bsblock.num_data_rec < 0)  ||
       (bsblock.bytperrec <= 0) ||
       (bsblock.bytperent > bsblock.bytperrec)) {
      printf("File is in an improper format!!!\n");
      printf("Cannot proceed\n");
      exit(-1);
   }

   if (bsblock.typesdd != 0 && ! recindx) {
      printf("File is in RECORDS format!!!\n");
      printf("Cannot proceed\n");
      exit(-1);
   }
   
   numindexperrec = bsblock.bytperrec / bsblock.bytperent;
   maxindexent = (bsblock.num_index_rec - 1)*numindexperrec;

   if (recindx) {
      printf("Maximum number of records which could be stored %i\n",maxindexent);
   } else {
      printf("Maximum number of scans which could be stored %i\n",maxindexent);
   }
   printf("Number of index items per record %i\n", numindexperrec);

/*		set up two internal indexes, each large enough to hold
 *		the entire index.  NOTE: this currently requires that
 *		both fit in available memory. */

   if ((indexed = (sdd_index_entry *)calloc(maxindexent, sizeof(sdd_index_entry))) == NULL) {
      perror("Can not make first index, sorry.\n");
      printf("This is probably due to a lack of memory.\n");
      printf("Try stopping some other processes to free up memory\n");
      exit(1);
   }

   if ((un_indexed = (sdd_index_entry *)calloc(maxindexent, sizeof(sdd_index_entry))) == NULL) {
      perror("Can not make second index, sorry.\n");
      printf("This is probably due to a lack of memory.\n");
      printf("Try stopping some other processes to free up memory\n");
      exit(1);
   }

/*		reset bootstrap values to default values */

   bsblock.num_data_rec = 0;
   bsblock.num_entries_used = 0;
   bsblock.counter = 0;
   if (recindx) bsblock.typesdd = 1;
   for(n=0;n<INDZERO;n++) bsblock.zeros[n]=0;

/*		seek to end of index records */

   offset = bsblock.num_index_rec * bsblock.bytperrec;
   if (lseek(fd,offset,SEEK_SET) != offset) {
      printf("Error reading to the end of the index\n");
      printf("Can not continue\n");
      exit(1);
   }
   irec = bsblock.num_index_rec;

/*		read from the file until a valid first record is identified
 *              or the end of the file is reached */

   if ((buf = (double *)malloc(bsblock.bytperrec)) == NULL) {
      perror("Unable to read in data\n");
      printf("Can not continue\n");
      exit(1);
   }
   printf("\n\n     Item   Start    Stop PC   H Coord   V Coord Source Name");
   printf("     Scan No    Del F     Rest Freq        LST    UT Date  OBSMODE");
   if (recindx) printf(" Phse Rcrd");
   printf("\n                                                            ");
   printf("              Smple Rate  Slew Rate\n\n");


   while((n=read(fd,buf,bsblock.bytperrec)) == bsblock.bytperrec &&
          numdata < maxindexent) {
      irec++;
/*		set up some pointers, first class_0 */
/*		sanity check on class 0 values */
/*		make sure all >= 0 */
      class_0 = (short *)buf;
      if (class_0[0] <= 0 || class_0[0] > 14) continue;
      for (i=1;i<=class_0[0];i++) {
         if (class_0[i] < 0) break;
      }
      if (i <= class_0[0]) continue;
/*		Class 0 looks ok, set up pointers to needed header items */
/*		remember, class_0 is 1 relative !! */
/*		first, we need headlen to set class_0 limit */
/*		headlen & datalen should be in the first record just read */
/*		if not, skip this scan without warning */
      hdr[HEADLEN].hoffset = (class_0[hdr[HEADLEN].class] - 1) + 
                                hdr[HEADLEN].coffset;
      hdr[DATALEN].hoffset = (class_0[hdr[DATALEN].class] - 1) + 
                                hdr[DATALEN].coffset;
      if ((hdr[HEADLEN].hoffset+1)*sizeof(double) >= bsblock.bytperrec) continue;
      if ((hdr[DATALEN].hoffset+1)*sizeof(double) >= bsblock.bytperrec) continue;

      headlen = buf + hdr[HEADLEN].hoffset;
      datalen = buf + hdr[DATALEN].hoffset;
/*		set class_0 values so that they are useful for sanity checks */
      class_0[class_0[0]+1] = *headlen / sizeof(double) + 1;
      doagain = 1;
      while (doagain) {
         doagain = 0;
         for (i=1;i<class_0[0]+1;i++) {
            if (class_0[i] <= 0) {
               class_0[i] = class_0[i+1];
               doagain = 1;
            }
         }
      }

/*		Ok, now we can set and check each header pointer */
      maxhoff = 0;
      for (i=0;i<MAXHDR;i++) {
/*		set hoffset to -1 in case it can't be set */
         hdr[i].hoffset = -1;
/*		check that desired class exists */
         if (hdr[i].class <= class_0[0]) {
/*		check that coffset doesn't go into the next class */
            if (hdr[i].coffset < (class_0[hdr[i].class+1] - class_0[hdr[i].class])) {
/*		if size = 2, make sure the second one is also OK */
               if ((hdr[i].size == 2 && hdr[i].coffset+1 < class_0[hdr[i].class+1]) ||
                    hdr[i].size == 1) {
/*		Ok, set hoffset */
                  hdr[i].hoffset = hdr[i].coffset +
                                   class_0[hdr[i].class] - 1;
               }
            }
         }
         maxhoff = max(maxhoff, hdr[i].hoffset);
      }

      numbytes = *headlen + *datalen;
     
      if (numbytes < bsblock.bytperrec) continue;
      numrecords = (numbytes % bsblock.bytperrec) ? 
                  (numbytes / bsblock.bytperrec + 1) :
                  (numbytes / bsblock.bytperrec);
      numbytes = numrecords * bsblock.bytperrec;
/*		read in enough for 1 extra double */
      numhdr = (maxhoff+2)*sizeof(double);

/*		remember location of this scan */
      irec1 = irec;
      irec2 = irec + numrecords - 1;

/*		read the header into buf, make sure its big enough */

      if ((buf = (double *)realloc (buf, numhdr)) == NULL) {
         perror("");
         printf("Unable to read scan %f8.2, too many bytes\n", *scan_number);
         printf("HEADLEN = %f\n", *headlen);
         printf("DATALEN = %f\n", *datalen);
         printf("Skipping this scan\n");
         continue;
      }

      numhdr = numhdr - bsblock.bytperrec;
      if (read(fd,(buf+bsblock.bytperrec/8), numhdr) != numhdr) {
         perror("");
         printf("Unexpected problem reading data for scan %f8.2\n",*scan_number);
         printf("Can not continue\n");
         exit(1);
      }
/*		lseek to end of this data block */
      numbytes = numbytes - (numhdr + bsblock.bytperrec);
      lseek(fd, numbytes, SEEK_CUR);

      irec = irec2;
      numdata++;

/*		OK, NOW we can set all of the valid header pointers */
      for (i=0;i<MAXHDR;i++) {
         if (hdr[i].hoffset < 0) {
/*		point at zero if not set, deal with char ptrs as special case*/
            *(hdr[i].ptr) = &zero;
         } else {
            *(hdr[i].ptr) = buf + hdr[i].hoffset;
         }
      }
      if (hdr[OBJECT].hoffset < 0) *(hdr[OBJECT].ptr) = (double *)blank16;
      if (hdr[OBSMODE].hoffset < 0) *(hdr[OBSMODE].ptr) = (double *)blank8;
      if (hdr[COORDCD].hoffset < 0) *(hdr[COORDCD].ptr) = (double *)blank8;

/*			Ok, you've got it, now make the index */
/*			Is it already indexed, and if so, does it fit */
/*			in the available index and is that space open */
      
      if (*save_bin == infinity())  *save_bin = 0;
      sbin = *save_bin;
      if (sbin > 0 && sbin <= maxindexent) {
         index = (indexed + sbin - 1);
         if (index->start_rec > 0) {
            index = (un_indexed + maxunindexed);
            maxunindexed++;
            sbin = 0;
         } else {
/*			alter the bs block copy to reflect this change */
            bsblock.num_data_rec = max(bsblock.num_data_rec,(irec2-bsblock.num_index_rec));
            bsblock.num_entries_used = 
                max(bsblock.num_entries_used, sbin);
         }
      } else {
         index = (un_indexed + maxunindexed);
         maxunindexed++;
         sbin = 0;
      }

      index->start_rec = irec1;
      index->end_rec = irec2;
      index->h_coord = *sourcex;
      index->v_coord = *sourcey;
      for(i=0;i<16;i++) index->source[i] = source_name[i];
      index->scan = *scan_number;
      index->lst = *lst;
      index->UT = *ut_date;
      if (recindx) {

/*   	   records file */
/*	   Use change in LST to reset number of phases and increment record */
/*         counter; use change in scan number to reset record and phase number */
/*	   Code record/phase numbers as 64*record_num + phase_num */
/*	   This only works for GB data, but there is currently no check */
/*	   that it is, in fact, GB data */

         iscan = *scan_number;
         if (iscan != oldscan) {
            rec_num = 1;
            phase_num = 1;
            oldscan = iscan;
            oldlst = *lst;
         } else {
            if (*lst == oldlst) {
               ifeed = ((*scan_number - iscan) * 100.0 + 0.5);
               if (ifeed == 0 || ifeed == 1) phase_num++;
            } else {
               rec_num++;
               phase_num = 1;
               oldlst = *lst;
            }
         }
         index->phase_rec = 64*rec_num + phase_num;
      } else {
         index->phase_rec = -1;
      }
      for (i=0;i<INDZERO;i++) index->zeros[i] = 0;
/*		default pos_code is 2 = 1950RADC */
      index->pos_code = 2;
      for (i=1;i<MAXCTYPE;i++) {
         if (strncmp(coord_sys_code, ctype[i],8) == 0) {
            index->pos_code = i;
            break;
         }
      }

      modeid_((type_data+4),&id,&irtn);
      index->obsmode = id;
      index->freq_res = *freq_resolution;
      index->rest_freq = *rest_freq;
      if (strncmp("LINE",type_data,4) == 0) {
         index->obsmode = index->obsmode + 512;
      } else if (strncmp("CONT",type_data,4) == 0) {
         index->obsmode = index->obsmode + 256;
         temp = *deltax_rate * cos(*sourcey * M_PI / 180.0);
         temp2 = *deltay_rate;
         index->freq_res = sqrt(temp*temp + temp2*temp2);
         index->rest_freq = *length_sample;
      }

      if (sbin != 0) {
         printf(" %8i",sbin);
         printf("%8i",index->start_rec);
         printf("%8i",index->end_rec);
         printf("%3i",index->pos_code);
         printf("%10.4f",index->h_coord);
         printf("%10.4f",index->v_coord);
         printf(" %16.16s",index->source);
         printf("%8.2f",index->scan);
         printf("%11.6f",index->freq_res);
         printf("%- #16.9g",index->rest_freq);
         printf("%8.4f",index->lst);
         printf("%10.4f",index->UT);
         printf(" %8.8s",type_data);
         if (recindx) {
            printf("%4i",phase_num);
            printf("%4i\n", rec_num);
         } else {
            printf("\n");
         }
         if (irtn != 0) {
            printf(" Scan number %8.2f OBSMODE %8.8s is not standard\n",
                     index->scan, type_data);
         }
      }
   }
/*		Did it fail because the index is full? */
   if (numdata >= maxindexent) {
      printf("\n\nOut of space in index, too much data!!!.\n");
      printf("The index needs to be expanded before all of the data\n");
      printf("can be accessed.  Use the expandsdd utility.\n");
      printf("Wait while I write the index back to disk.\n");
   } else {
/*		Ok, the read failed, either a problem or EOF */
      if (n != 0) {
/*		A problem, give a warning before continuing */
         perror("");
         printf("Unexpected number of bytes read at end of file.\n");
         printf("continuuing ...\n");
      }
   }

/*		Deal with unindexed index */
   n = 0;
   free(buf);
   if ((buf = (double *)malloc(bsblock.bytperrec)) == NULL) {
      perror("Can not finish indexing the file.\n");
      maxunindexed = 0;
   }
   for (i=0; i < maxunindexed; i++) {
/*		find an empty spot in the real index, don't reset n */
      for (;n<maxindexent;n++) {
         if (indexed[n].start_rec == 0) break;
      }
/*		double check that its really empty */
      if (indexed[n].start_rec != 0) {
/*		This indicates a bug someplace !! */
         printf("The index is too small, but at this point in makeindex\n");
         printf("this should NEVER happen.  This is a bug.  Please report\n");
         printf("it to a unipops guru.\n");
         printf("Please wait while I write the index as I have it to disk.\n");
         break;
      }
/*		copy the index */
      indexed[n].start_rec = un_indexed[i].start_rec;
      indexed[n].end_rec = un_indexed[i].end_rec;
      indexed[n].pos_code = un_indexed[i].pos_code;
      indexed[n].h_coord = un_indexed[i].h_coord;
      indexed[n].v_coord = un_indexed[i].v_coord;
      for (j=0;j<16;j++) indexed[n].source[j] = un_indexed[i].source[j];
      indexed[n].scan = un_indexed[i].scan;
      indexed[n].freq_res = un_indexed[i].freq_res;
      indexed[n].rest_freq = un_indexed[i].rest_freq;
      indexed[n].lst = un_indexed[i].lst;
      indexed[n].UT = un_indexed[i].UT;
      indexed[n].obsmode = un_indexed[i].obsmode;
      indexed[n].phase_rec = un_indexed[i].phase_rec;
      for(j=0;j<INDZERO;j++) indexed[n].zeros[j] = 0;
/*		Update the bootstrap block */
      bsblock.num_data_rec = max(bsblock.num_data_rec,
                 (indexed[n].end_rec-bsblock.num_index_rec));
      bsblock.num_entries_used = max(bsblock.num_entries_used, n+1);
/*		Ok, now the tricky part, find the data in the file and */
/*		modify save_bin for this scan */
      curpos = lseek(fd,0,SEEK_CUR);
      offset = (indexed[n].start_rec - 1) * bsblock.bytperrec - curpos;
      if (lseek(fd, offset, SEEK_CUR) != (curpos + offset)) {
         printf("Unable to find previously indexed data in file.\n");
         printf("The file has changed since this program started.\n");
         printf("Unable to continue\n");
         exit(1);
      }
      curpos = lseek(fd,0,SEEK_CUR);
      if (read(fd,buf,bsblock.bytperrec) != bsblock.bytperrec) {
         printf("Unable to read previously indexed data in file.\n");
         printf("The file has changed since this program started.\n");
         printf("Unable to continue\n");
         exit(1);
      }
/*		no sanity check this time on class_0 values */
/*		Note, this assumes that save_bin can be found in the first */
/*              bytperrec bytes, if not, continue */
      
      class_0 = (short *)buf;

      hdr[SAVENUM].hoffset = -1;
      if (hdr[SAVENUM].class <= class_0[0]) {
         if (hdr[SAVENUM].coffset < 
             (class_0[hdr[SAVENUM].class+1] - class_0[hdr[SAVENUM].class])) {
            hdr[SAVENUM].hoffset = hdr[SAVENUM].coffset +
                                   class_0[hdr[SAVENUM].class] - 1;
         }
      }
      if ((hdr[SAVENUM].hoffset+1)*sizeof(double) >= bsblock.bytperrec) continue;
      if (hdr[SAVENUM].hoffset < 0) {
         *(hdr[SAVENUM].ptr) = &zero;
      } else {
         *(hdr[SAVENUM].ptr) = buf + hdr[SAVENUM].hoffset;
      }
      if (*save_bin != infinity() && *save_bin != 0) {
/*		Don't change anything if it wasn't already set */
/*		remember old value */
         old_save_bin = (int) (*save_bin+0.5);
         *save_bin = n+1;
         offset = - bsblock.bytperrec;
         if (lseek(fd, offset, SEEK_CUR) != curpos){
            printf("Unable to find previously indexed data in file.\n");
            printf("The file has changed since this program started.\n");
            printf("Unable to continue\n");
            exit(1);
         }
         if (write(fd, buf, bsblock.bytperrec) != bsblock.bytperrec) {
            perror("Unable to update indexed data in file.\n");
            printf("Unable to continue\n");
            exit(1);
         }
      } else {
         old_save_bin = 0;
      }
/*		Ok, finally, show the user this index entry in full */
      printf(" %8i",n+1);
      printf("%8i",indexed[n].start_rec);
      printf("%8i",indexed[n].end_rec);
      printf("%3i",indexed[n].pos_code);
      printf("%10.4f",indexed[n].h_coord);
      printf("%10.4f",indexed[n].v_coord);
      printf(" %16.16s",indexed[n].source);
      printf("%8.2f",indexed[n].scan);
      printf("%11.6f",indexed[n].freq_res);
      printf("%- #16.9g",indexed[n].rest_freq);
      printf("%8.4f",indexed[n].lst);
      printf("%10.4f",indexed[n].UT);
      id = indexed[n].obsmode % 256;
      modefield_(&id, field, &irtn);
      switch (indexed[n].obsmode - id) {
         case 256:
            printf(" CONT%4.4s", field);
            break;
         case 512:
            printf(" LINE%4.4s", field);
            break;
         default:
            printf("     %4.4s", field);
            break;
      }
      if (recindx) {
         phase_num = indexed[n].phase_rec % 64;
         rec_num = (indexed[n].phase_rec - phase_num)/64;
         printf("%4i",phase_num);
         printf("%4i\n",rec_num);
      } else {
         printf("\n");
      }
/*		issue a warning if old_save_bin was non-zero and different */
      if (old_save_bin != 0 && old_save_bin != (n+1)) {
         printf(" ^^^^^^^^ New SAVENUM value (old value was %i)\n",old_save_bin);
      }
   }
/*		Write out the bootstrap and index */

   if (lseek(fd, 0, SEEK_SET) != 0) {
      perror("");
      printf("Can not continue\n");
      exit(1);
   }
   if (isold == 1) {
/*		check for overflows, don't make old index if anything too large */
/*		first, the 3 likely places in the bs block */	
      if (bsblock.num_index_rec >= 32768 ||
          bsblock.num_data_rec >= 32768 ||
          bsblock.num_entries_used >= 32768) isold = 0;
/*		Then each index entry */
      for (i=0;i<bsblock.num_entries_used;i++) {
         if (indexed[i].start_rec >= 32768 ||
             indexed[i].end_rec >= 32768) {
            isold = 0;
            break;
         }
      }
/*		At this point, isold indicates what should be done */
      if (isold == 1) {
/*		transfer bsblock to oldbsblock */
         oldbsblock.num_index_rec = bsblock.num_index_rec;
         oldbsblock.num_data_rec = bsblock.num_data_rec;
         oldbsblock.bytperrec = bsblock.bytperrec;
         oldbsblock.bytperent = bsblock.bytperent;
         oldbsblock.num_entries_used = bsblock.num_entries_used;
         oldbsblock.counter = 0;
         oldbsblock.typesdd = bsblock.typesdd;
         for (i=0;i<OLDBSZERO;i++) oldbsblock.zeros[i] = 0;
/*		and write it out */
         if (write(fd,&oldbsblock,sizeof(oldbsblock)) != sizeof(oldbsblock)) {
            perror("Error writing bootstrap block\n");
            printf("Can not continue\n");
            exit(1);
         }
/*		if not old, just write out bsblock */
      } else {
         bsblock.sddversion = 1;
         if (write(fd,&bsblock,sizeof(bsblock)) != sizeof(bsblock)) {
            perror("Error writing bootstrap block\n");
            printf("Can not continue\n");
            exit(1);
         }
      }
   } else {
      bsblock.sddversion = 1;
      if (write(fd,&bsblock,sizeof(bsblock)) != sizeof(bsblock)) {
         perror("Error writing bootstrap block\n");
         printf("Can not continue\n");
         exit(1);
      }
   }
   offset = bsblock.bytperrec;
   if (lseek(fd,offset,SEEK_SET) != offset) {
      perror("");
      printf("Can not continue\n");
      exit(1);
   }
/*		first, copy index to cbuf and write out each cbuf */
/*		make sure cbuf is one record */
   if ((cbuf = (char *)malloc(bsblock.bytperrec)) == NULL) {
      perror("Can not finish indexing the file.\n");
      maxunindexed = 0;
   }
/*		then loop over the total number of records */
   index = indexed;
   indsize = bsblock.bytperent;
   numbytes = bsblock.bytperrec;
   lastused = (indsize*numindexperrec) / sizeof(char);
   for(i=0;i<(bsblock.num_index_rec-1);i++) {
/*		And over the number of index ent/record */
      for(n=0;n<numindexperrec;n++) {
/*		for old index, first copy new index values */
/*		over to old index structure, then copy old */
/*		index to cbuf */
         if (isold == 1) {
            oldindex.start_rec = index->start_rec;
            oldindex.end_rec = index->end_rec;
            oldindex.magic = 2;
            oldindex.pos_code = index->pos_code;
            oldindex.h_coord = index->h_coord;
            oldindex.v_coord = index->v_coord;
            for(j=0;j<16;j++) {
               oldindex.source[j] = index->source[j];
            }
            oldindex.scan = index->scan;
            oldindex.freq_res = index->freq_res;
            oldindex.rest_freq = index->rest_freq;
            oldindex.lst = index->lst;
            oldindex.UT = index->UT;
            oldindex.obsmode = index->obsmode;
            oldindex.phase_rec = index->phase_rec;
            for(j=0;j<OLDINDZERO;j++) oldindex.zeros[j]=0;
/*		copy the bytes over */
            copybytes((char *)&oldindex, (cbuf+n*indsize), indsize);
         } else {
/*		copy the bytes over */
            copybytes((char *)index, (cbuf+n*indsize), indsize);
         }
/*		increment index pointer */
         index++;
      }
/*		make sure the rest of cbuf is zero */
      for (j=lastused;j<(numbytes/sizeof(char));j++) {
         cbuf[j] = '\0';
      }
/*		write cbuf out */
      if (write(fd,cbuf,numbytes) != numbytes) {
         perror("Error writing index.\n");
         printf("Can not continue\n");
         exit(1);
      }
   }
   exit(0);
}

void copybytes(inbuf, outbuf, size) 
   char *inbuf, *outbuf;
   int size;
{
   int j;
   for (j=0;j<size;j++) {
      outbuf[j] = inbuf[j];
   }
}

void sethdr(kword, class, coffset, size, ptr)
   int kword, coffset, class, size;
   double **ptr;
{
   hdr[kword].class = class;
   hdr[kword].coffset = coffset;
   hdr[kword].hoffset = -1;
   hdr[kword].size = size;
   hdr[kword].ptr = ptr;
}

