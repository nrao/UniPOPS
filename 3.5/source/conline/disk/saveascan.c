
/* @(#)saveascan.c	5.1 06/22/94 */

/* saves the scan found in buf at location as determined by flag and */
/* overwrite.  if flag = 0 then location is an index entry, if flag = 1 */
/* then location is a scan number.  If overwrite is 0, overwritting is */
/* not allowed, else it is allowed */
/* obuf is the compressed data, i.e. its what needs to be written */

/* If an entry is occuped and it will be overwritten then there is the */
/* potential for wasted space: if the new data does NOT fit in the old */
/* data then the old location is zeroed out and the new data is put at */
/* the end of the file, effectively leaving the old space unusable */
/* Similary, if the new data is smaller than the data, the difference will */
/* be zeroed out and remain wasted */

/* lrtn = 0 if everything went ok */
/*      = 1 if file not open */
/*      = 2 if problems reading file or positioning file for read */
/*	= 3 if bootstrap info is bizzare */
/*      = 4 if file index is full, can't store any more */
/*      = 5 if improper location specified on a savebyloc */
/*      = 6 if overwrite is not allowed and attempted to overwrite */
/*      = 7 if modeid returned an error */
/*      = 8 if error writing to file or positioning file for write */
/*	= 9 warning if overwrite happened and overwrite was allowed */
/*	=10 both 7 and 9 occured
/* of these, 7, 9 & 10 are warnings, all of the rest are severe and indicate */
/* that nothing was saved to the file or the file may be corrupt */

#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <files.h>
#include <sdd.h>

void saveascan_(unit, location, flag, buf, obuf, overwrite, lrtn)
int *unit, *flag, *overwrite, *lrtn;
double *location, *buf, *obuf;

{

   int i, n, scan, feed, entry, size, offset, curpos;
   int outstart, outnum, zerostart, zeronum, outelem, getbs();
   double temp1, temp2;
   short int ier;
   void modeid_();
   static char zeros[SDDBUFSIZE] = {0};
   sddfile *fptr, *getslot();
   sindex *index;
   sdd_index_entry sddindex;
   old_sdd_index_entry oldsddindex;
   sdd_bs_block sddbs;
   old_sdd_bs_block oldsddbs;
   sddheader *header, *oheader;
   char coord_sys_code[8];
   static struct {
      char code[8];
   } csc[14] = {
      "GALACTIC",
      "1950RADC",
      "EPOCRADC",
      "MEANRADC",
      "APPRADC ",
      "APPHADC ",
      "1950ECL ",
      "EPOCECL ",
      "MEANECL ",
      "APPECL  ",
      "AZEL    ",
      "USERDEF ",
      "2000RADC",
      "INDRADC " 
   };

   *lrtn = 0;
   if ((fptr=getslot(unit)) == 0) {
      *lrtn = 1; return;
   } else {
      if (fptr->fd == -1) {
         *lrtn = 1; return;
/*			not there or just not open */
      } else {
         if ((*lrtn = getbs(fptr)) != 0) {
            *lrtn = *lrtn + 1; return;
         }
      }
   }

/*		ok, at this point the bs block has been read, checked and */
/*		updated if necessary due to a change */

/*		if this is by scan number, look for it in the index */

   entry = 0;
   if (*flag) {
      index = fptr->index;
      scan = *location;
      feed = ((*location - scan) * 100.0 + 0.5);
      if (feed == 0) feed = 1;
      for (i=0;i<fptr->bs.num_entries_used;i++) {
         if ((scan == index->scan_number) && (feed == index->feed_number)) {
            entry = i + 1;
            break;
         }
         index++;
      }
   } else {
      entry = *location;
   }

   if ((entry <= 0) && *flag) {
/*			need to find an available empty entry */
      index = fptr->index;
      for (i=0;i<fptr->bs.num_entries_used;i++) {
         if (index->scan_number == 0) break;
      }
      entry = i + 1;
   } if ((entry <= 0) && ! *flag) {
/*			woops, there should have been an entry specified */
      *lrtn = 5; return;
   }

   if (entry > fptr->nb_index_elem) {
/*			file is full */
      *lrtn = 4; return;
   }

   if (entry > fptr->bs.num_entries_used) {
/*		someplace past the end, make sure its zero */
      index = fptr->index + entry - 1;
      index->scan_number = 0;
   }

   header = (sddheader *) buf;
   oheader = (sddheader *)obuf;

/*				store entry into header.save_bin */
/*				this assumes that the compression hasn't */
/*				affected the location of save_bin, which is */
/*				probably true */
   oheader->save_bin = entry;

/*				recalculate outnum and outelem	*/
   outnum = oheader->header_length + oheader->data_length;
   outnum = (outnum % fptr->bs.bytperrec) ? 
                   (outnum/fptr->bs.bytperrec) + 1  :
                   (outnum/fptr->bs.bytperrec);
   outelem = outnum * fptr->bs.bytperrec / sizeof(double);

   index = fptr->index + entry - 1;

   if (index->scan_number != 0) {
      if (*overwrite) {
/*					okay to overwrite this	*/
         *lrtn = 9;
         if (outnum <= index->nb_rec) {
/*					there IS room	*/
            outstart = index->start_rec;
            zeronum = index->nb_rec - outnum;
            zerostart = outstart + outnum;
         } else {
/*					there is NOT room */
            outstart = fptr->bs.num_index_rec + 
                       fptr->bs.num_data_rec;
            zerostart = index->start_rec;
            zeronum = index->nb_rec;
         }
      } else {
/*					NOT ok to overwrite */
         *lrtn = 6; return;
      }
   } else {
/*					an empty slot, add data to end */
      outstart = fptr->bs.num_index_rec + fptr->bs.num_data_rec;
      zerostart = 0;
      zeronum = 0;
   }

   sddindex.start_rec = outstart + 1;
   sddindex.end_rec = sddindex.start_rec + outnum - 1;
   sddindex.pos_code = 2;
   for (i=0;i<8;i++) coord_sys_code[i] = header->coord_sys_code[i];
   for (i=0;i<14;i++) {
      if ((n=strncmp(coord_sys_code, csc[i].code, 8)) == 0) {
         sddindex.pos_code = ++i; 
         break;
      }
   }
   sddindex.h_coord = header->sourcex;
   sddindex.v_coord = header->sourcey;
   for (i=0;i<16;i++) sddindex.source[i] = header->source_name[i];
   sddindex.scan = header->scan_number;
   sddindex.freq_res = header->freq_resolution;
   sddindex.rest_freq = header->rest_freq;
   sddindex.lst = header->lst;
   sddindex.UT = header->ut_date;
   modeid_((header->type_data+4),&sddindex.obsmode, &ier);
   if (ier != 0) {
/*				not found, set an errorcode but keep going */
      if (*lrtn == 9) {
         *lrtn = 10;
      } else {
         *lrtn = 7;
      }
   }
   if ((strncmp(header->type_data,"LINE",4)) == 0) {
      sddindex.obsmode = sddindex.obsmode + 512;
   } else if ((strncmp(header->type_data,"CONT",4)) == 0) {
      sddindex.obsmode = sddindex.obsmode + 256;
      temp1 = header->deltax_rate * cos(header->sourcey * M_PI / 180.);
      temp2 = header->deltay_rate;
      sddindex.freq_res = hypot(temp1, temp2);
      sddindex.rest_freq = header->length_sample;
   }
   for (i=0;i<INDZERO;i++) sddindex.zeros[i] = 0;

/*				set bootstrap		*/

   sddbs.num_index_rec = fptr->bs.num_index_rec;
   sddbs.num_data_rec = fptr->bs.num_data_rec;
   sddbs.num_entries_used = fptr->bs.num_entries_used;
   sddbs.bytperrec = fptr->bs.bytperrec;
   sddbs.bytperent = fptr->bs.bytperent;
   sddbs.counter = (fptr->bs.counter < 32767) ? 
                       fptr->bs.counter + 1 : 0;
   sddbs.typesdd = fptr->bs.typesdd;
   sddbs.sddversion = fptr->bs.sddversion;
   for (i=0;i<BSZERO;i++) sddbs.zeros[i] = 0;

/*				change any */

   if (entry > sddbs.num_entries_used) sddbs.num_entries_used = entry;
   if ((outstart + outnum) > (sddbs.num_index_rec + sddbs.num_data_rec)) {
      sddbs.num_data_rec = (outstart + outnum) - sddbs.num_index_rec;
   }

/*				okay, write out the new stuff */
/*				zeroing stuff as appropriate	*/

/*				change outstart, etc to bytes */
   outstart = outstart * fptr->bs.bytperrec;
   zerostart = zerostart * fptr->bs.bytperrec;
   outnum = outnum * fptr->bs.bytperrec;
/* 				where we are now */
   curpos = lseek(fptr->fd, 0, SEEK_CUR); 
   if (outstart < zerostart) {
      offset = outstart - curpos;
/*				where we want to be */
      curpos = lseek(fptr->fd, offset, SEEK_CUR);
      if (curpos != outstart) {
         *lrtn = 8; return;
      }
/*				are we actually there */
      if ((n=write(fptr->fd,(char *)obuf,outnum)) != outnum) {
         *lrtn = 8; return;
      }
      curpos = outstart + outnum;
/*				write it out */
/*				same with zeros */

      if (zeronum) {
         offset = zerostart - curpos;
         curpos = lseek(fptr->fd, offset, SEEK_CUR);
         if (curpos != zerostart) {
            *lrtn = 8; return;
         }
         for (i=0;i<zeronum;i++) {
            if ((n=write(fptr->fd,zeros,fptr->bs.bytperrec)) !=
                   fptr->bs.bytperrec) {
               *lrtn = 8; return;
            }
         }
      }
   } else {
/*				write out zeros first */
      if (zeronum) {
         offset = zerostart - curpos;
         curpos = lseek(fptr->fd, offset, SEEK_CUR); 
         if (curpos != zerostart) { 
            *lrtn = 8; return;
         } 
         for (i=0;i<zeronum;i++) { 
            if ((n=write(fptr->fd,zeros,fptr->bs.bytperrec)) != 
                   fptr->bs.bytperrec) { 
               *lrtn = 8; return;
            } 
         } 
         curpos = zerostart + fptr->bs.bytperrec * zeronum;
      }
/*				and then the data */
      offset = outstart - curpos;
      curpos = lseek(fptr->fd, offset, SEEK_CUR);
      if (curpos != outstart) {
         *lrtn = 8; return;
      }   
      if (write(fptr->fd,(char *)obuf,outnum) != outnum) {
         *lrtn = 8; return;
      }   
   }

/*			Ok, now the index */

/*			offset here calculated relative to start of file */
   offset = (entry-1) * fptr->bs.bytperent + fptr->bs.bytperrec;
   if (lseek(fptr->fd, offset, SEEK_SET) != offset) {
      *lrtn = 8; return;
   }
   if (fptr->bs.sddversion == 0) {
/*			old I*2 type */
/*			Note: there is NO check on overflow here */
/*			It is assumed that the file was made small */
/*			enough that this won't be a problem */
      oldsddindex.start_rec = sddindex.start_rec;
      oldsddindex.end_rec = sddindex.end_rec;
      oldsddindex.magic = 2;
      oldsddindex.pos_code = sddindex.pos_code;
      oldsddindex.h_coord = sddindex.h_coord;
      oldsddindex.v_coord = sddindex.v_coord;
      for (i=0;i<16;i++) oldsddindex.source[i] = sddindex.source[i];
      oldsddindex.scan = sddindex.scan;
      oldsddindex.freq_res = sddindex.freq_res;
      oldsddindex.rest_freq = sddindex.rest_freq;
      oldsddindex.lst = sddindex.lst;
      oldsddindex.UT = sddindex.UT;
      oldsddindex.obsmode = sddindex.obsmode;
      oldsddindex.phase_rec = sddindex.phase_rec;
      for (i=0;i<OLDINDZERO;i++) oldsddindex.zeros[i]=0;
      if (write(fptr->fd,(char *)&oldsddindex,sizeof(old_sdd_index_entry)) !=
               sizeof(old_sdd_index_entry)) {
         *lrtn = 8; return;
      }
   } else {
/*			current type */
      if (write(fptr->fd,(char *)&sddindex,sizeof(sdd_index_entry)) !=
               sizeof(sdd_index_entry)) {
         *lrtn = 8; return;
      }
   }

/*			And finally, the bootstrap block */

   if (lseek(fptr->fd, 0, SEEK_SET) != 0) {
      *lrtn = 8; return;
   }
   if (fptr->bs.sddversion == 0) {
/*			old I*2 type */
/*			Note: there is NO check on overflow here */
/*			It is assumed that the file was made small */
/*			enough that this won't be a problem */
      oldsddbs.num_index_rec = sddbs.num_index_rec;
      oldsddbs.num_data_rec = sddbs.num_data_rec;
      oldsddbs.bytperrec = sddbs.bytperrec;
      oldsddbs.bytperent = sddbs.bytperent;
      oldsddbs.num_entries_used = sddbs.num_entries_used;
      oldsddbs.counter = sddbs.counter;
      oldsddbs.typesdd = 0;
      for (i=0;i<OLDBSZERO;i++) oldsddbs.zeros[i]=0;
      if (write(fptr->fd,(char *)&oldsddbs, sizeof(old_sdd_bs_block)) !=
               sizeof(old_sdd_bs_block)) {
         *lrtn = 8; return;
      }
   } else {
/*			current type */
      if (write(fptr->fd,(char *)&sddbs, sizeof(sdd_bs_block)) !=
               sizeof(sdd_bs_block)) {
         *lrtn = 8; return;
      }
   }
/*				update internal copies of index and bs */
   index->scan_number = sddindex.scan;
   index->feed_number = (sddindex.scan - index->scan_number) * 100 + 0.5;
   index->start_rec = sddindex.start_rec - 1;
   index->nb_rec = sddindex.end_rec - index->start_rec;
   index->obsmode = sddindex.obsmode;

   fptr->bs.num_index_rec = sddbs.num_index_rec;
   fptr->bs.num_data_rec = sddbs.num_data_rec;
   fptr->bs.num_entries_used = sddbs.num_entries_used;
   fptr->bs.counter = sddbs.counter;
/*		bytperent and bytperrec don't change, hopefully */
/*		also, typesdd doesn't change */
/*		and sddversion */

/*				whew!  I think that about covers it */

}
