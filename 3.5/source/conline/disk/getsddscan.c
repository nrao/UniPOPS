
/* @(#)getsddscan.c	5.1 06/22/94 */

/* Routine to fetch a scan from unit and put maxsize (R*8) of it into buf */
/* flag indicate whether to fetch the scan by scan number (1) or entry (0) */
/* lrtn = 0 if everything went ok */
/*      = 1 if not open */
/*      = 2 if problem reading or positioning file */
/*      = 3 if bootstrap block is weird */
/*      = 4 if scan not found/ entry is empty */
/*      = 5 if scan has a weird value or entry is out of range */

#include <sys/types.h>
#include <unistd.h>
#include <files.h>

void getsddscan(fptr, location, flag, buf, maxsize, lrtn)
sddfile *fptr;
int *flag, *maxsize, *lrtn;
float *location;
double *buf;

{

   int i, scan, feed, entry, size, offset, curpos;
   sindex *index;

   *lrtn = 0;
   if (fptr->fd == -1) {
      *lrtn = 1; return;
   }
/*		not there or just not open */

   if ((*lrtn = getbs(fptr)) != 0) {
      *lrtn = *lrtn + 1; return;
   }

/*		ok, at this point the bs block has been read, checked and */
/*		updated if necessary due to a change */

/*		if this is by scan number, look for it in the index */

   index = fptr->index;
   if (*flag) {
      if ((scan = *location) <= 0) {
         *lrtn = 5; return;
      }
      feed = ((*location - scan) * 100.0 + 0.5);
      if (feed == 0) feed = 1;
      for (i=0;i<fptr->bs.num_entries_used;i++) {
         if ((scan == index->scan_number) && (feed == index->feed_number)) {
            break;
         }
         index = index++;
      }
      if (i >= fptr->bs.num_entries_used) {
         *lrtn = 4; return;
      }
   } else {
      entry = *location;
      if ((entry > 0) && (entry <= fptr->bs.num_entries_used)) {
         index = fptr->index + entry - 1;
         if (index->scan_number == 0) {
            *lrtn = 4; return;
         }
      } else {
         *lrtn = 5; return;
      }
   }

/*		ok, just read it in	*/

   offset = index->start_rec * fptr->bs.bytperrec;
   curpos = lseek(fptr->fd, 0, SEEK_CUR);
   *lrtn = 2;
   if (curpos != -1) {
      offset = offset - curpos;
      curpos = lseek(fptr->fd, offset, SEEK_CUR);
      if (curpos != -1) {
         size = index->nb_rec * fptr->bs.bytperrec;
         size = (size % sizeof(double)) ? size/sizeof(double) + 1:
                                          size/sizeof(double);
         size = (size > *maxsize) ? *maxsize : size;
         size = size * sizeof(double);
         if ((i = read(fptr->fd, (char *)buf, size)) == size) {
            *lrtn = 0;
         }
      } 
   }
}
