
/* @(#)getarec.c	5.1 06/22/94 */

/* Routine to fetch a record from unit and put maxsize (R*8) of it into buf */
/* lrtn = 0 if everything went ok */
/*      = 1 if not open */
/*      = 2 if problem reading or positioning file */
/*      = 3 if bootstrap block is weird */
/*      = 4 if scan not found/ entry is empty */
/*      = 5 if scan has a weird value or entry is out of range */

#include <sys/types.h>
#include <unistd.h>
#include <files.h>

void getarec_(unit, location, phase_no, rec_no, buf, maxsize, lrtn)
int *unit, *maxsize, *lrtn, *phase_no, *rec_no;
float *location;
double *buf;

{

   int i, scan, feed, entry, size, offset, curpos;
   long phase_rec;
   sddfile *fptr, *getslot();
   sindex *index;

   *lrtn = 0;
   if ((fptr=getslot(unit)) == 0) {
      *lrtn = 1; return;
   } 
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
   if ((scan = *location) <= 0) {
         *lrtn = 5; return;
   }
   feed = ((*location - scan) * 100.0 + 0.5);
   phase_rec = *phase_no + *rec_no*64;
   if (feed == 0) feed = 1;
   for (i=0;i<fptr->bs.num_entries_used;i++) {
      if ((scan == index->scan_number) && (feed == index->feed_number) &&
	  (phase_rec == index->phase_rec) ) {
         break;
      }
      index = index++;
   }
   if (i >= fptr->bs.num_entries_used) {
      *lrtn = 4; return;
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
