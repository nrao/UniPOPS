
/* @(#)scanloc.c	5.1 06/22/94 */

/* gets the scan number stored at location */
/* lrtn = 0 if OK */
/*        1 if file not open */
/*        2 if problems reading from file */
/*        3 if bootstrap info is bizzar */
/*        4 if location is outside of full index range */

#include <files.h>

void scanloc_(unit, location, scan, lrtn)
int *unit;
int *location;
float *scan;
int *lrtn;

{

   int getbs(), i;
   sddfile *fptr, *getslot();
   sindex *index;

   if ((fptr=getslot(unit)) == 0) {
      *lrtn = 1; return;
   }
   
   if (fptr->fd == -1) {
      *lrtn = 1; return;
   }

/*		file not open or not there */

   if ((i=getbs(fptr)) != 0) {
      *lrtn = i + 1; return;
   }

/*		bootstrap problems */

   if ((*location <= 0) || (*location > fptr->nb_index_elem)) {
      *lrtn = 4; return;
   }

/*		*location is outside possible range for file */

   *lrtn = 0;
   if (*location <= fptr->bs.num_entries_used) {
      index = fptr->index + *location - 1;
      *scan = index->scan_number + index->feed_number / 100.;
   } else {
      *scan = -1;
   }
}
