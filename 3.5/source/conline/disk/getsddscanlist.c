
/* @(#)getsddscanlist.c	5.1 06/22/94 */

/* getsddscanlist returns in rbuf the scans numbers of whats on disk */
/* actelem is set to the actual number of scans returned */
/* maxelem is the maximum number of elements of rbuf */
/* ier = 0 if everything went ok */
/*     = 1 if file is not open */
/*     = 2 if there was a problem reading or positioning the file */
/*     = 3 if the bootstrap information is bizzare */

#include <files.h>

void getsddscanlist(fptr, rbuf, actelem, maxelem, ier)
sddfile *fptr;
int *actelem, *maxelem, *ier;
float *rbuf;

{

   int i;
   sindex *index;
   float scan, feed;

   *actelem = 0;
   *ier = 0;
   if (fptr->fd == -1) {
/*			not there or just not open */
      *ier = 1;
   } else {
      if ((*ier = getbs(fptr)) == 0) {
         index = fptr->index;
         for (i=0;i<*maxelem && i<fptr->bs.num_entries_used;i++) {
            if ((index+i)->scan_number != 0) {
               *(rbuf+i) = (index+i)->scan_number + 
                           (index+i)->feed_number / 100.;
            } else {
/*			if scan_number is 0, its really empty */
               *(rbuf+i) = 0;
            }
         }
         *actelem= i;
      } else {
/*		translate the getbs error	*/
         if (*ier > 0) *ier = *ier + 1;
      }
   }
}
