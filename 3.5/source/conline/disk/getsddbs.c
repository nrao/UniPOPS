
/* @(#)getsddbs.c	5.1 06/22/94 */

/* returns the bootstrap parameters, intended for use by fortran routines */
/* that want to know what the bootstrap parameters are */
/* ier = 0 if no error */
/*     = 1 if file not open */
/*     = 2 if there was a problem reading or positioning the file */
/*     = 3 if bootstrap information is bizzare */

#include <files.h>

void getsddbs_(unit, num_index_rec, num_data_rec, bytperrec, 
               bytperent, num_entries_used, ier)
int *unit, *num_index_rec, *num_data_rec, *bytperrec, *bytperent,
    *num_entries_used, *ier;

{

   int getbs();
   sddfile *fptr, *getslot();

   if ((fptr=getslot(unit)) == 0) { 
      *ier = 1;
   } else {
      if (fptr->fd == -1) {
/*		not there or just not open	*/
         *ier = 1;
      } else {
         if ((*ier = getbs(fptr)) == 0) {
            *num_index_rec = fptr->bs.num_index_rec;
            *num_data_rec = fptr->bs.num_data_rec;
            *bytperrec = fptr->bs.bytperrec;
            *bytperent = fptr->bs.bytperent;
            *num_entries_used = fptr->bs.num_entries_used;
         } else {
/*		translate the getbs error */
            if (*ier > 0) *ier = *ier + 1;
         }
      }
   }
}
