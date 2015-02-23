
/* @(#)initfile.c	5.1 06/22/94 */

/*	initializes all of the important file information, including index */
/*	returns -1 on failure	*/

#include <fcntl.h>
#include <string.h>
#include <files.h>

int initfile(fptr)
sddfile *fptr;

{
   int n;

/*			default values */

   fptr->fd = -1;
   fptr->bs.num_index_rec = 0;
   fptr->bs.num_data_rec = 0;
   fptr->bs.bytperrec = 0;
   fptr->bs.bytperent = 0;
   fptr->bs.num_entries_used = 0;
   fptr->bs.counter = 0;
   fptr->bs.typesdd = 0;
   fptr->bs.sddversion = 0;
   fptr->index = 0;
/*			unit isn't used here */
   fptr->unit = -1;

/*			open the file */

   if ((fptr->fd = open(fptr->name, O_RDONLY)) == -1) {
/*			can't open the file */
      return(1);
   }

/*			getbs does the rest */

   n = getbs(fptr);
   return(n);
}
