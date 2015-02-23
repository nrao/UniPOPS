
/* @(#)filefns.c	5.1 06/22/94 */
/* This contains all functions that need to know */
/* about the files array */

#include <files.h>

#define NFILES 10
sddfile file[NFILES];


/* sddinit_ initiallizes the files array */
/*    It should be called only once */

void sddinit_()

{
   int i;

   for(i=0;i<NFILES;i++) {
      file[i].fd = -1;
      file[i].unit = 0;
   }
}

/* closeall_ closes all open entries in the files array */

void closeall_()

{
   int i;
   void sddclose_();

   for (i=0;i<NFILES;i++) {
      if (file[i].fd != -1) sddclose_(&file[i].unit);
   }
}

/* getslot returns a pointer to the sddfile corresponding to the */
/* slot in file array indicated by abs(*unit) if available, 0 if 
/* not available */
/* Note that if file[i].unit = 0 this means that that slot is not in use */
/* So ... this function can also be used to return a pointer to the first */
/* unused slot in the file array by simply calling it with *unit = 0 */

sddfile *getslot(unit)
int *unit;

{

   int i, iunit;

   iunit = (*unit < 0) ? -(*unit) : *unit;
   for(i=0;i<NFILES;i++) {
      if (file[i].unit == iunit) return(&file[i]);
   }

   return(0);
}

