
/* @(#)makesindex.c	5.1 06/22/94 */

/* makesindex reads the full sdd index from a file and */
/* fills the makesindex linked list appropriately */
/* returns 0 if ok, else 1 which means there was a problem reading or */
/* positioning the file */

#include <stdio.h>
#include <malloc.h>
#include <sys/types.h>
#include <unistd.h>
#include <files.h>
#include <sdd.h>

int makesindex(fptr)
sddfile *fptr;

{

   sindex *index;
   sdd_index_entry sddindex;
   old_sdd_index_entry oldsddindex;
   int i, n, offset;

   if (fptr->index) free(fptr->index);
   fptr->index = (sindex *)calloc(fptr->nb_index_elem, sizeof(sindex));
   index = fptr->index;
   if (index == NULL) {
      return(1);
   }

/* get to start of index block */

   if ((offset=lseek(fptr->fd, fptr->bs.bytperrec, SEEK_SET)) != 
          fptr->bs.bytperrec) return(1);

/* read each index */

   for (i=0;i<fptr->bs.num_entries_used;i++) {
      if (fptr->bs.sddversion == 1) {
/*		new I*4 index */
         if ((n=read(fptr->fd, &sddindex, fptr->bs.bytperent))
               != fptr->bs.bytperent) return(1);
/*		bad number of blocks read */
/*		set values from sddindex if sddindex.scan != 0 */
         if (sddindex.scan) {
            index->scan_number = sddindex.scan;
            index->feed_number = 
               (sddindex.scan - index->scan_number) * 100. + 0.5;
            index->start_rec = sddindex.start_rec - 1;
            index->nb_rec = sddindex.end_rec - index->start_rec;
            index->obsmode = sddindex.obsmode;
            index->phase_rec = sddindex.phase_rec;
         } else {
            index->scan_number = 0;
         }
      } else {
/*		old I*2 index */
         if ((n=read(fptr->fd, &oldsddindex, fptr->bs.bytperent))
               != fptr->bs.bytperent) return(1);
/*		bad number of blocks read */
/*		set values from oldsddindex if oldsddindex.scan != 0 */
         if (oldsddindex.scan) {
            index->scan_number = oldsddindex.scan;
            index->feed_number = 
               (oldsddindex.scan - index->scan_number) * 100. + 0.5;
            index->start_rec = oldsddindex.start_rec - 1;
            index->nb_rec = oldsddindex.end_rec - index->start_rec;
            index->obsmode = oldsddindex.obsmode;
            index->phase_rec = oldsddindex.phase_rec;
         } else {
            index->scan_number = 0;
         }
      }
      index++;
   }
   return(0);
}
