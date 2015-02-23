
/* @(#)incrindex.c	5.1 06/22/94 */


#include <stdio.h>
#include <malloc.h>
#include <sys/types.h>
#include <unistd.h>

#include <files.h>
#include <sdd.h>

int incrindex(fptr, num_entries_used)
sddfile *fptr;
int num_entries_used;

{

   int i, n, curend, newend, start, offset, fd, size, bytperent;
   sdd_index_entry tmp;
   old_sdd_index_entry oldtmp;
   sindex test[8];

   fd = fptr->fd;
   if (fd == -1) {
      return(1);
   }
   if (fptr->index == 0) return(1);

   bytperent = fptr->bs.bytperent;

/*			look at the last 8 index entries before new ones */

   curend = fptr->bs.num_entries_used;
   newend = num_entries_used;
   start = curend - 8;
   if (start < 0) start = 0;

/*			offset into file to start index entry */

   offset = start * bytperent + SDDBUFSIZE;
   if (lseek(fd, offset, SEEK_SET) != offset) {
      return(1);
   }

/*			read next 8 entries */

   for (i=0;i<8;i++) {
      size = bytperent;
      if (fptr->bs.sddversion == 1) {
/*			I*4 index */
         if ((n=read(fd, &tmp, size)) != size) {
            return(1);
         }
      } else {
/*			old I*2 index */
         if ((n=read(fd, &oldtmp, size)) != size) {
            return(1);
         }
/*			copy important values over to test[i] */
         tmp.scan = oldtmp.scan;
         tmp.start_rec = oldtmp.start_rec;
         tmp.end_rec = oldtmp.end_rec;
         tmp.obsmode = oldtmp.obsmode;
         tmp.phase_rec = oldtmp.phase_rec;
      }
/*			copy values over into the sindex test area */
      test[i].scan_number = tmp.scan;
      test[i].feed_number =(tmp.scan - test[i].scan_number) * 100. + 0.5;
      test[i].start_rec = tmp.start_rec - 1;
      test[i].nb_rec = tmp.end_rec - test[i].start_rec;
      test[i].obsmode = tmp.obsmode;
      test[i].phase_rec = tmp.phase_rec;
   }

/*	compare with values in fptr */
   for (i=0;i<8;i++) {
      if ((fptr->index[i+start].scan_number != test[i].scan_number) ||
          (fptr->index[i+start].feed_number != test[i].feed_number) ||
          (fptr->index[i+start].start_rec != test[i].start_rec) ||
          (fptr->index[i+start].nb_rec != test[i].nb_rec) ||
          (fptr->index[i+start].obsmode != test[i].obsmode) ||
          (fptr->index[i+start].phase_rec != test[i].phase_rec)) {

/*				the file has changed significantly */
               return(1);
      }
   }

/*			ok, same file, just get the new entries */

   
   for (i=curend;i<newend;i++) {
      if (fptr->bs.sddversion == 1) {
/*			I*4 index */
         if ((n=read(fd, &tmp, size)) != size) {
            return(1);
         }
      } else {
/*			old I*2 index */
         if ((n=read(fd, &oldtmp, size)) != size) {
            return(1);
         }
/*			copy important values over to test[i] */
         tmp.scan = oldtmp.scan;
         tmp.start_rec = oldtmp.start_rec;
         tmp.end_rec = oldtmp.end_rec;
         tmp.obsmode = oldtmp.obsmode;
         tmp.phase_rec = oldtmp.phase_rec;
      }
/*			copy values over into the fptr index */
      fptr->index[i].scan_number = tmp.scan;
      fptr->index[i].feed_number =
         (tmp.scan - fptr->index[i].scan_number) * 100. + 0.5;
      fptr->index[i].start_rec = tmp.start_rec - 1;
      fptr->index[i].nb_rec = tmp.end_rec - fptr->index[i].start_rec;
      fptr->index[i].obsmode = tmp.obsmode;
      fptr->index[i].phase_rec = tmp.phase_rec;
   }

   return(0);
}
