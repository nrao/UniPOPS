
/* @(#)getbs.c	5.1 06/22/94 */

/* getbs gets the bootstrap block and checks it agains any existing values */
/* if values differ, reread the index */
/* returns 0 if everything is ok */
/*         1 if file read/positioning problems */
/*         2 if bootstrap values are bizzare */

#include <stdio.h>
#include <malloc.h>
#include <sys/types.h>
#include <unistd.h>
#include <files.h>
#include <sdd.h>

int getbs(fptr)
sddfile *fptr;

{

   sdd_bs_block *sddbs, bs;
   old_sdd_bs_block *oldsddbs;
   char cbuf[SDDBUFSIZE];
   int n, offset, makesindex();

   
/*		rewind file */
   if ((offset = lseek(fptr->fd, 0, SEEK_SET)) != 0) {
      return(1);
   }
   if ((n = read(fptr->fd, cbuf, SDDBUFSIZE)) != SDDBUFSIZE) {
      return(1);
   }
/*		is it I*2 or I*4 */
   if (((sdd_bs_block *)cbuf)->sddversion == 1) {
/*		I*4 */
      sddbs = (sdd_bs_block *)cbuf;
   } else {
/*		I*2, copy it over to sddbs */
      sddbs = &bs;
      oldsddbs = (old_sdd_bs_block *)cbuf;
      sddbs->num_index_rec = oldsddbs->num_index_rec;
      sddbs->num_data_rec = oldsddbs->num_data_rec;
      sddbs->bytperrec = oldsddbs->bytperrec;
      sddbs->bytperent = oldsddbs->bytperent;
      sddbs->num_entries_used = oldsddbs->num_entries_used;
      sddbs->counter = oldsddbs->counter;
      sddbs->typesdd = oldsddbs->typesdd;
      sddbs->sddversion = 0;
   }

   n = 0;
   if ((sddbs->num_index_rec    != fptr->bs.num_index_rec) ||
       (sddbs->num_data_rec     != fptr->bs.num_data_rec)  ||
       (sddbs->bytperrec        != fptr->bs.bytperrec)     ||
       (sddbs->bytperent        != fptr->bs.bytperent)     ||
       (sddbs->num_entries_used != fptr->bs.num_entries_used) ||
       (sddbs->counter          != fptr->bs.counter)       ||
       (sddbs->typesdd          != fptr->bs.typesdd)       ||
       (sddbs->sddversion       != fptr->bs.sddversion)) {
/*		bs block has changed or was never set, first, sanity checks */
/*		most of these are obvious, however, these routines assume */
/*		that there are an integer number of index entries per record */
/*		so, this checks that that assumption is true */
      if ((sddbs->num_index_rec <= 0) ||
          (sddbs->num_data_rec < 0) ||
          (sddbs->bytperrec <= 0) ||
          (sddbs->bytperent != sizeof(sdd_index_entry)) ||
          (sddbs->num_entries_used < 0) ||
          (sddbs->counter < 0) ||
          (sddbs->bytperrec % sddbs->bytperent)) {
         return(2);
      }

      fptr->bs.num_index_rec = sddbs->num_index_rec;
      fptr->bs.num_data_rec = sddbs->num_data_rec;
      fptr->bs.bytperrec = sddbs->bytperrec;
      fptr->bs.bytperent = sddbs->bytperent;
      fptr->bs.num_entries_used = sddbs->num_entries_used;
      fptr->bs.counter = sddbs->counter;
      fptr->bs.typesdd = sddbs->typesdd;
      fptr->bs.sddversion = sddbs->sddversion;
/*		the following is usefull to calculate now and keep around */
      fptr->nb_index_elem = 
               (sddbs->num_index_rec - 1)* sddbs->bytperrec / sddbs->bytperent;
/*		final sanity check	*/
      if (fptr->nb_index_elem < fptr->bs.num_entries_used) return(2);

      if (makesindex(fptr)) return(1);
   }
   return(n);
}
