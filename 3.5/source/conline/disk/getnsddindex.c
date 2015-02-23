
/* @(#)getnsddindex.c	5.1 06/22/94 */

/* returns a pointer to nelem index entries from fptr begining at entry */
/* lrtn = 0 on successfull return, */
/*      = 1 if file not open */
/*      = 2 if a problem reading or positioning file */
/*      = 3 if a problem with the bootstrap information */
/*      = 4 if entry or nelem are not appropriate */
/* nelem is reset to the number actually returned */
/* This assumes that there are an integer number of index blocks per */
/* file record, i.e. that bytperrec / bytperent = 0 */
/* This assumption is checked as part of the bootstrap santity checks */

#include <sys/types.h>
#include <unistd.h>
#include <files.h>
#include <sdd.h>

void getnsddindex(fptr, entry, index, nelem, lrtn)
sddfile *fptr;
int *entry, *nelem, *lrtn;
sdd_index_entry *index;

{

   int i, size, offset, getbs();
   old_sdd_index_entry *oldindex;
   sdd_index_entry tmpindex;

   if (fptr == NULL || fptr->fd == -1) {
      *lrtn = 1; return;
   } 
/*			not there or just not open */
/*			getbs checks the bs on disk with that stored in */
/*			the file structure, if a difference is found, the */
/*			index is remade from scratch, if a problem happens */
/*			here, the file is screwed up or a read error occured */
   
   if ((*lrtn = getbs(fptr)) != 0) {
      *lrtn = *lrtn + 1; return;
   }
   if (*entry < 1 || *nelem < 1) {
      *lrtn = 4; return;
   }

   i = fptr->bs.num_entries_used - *entry + 1;
   if (i < 0) i = 0;
   *nelem = (*nelem > i) ? i : *nelem;

   if (*nelem) {
      size = *nelem * fptr->bs.bytperent;
      offset = (*entry - 1) * fptr->bs.bytperent + 
                         fptr->bs.bytperrec;

      if (lseek(fptr->fd, offset, SEEK_SET) != offset) {
/*				couldn't seek to start of index specified */
         *lrtn = 2; return;
      }

      if ((i = read(fptr->fd, index, size)) != size) {
/*				file read error */
         *lrtn = 2; return;
      }
/*				If this is an I*2 index, fancy footwork */
/*				This relys on the fact that most of the */
/*				index hasn't changed. */
      if (fptr->bs.sddversion == 0) {
         for (i=0;i<*nelem;i++) {
            oldindex = (old_sdd_index_entry *)&index[i];
            tmpindex.start_rec = oldindex->start_rec;
            tmpindex.end_rec = oldindex->end_rec;
            tmpindex.pos_code = oldindex->pos_code;
            index[i].start_rec = tmpindex.start_rec;
            index[i].end_rec = tmpindex.end_rec;
            index[i].pos_code = tmpindex.pos_code;
         }
      }
   }
}
