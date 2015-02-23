
/* @(#)getnindex.c	5.1 06/22/94 */

/* returns a pointer to nelem index entries from unit begining at entry */
/* nelem is reset to the number actually returned */
/* this is a front end to getnsddindex and is intended for use by the */
/* fortran side of unipops */
/* see getnsddindex for the full list of errors */

#include <files.h>
#include <sdd.h>

void getnindex_(unit, entry, index, nelem, lrtn)
int *unit, *entry, *nelem, *lrtn;
sdd_index_entry *index;

{

   sddfile *fptr, *getslot();
   void getnsddindex();

   if ((fptr=getslot(unit)) == 0) {
      *lrtn = 1; return;
   } 

   getnsddindex(fptr, entry, index, nelem, lrtn);
}
