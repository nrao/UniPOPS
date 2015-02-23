
/* @(#)getslist.c	5.1 06/22/94 */

/* getslist returns in rbuf the scans numbers of whats on disk */
/* it is mearly a front end for getscanlist and is intended to be used */
/* only by the fortan side of unipops */
/* actelem is set to the number of scans returned */
/* maxelem is the maximum number of elements of rbuf */
/* see getsddscanlist for the list of possible errors returned via ier */

#include <files.h>

void getslist_(unit, rbuf, actelem, maxelem, ier)
int *unit, *actelem, *maxelem, *ier;
float *rbuf;

{

   sddfile *fptr, *getslot();
   void getsddscanlist();

   if ((fptr=getslot(unit)) == 0) {
      *ier = 1;
   } else {
      getsddscanlist(fptr, rbuf, actelem, maxelem, ier);
   }
}
