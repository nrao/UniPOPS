
/* @(#)getascan.c	5.1 06/22/94 */

/* Routine to fetch a scan from unit and put maxsize (R*8) of it into buf */
/* flag indicate whether to fetch the scan by scan number (1) or entry (0) */
/* This is a frontend to getsddscan and is intended primarily for use by */
/* the fortran routines in unipops */
/* see the comments for getsddscan for a complete list of errors */

#include <sys/types.h>
#include <unistd.h>
#include <files.h>

void getascan_(unit, location, flag, buf, maxsize, lrtn)
int *unit, *flag, *maxsize, *lrtn;
float *location;
double *buf;

{

   sddfile *fptr, *getslot();
   void getsddscan();

   *lrtn = 0;
   if ((fptr=getslot(unit)) == 0) {
      *lrtn = 1; return;
   } 

   getsddscan(fptr, location, flag, buf, maxsize, lrtn);
   
}
