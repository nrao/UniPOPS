
/* @(#)iounits.c	5.1 06/22/94 */
/*
 *  A set of functions for keeping track of the open fortran IO unit numbers.
 *  Unit numbers are the NUMIO consecutive integers starting with MINIO.
 *
 *  The functions (all long int = integer*4) are:
 *     ioinit_() initiallizes the io table.  Can only be called once.  Returns
 *               0 on first call and -1 if called more than once.
 *               The other 4 functions can tell if it has been called and will
 *               call it if necessary so there is no need to ever call this.
 *     ioget_()  returns lowest unused IO number and marks it as used in the 
 *               table.  Returns -1 if all numbers assigned. 
 *     ioput_(ionum) ionum is a long int, ioput resets the table entry for
 *               ionum marking it as available for use. 
 *               Returns -1 if ionum is not marked as in use and
 *                       -2 if ionum is outside the range of the table
 *     iostat_() Returns the number of available IO numbers in the table.
 *     ioset_(ionum) Marks the entry in the table for ionum as being in
 *               use (i.e. like ioget except you get to choose the number).
 *               Returns -1 if ionum is already markes as in use and
 *                       -2 if ionum is outside the range of the table.
 *  A return of -3 in the last 4 functions means that they thought it was
 *  necessary to call ioinit but ioinit returned an error.  This indicates a
 *  serious problem which should never occur.
 */

#define NUMIO 50
#define MINIO 50

static long int iotable[NUMIO];
static int isinit = -1;
   
long int ioinit_()
{
   int i, iret;
   iret = -1;
   if (isinit != 1) {
      for (i=0;i<NUMIO;iotable[i++]=-1);
      isinit = 1;
      iret = 0;
   }
   return (iret);
}

long int ioget_()
{
   long int i, ionum;

   if (isinit == -1) {
      ionum = ioinit_();
      if (ionum < 0) return(-3);
   }
   ionum = -1;

   for (i=0;i<NUMIO;i++) if (iotable[i]==-1) break;

   if ( i < NUMIO ) {
      iotable[i] = 1;
      ionum = i + MINIO;
   }

   return(ionum);
}

long int ioput_(ionum)
long int *ionum;
{
   long int i, iret;

   if (isinit == -1) {
      iret = ioinit_();
      if (iret < 0) return(-3);
   }

   i = *ionum - MINIO;
   if (i < 0 || i >= NUMIO) {
      iret = -2;
   } else if (iotable[i] == -1) {
      iret = -1;
   } else {
      iotable[i] = -1;
      iret = 0;
   }

   return(iret);
}

long int iostat_()
{
   long int i, inum;

   if (isinit == -1) {
      inum = ioinit_();
      if (inum < 0) return(-3);
   }

   inum = 0;
   for (i=0;i<NUMIO;i++) if (iotable[i]==-1) inum++;

   return(inum);
}

long int ioset_(ionum)
long int *ionum;
{
   long int i, iret;

   if (isinit == -1) {
      iret = ioinit_();
      if (iret < 0) return(-3);
   }

   i = *ionum - MINIO;
   if (i < 0 || i >= NUMIO) {
      iret = -2;
   } else if (iotable[i] != -1) {
      iret = -1;
   } else {
      iotable[i] = 1;
      iret = 0;
   }

   return(iret);
}
