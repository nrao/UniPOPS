
/* @(#)sddopen.c	5.2 03/07/95 */

/* open an sdd file, file bs and index */
/* ier = 0 if opened ok */
/*     = 1 if unit = 0 */
/*     = 2 if file can't be found/doesn't exist */
/*     = 3 if can't access file with desired mode */
/*     = 4 if attempt to open already open file with unit > 0 */
/*     = 5 if files array is full, can't open any more files */
/*     = 6 if open failed, which is bizzare if it passed the access check */
/*     = 7 = getbs error 1, error reading or positioning file */
/*     = 8 = getbs error 2, bootstrap parameters are bizzare */

#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <string.h>
#include <files.h>

/*		The following are defined in unistd.h in 4.1 and later but */
/*		are found in sys/file.h in 4.0.3, define them here if */
/*		they have not already been defined */
/*		If one has been defined, its probably safe to assume they all */
/*		have been defined */

#ifndef F_OK
#define F_OK 0
#define X_OK 1			/* not actually used here */	
#define W_OK 2
#define R_OK 4
#endif

void sddopen_(unit,name,mode,ier)
int *unit;			/* reference number */
char *name;			/* path name of file */
char *mode;			/* "READ" or "WRIT" */
int *ier;

{
#define NCHAR 1023
   int i, flags, acmode, getbs(), offset, n0 = 0;
   char *fname;
   sddfile *fptr, *getslot();
   void sddclose_();

/* simple basic sanity check, can't open unit 0 */

   *ier = 0;
   if (*unit == 0) {
      *ier = 1;
      return;
   }

/* check on existance of file and access modes */

   fname = strdup(name);
/*	make sure the file name is null terminated */
   offset =  (strchr(fname,32) - fname);
   if (offset < 0 || offset > NCHAR) {
      offset = ((NCHAR + 1) > (int )strlen(fname)) ? 
	strlen(fname) : (NCHAR + 1);
   } 
   *(fname + offset) = 0;
   if (access(fname, F_OK) != 0) {
      *ier = 2;
      return;
   }

   if (strncmp(mode,"r ",2) == 0) {
      acmode = R_OK;
      flags = O_RDONLY;
   } else if (strncmp(mode,"w ",2) == 0) {
      acmode = W_OK;
      flags = O_WRONLY;
   } else if (strncmp(mode,"rw",2) == 0) {
      acmode = W_OK | R_OK;
      flags = O_RDWR;
   } else {
      *ier = 3;
      return;
   }

   if (access(fname, acmode) != 0) {
      *ier = 3;
       return;
   }

/*		see if there is a slot already reserved for *unit */

   fptr = getslot(unit);
   if (fptr != 0 && fptr->fd != -1) {
/*		there is, and its currently open	*/
      if (*unit > 0) {
/*		oops, but we've been told to expect it to be closed, error */
         *ier = 4;
         return;
      } else {
/*		ok, close it */
         sddclose_(unit);
      }
   } else if (fptr == 0) {
/*		not there, find an empty one */
      if ((fptr = getslot(&n0)) == 0) {
/*		none available, error */
         *ier = 5;
         return;
      }
   }

/*		OK, we have someplace to put it, and we know the file */
/*		permissions are good, so open it	*/

   fptr->fd = open(fname, flags, 0);
   if (fptr->fd < 0) {
/*		oops, couldn't open it, bizzare!  */
      *ier = 6;
   }

   fptr->unit = (*unit < 0) ? -(*unit) : *unit;
   fptr->name = strdup(fname);
   fptr->index = 0;
   fptr->bs.bytperrec = 0;

/*		get bootstrap, this will also do the index */
   if ((i = getbs(fptr)) != 0) {
      *ier = 6 + i;
/*		woops, close it */
      sddclose_(unit);
      return;
   }

   *ier = -(fptr->bs.typesdd);
/*  Return code is the negative of the file-type  */
   
}
