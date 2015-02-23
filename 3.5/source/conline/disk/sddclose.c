
/* @(#)sddclose.c	5.1 06/22/94 */

/* closes an sdd file */

#include <stdio.h>
#include <malloc.h>
#include <files.h>

void sddclose_(unit)
int *unit;

{
  sddfile *fptr, *getslot();

  fptr = getslot(unit);
  if (fptr != 0 && fptr->fd != -1) {
      close(fptr->fd);
      free(fptr->index);
      fptr->index = 0;
      fptr->fd = -1;
      if (*unit > 0) fptr->unit = 0;
  } 
}
