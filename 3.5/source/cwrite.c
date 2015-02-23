
/* @(#)cwrite.c	5.1 06/22/94 */

/* write via c calls */

#include <sys/types.h>
#include <unistd.h>

cwrite_ (fd, start, num, buf, blksize, ier)
long int *fd, *start, *num, *blksize, *ier;
char *buf;

/* fd = file descriptor, need to be converted from lun in before calling */
/* start = starting block number */
/* num = number of blocks to write */
/* blksize = blocksize in bytes */
/* buf = buffer to hold the stuff from the file */
/* ier = error number to return */
/* block numbers are 1 relative, ala fortran */


{
   off_t offset, curplace, newplace;
   int i;

/*	seek to the desired starting place */

   curplace = lseek(*fd, 0L, SEEK_CUR);
   newplace = *blksize * (*start - 1);
   offset = newplace - curplace;
   curplace = lseek(*fd, offset, SEEK_CUR);
   if (curplace != newplace) {
      *ier = 1;
      return;
   }

   for(i=0;i<*num;i++) {
      if (write(*fd, buf, *blksize) != *blksize) {
         *ier = 2;
         return;
      }
      buf = buf + *blksize;
   }

   *ier = 0;
   return;
}
