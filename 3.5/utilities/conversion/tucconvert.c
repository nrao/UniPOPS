
/* @(#)tucconvert.c	5.1 06/22/94 */

#include <malloc.h>
#include <tuc_online_sdd.h>
#define USETUC
#include <sdd.h>

void tucconvert_(buffer, feed_number, size, irtn)
char *buffer;
int *feed_number, *irtn, *size;
{
   char *sdd, *sddptr ;
   void tuc2uni();
   int tucalign();
   void tucraz_();
   int i;

   sdd = (char *)malloc (sizeof(sddformat));
   if (sdd ==0) {
      perror("Server: convert");
      *irtn = 1;
      return;
   }
   sddptr = sdd;

   tucraz_(sdd);
   if ((*irtn = tucalign((tucsddformat *) buffer, feed_number)) != 0) {
      free(sdd);
      return;
   }
   tuc2uni((tucsddformat *) buffer,(sddformat *) sdd, size, feed_number);
   
   for (i=0;i<*size;i++) {
      *(buffer++) = *(sddptr++);
   }
   free(sdd);
   *irtn = 0;
}
   
