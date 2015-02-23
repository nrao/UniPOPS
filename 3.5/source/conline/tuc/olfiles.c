
/* @(#)olfiles.c	5.2 07/08/94 */
/* simply displays the status of the online files */

#include <stdio.h>
#include <string.h>
#include <tucaccess.h>

void olfiles_()
{


   int i;
   char *name;

   printf("\nCurrently open 12-m online data files :\n");
   printf("   Directory : %s\n",datadir);
   if (filetype != CONTYPE) {
/*		for now, filetype can only = CONTYPE if condar is running */

      for (i=0;i<2;i++) {
         name = onfile[i].datafile.name + strlen(datadir) + 1;
         if (onfile[i].data_status == ST_OK) printf("      %s\n",name);
         name = onfile[i].gainfile.name + strlen(datadir) + 1;
         if (onfile[i].gain_status == ST_OK) printf("      %s\n",name);
      }
   } else {
      name = onfile[CONTYPE-1].datafile.name + strlen(datadir) + 1;
      if (onfile[CONTYPE-1].data_status == ST_OK) printf("      %s\n",name);
   }

   printf("\n To change the online files, use the CHNGVER or CHNGONLINE verbs.\n");
   return;
}
