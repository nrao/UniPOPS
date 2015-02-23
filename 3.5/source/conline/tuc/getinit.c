
/* @(#)getinit.c	5.1 06/22/94 */

/* sets the values of obsinit and datadir env variables */
/* obsinit = $OBSINIT */
/* datadir = $TOPDATA/$OBSINIT */

#include <stdlib.h>
#include <string.h>
#include <tucaccess.h>

void getinit()

{

   char *topdata;

   if (getenv("OBSINIT") != NULL) strcpy(obsinit, getenv("OBSINIT"));
   topdata = getenv("TOPDATA");

   datadir[0] = '\0';

   if (topdata != NULL && strlen(obsinit)) {
      strcpy(datadir, topdata);
      strcat(datadir,"/");
      strcat(datadir,obsinit);
   }

   if (strlen(datadir) == 0) strcpy(datadir, ".");
}
