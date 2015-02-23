
/* @(#)modefn.c	5.1 06/22/94 */

/* 2 functions for using modes.h and the modes array */
/* both set *irtn to zero if successfull and to 1 if an error occured */

#include <modes.h>

void modeid_ (field, id, irtn) 

   char *field;
   short int *id, *irtn;

/*	given field (a pointer to a 4 char string) return the matching id */

{
   int i, i4 = 4;

   for (i=0;i<NUMMODES;i++) {
      if (strncmp(modes[i].field,field,i4) == 0) break;
   }

   if (i != NUMMODES) {
      *irtn = 0;
      *id = i;
   } else {
/*	Not found, set it to the null id and report an error */
      *irtn = 1;
      *id = 0;
   }
}

void modedesc_ (id, desc, irtn)

   short int *id, *irtn;
   char *desc;

/*	given an id, set *desc to the description of that mode */
/*	this is currently 32 character string, pad *desc out to */
/* 	32 characters if necessary. */

{
   int i;

   if (*id >= 0 || *id < NUMMODES) {
      *irtn = 0;
      strcpy(desc, modes[*id].description);
      for (i=strlen(desc);i<32;i++) {
         *(desc+i) = ' ';
      }
   } else {
/*	a bad id, don't change desc and report error */
      *irtn = 1;
   }
}

void modefield_(id, field, irtn)

   short int *id, *irtn;
   char *field;

/*	given an id, return the field associated with it */

{
   int i, i4 = 4;

   if (*id >= 0 || *id < NUMMODES) {
      *irtn = 0;
      strncpy(field, modes[*id].field, i4);
   } else {
/*	a bad id, set field to 0 and report error */
      strncpy(field, modes[0].field, i4);
      *irtn = 1;
   }
}
