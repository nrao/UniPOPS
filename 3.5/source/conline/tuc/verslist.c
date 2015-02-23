
/* @(#)verslist.c	5.1 06/22/94 */

/* returns a list of versions of files of type "type" */

#include <tucaccess.h>
#include <string.h>
#include <unistd.h>
#include <malloc.h>
#include <dirent.h>
#include <math.h>

void verslist(type, list, maxlist)
int type, maxlist;
int *list;

{

   char *template, *pdtemp="sdd.   _", *hctemp="sdd_hc.   _";
   char *cptr, ctmp;
   char ftmp[160];
   int i, j, k, initptr, vers, templen, digit;
   int *ltmp, size, lsize, val;
   double expon, d10 = 10;
   DIR *dirp;
   struct dirent *dp;

   if (type == HCTYPE) {
      template = hctemp;
   } else if (type == FBTYPE || type == CONTYPE) {
      template = pdtemp;
   } else {
/*			unrecognized type, just return the list empty */
/*			empty and last place used in list indicated by -1 */
      *list = -1;
      return;
   }

/*			fill initials in */

   cptr = strchr(template,'.');
   for (i=0;i<3;i++) {
      *(cptr+i+1) = obsinit[i];
   }

   templen = strlen(template);

/*			search for matches in datadir */

   size = 0;
   if ((dirp = opendir(datadir)) != NULL) {
      for (dp = readdir(dirp);dp != NULL; dp = readdir(dirp)) {
         if ((strncmp(dp->d_name, template, templen)) == 0) {

/*			there MUST be 3 and only 3 characters after the _ */

            if ((strlen(dp->d_name) - templen) == 3) {
/*			make sure we can access it */
               strcpy(ftmp, datadir);
               strcat(ftmp,"/");
               strcat(ftmp,dp->d_name);
               if (access(ftmp, 4) == 0) {
/*			get the version, make sure they're digits */
                  vers = 0;
                  for (i=0;i<3;i++) {
                     ctmp = dp->d_name[templen+i];
                     if (ctmp < '0' || ctmp > '9') {
                        vers = -1;
                        break;
                     }
                     digit = ctmp - '0';
                     expon = (2-i);
                     vers = digit * pow(d10,expon) + vers;
                  }
/*			Ok, if vers is >0 and < 1000 its ok */
                  if ((vers > 0) && (vers < 1000)) {
                     *(list+size) = vers;
                     size++;
                     if (size > maxlist) break;
                  }
               }
            }
         }
      }
   }
   if (size < maxlist) *(list+size) = -1;
   if (size < 1) return;

/*			finally, sort the list in place */

    ltmp = (int *) malloc(size*sizeof(int));

    *ltmp = *list;
    lsize = 1;
    for (i=1;i<size;i++) {
       val = *(list+i);
       for (j=0;j<lsize;j++) {
          if (val > *(ltmp+j)) break;
       }
/*			val belongs at position j in tmp list */
/*			move upper half up 1 slot and insert */
       for (k=lsize;k>j;k--) {
          *(ltmp+k) = *(ltmp+k-1);
       }
       *(ltmp+j) = val;
       lsize++;
   }

/*			Now, copy it back to the list */

   for (i=0;i<size;i++) {
      *(list+i) = *(ltmp+i);
   }
   free(ltmp);
/*			and that should do it */
}
