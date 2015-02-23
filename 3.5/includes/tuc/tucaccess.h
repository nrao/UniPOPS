
/* @(#)tucaccess.h	5.1 06/22/94 */

#ifndef TUCACCESS_H

#include <files.h>

typedef struct {
   sddfile datafile;
   sddfile gainfile;
   int data_status;
   int gain_status;
   int version;
   char type[8];
} tucfile;

tucfile onfile[3];

char obsinit[6], datadir[160];

int filetype;


#define ST_OK 0			/* the file is fine */
#define ST_UNAVAIL -1		/* the file is not be opened */

/*			these match the values in POPSDAT.ADVERBS */

#define HCTYPE 1
#define FBTYPE 2
#define CONTYPE 3

static struct {
   char *c;
} ftype[3] = {
   "HCTYPE",
   "FBTYPE",
   "CONTYPE"
};

#define SDDFMT 0
#define IEEEFMT 1

typedef struct {
   double def_val;
   char source_name[16];
   char obs_mode[8];
   double scan_min, scan_max;
   double feed_min, feed_max;
   double x_min, x_max;
   double y_min, y_max;
   double lst_min, lst_max;
   double ut_min, ut_max;
   double bw_min, bw_max;
   double f_min, f_max;
   double rate_min, rate_max;
   double int_min, int_max;
} sel_params;

#define TUCACCESS_H
#endif
