
/* @(#)gnextindex.c	5.1 06/22/94 */

/* gnextindex_ sets feed_number in index (1st i*2 value) as follows :
 *      -100 if a problem occured
 *      -(feed_number) if feed_number exists but is identical to prev index
 *      +(feed_number) if index has changed or first call
 *   first call is assumed if feed_number is initially zero
 *
 *  idata : the data, in unpadded sdd format (returned via gshead or gchead) 
 *  index : the index to be filled from the info in idata
 */

#include <math.h>
#include <sdd.h>

struct in {
      short feed_number;
      short dummy[3];
      float sourcex;
      float sourcey;
      char source_name[16];
      float scan_number;
      float freq_resolution;
      double rest_freq;
      float lst;
      float ut_date;
      short obsmode, zero1, position_code, zero2;
   };

void gnextindex_(idata, index)
   char *idata;
   struct in *index;

{
   int i;
   struct in test;
   int site, irtn4, i8 = 8, ifeed, i4 = 4, iscan;
   double temp, temp2;
   short irtn;
   sddformat tmpdata;
#define SDD (tmpdata.header)
   int onlinesite_();
   char csc[8];
   short int id;
   void modeid_();

   site = onlinesite_();

/*	verify that site is either gb (0) or tuc (1)   */

   if (site < 0 || site > 1) {
      index->feed_number = -100;
      return;
   }

   if (index->feed_number < 0) index->feed_number = - index->feed_number;
   ifeed = index->feed_number;
#ifdef CONTINUUM
#else
   if (site == 0 && ifeed != 0) {
/*		green bank online data		*/
/*		If this is not the first pass (i.e. feed_number is */
/*		non-zero, we need to do a gshead for line data to get */
/*		the NEXT feed number if available */
      index->feed_number++;
/*		the max feed is 4 */
      if (index->feed_number > 4) {
         index->feed_number = -100;
         return;
      }
      ifeed = index->feed_number;
      iscan = index->scan_number + 0.5;
      gshead_(&iscan, &ifeed, idata, &irtn4);
      if (irtn4 != 0) {
         index->feed_number = -100;
         return;
      }
   } 
#endif
/*    convert the data to the internal, padded SDD format */
   raz_(&tmpdata);
   conversion2_(idata, &tmpdata, &irtn);
   if (irtn !=  0) {
      index->feed_number = -100;
      return;
   }

/*	if feed_number is the default first feed number, just load the index */

   if (ifeed == deffeed_() || ifeed == 0) {
      index->position_code = -1;
      strncpy(csc,SDD.coord_sys_code,i8);
      if (strncmp(csc,"GALACTIC",i8) == 0) index->position_code=1;
      if (strncmp(csc,"1950RADC",i8) == 0) index->position_code=2;
      if (strncmp(csc,"EPOCRADC",i8) == 0) index->position_code=3;
      if (strncmp(csc,"MEANRADC",i8) == 0) index->position_code=4;
      if (strncmp(csc,"APPRADC ",i8) == 0) index->position_code=5;
      if (strncmp(csc,"APPHADC ",i8) == 0) index->position_code=6;
      if (strncmp(csc,"1950ECL ",i8) == 0) index->position_code=7;
      if (strncmp(csc,"EPOCECL ",i8) == 0) index->position_code=8;
      if (strncmp(csc,"MEANECL ",i8) == 0) index->position_code=9;
      if (strncmp(csc,"APPECL  ",i8) == 0) index->position_code=10;
      if (strncmp(csc,"AZEL    ",i8) == 0) index->position_code=11;
      if (strncmp(csc,"USERDEF ",i8) == 0) index->position_code=12;
      if (strncmp(csc,"2000RADC",i8) == 0) index->position_code=13;
      if (strncmp(csc,"INDRADC ",i8) == 0) index->position_code=14;
      if (strncmp(csc,"        ",i8) == 0) index->position_code=2;
      if (index->position_code < 0) {
         index->feed_number = -100;
         return;
      }
      index->sourcex = SDD.sourcex;
      index->sourcey = SDD.sourcey;
      for (i=0;i<16;i++) index->source_name[i] = SDD.source_name[i];
      index->scan_number = SDD.scan_number;
      iscan = index->scan_number + 0.5;
      if (ifeed == 0) {
         index->feed_number = (SDD.scan_number - iscan)*100.0 + 0.5;
      }
      index->lst = SDD.lst;
      index->ut_date = SDD.ut_date;

      if (strncmp(SDD.type_data, "CONT", i4) == 0) {
         index->obsmode = 256;
         temp = SDD.deltax_rate * cos(SDD.sourcey * M_PI / 180.0);
         temp2 = SDD.deltay_rate;
         index->freq_resolution = hypot(temp, temp2);
         index->rest_freq = SDD.length_sample;
      } else if (strncmp(SDD.type_data, "LINE", i4) == 0) {
         index->obsmode = 512;
         index->freq_resolution = fabs(SDD.freq_resolution);
         index->rest_freq = SDD.rest_freq;
      } else {
         index->obsmode = 0;
         index->freq_resolution = fabs(SDD.freq_resolution);
         index->rest_freq = SDD.rest_freq;
      }

      modeid_(((SDD.type_data)+4), &id, &irtn);
      index->obsmode = index->obsmode + id;
   } else {

/*	put feed dependant info into a test index */
/*	in tucson, only rest_freq and freq_resolution are liable to change */
/*	but put sourcex and sourcey in for compatability with gb side */

      test.sourcex = SDD.sourcex;
      test.sourcey = SDD.sourcey;
      if (strncmp(SDD.type_data, "CONT", i4) == 0) {
         temp = SDD.deltax_rate * cos(SDD.sourcey * M_PI / 180.0);
         temp2 = SDD.deltay_rate;
         test.freq_resolution = hypot(temp, temp2);
         test.rest_freq = SDD.length_sample;
      } else if (strncmp(SDD.type_data, "LINE", i4) == 0) {
         test.freq_resolution = fabs(SDD.freq_resolution);
         test.rest_freq = SDD.rest_freq;
      } else {
         test.freq_resolution = fabs(SDD.freq_resolution);
         test.rest_freq = SDD.rest_freq;
      }

/*	ok, now check for any differences   */


      if (test.sourcex != index->sourcex || 
          test.sourcey != index->sourcey ||
          test.freq_resolution != index->freq_resolution ||
          test.rest_freq != index->rest_freq) {

         index->sourcex = test.sourcex;
         index->sourcey = test.sourcey;
         index->freq_resolution = test.freq_resolution;
         index->rest_freq = test.rest_freq;
      } else {
/*			else this is the same as the last time */
         index->feed_number = -(index->feed_number);
      }
   }
   return;
}
