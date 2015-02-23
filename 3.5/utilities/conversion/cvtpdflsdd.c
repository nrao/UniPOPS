
/* @(#)cvtpdflsdd.c	5.3 07/08/94 */

/* converts pdfl files into sdd format files */

#include <stdio.h>
#include <malloc.h>
#ifdef SOLARIS
# include <string.h>
#endif

#define USETUC
#include <pdfl.h>
#include <sdd.h>
#include <tuc_online_sdd.h>

/* This program reads the pdfl file and writes the sdd records */
/* to stdout.  It will be necessary to run "makeindex.exe" over the resulting */
/* file to appropriately index it.  The index information contained in the */
/* pdfl file is totally ignored.  The bootstrap information is not. */
/* This routine will work on pdfl and gzfl files.  */
/* It may also work on pkfl files.  */
/* PDFL files can hold up to 2048 * 2 separate scans in their index. */
/* The value of nextent in the bootstrap block will determine */
/* the size of the PDFL file to create.  Enough space will be reserved so */
/* that the nearest multiple of 1024 larger than nextent * 2 scans */
/* can be indexed.  If the user wishes the outgoing file to be larger */
/* an optional parameter equal to the number of index entries to reserve */
/* in the sdd file can be specified.  */

/* useage is therefore :  cvtpdflsdd pdfl_file [size] > sdd_file */

main(argc, argv)
int argc;
char **argv; 

{
   int size = 1024;
   int i, j, n, numout, ndbl,isize;
   int totlen, ndir, extra, nchan, feed, irtn, c3nrch;
   long int offset, curplace, newplace;
   double hdrlen, datalen, norchan, *backend;
   pdfl_bs_block pdflbs;
   pdfl_index_entry index[MAXENT];
   old_pdfl_index_entry old_index[MAXENT];
   pkfl_index_entry pkindex[MAXENT];
   int indsize;
   FILE *infile;
   sdd_bs_block sddbs;
#define FULL_ARRAY_SIZE ((sizeof(sddformat)/SDDBUFSIZE + 1) * SDDBUFSIZE)
   double inbuf[FULL_ARRAY_SIZE/sizeof(double)];
   double outbuf[FULL_ARRAY_SIZE/sizeof(double)];
   short int shortswap_();
   double vax2dbl_();
   char nullc = '\0';
   void tucconvert_();
   union {
      short class_0[16];
      double d[4];
   } u;

/*	see if a size is specified on the command line */

   if (argc <= 1 || argc > 3) {
      fprintf(stderr,"cvtpdflsdd pdfl_file [size] \n"); 
      fprintf(stderr,"  where size is an optional integer number of index\n");
      fprintf(stderr,"  entries to create in the output stream.\n");
      fprintf(stderr,"  The default size is 1024\n");
      fprintf(stderr,"  The sdd file is written to stdout.\n");
      exit(1);
   } else {
      infile = fopen(*++argv, "r");
      if (infile == NULL) {
         fprintf(stderr,"cvtpdflsdd: could not open file %s\n", *argv);
         exit(1);
      } 
   }

   if (argc == 3) {
      if (sscanf(*++argv,"%i", &size) != 1) {
         fprintf(stderr,"cvtpdflsdd: invalid argument %s\n", *argv);
         exit(1);
      }
      if (size < 0 ) {
         fprintf(stderr,"size must be greater than 0\n");
         exit(1);
      }
/*		conver size to correspond to an integer number of */
/*		SDDBUFSIZE records */
/*		This assumes that there are an integer number of */
/*		index entries in each record */
      size = size * sizeof(sdd_index_entry);
      size = (size % SDDBUFSIZE) ? (size/SDDBUFSIZE + 1) : (size/SDDBUFSIZE);
      size = size * SDDBUFSIZE / sizeof(sdd_index_entry);
   }

/* read in and translate the pdfl bootstrap block */

   if ((n = fread(&pdflbs, sizeof(char), PDBUFSIZE, infile)) != PDBUFSIZE) {
      if (n == 0) perror("cvtpdflsdd :");
      fprintf(stderr,"Bad read of pdfl bs block n = %i\n",n);
      exit(1);
   }
   pdflbs.ndir = shortswap_(pdflbs.ndir);
   pdflbs.maxent = shortswap_(pdflbs.maxent);
   pdflbs.nextent = shortswap_(pdflbs.nextent);
   pdflbs.bytperent = shortswap_(pdflbs.bytperent);

/* minimal sanity check, make sure that nextent is not > maxent + 1 */
/* and that both are positive as is ndir */

   if (pdflbs.ndir < 0 || pdflbs.nextent < 0 || pdflbs.maxent < 0 ||
       pdflbs.nextent > pdflbs.maxent + 1) {
      fprintf(stderr,"Bad parameters found in bootstrap block of pdfl file.\n");
      fprintf(stderr,"  ndir = %i\n", pdflbs.ndir);
      fprintf(stderr,"  maxent = %i\n", pdflbs.maxent);
      fprintf(stderr,"  nextent = %i\n", pdflbs.nextent);
      exit(1);
   }

/* make up the empty sdd bs block and write it to stdout followed by enough */
/* null characters for size index entries */

   sddbs.num_data_rec = 0;
   sddbs.bytperrec = SDDBUFSIZE;
   sddbs.bytperent = sizeof(sdd_index_entry);
   sddbs.num_index_rec = size * sddbs.bytperent / SDDBUFSIZE + 1;
   sddbs.num_entries_used = 0;
   sddbs.counter = 0;
   sddbs.typesdd = 0;
   sddbs.sddversion = 1;
   for (i=0;i<BSZERO;i++) sddbs.zeros[i] = 0;

   if ((n = fwrite(&sddbs, sizeof(char), SDDBUFSIZE, stdout)) != SDDBUFSIZE) {
      if (n == 0) perror("cvtpdflsdd :");
      fprintf(stderr,"Problem writting sdd bootstrap block to stdout.");
      fprintf(stderr," n = %i\n", n);
      exit(1);
   }

   size = size * sddbs.bytperent;
   for (i=0;i<size;i++) {
      if ((n = fwrite(&nullc, sizeof(char), 1, stdout)) != 1) {
         if (n == 0) perror("cvtpdflsdd :");
         fprintf(stderr,"Problem writting empty sdd index to stdout.");
         fprintf(stderr," n = %i\n", n);
         exit(1);
      }
   }

/*	read index and translate important parts (block locations) */

   pdflbs.maxent = (pdflbs.maxent > MAXENT) ? MAXENT : pdflbs.maxent;

   if (pdflbs.bytperent == BYTPERENT) {
      indsize = pdflbs.bytperent;
      for (i=0;i<pdflbs.maxent;i++) {
         if ((n = fread(&(index[i]), sizeof(char), indsize, infile)) 
                    != indsize) {
            if (n == 0) perror("cvtpdflsdd :");
            fprintf(stderr,"Problem reading index\n");
            exit(1);
         }
      }

      for (i=0;i<(pdflbs.nextent-1);i++) {
         index[i].scan_number = longswap_(index[i].scan_number);
         index[i].blk1 = longswap_(index[i].blk1);
         index[i].blk2 = longswap_(index[i].blk2);
         index[i].nblk1 = longswap_(index[i].nblk1);
         index[i].nblk2 = longswap_(index[i].nblk2);
         index[i].subscan = shortswap_(index[i].subscan);
      }
   } else if (pdflbs.bytperent == sizeof(old_pdfl_index_entry)) {
      indsize = pdflbs.maxent * pdflbs.bytperent;
      if ((n = fread(old_index, sizeof(char), indsize, infile)) != indsize) {
         if (n == 0) perror("cvtpdflsdd :");
         fprintf(stderr,"Problem reading old style index\n");
         exit(1);
      }

      for (i=0;i<(pdflbs.nextent-1);i++) {
         index[i].scan_number = longswap_(old_index[i].scan_number);
         index[i].blk1 = longswap_(old_index[i].blk1);
         index[i].blk2 = longswap_(old_index[i].blk2);
         index[i].nblk1 = longswap_(old_index[i].nblk1);
         index[i].nblk2 = longswap_(old_index[i].nblk2);
         index[i].subscan = 0;
      }
   } else if (pdflbs.bytperent == sizeof(pkfl_index_entry)) {
      indsize = pdflbs.maxent * pdflbs.bytperent;
      if ((n = fread(pkindex, sizeof(char), indsize, infile)) != indsize) {
         if (n == 0) perror("cvtpdflsdd :");
         fprintf(stderr,"Problem reading index\n");
         exit(1);
      }
      for (i=0;i<(pdflbs.nextent-1);i++) {
         index[i].scan_number = longswap_(pkindex[i].scan_number);
         index[i].blk1 = shortswap_(pkindex[i].blk);
         index[i].nblk1 = shortswap_(pkindex[i].nblk);
         index[i].blk2 = 0;
         index[i].nblk2 = 0;
         index[i].subscan = 0;
      }
   } else {
      fprintf(stderr,"Unrecognized index format in input file\n");
      exit(1);
   }


/* loop through index */

   size = size / sddbs.bytperent;
   numout = 0;

/* begin converting it */

   fprintf(stderr,"  Begin conversion of %i index entries.\n", 
                  (pdflbs.nextent-1));

   for (i=0;i<(pdflbs.nextent-1);i++) {

      if (index[i].blk1 > 0) {

/*	seek to blk1 */

         curplace = ftell(infile);
         newplace = PDBUFSIZE * (index[i].blk1 - 1);
         offset = newplace - curplace;
         if (fseek(infile, offset, 1) < 0) {
          fprintf(stderr,"Error finding start of data indexed by entry %i\n",i);
          fprintf(stderr,"   scan %i, start at block %i\n",
                          index[i].scan_number, index[i].blk1);
          fprintf(stderr,"   attempting to skip this one\n");
          continue;
         }

/*	read it  */

         isize = PDBUFSIZE * index[i].nblk1;
         if (isize > sizeof(inbuf)) {
            fprintf(stderr,"Scan number %i, subscan %i, is too large, ",
                           index[i].scan_number, index[i].subscan);
            fprintf(stderr,"it will be truncated at %i data points\n", 
                           MAX_DATA_POINTS);
            isize = sizeof(inbuf);
         }
         if ((n = fread(inbuf, sizeof(char), isize, infile)) != isize) {
            fprintf(stderr," Bad read of indexed data entry %i\n",i);
            fprintf(stderr," Read returned %i bytes instead of %i",n,isize);
            fprintf(stderr," Scan number %i\n", index[i].scan_number);
            fprintf(stderr," Subscan number %\n", index[i].subscan);
            fprintf(stderr," skipping\n");
            continue;
         }

/*	Ok, now convert it  */

         u.d[0] = inbuf[0];
         c3nrch = shortswap_(u.class_0[3]) + 3;
         norchan = vax2dbl_(&(inbuf[c3nrch - 1]));
         nchan = norchan;
         if (nchan <= 0) nchan = 1;

         for (j=0;j<nchan;j++) {

/*	copy inbuf to outbuf  */

#ifdef SOLARIS
            memcpy(outbuf, inbuf, FULL_ARRAY_SIZE);
#else
            bcopy(inbuf, outbuf, FULL_ARRAY_SIZE);
#endif

            if (index[i].subscan != 0) {
/*			it's the new index, no need to be fancy here */
               feed = index[i].subscan + j;
            } else {
/*			is this a HySpec data set */ 
               backend = outbuf + 16;
               if (strncmp((char *)backend, "HySpec", 6) == 0) {
   	          feed = j + 11;
               } else {
                  feed = j + 1;
               }
            }

            size = FULL_ARRAY_SIZE;
            tucconvert_(outbuf, &feed, &size, &irtn);

            if (irtn != 0) {
               fprintf(stderr,"Error during conversion\n");
               fprintf(stderr,"Will attempt to go on\n");
               break;
            }

/*	write it to stdout  */

            hdrlen  = outbuf[4];
            datalen = outbuf[5];
            totlen = hdrlen + datalen;
            ndir = ((totlen % SDDBUFSIZE) == 0) ? totlen / SDDBUFSIZE :
                                                  totlen / SDDBUFSIZE + 1;
 
            if ((n= fwrite(outbuf, sizeof(char), ndir*SDDBUFSIZE, stdout)) !=
                       ndir*SDDBUFSIZE) {
               fprintf(stderr,"Problems writing to scan to stdout\n");
               fprintf(stderr,"After %i scans successfully converted", numout);
               fprintf(stderr,"Write of %i bytes expected, only wrote %i bytes",
                         ndir*SDDBUFSIZE, n);
               exit(1);
            }

            numout++;
         }
      }

      if (index[i].blk2 > 0) {

/*	seek to blk1 */

         curplace = ftell(infile);
         newplace = PDBUFSIZE * (index[i].blk2 - 1);
         offset = newplace - curplace;
         if (fseek(infile, offset, 1) < 0) {
          fprintf(stderr,"Error finding start of data indexed by entry %i\n",i);
          fprintf(stderr,"   scan %i, start at block %i\n",
                          (index[i].scan_number+1), index[i].blk2);
          fprintf(stderr,"   attempting to skip this one\n");
          continue;
         }

/*	read it  */

         isize = PDBUFSIZE * index[i].nblk2;
         if (isize > sizeof(inbuf)) {
            fprintf(stderr,"Scan number %i, is too large, ",
                           index[i].scan_number);
            fprintf(stderr,"it will be truncated at %i data points\n", 			  MAX_DATA_POINTS);
            isize = sizeof(inbuf);
         }
         if ((n = fread(inbuf, sizeof(char), isize, infile)) != isize) {
            fprintf(stderr," Bad read of indexed data entry %i\n",i);
            fprintf(stderr," Read returned %i bytes instead of %i",n,isize);
            fprintf(stderr," Scan number %i\n", (index[i].scan_number+1));
            fprintf(stderr," skipping\n");
            continue;
         }

/*	Ok, now convert it  */

         u.d[0] = inbuf[0];
         c3nrch = shortswap_(u.class_0[3]) + 3;
         norchan = vax2dbl_(&(inbuf[c3nrch - 1]));
         nchan = norchan;
         if (nchan <= 0) nchan = 1;

         for (j=0;j<nchan;j++) {

/*	copy inbuf to outbuf  */

#ifdef SOLARIS
            memcpy(outbuf, inbuf, FULL_ARRAY_SIZE);
#else
            bcopy(inbuf, outbuf, FULL_ARRAY_SIZE);
#endif
            feed = j + 1;
            size = FULL_ARRAY_SIZE;
            tucconvert_(outbuf, &feed, &size, &irtn);

            if (irtn != 0) {
               fprintf(stderr,"Error during conversion\n");
               fprintf(stderr,"Will attempt to go on\n");
               break;
            }

/*	write it to stdout  */

            hdrlen  = outbuf[4];
            datalen = outbuf[5];
            totlen = hdrlen + datalen;
            ndir = ((totlen % SDDBUFSIZE) == 0) ? totlen / SDDBUFSIZE :
                                                  totlen / SDDBUFSIZE + 1;
 
            if ((n= fwrite(outbuf, sizeof(char), ndir*SDDBUFSIZE, stdout)) !=
                       ndir*SDDBUFSIZE) {
               fprintf(stderr,"Problems writing scan to stdout\n");
               fprintf(stderr,"After %i scans successfully converted", numout);
               fprintf(stderr,"Write of %i bytes expected, only wrote %i bytes",
                         ndir*SDDBUFSIZE, n);
               exit(1);
            }

            numout++;
         }
      }
   }

/*	all finished	*/

   fprintf(stderr,"   Finished ... %i scans written.\n", numout);
   exit(0);
}
