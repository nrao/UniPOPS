
/* @(#)files.h	5.1 2 */
/* structure for sdd file access */

typedef struct short_index {
   int scan_number;
   int feed_number;
   int start_rec;
   int nb_rec;
   int obsmode;
   int phase_rec;
} sindex;

typedef struct short_bs {
   int num_index_rec;
   int num_data_rec;
   int bytperrec;
   int bytperent;
   int num_entries_used;
   int counter;		/* increment this when something is written to file */
			/* start over at 0 when 2**31 - 1 is reached */
   int typesdd;		/* Should be 0 for a normal SDD file and a 1 for a */
			/* Records file  */
   int sddversion;	/* 1 for I*4 BS and index, else 0 */
} sbs;


typedef struct unipops_file {
   int fd;			
   int unit;			/* reference number (old fortran unit no.) */
   char *name;			/* file name */
   sbs bs;
   sindex *index;
   int nb_index_elem;
} sddfile;
