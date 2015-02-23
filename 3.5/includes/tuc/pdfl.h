/*
** @(#)pdfl.h	5.1 06/22/94
**
** data structures usefull for tucson pdfl files
**
*/

#ifndef PDFL_H

typedef struct 	{
	short	ndir;		/* No. of blocks in directory table */
        short   maxent;		/* Max no. of entries in dir table  */
        short   bytperent;	/* bytes per dir table entry */ 
	short   nextent;	/* next available dir table entry */
	short   lastblk;	/* block of next available entry */
	short   nextblk;        /* next block location in file for data */

				/* these 6 are not used by unipops */
	short   nxtb;		/* last dir table block currently used */
	short	nbcal;		/* first block where cal data are stored */
	short	nbsv;		/* first block where save scans stored */
	short   nbwrk;		/* first block of integration work area */
	short	nbscr;		/* first block of link task scratch area */
        short	pftype;		/* POPS file type */

	char	obsname[16];	/* observer name */
	char	pid[8];		/* Project ID */

	char	binfmt_id[16];	/* Binary number format ID */

	short	ifiller[224];   /* fills it out to 512 bytes */ 	
	}	pdfl_bs_block;  /* bootstrap block */

/*	WARNING:  This gets padded out to 64 bytes !!! */

typedef struct {
	long	scan_number;	
	char	mode[4];	
	long	blk1;		/* block location of first "feed" */
	long    nblk1; 		/* number of blocks for first "feed" */
	long    blk2;		/* block location of second "feed" */
        long    nblk2;		/* number of block for second "feed" */
	char	source_name[16];
	double	velocity, rest_freq;
        short   subscan;
} pdfl_index_entry;

typedef struct {
	long	scan_number;	
	char	mode[4];	
	long	blk1;		/* block location of first "feed" */
	long    nblk1; 		/* number of blocks for first "feed" */
	long    blk2;		/* block location of second "feed" */
        long    nblk2;		/* number of block for second "feed" */
	char	source_name[16];
	double	velocity, rest_freq;
} old_pdfl_index_entry;

typedef struct {
	long	scan_number;
        }	gzfl_index_entry;   /* old, pre Sept 91 gzfl index format */

typedef struct {
	long	scan_number;
	char	type[2]; 	/* ED, edit scans, KP, keep scans */
	char	mode[2];	/* LI, line, CO, continuum */
	short	blk;
	short   nblk;		
	}	pkfl_index_entry;	

#define PDBUFSIZE sizeof(pdfl_bs_block)
#define MAXENT 2048
/*		see warning above for reason why sizeof won't work here */
#define BYTPERENT 58
#define OLD_BYTPERENT sizeof(old_pdfl_index_entry)
#define NDIR ((MAXENT * BYTPERENT) / PDBUFSIZE)
#define OLD_NDIR ((MAXENT * OLD_BYTPERENT) / PDBUFSIZE)
#define INDEX_BLOCK(ENTRY) (((ENTRY)*(BYTPERENT))/(PDBUFSIZE))
#define BLOCK_OFFSET(ENTRY)(((ENTRY)*(BYTPERENT)) % (PDBUFSIZE))
#define ISNEW(A) ((A).bs.bytperent == BYTPERENT)

typedef struct {
	int fd;		/* file descriptor */
	char name[256]; /* name of file */
	pdfl_bs_block bs; 	/* boot strap block */
	pdfl_index_entry index[MAXENT]; /* index, gains if GZFL file */
        old_pdfl_index_entry old_index[MAXENT]; /* used only for old type */
	} pdfl_file;

#define PDFL_H
#endif
