/*  @(#)summary.c	5.1 06/22/94
** program to print converted telescope tape
*/
#define buffsize 5120
/*
** Structure for converted telescope tape format:
**   Spectral Line Data - 140 Foot and 300 Foot
** Record length in bytes:  5120
*/
struct telescope_tape
{
	float    f_01[2];
	char     c_02[12];
	short    i_03;
	char     c_04[18];
	double   d_05;
	short    i_06[4];
	float    f_07[2];
	short    i_08[20];
	float    f_09;
	short    i_10[2];
	float    f_11[21];
	short    i_12[8];
	float    f_13[72];
	short    i_14[2];
	double   d_15[8];
	char     c_16[4][18];
	double   d_17[6];
	float    f_18[4];
	short    i_19[112];
	float    f_20[4];
	float    f_21[16];
	float    f_22[4];
	float    f_23[1024];
} Tfline;

#define readmode 0
#define openerr -1
#include <stdio.h>
#include <math.h>

main(argc,argv)
	int argc;
	char *argv[];
{
	int i, n, recno, fd;
	extern struct telescope_tape Tfline;

/* begin the program */

/* start the main body of the program */
	if (argc == 0)
	{
		printf ("Filename must be supplied\n\0");
		exit(0);
	}
	if (argc > 0 && (fd=open(argv[1],readmode)) == openerr)
	{
		printf ("Can't open %s\n", argv[1]);
		exit(0);
	}

	recno = 1;
	n = read(fd,&Tfline,buffsize);

	printf("\n\n\t--------          Index of Input Tape           --------\n\n");
	printf("Record  Scan   Off    Source     Observer  RA(1950)  \
DEC(1950)   Date     LST\n");
	printf("No      No     Scan   Name          No    hh mm ss  \
dd mm ss  mm/dd/yy hh:mm:ss\n\n");

	while (n == buffsize)
	{
		pr_summary(recno);
		recno++;
		n = read(fd,&Tfline,buffsize);
	}
	if (n == 0)
		printf("\nEnd of Input\n");
	else
		printf("I/O error reading record number %5d\n",recno);
	
/* end the program */

	recno--;
	printf("Number input records  %6d\n",recno);

	close(fd);

	exit(0);
}

pr_summary(recno)
	int recno;

{
	extern struct telescope_tape Tfline;
        int i;
	int j;

	float  scan_no;
	float  off_scan_no;
	char source[12]; 
	short obs_no;
	char obs_name[18];
	double julian_date;
	short day_of_year;
	short month;
	short day;
	short year;
	float  lst;
	float  est;
	short telescope;
	short obs_pgm;
	float  ra_1950;
	float  dec_1950;

	int ra_hh, ra_mm, ra_ss;
	int dec_dd, dec_mm, dec_ss;
	int lst_hh, lst_mm, lst_ss;
	static int lineno = 9;

	scan_no = Tfline.f_01[0];
	off_scan_no = Tfline.f_01[1];
	for(i = 0; i != 12; i++) source[i] = Tfline.c_02[i];
	obs_no = Tfline.i_03;
	for(i = 0; i != 18; i++) obs_name[i] = Tfline.c_04[i];
	julian_date = Tfline.d_05;
	day_of_year = Tfline.i_06[0];
	month = Tfline.i_06[1];
	day = Tfline.i_06[2];
	year = Tfline.i_06[3];
	lst = Tfline.f_07[0];
	est = Tfline.f_07[1];
	telescope = Tfline.i_08[0];
	obs_pgm = Tfline.i_08[1];
        ra_1950 = Tfline.f_13[8];
	dec_1950 = Tfline.f_13[12];
	if (obs_pgm == 3 || obs_pgm == 4 || obs_pgm == 8) 
	{
	    ra_1950 = Tfline.f_13[0];
	    dec_1950 = Tfline.f_13[1];
        }

	i = ra_1950 * 13750.98708 + 0.5; /* radians converted to seconds of time */
	ra_hh = i/3600;
	ra_mm = i/60 - ra_hh*60;
	ra_ss = i - ra_mm*60 - ra_hh*3600;
	i = dec_1950 * 206264.8063 + 0.5; /* radians converted to seconds of arc */
	if (i < 0)
	{
		i = -i;
		j = -1;
	}
	else
		j = 1;
	dec_dd = i/3600;
	dec_mm = i/60 - dec_dd*60;
	dec_ss = i - dec_mm*60 - dec_dd*3600;
	i = lst * 13750.98708 + 0.5; /* radians converted to seconds of time */
	lst_hh = i/3600;
	lst_mm = i/60 - lst_hh*60;
	lst_ss = i - lst_mm*60 - lst_hh*3600;

	lineno++;
	if (lineno == 51)
	{
		printf("Record  Scan   Off    Source     Observer RA(1950)  \
DEC(1950)   Date     LST\n");
		printf("No      No     Scan   Name          No    hh mm ss  \
dd mm ss  mm/dd/yy hh:mm:ss\n\n");
		lineno = 1;
	}

	printf("%-4d %8.2f %5.0f  %.12s %5d   %02d %02d %02d ",
		recno,scan_no,off_scan_no,source,obs_no,ra_hh,ra_mm,ra_ss);
	if (j < 0)
		printf("-%02d",dec_dd);
	else
		printf(" %02d",dec_dd);
	printf(" %02d %02d  %02d/%02d/%02d %02d:%02d:%02d\n",
		dec_mm,dec_ss,month,day,year,lst_hh,lst_mm,lst_ss);
}
