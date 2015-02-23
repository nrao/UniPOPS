/*  @(#)ieee2ascii.c	5.1 06/22/94
** program to print converted telescope tape
*/

#include <stdio.h>
#include <math.h>

/*
** Structure for converted telescope tape format:
**   Continuum Data 
** Record length in bytes:  5120
*/
typedef struct telescope_tape_cont
	{
		float    f_01[2];
		char     c_02[12];
		short    i_03;
		char     c_04[18];
		double   d_05;
		short    i_06[4];
		float    f_07[2];
		short    i_08[12];
		float    f_09;
		short    i_10[8];
		float    f_11[22];
		short    i_12[8];
		float    f_13[15];
		short    i_14[2];
		float    f_15[18];
		float    f_16[1193];
	}  Tfcont;

/*
** Structure for converted telescope tape format:
**   Spectral Line Data - 140 or 300 Foot
** Record length in bytes:  5120
*/
typedef struct telescope_tape_line
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

main(argc,argv)
	int argc;
	char *argv[];
{
	int buffsize, readbuff();
	int iline, icont;
	short	buff[2560];

/* begin the program */

/* start the main body of the program */
	icont = 1;
	iline = 1;	
	if (argc > 1) 
	{
		if (*argv[1] == 'c' | *argv[1] == 'C') iline = 0;
		if (*argv[1] == 'l' | *argv[1] == 'L') icont = 0;
	}

	buffsize = 5120;
	while (readbuff(buffsize, buff) == buffsize)
	{
		if ( (buff[33] == 1 || buff[33] == 2 || buff[33] == 5 || 
		      buff[33] == 6) && iline > 0) pr_recno_line(buff);
		if ( (buff[33] == 3 || buff[33] == 4 || buff[33] == 8)
			&& icont > 0) pr_recno_cont(buff);
	}

/* end the program */

	exit(0);
}

pr_recno_cont(tfcont)

	Tfcont	*tfcont;

{
	int i;

	/* scan number, sub scan number */
	printf("%#15.7E%#15.7E\n",(*tfcont).f_01[0],(*tfcont).f_01[1]);

	/* source */
	for (i = 0; i < 12; i++)
		printf("%c",(*tfcont).c_02[i]);
	printf("\n");

	/* observer number */
	printf("%#6d\n",(*tfcont).i_03);

	/* observer name */
	for (i = 0; i < 18; i++)
		printf("%c",(*tfcont).c_04[i]);
	printf("\n");

	/* Julian date */
	printf("%#20.12E\n",(*tfcont).d_05);
	
	/* Solar day of year, Month, Day, Year */
	printf("%#6d%#6d%#6d%#6d\n",(*tfcont).i_06[0],
		(*tfcont).i_06[1],(*tfcont).i_06[2],(*tfcont).i_06[3]);

	/* LST, EST */
	printf("%#15.7E%#15.7E\n",(*tfcont).f_07[0],(*tfcont).f_07[1]);

	/* telescope, etc. */
	for (i = 0; i < 12; i++)
	{
		printf("%#6d",(*tfcont).i_08[i]);
	}
	printf("\n");

	/* DCR Cal factor */
	printf("%#15.7E\n",(*tfcont).f_09);

	/* VREF, VDEF, etc. */
	for (i = 0; i < 8; i++)
	{
		printf("%#6d",(*tfcont).i_10[i]);
	}
	printf("\n");

	/* DCR System Temperature, etc. */
	for (i = 0; i < 22; i++)
	{
		printf("%#15.7E",(*tfcont).f_11[i]);
		if (((i + 1) % 5) == 0)
			printf("\n");
	}
	printf("\n");

	/* environment values */
	for (i = 0; i < 8; i++)
		printf("%#6d",(*tfcont).i_12[i]);
	printf("\n");

	/* Epoch RA, Apparent RA, etc. */
	for (i = 0; i < 15; i++)
	{
		printf("%#15.7E",(*tfcont).f_13[i]);
		if (((i + 1) % 5) == 0)
			printf("\n");
	}

	/* unused, # scans ACCUM'ed */
	printf("%#6d%#6d\n",(*tfcont).i_14[0],(*tfcont).i_14[1]);

	/* ACCUM stack */
        for (i = 0; i < 18; i++)
	{
		printf("%#15.7E",(*tfcont).f_15[i]);
		if (((i + 1) % 5) == 0)
			printf("\n");
	}
	printf("\n");

	/* data points */
	for (i = 0; i < 600; i++)
	{
		printf("%#12.4E",(*tfcont).f_16[i]);
		if (((i + 1) % 6) == 0)
			printf("\n");
	}
	printf("\n");
	printf("SCAN ###################################\n");
}
pr_recno_line(tfline)

	Tfline	*tfline;
	
{
	int i;
	int j;

	/* scan number, off scan number */
	printf("%#15.7E%#15.7E\n",(*tfline).f_01[0],(*tfline).f_01[1]);

	/* source */
	for (i = 0; i < 12; i++)
		printf("%c",(*tfline).c_02[i]);
	printf("\n");

	/* observer number */
	printf("%#6d\n",(*tfline).i_03);

	/* observer name */
	for (i = 0; i < 18; i++)
		printf("%c",(*tfline).c_04[i]);
	printf("\n");

	/* Julian date */
	printf("%#20.12E\n",(*tfline).d_05);
	
	/* Solar day of year, Month, Day, Year */
	printf("%#6d%#6d%#6d%#6d\n",(*tfline).i_06[0],
		(*tfline).i_06[1],(*tfline).i_06[2],(*tfline).i_06[3]);

	/* LST, EST */
	printf("%#15.7E%#15.7E\n",(*tfline).f_07[0],(*tfline).f_07[1]);

	/* telescope, etc. */
	for (i = 0; i < 20; i++)
	{
		printf("%#6d",(*tfline).i_08[i]);
		if ((i + 1) == 7)
			printf("\n");
		if ((i + 1) == 15)
			printf("\n");
		if ((i + 1) == 20)
			printf("\n");
	}

	/* scan time */
	printf("%#15.7E\n",(*tfline).f_09);

	/* statuc word 1-2 */
	printf("%#6d%#6d\n",(*tfline).i_10[0],(*tfline).i_10[1]);

	/* integration time, etc. */
	for (i = 0; i < 21; i++)
	{
		printf("%#15.7E",(*tfline).f_11[i]);
		if ((i + 1) == 4)
			printf("\n");
		if ((i + 1) == 8)
			printf("\n");
		if ((i + 1) == 11)
			printf("\n");
		if ((i + 1) == 14)
			printf("\n");
		if ((i + 1) == 19)
			printf("\n");
		if ((i + 1) == 21)
			printf("\n");
	}

	/* environment values */
	for (i = 0; i < 8; i++)
		printf("%#6d",(*tfline).i_12[i]);
	printf("\n");

	/* Apparent RA, etc. */
	for (i = 0; i < 72; i++)
	{
		printf("%#15.7E",(*tfline).f_13[i]);
		if (((i + 1) % 4) == 0)
			printf("\n");
	}

	/* unused */
	printf("%#6d%#6d\n",(*tfline).i_14[0],(*tfline).i_14[1]);

	/* center freq, etc */
	for (i = 0; i < 8; i++)
	{
		printf("%#20.12E",(*tfline).d_15[i]);
		if (((i + 1) % 4) == 0)
			printf("\n");
	}

	/* C-F formula */
	for (i = 0; i < 4; i++)
	{
		for (j = 0; j < 18; j++)
			printf("%c",(*tfline).c_16[i][j]);
		printf("\n");
	}

	/* L1, etc. */
	for (i = 0; i < 6; i++)
	{
		printf("%#20.12E",(*tfline).d_17[i]);
		if (((i + 1) % 3) == 0)
			printf("\n");
	}

	/* LA, etc. */
	for (i = 0; i < 4; i++)
		printf("%#15.7E",(*tfline).f_18[i]);
	printf("\n");

	/* A/C words, unused */
	for (i = 0; i < 112; i++)
	{
		printf("%#6d",(*tfline).i_19[i]);
		if (((i + 1) % 10) == 0)
			printf("\n");
	}
	printf("\n");

	/* ref system temp */
	for (i = 0; i < 4; i++)
		printf("%#15.7E",(*tfline).f_20[i]);
	printf("\n");

	/* power counters */
	for (i = 0; i < 16; i++)
	{
		printf("%#15.7E",(*tfline).f_21[i]);
		if (((i + 1) % 4) == 0)
			printf("\n");
	}

	/* channel zero values */
	for (i = 0; i < 4; i++)
		printf("%#15.7E",(*tfline).f_22[i]);
	printf("\n");

	/* spectrum - signal Only convert the number of channels */
 	/* specific to the telescope */
        if ((*tfline).i_08[0] == 140)
	{
	for (i = 0; i < 1024; i++)
	{
		printf("%#15.7E",(*tfline).f_23[i]);
		if (((i + 1) % 5) == 0)
			printf("\n");
	}
	}
        if ((*tfline).i_08[0] == 300)
	{
	for (i = 0; i < 384; i++)
	{
		printf("%#15.7E",(*tfline).f_23[i]);
		if (((i + 1) % 5) == 0)
			printf("\n");
	}
	}
		printf("\n");
	printf("SCAN ###################################\n");
}


#include <stdio.h>

/*	BUFSIZ is set in stdio.h 	*/

int readbuff(igoal, buffer)

int	igoal;
char	*buffer;

{
	int	iread, n;

	iread = 0;
	while (igoal > 0) {
		if ((n = read(0,buffer+iread, 
			(igoal > BUFSIZ) ? BUFSIZ: igoal)) > 0) {
			iread = iread + n;
			igoal = igoal - n;
		}
		else	{
			iread = n;
			igoal = 0;
		}
	}
	return iread;
}

