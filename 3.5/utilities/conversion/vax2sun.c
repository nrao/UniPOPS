
/* @(#)vax2sun.c	5.1 06/22/94 */

#include <stdio.h>

/* VAX2SUN.C -- Conversion routines for short, long, float and double types.
 * These functions convert and return a value for one storage location
 * of the named type.
 *
 * C. D. Biemesderfer, NRAO, 30 November 1989
 * double conversion fixed R.Garwood, NRAO, November 1990
 */


/* SHORTSWAP -- Byte swap a short int.
 */

short int
shortswap_ (le_short)
short int	le_short;
{
	union	{
		short	bs;
		char	bc[2];
	} bsbuf;
	char	chold;

	bsbuf.bs = le_short;
	chold = bsbuf.bc[0];
	bsbuf.bc[0] = bsbuf.bc[1];
	bsbuf.bc[1] = chold;

	return (bsbuf.bs);
}


/* LONGSWAP -- Word and byte swap a long int.
 */

long int
longswap_ (le_long)
long int	le_long;
{
	union	{
		long	wl;
		unsigned short	ws[2];
	} wsbuf;
	short	shold;

	wsbuf.wl = le_long;
	shold = wsbuf.ws[0];
	wsbuf.ws[0] = wsbuf.ws[1];
	wsbuf.ws[1] = shold;

	wsbuf.ws[0] = shortswap_ (wsbuf.ws[0]);
	wsbuf.ws[1] = shortswap_ (wsbuf.ws[1]);

	return (wsbuf.wl);
}


/* VAX2FLT -- Convert 32-bit VAX F-floating number to IEEE float.
 */

float
vax2flt_ (le_flt)
unsigned short *le_flt;
{
	union {
	    float  ff;
	    unsigned short fs[2];
	    struct {				/* VAX F-floating */
		unsigned int sign:1;
		unsigned int exponent:8;
		unsigned int mantissa:23;
	    } v;
	} vaxbuf;
       
        union {
            float ff;
            unsigned short fs[2];
            struct {
                unsigned int sign:1;
                unsigned int exponent:8;
                unsigned int mantissa:23;
            } v;
        } ieeebuf;
        int i;

        for (i=0;i<2;i++) vaxbuf.fs[i] = *(le_flt++);

	ieeebuf.fs[0] = shortswap_ (vaxbuf.fs[0]);
	ieeebuf.fs[1] = shortswap_ (vaxbuf.fs[1]);

	if (ieeebuf.v.exponent < 3) 		/* prevent underflow */
	    	ieeebuf.ff = 0.0;
	else
		ieeebuf.ff = ieeebuf.ff / 4.0;
	
	return (ieeebuf.ff);
}


/* VAX2DBL -- Convert 64-bit VAX D-floating number to IEEE double.
 */

double
vax2dbl_ (le_dbl)
unsigned short *le_dbl;
{
	union {
	    double dd;
	    unsigned short ds[4];
	    struct {				/* IEEE 64-bit floating */
		unsigned int sign:1;
		unsigned int exponent:11;
		unsigned int mantissa:20;
	    	unsigned long lomant;
	    } d;
	} ieeebuf;
	union {
	    double dd;
	    unsigned short ds[4];
	    struct {				/* VAX D-floating */
		unsigned int sign:1;
		unsigned int exponent:8;
		unsigned int mantissa:23;
	    	unsigned long lomant;
	    } v;
	} vaxbuf;
	int	i;
	
        for (i=0; i<4; i++) vaxbuf.ds[i] = *(le_dbl++);

	for (i=0; i<4; i++)
		vaxbuf.ds[i] = shortswap_ (vaxbuf.ds[i]);

	if (vaxbuf.v.exponent < 3) 		/* prevent underflow */
	    	ieeebuf.dd = 0.0;
	else {
		ieeebuf.d.sign = vaxbuf.v.sign;
		ieeebuf.d.exponent = vaxbuf.v.exponent - 127 + 1023;
						/* shift fraction right 3 */
                ieeebuf.d.lomant = vaxbuf.v.lomant >> 3;
                ieeebuf.d.lomant |= vaxbuf.v.mantissa << 29;
                ieeebuf.d.mantissa = vaxbuf.v.mantissa >> 3;
		ieeebuf.dd = ieeebuf.dd / 4.0;
	}
	
	return (ieeebuf.dd);
}
