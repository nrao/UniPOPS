
/* @(#)getsprec.c	5.1 06/22/94 */

/* Stubbed version of getsprec_() */
/*   Used during linking by utilities using lconversion that do not have */
/*   access to the online gb data */

void getsprec_(buffer, ind, phase, rec, errcode)
/*		buffer is really an sddformat struct, but isn't used here */
int *buffer, *ind, *phase, *rec, *errcode;
{
/*		just return a non-zero errcode */
   *errcode = 1;
}
