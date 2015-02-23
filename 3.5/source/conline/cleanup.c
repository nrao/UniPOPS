
/* @(#)cleanup.c	5.1 06/22/94 */

/* workaround hack to avoid message about NaNs and Infs being written */
/* only needed for sun3 version.  May not work on anything but OS 4.1 */
/* and fortran 1.4 */

#ifdef IEEEHACK

extern int	__inf_read, __inf_written, __nan_read, __nan_written;

void cleanup_()
{
   __inf_read = 0;
   __inf_written = 0;
   __nan_read = 0;
   __nan_written = 0;
}

#else

void cleanup_()
{
}

#endif
