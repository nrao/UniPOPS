C-------------------------------------------------------------------------------
C  @(#)params.inc	1.1 09/10/98
C-------------------------------------------------------------------------------
C
C   This contains parameters used in appl.inc and elsewhere 
C   in the code which specify the size of the data array and
C   the headers.  They are placed here because there are a
C   few places which need to know these values without also
C   including appl.inc.  Changing these values does most
C   of the work, but care must also be taken to change the
C   values sdd.h and tuc_online_sdd.h.  In addition, any
C   change to MAX_DATA_POINTS must be reflect in amathparser.f
C   which contains a POPS line which is not interpreted until
C   run-time:   'amathon; for n_n = 1 to 16384'.
C   In the utility area, in fits you need to change unipops.h
C   and in the main utility area you need to change U2f_trans.
C   There are also a number of occurances in the explain
C   text.  Simply search for the current value of MAX_DATA_POINTS.
C   Finally, make sure that the RAPPLSIZE in appl.inc is large enough.
C
C                the maximum number of data points found in
C                one of the data arrays
      integer*4 MAX_DATA_POINTS 
C                the size of the HDR in terms of R*8 values
C                taking into account any padding (i.e. this
C                is after the SDD structure on disk has been
C                passed through conversion2).
      integer*4 PADDED_HDR_SIZE
C                size of padded header + data in r*4 units
      integer*4 HDU_FLOAT_SIZE
c	and actually set their values here
      parameter (PADDED_HDR_SIZE=395)
      parameter (MAX_DATA_POINTS=16384)
      parameter (HDU_FLOAT_SIZE=(PADDED_HDR_SIZE*2 + MAX_DATA_POINTS))
C
