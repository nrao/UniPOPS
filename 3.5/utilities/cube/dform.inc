c
c     @(#)dform.inc	5.1 06/22/94
c
C       Class 1 :  Basic Information
C       Class 2 :  Pointing Parameters
C       Class 3 :  Observing Parameters
C       Class 4 :  Positions
C       Class 5 :  Environment
C       Class 6 :  Map Parameters
C       Class 7 :  Data Parameters
C       Class 8 :  Engineering Parameters
C       Class 9 :  Telescope Dependent Parameters
C       Class 10:  Open Parameters
C       Class 11:  Phase Block
C       Class 12:  Receiver Descriptor Block
C       Class 13:  Reduced Data Values
C
C       C pointers for the currently referenced array
C
      common/dform/c1sno,c1tel,c1ona,c1stc,c3dat,c4epo,c4sx,c4sy,c4era,
     .             c4edc,c4gl,c4gb,c4az,c4el,c4ix,c4iy,c4csc,c7osn,
     .             c12spn,c12ni,c12x0,c12dx,c12rp,idatoff
c
      integer c1sno,c1tel,c1ona,c1stc,c3dat,c4epo,c4sx,c4sy,c4era,c4edc,
     .        c4gl,c4gb,c4az,c4el,c4ix,c4iy,c4csc,c7osn,c12spn,c12ni,
     .        c12x0,c12dx,c12rp,idatoff
c
      integer d1sno,d1tel,d1ona,d1stc,d3dat,d4epo,d4sx,d4sy,d4era,d4edc,
     .        d4gl,d4gb,d4az,d4el,d4ix,d4iy,d4csc,d7osn,d12spn,d12ni,
     .        d12x0,d12dx,d12rp
c
      data d1sno/2/,d1tel/6/,d1ona/8/,d1stc/10/
      data d3dat/0/
      data d4epo/0/,d4sx/1/,d4sy/2/,d4era/5/,d4edc/6/,d4gl/7/,d4gb/8/,
     .     d4az/9/,d4el/10/,d4ix/11/,d4iy/12/,d4csc/16/
      data d7osn/1/
      data d12spn/15/,d12ni/14/,d12x0/11/,d12dx/12/,d12rp/10/
c
c     D1... = Offsets from start of each class to the required header parameter
c     C1... = Actual pointers to each header parameter.
c     IDATOFF+1 = position of first data point in arrays in R*4 words
