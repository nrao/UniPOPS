      program initexplain
c
c     @(#)initexplain.f	5.1 06/22/94
c
c     Initializes the linked-list arrays
c
      include 'explain.inc'
c
      integer*2 ISTDOUT, ISTDIN
      integer*4 i
      character*128 file
c
      parameter (ISTDOUT = 6)
      parameter (ISTDIN = 5)
c
      open(unit=ISTDOUT, form="print")
c
      write(ISTDOUT,*) ' '
      write(ISTDOUT,*) 'Creates an empty/initilaized explain ',
     . 		       'linked-list information file.'
      write(ISTDOUT,*) ' '
99    write(ISTDOUT,fmt='(1x,a,$)') 'Enter name of file (128 chars): '
      read(ISTDIN,*) file
c
      start = 0
      availn = 1
      availe = 1
c
      do 10 i = 1, NUMNODES
	nodes(i) = ' '
	filenames(i) = ' '
	adj(i) = 0
10	continue
c
      do 20 i = 1, NUMNODES-1
	next(i) = i+1
20	continue
      next(NUMNODES) = NULL
c
      do 30 i = 1, NUMDEST
	dest(i) = 0
30	continue
c
      do 40 i = 1, NUMDEST-1
	link(i) = i+1
40	continue
      link(NUMDEST) = 0
c
      call writeinfo(file)
c
      write(ISTDOUT,*) ' '
      write(ISTDOUT,*) 'Done '
      write(ISTDOUT,*) ' '
c
      stop
      end
c
