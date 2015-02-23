c
c @(#)explainmap.inc	5.1 06/22/94
c
      integer*2 ISTDOUT, ISTDERR, ISTDIN
      parameter (ISTDERR = 0)
      parameter (ISTDIN = 5)
      parameter (ISTDOUT = 6)
c
      include 'explain.inc'
c
      integer*4 hardhit(NUMNODES), softhit(NUMNODES)
      integer*4 hlinks(NUMNODES)
      integer*4 tophlink
c
      common /libcom/ hardhit, softhit, hlinks, tophlink
c
c	hardhit : record number of hard links to each node
c       softhit : record number of soft links to each node
c	hlinks : stack to keep record of hard links that need to be followed
c       tophlink : the pointer to the top of hlinks
c
