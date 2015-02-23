c
c     @(#)explain.inc	5.1 06/22/94
c
c     Include file for explain facility
c
      integer*4 NUMNODES, NUMDEST, NULL
      integer*2 iodevice
c
      parameter (NUMNODES = 1024)
      parameter (NUMDEST = 8092)
      parameter (NULL = 0)
c
      character*32 nodes(NUMNODES)
      character*128 filenames(NUMNODES)
      integer*4 next(NUMNODES), adj(NUMNODES)
      integer*4 dest(NUMDEST), link(NUMDEST), start, availn, availe
c
      common/explain/next, adj, dest, link, start, availn, availe,
     .		     nodes,filenames
c
c     NUMNODES = number of nodes in graph
c     NUMDEST = total number of paths
c     NULL = zero value and end of link flag
c     nodes = linked-list of node names
c     filenames = linked-list of filenames corresponding to the nodes
c     next = pointers for linked-list of nodes
c     adj = links to adjacency linked-list
c     dest = linked-list of destinations
c     link = pointers for linked-list of destinations
c     start = pointer to first node in graph
c     availn = first available node in node linked-list
c     availe = first available node in destination linked-list
c
      data iodevice/6/
c     IODEVICE = Default text output device number
