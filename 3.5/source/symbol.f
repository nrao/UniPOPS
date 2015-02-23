      SUBROUTINE SYMBOL
C-------------------------------------------------------------------------------
C  @(#)symbol.f	5.1 06/22/94
C-------------------------------------------------------------------------------
C----------------------------------------------------------------------
C   Obtain a symbol from the symbol list.  
c
C---------------------------------------------------------------------
c
      integer*2 istat, length, iwpc, nt
      integer*2 n4, n80
c
      INCLUDE 'core.inc'
      include 'smstuf.inc'
      include 'stk.inc'
      INCLUDE 'cio.inc'
c
      data n4, n80 /4, 80/
c
C---------------------------------------------------------------------
C					Search symbol table.
      NT = IWPC(NKAR)
      local = .false.
c
      IF (MODE.EQ.0) THEN
c	 We are not trying to compile a proc so use min/match and only go
c	 thru globals
c
	 L=K(1)
     	 CALL MHUNT (K,L,NT,n4,KPAK,NKAR)
      ELSE
c	 We may be trying to compile a proc so don't use min/match and go
c	 thru locals and, if not found, then thru globals.
c
	 l = 0
	 if (lclstart .gt. 0) then
	   l = max(0, k(lclstart))
	   call hunt (k, l, nt, n4, kpak)
	   if (l .ne. 0) local = .true.
	 endif
	 if (l .eq. 0) then
	    L=K(1)
            CALL HUNT (K,L,NT,n4,KPAK)
	 endif
      ENDIF
c     if reading in a procedure, then no min match
c
      IF (IDEBUG.GT.0) then
	WRITE (outbuf,102,iostat=istat) MODE,L,NT
 102    FORMAT (1X,'SYMBOL ', 3I6)
	call pwrite(ioutbuf, n80)
      endif
c
      IF (L.gt.0) then
C	Old symbol found.
c
	LENGTH=K(L+1)/16
	TYPE=K(L+1)-16*LENGTH
c
	if (type .ne. 4 .and. type .ne. 5) LOCSYM=L
	TAG=K(L+2)
c
      else 
C	Symbol doesn't exist
c
        TYPE=0
	TAG=-1
c
      endif
c
      RETURN
      END
c
      subroutine creatsym(type, kpak, nkar, tag, err, locsym)
c
c     Creates a global symbol of type TYPE with name stored in KPAK of NKAR
c	characters.  The symbol is given the tag TAG.  LOCSYM is given
c	the location in CORE of the new item.  If ERR=0 then an error
c	if symbol already exists, else no error.
c
      integer*2 n1, n3, n4
      integer*2 type, kpak(*), nkar, l, nt, n, llocat, locsym, mk,
     .		tag, err, iwpc
c
      include 'core.inc'
c
      data n1, n3, n4 /1, 3, 4/
c
      L=K(1)
      NT = IWPC(NKAR)
      CALL HUNT (K,L,NT,n4,KPAK)
c
      IF (L.gt.0) then
c
	if (err .eq. 0) call oerror(n3, n1, ' ')
c
      else
c
         N=NT+4
         L=LLOCAT(N,K,K(9))
         K(L+1)=16*NT+TYPE
         LOCSYM=L
         MK = L+4
         CALL COPY(NT,KPAK,K(MK))
         K(L+2)=TAG
c
      endif
c
      RETURN
      END
c
      subroutine creatloc(type, kpak, nkar, tag, err, locsym)
c
c     Creates a local symbol of type TYPE with name stored in KPAK of NKAR
c	characters.  The symbol is given the tag TAG.  LOCSYM is given
c	the location in CORE of the new item.  If ERR=0 then an error
c	if symbol already exists, else no error.
c
      integer*2 n1, n3, n4
      integer*2 type, kpak(*), nkar, nt, n, llocat, locsym, mk,
     .		tag, err, iwpc
c
      include 'core.inc'
      include 'stk.inc'
c
      data n1, n3, n4 /1, 3, 4/
c
      NT = IWPC(NKAR)
      if (lclstart .gt. 0) then
	   l = max(0, k(lclstart))
	   call hunt (k, l, nt, n4, kpak)
      endif
c
      IF (L.gt.0) then
c
	if (err .eq. 0) call oerror(n3, n1, ' ')
c
      else
c
         N=NT+4
         lcllast=LLOCAT(N,K,lcllast)
         K(lcllast+1)=16*NT+TYPE
         LOCSYM=lcllast
         MK = lcllast+4
         CALL COPY(NT,KPAK,K(MK))
         K(lcllast+2)=TAG
c
      endif
c
      RETURN
      END
c
      subroutine creatspc(numi2, tag)
c
c     Reserves NUMI2 words in the KX array; returns the TAG of the
c	location used in KX.
c
      integer*2 numi2, tag, kxlink, irealp, llocat
c
      include 'core.inc'
c
      KXLINK=1
      TAG=LLOCAT(numi2,Kx,KXLINK)
      TAG=IREALP(TAG) + irealp(kxorg) - 1
c
      return
      end
c
