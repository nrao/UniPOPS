c
c     TAGS.INC
c
C-------------------------------------------------------------------------------
C  @(#)tags.inc	5.1 06/22/94
C-------------------------------------------------------------------------------
c
c     Sets up variables to reflect the TAGS assigned by POPSDAT for
c     various operators
c
c     If POPSDAT is changed, make any needed change to this include file
c	so that STORES, PSEUDO, COMPIL, etc. will know about the changes.
c
      integer*4 tagsdummy
      integer*2 i0, iomega, igetargs, icomma, ilparen, irparen, 
     .		icompare, iuplus,iuminus, itimes, iminus, iloopto, 
     .		iassign, icolon,iloopfor, iforend, iread, iprint, 
     .		ireturn, igets, irun, iprotect, isread, ifunct, idhfunc1,
     .		idhfunc2, idhset1, idhset2, ifunfunct, igenop, irtops,
     .		iifelse, iifthen, iwhile, ipseudo, iprocedure, iarray,
     .		istring, iscalar,  ipointer, ialias, ieditor, istores,
     .		ihelps, iverbs, iadverbs, idumend, iwhilesetup, isubs,
     .		iaubegin(20), ilastverb, ifread, ifinish
c
      data tagsdummy / 0/
c
      data i0/0/, iomega/1/, igetargs/3/ 
      data icomma/1/, ilparen/2/, irparen/3/, icompare/4/, iuplus/5/
      data iuminus/6/, itimes/7/, iminus/13/, iloopto/18/, iassign/20/
      data icolon/23/, iloopfor/24/, iforend/25/, iread/26/, iprint/27/
      data ireturn/28/, igets/29/, irun/30/, iprotect/32/, isread/35/
      data ifread/37/
c
      data ifunct/51/
      data idhfunc1/73/, idhfunc2/101/, idhset1/219/, idhset2/247/
c
      data ifunfunct/150/, igenop/201/
c
      data irtops/300/,iifelse/300/, iifthen/301/, iwhile/303/
c
      data ipseudo/310/, iprocedure/310/, ifinish/311/, iarray/313/
      data istring/314/, iscalar/315/, ipointer/316/, ialias/317/
c
      data ieditor/320/, istores/330/
c
      data ihelps/360/, iverbs/361/, iadverbs/362/
      data idumend/368/, iwhilesetup/369/, isubs/370/
c
      data iaubegin/400,430,460,470,485,530,535,540,560,570,590,601,
     .		    8*0/
      data ilastverb/601/
c
c     i0 = tag of zero operator
c     iomega = tag of omega operator in QUICK
c     igetargs = tag of get arguments in QUICK
c     icomma, ilparen, irparen, icompare = tags of , ( ) and = [compare]
c     iuplus, iuminus = tags of unary plus, minus
c     itimes, iminus = tags of * -
c     iloopto = tag of TO; iassign = tag of assignment =
c     icolon = tag of ;
c     iloopfor, iforend  = tag of FOR, END
c     iread, iprint, isread, ifread  = tags of READ and PRINT (and ?), SREAD,
c	and FREAD.
c     ireturn = tag of RETURN
c     igets, irun, iprotect = tags of GETS, RUN, and PROTECT in QUICK
c     ifunct, igenop = tags of first operators in FUNCT, GENOP
c     ifunfunct = tag of first operator in FUNFUNCT
c     idhfunc1,2 = tags of first/last function for retrieving data/header values
c     idhset1,2 = tags of first/last verb for settinh data/header values.
c     irtops = tag of first pseudo operator in RTOPS
c     iifelse, iifthen, iwhile = tags of ELSE, THEN, WHILE in RTOPS
c     ipseudo = tag of first pseudo operator in PSEUDO
c     iprocedure = tag of PROCEDURE
c     iarray, iscalar, iverbs, iadverbs, ialias, ipointer, istring = tags of   
c	operators ARRAY, SCALAR, VERBS, ADVERBS, ALIAS, STRING, and POINTER
c     ieditor, istores, ihelps = tags of first operators in EDITOR, STORES,
c	and HELPS
c     iverbs, iadverbs = tags of VERBS and ADVERBS in HELP
c     idumend, iwhilesetup, isubs = tags of DUMEND, WHILE-SETUP, and SUBS
c	in QUICK.
c     iau* = tags of first verbs in AU* modules; one more than number of au*
c	modules and last iau should have the same value as ilastverb
c     ilastverb > tag of last verb in POPSDAT
c
