C					Include	CORE.
C-------------------------------------------------------------------------------
C  @(#)core.inc	5.1 06/22/94
C-------------------------------------------------------------------------------
c
      integer*2 k(32766),kx(32766),kkt(150),kxorg
      real*4 c,vers,nbox,nfit,nsave,zlline,slabel,fs,fr,bmark,mratio,
     . ngauss,bgauss,egauss,niter,fixc,fixhw,fixh,fixrelc,fixrelhw,
     . fixrelh,rampltde,rphase,rperiod,ramperr,rphaerr,rpererr,rfixamp,
     . rfixpha,rfixper,acount,nsigma,pflag,gmfrac,newres,oshft,bscan,
     . escan,bdrop,edrop,fact,ylmin,ylincr,cpmin,cpmax,vrms,calval,
     . bwind,ewind,ashift,weight,bbase,ebase,errcode,sprotect,kprotect,
     . nspacen,mxmin,mxmax,mymin,mymax,gout,mlims,size,nregon,gregon,
     . xmark,cmark,xmnp,ymnp,xmxp,ymxp,fmark,hmark,center,height,hwidth,
     . xlog,ylog,smwgt,badpt,levs,flevs,bparm,title,xtitle,ytitle,
     . protected,status,mdbox,dcpct,x_min,x_max,y_min,y_max,scan_min,
     . scan_max,feed_min,feed_max,lst_min,lst_max,ut_min,ut_max,f_min,
     . f_max,rate_min,rate_max,bw_min,bw_max,it_min,it_max,s_mode,
     . s_sort,s_object,default,cnterr,hghterr,hwerr,conline,clut,
     . typetip,eta,tau0,ftm,ftsbr,trcvr,etafree,tvane,userprompt,
     . fbver,hcver,conver,cclick,tclick,vclick,fclick,mclick,xclick,
     . yclick,crossflag,defmode
      character*32 ctitle
      character*16 cxtitle,cytitle,cs_object
      character*8 cuserprompt
      character*4 cs_mode
c
c    
c	NOTE:  Make sure that the order and sizes of variables agree with
c	what is set in POPSDAT.  Array sizes MUST be declared in the
c	common block definition below and NOT in the declerations above.
c	Except for K arrays,all variables must be R*4's -- use equivalneces
c	for strings.  Initial values for arrays and strings are set in
c	ARAININT; for scalars,values are set in POPSDAT.
c
      COMMON /CORE/ C(16389),VERS,NBOX,NFIT,NSAVE,ZLLINE,SLABEL,FS,FR,
     . BMARK,MRATIO,NGAUSS,BGAUSS,EGAUSS,NITER,FIXC,FIXHW,FIXH,
     . FIXRELC,FIXRELHW,FIXRELH,RAMPLTDE,RPHASE,RPERIOD,RAMPERR,
     . RPHAERR,RPERERR,RFIXAMP,RFIXPHA,RFIXPER,ACOUNT,NSIGMA,PFLAG,
     . NEWRES,OSHFT,BSCAN,ESCAN,BDROP,EDROP,FACT,YLMIN,YLINCR,CPMIN,
     . CPMAX,VRMS,calval,BWIND,EWIND,ASHIFT,WEIGHT,BBASE,EBASE,XMNP,
     . XMXP,XLOG,YMNP,YMXP,YLOG,crossflag,defmode,cclick,tclick,vclick,
     . fclick,mclick,xclick,yclick,x_min,x_max,y_min,y_max,scan_min,
     . scan_max,feed_min,feed_max,lst_min,lst_max,ut_min,ut_max,f_min,
     . f_max,rate_min,rate_max,bw_min,bw_max,it_min,it_max,s_sort,
     . typetip,eta,tau0,ftm,ftsbr,trcvr,etafree,tvane,ERRCODE,SPROTECT,
     . KPROTECT,dcpct,mdbox,mxmin,mxmax,mymin,mymax,NSPACEN,default,
     . TITLE(8),XTITLE(4),YTITLE(4),s_object(4),s_mode(1),userprompt(2),
     . SIZE(2),NREGON(32),GREGON(48),XMARK(12),CMARK(12),FMARK(12),
     . HMARK(12),CENTER(24),HEIGHT(24),HWIDTH(24),cnterr(24),
     . hghterr(24),hwerr(24),SMWGT(12),BADPT(12),gmfrac(5),gout(6,5),
     . mlims(2,5),LEVS(32),flevs(32),BPARM(15),status(32),conline(32),
     . clut(3,32),fbver,hcver,conver,PROTECTED(255)
c
      equivalence (k(1),c(1)),(k(8),kxorg),(kkt(1),k(51)),
     .            (kx(1),c(16384))
      equivalence (title,ctitle),(xtitle,cxtitle),
     .		  (ytitle,cytitle)
      equivalence (s_object,cs_object),(s_mode,cs_mode)
      equivalence (userprompt,cuserprompt)
c
