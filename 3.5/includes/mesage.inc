C                                    Error Messages
C-------------------------------------------------------------------------------
C  @(#)mesage.inc	5.2 05/04/98
C-------------------------------------------------------------------------------
      data iambiguous/20/, iamath/128, 245/
c     iambiguous = error number for AMBIGUOUS error message
c     iamath = error numbers for Array Math error messages
c
c     Errors generated while compiling an entered line
c
      DATA MESAGE(1) /'Blew core - Exceeded PROC or ADVERB space'/
      DATA MESAGE(2) /'Unknown symbol'/
      DATA MESAGE(3) /'Symbol is already defined'/
      DATA MESAGE(4) /'Too many characters in line'/
      DATA MESAGE(5) /'Symbol has too many characters (10 max.)'/
      DATA MESAGE(6) /'Cannot alter a BUILTIN operator or operand'/
      DATA MESAGE(7) /'STACK limits exceeded'/
      DATA MESAGE(8) /'Error in syntax -- No closing )'/
      DATA MESAGE(9) /'Operating system command has failed'/
      DATA MESAGE(10) /'Left hand side of = is not an adverb'/
      DATA MESAGE(11) /'No procedure by that name'/
      DATA MESAGE(12) /'Must give a line number when editing procedure'/
      data mesage(13) /'You are not POPSEDITing a procedure'/
      DATA MESAGE(14) /'Too many characters in string (60 max.)'/
      DATA MESAGE(15) /'Error in syntax -- Bad numerical field'/
      data mesage(16) /'Cannot perform outside of compile mode'/
      data mesage(17) /'Cannot perform within compile mode'/
      DATA MESAGE(18) /'Error in syntax in definition of procedure'/
      DATA MESAGE(19) /'Error in syntax -- No opening ('/
      DATA MESAGE(20) /'Ambiguous symbol'/
      DATA MESAGE(21) /'Error in IF, FOR, or WHILE syntax'/
      DATA MESAGE(22) /'Error in dimension statement'/
      DATA MESAGE(25) /'Error in string definition'/
      data mesage(26) /'Cannot use MENUS from within a MENUS session!'/
      DATA MESAGE(27) /'Error in syntax in ! command'/
      data mesage(28) /'No more input devices can be opened.'/
      DATA MESAGE(29) /'Insufficient or Bad argument list'/
      DATA MESAGE(30) /'Only STORE bins 1 - 3 exist'/
      DATA MESAGE(31) /'Error in CHNGFILE'/
      data mesage(32) /'Cannot UNDO or nothing to UNDO'/
      data mesage(34) /'Error in Syntax -- Unbalanced Parentheses'/
      data mesage(35) /'Error in Syntax -- Unbalanced Braces'/
      data mesage(36) /'PROCEDURE name already exists'/
      data mesage(37) /'No closing quote'/
      data mesage(38) /'No OPERATOR on input line'/
      data mesage(39) /'Must be the only command on the input line'/
      data mesage(40) /'Specified line number cannot be POPSEDITed'/
      data mesage(41) /'Two or more names are identical'/
      data mesage(42) /'Statement not allowed or out of sequence'/
      data mesage(43) /'Pseudo operators not allowed'/
      data mesage(44) /'No OPERATOR in string'/
c
      DATA MESAGE(104) /'Too many characters in input line'/
      DATA MESAGE(105) /'Arrays are numbered between 0 and 9'/
      DATA MESAGE(106) /'Error in use of an array'/
      DATA MESAGE(107) /'STACK limits exceeded'/
      DATA MESAGE(111) /'No procedure by that name'/
      DATA MESAGE(112) /'Insufficient or Bad argument list'/
      DATA MESAGE(114) /'Too many characters in string'/
      DATA MESAGE(115) /'Error in FOR loop'/
      DATA MESAGE(116) /'Error in RETURN'/
      DATA MESAGE(118) /'Infinite FOR loop?'/
      data mesage(120) /'*** INTERNAL Error ***'/
      data mesage(123) /'Error in data type for READ'/
      data mesage(124) /'Error in data type for PRINT'/
      DATA MESAGE(126) /'Error in asignment statement'/
      DATA MESAGE(127) /'Too many characters in right-hand string'/
      DATA MESAGE(128) /'Error in use of an array'/
      DATA MESAGE(129) /'COPYOFF in affect -- no hardcopy generated'/
      data mesage(130) /'Too many or too few operands'/
      data mesage(131) /'No room left in CORE for run-time variables'/
      data mesage(132) /'Error while COMPRESSing'/
      data mesage(133) /'Two or more imbedded array math constructs'/
c
      DATA MESAGE(219) /'Cannot have YMINP or YMAXP <= 0 for log plots'/
      DATA MESAGE(220) /'Cannot have XMINP or XMAXP <= 0 for log plots'/
      DATA MESAGE(221) /'No data in Arrays (0), (1), or (2)'/
      DATA MESAGE(222) /'No data in Arrays (0) or (1)'/
      DATA MESAGE(223) /'No data in Array (2)'/
      DATA MESAGE(224) /'No SCAN in Array (0)'/
      DATA MESAGE(225) /'No data in Array (0)'/
      DATA MESAGE(226) /'No data in specified Array'/
      DATA MESAGE(227) /'Nothing to average'/
      DATA MESAGE(228) /'NBOX must be odd, > 0 and <= 13'/
      data mesage(229) /'Bad GMFRAC -- Don''t expect the right answer'/
      data mesage(230) /'Array (0) and internal ACCUM array differ'/
      data mesage(231) /'Bad scan numbers'/
      data mesage(232) /'Cannot be used on Green Bank on-line data'/
      DATA MESAGE(233) /'Error in NREGION'/
      DATA MESAGE(234) /'Fit failed'/
      DATA MESAGE(235) /'Improper use of SMWGT(1) - either < 1 or > 10'/
      DATA MESAGE(236) /'Error in NREGION, BBASE, or EBASE'/
      DATA MESAGE(237) /'DCPCT must be > 0 and < 100'/
      DATA MESAGE(238) /'Scan has different filterbank resolution'/
      DATA MESAGE(239) /'STACK is full, (5120 max.)'/
      data mesage(240) /'Only for Green Bank data'/
      data mesage(241) /'Only for Spectral Processor data'/
      DATA MESAGE(242) /'MDBOX must be odd and < number of data points'/
      DATA MESAGE(243) /'New channel width is less than old'/
      DATA MESAGE(244) /'Crosshair was not positioned over a plot'/
      DATA MESAGE(245) /'Index out of range'/
      DATA MESAGE(246) /'Only for NRAO 12m data'/
      data mesage(247) /'Not a frequency-switched spectrum'/
      data mesage(248) /'Data is not from the NRAO 12m telescope'/
      data mesage(249) /'Array (1) has more/less data then Array (0)'/
      data mesage(250) /'Not an antenna tipping'/
      data mesage(251) /'Bad value for ASHIFT'/
      data mesage(252) /'Too few data points in array'/
      DATA MESAGE(253) /'No SCAN in Internal ACCUM Array'/
      data mesage(254) /'Bad MXMIN, MYMIN, MXMAX, or MYMAX'/
      data mesage(255) /'Nothing in Matrix (0)'/
      data mesage(256) /'Nothing in specified matrix'/
      data mesage(257) /'Cannot copy/move a matrix onto itself'/
      DATA MESAGE(258) /'Out of the range of matrix'/
      DATA MESAGE(259) /'Bad LEVS values'/
      DATA MESAGE(260) /'Pixel size too big for graphics screen'/
      DATA MESAGE(261) /'Bad color value'/
      DATA MESAGE(262) /'Bad Line type'/
      data mesage(263) /'Cannot copy/move an array onto itself'/
      data mesage(264) /'Previous plot was not a MATRIX plot'/
      data mesage(265) /'Invalid number of data points in array'/
      data mesage(266) /'Invalid value specified for NOINT or SPN'/
      data mesage(267) /'Array and Matrix size are not the same'/
      data mesage(268) /'Undefined data point exists'/
      data mesage(269) /'Array must have an even number of points'/
      data mesage(270) /'Incomplete velocity header parameters'/
      data mesage(271) /'Incomplete frequency header parameters'/
      data mesage(272) /'Incomplete header parameters'/
      data mesage(273) /'Incomplete matrix header parameters'/
      data mesage(274) /'Nothing in Matrix (1)'/
      data mesage(275) /'Matrices (0) and (1) have different shapes'/
      data mesage(281) /'Bad ACOUNT (either < 0 or > 5120)'/
      data mesage(282) /'Iteration limit exceeded'/
      data mesage(283) /'Data is not from an NRAO telescope'/
      data mesage(284) /'Bad parameters; check all associated adverbs'/
      data mesage(285) /'Bad value for BGAUSS or EGAUSS'/
      data mesage(286) /'Bad value in GREGION, BGAUSS, or EGAUSS'/
      data mesage(287) /'Bad value for BBASE or EBASE'/
      data mesage(288) /'Bad value for BDROP or EDROP'/
      data mesage(289) /'Bad value for NGAUSS'/
      data mesage(290) /'Bad CENTER or HWIDTH'/
      data mesage(291) /'Bad FS or FR'/
      data mesage(292) /'Bad BMOMENT or EMOMENT'/
      data mesage(293) /'Bad WEIGHT'/
      data mesage(294) /'Cannot use after a PLOT'/
      data mesage(295) /'Error in GAUSS'/
      data mesage(296) /'Bad value for MRATIO'/
      data mesage(297) /'Bad NFIT or too few channels for fit'/
      data mesage(298) /'Last plot was not a SHOW'/
      data mesage(299) /'No previous plot'/
      data mesage(300) /'Inappropriate On-Line file type'/
      data mesage(301) /'Invalid DATE string'/
c
      DATA MESAGE(351) /'MENU write error'/
      DATA MESAGE(352) /'Error in opening file'/
      DATA MESAGE(353) /'PRINTOUT file is unavailable'/
      DATA MESAGE(354) /'Command Logging file is unavailable'/
      DATA MESAGE(355) /'File write error'/
      DATA MESAGE(356) /'Error in closing file'/
      DATA MESAGE(357) /'File read error'/
      DATA MESAGE(358) /'SAVE file is unavailable'/
      DATA MESAGE(359) /'Data file has a problem with its INDEX'/
      DATA MESAGE(360) /'Data conversion problem'/
      DATA MESAGE(361) /'No current scan or ONLINE data not available'/
      DATA MESAGE(362) /'Scan not found or data file not available'/
      DATA MESAGE(363) /'Data file may have become corrupted'/
      DATA mesage(364) /'Bad file name or no file name was specified'/
      DATA mesage(365) /'FITS file is not in the required format'/
      DATA mesage(366) /'File cannot be deleted'/
      DATA mesage(367) /'Online data file is not available'/
      DATA mesage(368) /'NSAVE is empty.'/
      DATA mesage(370) /'File is not open.'/
      DATA mesage(371) /'NSAVE value is out of range for SAVE file.'/
      DATA mesage(372) /'Error on rewinding file'/
      DATA mesage(373) /'Bad format specification'/
      DATA mesage(374) /'Read or write error'/
      data mesage(375) /'Requested version is unavailable, use CHNGVER'/
      data mesage(376) /'File name could not be expanded'/
c
