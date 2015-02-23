
/*
 * @(#)modes.h	5.2 04/28/95
 *
 *  Observing modes and the integer associated with them in the index
 *  plus a short description.
 *
 */

#define NUMMODES 52

static struct mode {

      short int id;
      char field[4];
      char description[32];

   } modes[NUMMODES] = {
	 0,	"    ",	"No mode present",
	 1,	"PS  ",	"Position Switched",
	 2,	"APS ",	"Absolute Position Switched",
	 3,	"FS  ",	"Frequency Switched",
	 4,	"BSP ",	"Beam_Switch + Position_Switch",
	 5,	"TPON",	"Total Power On",
	 6,	"TPOF",	"Total Power Off",
	 7,	"ATP ",	"Absolute Total Power",
	 8,	"PSM ", "Position Switched Map",
	 9,	"APM ",	"Absolute Position Switched Map",
	10,	"FSM ",	"Frequency Switched Map",
	11,	"TPMO",	"Total Power Map On",
	12,	"TPMF",	"Total Power Map Off",
	13,	"DRF ", "Drift Map",
	14,	"PCAL",	"Position Calibrate",
	15,	"BCAL",	"Beam Calibrate",
	16,	"BLNK",	"Blanking",
	17,	"SEQ ",	"Sequence",
	18,	"FIVE",	"Five Point",
	19,	"MAP ",	"Continuum Map",
	20,	"FOC ",	"Focalize",
	21,	"NSFC",	"North-South Focalize",
	22,	"TTIP",	"Total Power Tip",
	23,	"STIP", "Switched Power Tip",
	24,	"DON ",	"Continuum On",
	25,	"CAL ",	"Calibration",
	26,	"FSPS",	"Freq Switch + Position Switch",
	27,	"BSPS",	"Beam Switch + Position Switch",
	28,	"ZERO",	"Zero Check",
	29,	"TLPW",	"Total Power",
	30,	"FQSW",	"Frequency Switched",
	31,	"NOCL",	"No Calibration",
	32,	"PLCL",	"Pulse Cal",
	33,	"ONOF",	"Continuum On-Off Scan",
	34,	"BMSW", "Nutation",
        35,     "PSSW", "Position Switched, Tucson, old",
	36,	"DRFT", "Continuum drift scans, Tucson",
	37,	"OTF ", "On-the-fly, Tucson",
	38,	"SON ", "",
	39,	"SOF ",	"",
	40,	"QK5 ",	"",
	41,	"QK5A",	"",
	42,	"PSS1",	"PS flip or PS-1 mode",
	43,	"VLBI", "VLBI",
	44, 	"PZC ", "",
	45,	"CPZM", "",
	46,	"PSPZ", "Position switched, polz., Tuc.",
	47,	"CPZ1", "",
	48,	"CPZ2", "",
	49,	"CCPZ", "",
	50,     "CCPM", "",
	51, 	"TPPZ", "Total Power, polz., Tuc."
     };
