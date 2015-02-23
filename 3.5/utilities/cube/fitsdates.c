
/* @(#)fitsdates.c	1.1 05/04/98 */

#include <time.h>

/* set and query the date style */
/* a style of 0 is the old FITS style of dd/mm/yy */
/* a style of 1 is the new FITS style of yyyy-mm-dd[Thh:mm:ss[.sss]] */
/* if whichStyle is a valid pointer, sets the style to that */
/* if whichStyle is not a valid pointer AND the style has not */
/* been set, set the style as appropriate for today */
/* Once the style has been set (is not -1) it can not be reset */
/* the style is returned */

int datestyle_(whichStyle)
     int *whichStyle;
{
  static int style = -1;
  time_t cltime;
  struct tm *ltime;

  if (style == -1) {
    if (whichStyle) {
      style = *whichStyle;
    } else {
      cltime = time((time_t *)0);
      ltime = localtime(&cltime);
      if (ltime->tm_year > 98) style = 1;
      else style = 0;
    }
  }
  return style;
}

/* The DATE fields in matrix and cube headers are stored as DOUBLES */
/* but they behave like strings.  This was to maintain the initial  */
/* behavior while keeping the ability to store full FITS compatable */
/* dates of the new form YYYY-MM-DD[Thh:mm:ss.[sss]] */
/* Since this requires more than 8 characters, the decision was made */
/* to convert these strings to/from doubles so that they can be */
/* stored in the same 8-character space in the header and hence */
/* POPSDAT does not need to change */

/* These are the function which do that conversion */

/* the value is stored as a modified JD number */

/* slen gives the max number of chars possibly in sdate */
/* sdate contains the string */
/* ddate will contain the result */
/* oldfits is 1 if this is the buggy old unipops with dd/mm reversed */
void todate_(slen, sdate, ddate, oldfits)
     int *slen;
     char *sdate;
     double *ddate;
     int *oldfits;
{
  static int days[] = { 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30 ,31 };
  int first, last, len;
  int yr, mon, day, hr, min, mday, mjd, csize, tmp;
  float secs;
  
  yr = mon = day = hr = min = 0;
  secs = 0.0;

  *ddate = -1.0;
  
  first=last=-1;
  while (last < *slen && sdate[++last] != '\0');
  while (last >= 0 && sdate[--last] == ' ' && last >= 0);
  while (first <= last && sdate[++first] == ' ');
  /* first points to the first non_blank char and last to the last */
  len = last - first + 1;
  /* try some forms */
  if ((len == 8 || (len == 7 && first == 1)) && 
      sdate[2] == '/' && sdate[5] == '/')
  {
    /* original fits format */
    sscanf(sdate, "%d/%d/%d", &day, &mon, &yr);
    if (*oldfits) {
      tmp = day;
      day = mon;
      mon = tmp;
    } 
    yr += 1900;
  } else if (len == 10 && sdate[4] == '-' && sdate[7] == '-') {
    /* new format, short form */
    sscanf(sdate, "%d-%d-%d", &yr, &mon, &day);    
  } else if (len >= 19 && sdate[4] == '-' && sdate[7] == '-' &&
	     sdate[10] == 'T' && sdate[13] == ':' && sdate[16] == ':') {
    /* new format, long form */
    sscanf(sdate, "%d-%d-%dT%d:%d:%f", &yr, &mon, &day,
	   &hr, &min, &secs);
  } else {
    /* anything else is unknown */
    return;
  }

  /* convert to decimal days if everything is ok so far */
  if (secs >= 60.0 || min >= 60 || hr >= 24 || mon > 12) return;
  
  mday = days[mon-1];
  if ((yr%400 == 0) || ((yr%4 == 0) && (yr%100 != 0))) mday++;

  if (day > mday) return;

  mjd = (1461*(yr+4800+(mon-14)/12))/4 + 
    (367*(mon-2-12*((mon-14)/12)))/12 -
    (3*((yr+4900+(mon-14)/12)/100))/4 + day-32075;
  mjd = mjd - 2400001;
  *ddate = mjd + (hr + (min + secs/60.0)/60.0)/24.0;
}

/* slen gives the number of available chars to set */
/* sset gives the number of chars actually set */
/* ddate contains the modified julian date to be turned into a string */
/* sdate will contain the string result it is filled with blanks if necessary
/* to pad out all slen chars */
void fromdate_(slen, sset, ddate, sdate)
     int *slen, *sset;
     double *ddate;
     char *sdate;
{
  double jdf;
  int jd, l, n, i, j, day, mon, yr, hr, min, nchars, tchars;
  float secs;
  *sset = 0;
  if (*ddate >= 0.0) {
    /* .0005 seconds is the resolution of the output string */
    double mindayfract = .0005/60.0/60.0/24.0;

    jdf = *ddate - (int)(*ddate); 
    jd = *ddate - jdf + 2400001; 
  

    l = jd+68569; 
    n=(4*l)/146097; 
    l=l-(146097*n+3)/4; 
    i=(4000*(l+1))/1461001; 
    l=l-(1461*i)/4+31; 
    j=(80*l)/2447; 
    day=l-(2447*j)/80; 
    l=j/11; 
    mon=j+2-12*l; 
    yr = 100*(n-49)+i+l;
    /* is there any fractional day to display */
    /* this forces the new style output */
    if (jdf > mindayfract) {
      if (*slen < 24) *sset = 0;
      else {
	jdf *= 24.0; 
	hr = jdf; 
	jdf = (jdf - hr)*60.0; 
	min = jdf ; 
	secs = (jdf - min)*60.0; 
	if (secs >= 59.999) { 
	  min += 1; 
	  secs -= 60.0;
	} 
	if (min >= 60) { 
	  hr += 1; 
	  min -= 60; 
	}
	if (secs < mindayfract) secs = 0.0;
	sprintf(sdate,"%04d-%02d-%02dT%02d:%02d:%06.3f",yr, mon, day, hr, min, secs);
	*sset = 23;
      }
    } else {
      if (*slen < 10) *sset = 0;
      else {
	/* which style to use, new or old */
	if (datestyle_(0) || yr >= 2000) {
	  /* for dates over 2000, we are forced to use this */
	  sprintf(sdate,"%04d-%02d-%02d",yr, mon, day);
	  *sset = 10;
	} else {
	  sprintf(sdate,"%02d/%02d/%02d",day,mon,(yr-1900));
	  *sset = 8;
	}
      }
    }
  }
  /* pad with spaces */
  for (i=*sset;i<*slen;i++) sdate[i] = ' ';
}

/* format iarr into an approprate FITS date string */
/* iarr has 3 elements, day, month, year */
/* the result will be padded with blanks out to nchars */
/* there is no sanity check that nchars is long enough */
void makedate_(nchars, sdate, iarr)
     int *nchars;
     char *sdate;
     int *iarr;
{
  int i, nc;
  if (datestyle_(0) || iarr[2] >= 2000) {
    sprintf(sdate, "%04d-%02d-%02d", iarr[2], iarr[1], iarr[0]);
    nc = 10;
  } else {
    sprintf(sdate, "%02d/%02d/%02d", iarr[0], iarr[1], (iarr[2]-1900));
    nc = 8;
  }
  for (i=nc;i<*nchars;i++) sdate[i] = ' ';
}

/* extract a fits keyword string value from the input string */
/* to the output string */
/* It is assumed that the input string is at least 80 chars */
/* long and that the output string can hold at least 80-11 chars */
/* nchars returns the number of chars of the output string actually set */
/* String values start after the first ' and end before the next ' or
/* column 80, whichever appears first */

void fitskeyvalue_(nchars, card, value)
     int *nchars;
     char *card, *value;
{
  int icount, ocount;
  icount = ocount = 0;
  /* move to the start */
  while (*card != '\0' && *card != '\'' && icount < 80) {
    card++;
    icount++;
  }
  if (*card == '\'') {
    card++;
    icount++;
    while (*card != '\0' && *card != '\'' && icount < 80) {
      *value = *card;
      value++;
      card++;
      ocount++;
      icount++;
    }
  }
  *nchars = ocount;
}

