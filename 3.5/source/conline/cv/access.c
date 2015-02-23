
/* @(#)access.c	5.2 07/06/94 */

/* stubbed versions of data access routines for installations that don't */
/* connect to any online data */


/*
** Access and attach shared memory and set the project code
*/
void openaccess_(anal_user_code, anal_user_name, anal_site_name, return_code)
	double *anal_user_code, *anal_user_name, *anal_site_name;
	int *return_code;
{
      *return_code = 10000;
}

/*
** Set the output format to SDD format
*/
void setformat_(fmtcode, return_code)
	int *fmtcode, *return_code;
{
	*return_code = 10000;
}

/*
** Change the project code
*/
void chgprj_(anal_user_code, anal_user_name, anal_site_name, return_code)
	double *anal_user_code, *anal_user_name, *anal_site_name;
	int *return_code;
{
	*return_code = 10000;
}

/*
** Get the list of scans that belongs to this project
*/
void gscanlist2_(actno,maxno,list,return_code)
	int *actno, *maxno, *return_code;
        float *list;
{
}

/*
** Get a particular scan
*/
void gscan_(scan_no,feed_no,record,return_code)
	int *scan_no, *feed_no, *record, *return_code;
{
}

/*
** Get the header for a particular scan
*/
void gshead_(scan_no,feed_no,record,return_code)
	int *scan_no, *feed_no, *record, *return_code;
{
}

/*
** ggscan, ggscanlist are for tucson data only
*/

void ggscan_(scan_no,feed_no,record,return_code)
   int *scan_no, *feed_no, *record, *return_code;
{
}

void glgscan_(record, feed_no, return_code)
   float *record;
   int *feed_no, *return_code;
{
}

void ggscanlist2_(actno,maxno,list,return_code)
	int *actno, *maxno, *return_code;
        float *list;
{
}

/*
** Get the last completed scan that belongs to this project
*/
void gcscan_(record,return_code)
	float *record;
	int *return_code;
{
}

/*
** Get the Spectral Processsor scan
*/
void gspscan_(scan_no,feed_no,record,return_code)
	int *scan_no;
	int *feed_no;
	float *record;
	int *return_code;
{
}

/*
** Get the Spectral Processsor individual record
*/
void gspir_(scan_no,feed_no,phase_no,rec_no,record,return_code)
	int *scan_no, phase_no, rec_no;
	int *feed_no;
	float *record;
	int *return_code;
{
}
/*
** Get the A/C individual record
*/
void gacir_(scan_no,feed_no,phase_no,rec_no,record,return_code)
	int *scan_no, phase_no, rec_no;
	int *feed_no;
	float *record;
	int *return_code;
{
}

/*
** Get the list of scans that belongs to this project
*/
void gcontlist2_(actno,maxno,list,return_code)
	int *actno, *maxno, *return_code;
        float *list;
{
}

/*
** Get a particular scan
*/
void gcont_(scan_no,feed_no,actualpoints,maxpoints,record,return_code)
	int *scan_no, *feed_no, *actualpoints, *maxpoints;
	float *record;
	int *return_code;
{
}

/*
** Get the header for a particular scan
*/
void gchead_(scan_no,feed_no,record,return_code)
	int *scan_no, *feed_no, *record, *return_code;
{
}

/*
** Get the last completed scan that belongs to this project
*/
void glcont_(feed_no,actualpoints,maxpoints,record,return_code)
	int *feed_no, *actualpoints, *maxpoints;
	float *record;
	int *return_code;
{
}

void gselect_(params, actno, maxno, list, return_code)
	double *params;
	int *actno, *maxno;
	float *list;
	int *return_code;
{
   *actno = 0;
   *return_code = 1;
}

void closeaccess_()
{
}

int useprojcode_()
{
      return(0);
}

int onlinesite_()
{
   return(-1);
}

void chngonline_(idtype, iver, anal_user_code, ier)
int *idtype;
int *iver;
char *anal_user_code;
short int *ier;
{
   *iver = -1;
   *ier = 232;
}


int deffeed_()
{
   return(1);
}

void olfiles_()
{
}

void changever_(idtype, iver, anal_user_code, ier)
int *idtype;
int *iver;
char *anal_user_code;
short int *ier;
{
   *iver = -1;
   *ier = 232;
}

int qvers_(iftype)
int *iftype;
{
/*   wants the current version of file type iftype, tuc only */
   return(-1);
}

void gotfrec_(scan_no, feed_no, rec_no, num_rec, record, return_code)
int *scan_no, *feed_no, *rec_no, *num_rec;
char *record;
int *return_code;
{
/*   get an OTF record - 12-m data only */
   *return_code = -1;
}

void gpzrec_(scan_no, feed_no, rec_no, num_rec, record, return_code)
int *scan_no, *feed_no, *rec_no, *num_rec;
char *record;
int *return_code;
{
/* Get a polariz. record - 12m on-line only */
  *return_code = -1;
}
