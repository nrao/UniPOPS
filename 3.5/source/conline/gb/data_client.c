
/* @(#)data_client.c	5.3 07/08/94 */

# include <sys/types.h>

# include <sys/socket.h>

# include <netinet/in.h>

# include <netdb.h>

# include <utmp.h>

# include <rpc/rpc.h>

# include <rpcsvc/rusers.h>

# include <errno.h>

# include <access.h>

# include <malloc.h>

#ifdef SOLARIS
# include <string.h>
#endif

int port_number = -1;
int port = -1;

ServiceRequest req;
ServiceReply reply;

int link_to_data(server_name)
	char *server_name;
{
	static struct sockaddr_in sinhim =
	{
	    AF_INET
	};
	int fd;
	struct hostent *hp;
	int request;
	int n;
	extern int errno;

	/* get the port number from the server */
	request = SEND_ANALYSIS_DATA_2;
	if ((n = callrpc(server_name,
		PROG,VERS,PROC,xdr_int,&request,xdr_int,&port_number)) != 0)
	{
	    port_number = -1;
	    return(21); /* callrpc failed */
	}
	if (port_number < 0)
	{
	    port_number = -1;
	    return(21);
	}

	/* connect to the port */
	hp = gethostbyname (server_name);
	if (!hp)
	{
	    port_number = -1;
	    return(22); /* Host was not found */
	}

#ifdef SOLARIS
	memcpy(&sinhim.sin_addr, hp->h_addr, sizeof(sinhim.sin_addr));
#else
	bcopy (hp->h_addr, &sinhim.sin_addr, sizeof(sinhim.sin_addr));
#endif
	sinhim.sin_port = port_number;

	if ((port = socket(AF_INET, SOCK_STREAM, 0)) < 0)
	{
	    port_number = -1;
	    return(23); /* Cannot create socket */
	}

	if (connect(port, (struct sockaddr *)&sinhim, sizeof(sinhim)) < 0)
	{
	    port_number = -1;
	    return(24); /* Connect failed */
	}

	return(0);
}

int talk_to_port(addr)
	char *addr;
{
	int i, n, nb;

	if (write(port,&req,sizeof(req)) != sizeof(req))
	    return(25);
	if (read(port,&reply,sizeof(reply)) != sizeof(reply))
	    return(26);
	if (reply.length_data > 0)
	{
	    n = reply.length_data;
	    do
	    {
		i = (n < PACKETSIZE) ? n : PACKETSIZE;
		if ((nb = read(port,addr,i)) == -1)
		    return(27);
		addr += nb;
		n -= nb;
	    } while (n > 0);
	}
	return(0);
}

# define RETURN(N) \
	{\
	    *return_code = N * 10000 + errno;\
	    return;\
	}

/*
** Access and attach shared memory and set the project code
*/
void openaccess_(anal_user_code, anal_user_name, anal_site_name, return_code)
	double *anal_user_code, *anal_user_name, *anal_site_name;
	int *return_code;
{
	char *p, *q, *r;
	int n;

	if ((n = link_to_data(DATA_HOST_NAME)) != 0)
	    RETURN(n)
	p = (char *)anal_user_code;
	q = (char *)anal_user_name;
	r = (char *)anal_site_name;
	req.request_code = OPEN;
	strncpy(req.user_code,p,8);
	strncpy(req.user_name,q,8);
	strncpy(req.site_name,r,8);
	if ((n = talk_to_port(0)) != 0)
	    RETURN(n)
	*return_code = reply.service_return_code;
}

void setformat_(fmtcode, return_code)
	int *fmtcode, *return_code;
{
	int n;

 	req.request_code = SETFMT;
        req.scan_number = *fmtcode;
	if ((n = talk_to_port(0)) != 0) RETURN(n)
	*return_code = reply.service_return_code;
}

/*
** Change the project code
*/
void chgprj_(anal_user_code, anal_user_name, anal_site_name, return_code)
	double *anal_user_code, *anal_user_name, *anal_site_name;
	int *return_code;
{
	char *p, *q, *r;
	int n;

	p = (char *)anal_user_code;
	q = (char *)anal_user_name;
	r = (char *)anal_site_name;
	req.request_code = CHGPRJ;
	strncpy(req.user_code,p,8);
	strncpy(req.user_name,q,8);
	strncpy(req.site_name,r,8);
	if ((n = talk_to_port(0)) != 0)
	    RETURN(n)
	*return_code = reply.service_return_code;
}

/*
** Front end to gscanlist, puts list into array of floats
**  all size checking is done is gscanlist
*/
void gscanlist2_(actno, maxno, rlist, return_code)
	int *actno;
	int *maxno;
	float *rlist;
 	int *return_code;
{
	int i, size, *p, *p1;
	float *rp;
	void gscanlist_();

	*actno = 0;
	*return_code = 0;
	size = *maxno * sizeof(int);
	p = (int *) malloc(size);
        p1 = p;
	if (p != NULL) {
	   gscanlist_(actno, maxno, p, return_code);
	   for (i=0,rp=rlist;i<*actno;i++,rp++,p++) *rp = *p;
           free(p1);
	}
        
}

/*
** Get the list of scans that belongs to this project
*/
void gscanlist_(actno,maxno,list,return_code)
	int *actno;
	int *maxno;
	int *list;
	int *return_code;
{
	int n;

	req.request_code = GSCANLIST;
	req.max_data_items = *maxno;
	if ((n = talk_to_port((char *)list)) != 0)
	    RETURN(n)
	*actno = reply.actual_data_items;
	*return_code = 0;
}

/*
** Get a particular scan
*/
void gscan_(scan_no,feed_no,record,return_code)
	int *scan_no;
	int *feed_no;
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GSCAN;
	req.scan_number = *scan_no;
	req.feed_number = *feed_no;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	if (*scan_no != record[0]) 
	{
		if ((n = talk_to_port((char *)record)) != 0)
	   	 RETURN(n)
	}

	*return_code = reply.service_return_code;
}

/*
** Get the header for a particular scan
*/
void gshead_(scan_no,feed_no,record,return_code)
	int *scan_no;
	int *feed_no;
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GSHEAD;
	req.scan_number = *scan_no;
	req.feed_number = *feed_no;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	if (*scan_no != record[0]) 
	{
		if ((n = talk_to_port((char *)record)) != 0)
	   	 RETURN(n)
	}

	*return_code = reply.service_return_code;
}

/*
** ggscan, ggscanlist are for tucson data only
*/

void ggscan_(scan_no,feed_no,record,return_code)
   int *scan_no;
   int *feed_no;
   float *record;
   int *return_code;
{
   *return_code = 232;
}

void glgscan_(record, feed_no, return_code)
   float *record;
   int *feed_no;
   int *return_code;
{
   *return_code = 232;
}

void ggscanlist2_(actno,maxno,list,return_code)
	int *actno;
	int *maxno;
	float *list;
	int *return_code;
{
   *return_code = 232;
}

/*
** Get the next scan that belongs to this project
*/
void gnscan_(record,return_code)
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GNSCAN;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	*return_code = reply.service_return_code;
}

/*
** Get the previous scan that belongs to this project
*/
void gpscan_(record,return_code)
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GPSCAN;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	*return_code = reply.service_return_code;
}

/*
** Get the last completed scan that belongs to this project
*/
void glscan_(record,return_code)
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GLSCAN;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	*return_code = reply.service_return_code;
}

/*
** Get the current scan in progress -- provided it belongs to this project
*/
void gcscan_(record,feed_no,return_code)
	float *record;
	int *feed_no;			/* ignored here */
	int *return_code;
{
	int n;

	req.request_code = GCSCAN;
	req.feed_number = *feed_no;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	*return_code = reply.service_return_code;
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
	int n;

	req.request_code = GSPSCAN;
	req.scan_number = *scan_no;
	req.feed_number = *feed_no;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	if (*scan_no != record[0]) 
	{
		if ((n = talk_to_port((char *)record)) != 0)
	   	 RETURN(n)
	}

	*return_code = reply.service_return_code;
}

/*
** Get the Spectral Processsor individual record
*/
void gspir_(scan_no,feed_no,phase_no,rec_no,record,return_code)
	int *scan_no;
	int *feed_no;
	int *rec_no;
	int *phase_no;
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GSPIR;
	req.scan_number = *scan_no;
	req.feed_number = *feed_no;
	req.phase_number = *phase_no;
	req.record_number = *rec_no;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	if (*scan_no != record[0]) 
	{
		if ((n = talk_to_port((char *)record)) != 0)
	   	 RETURN(n)
	}

	*return_code = reply.service_return_code;
}

/*
** Get the A/C individual record
*/
void gacir_(scan_no,feed_no,phase_no,rec_no,record,return_code)
	int *scan_no;
	int *feed_no;
	int *rec_no;
	int *phase_no;
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GACIR;
	req.scan_number = *scan_no;
	req.feed_number = *feed_no;
	req.phase_number = *phase_no;
	req.record_number = *rec_no;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	if (*scan_no != record[0]) 
	{
		if ((n = talk_to_port((char *)record)) != 0)
	   	 RETURN(n)
	}

	*return_code = reply.service_return_code;
}


/*
** Front end to gcontlist, puts list into array of floats
**  all size checking is done is gcontlist
*/
void gcontlist2_(actno, maxno, rlist, return_code)
	int *actno;
	int *maxno;
	float *rlist;
 	int *return_code;
{
	int i, size, *p, *p1;
	float *rp;
	void gcontlist_();

	*actno = 0;
	*return_code = 0;
	size = *maxno * sizeof(int);
	p = (int *) malloc(size);
	p1 = p;
	if (p != NULL) {
	   gcontlist_(actno, maxno, p, return_code);
	   for (i=0,rp=rlist;i<*actno;i++,rp++,p++) *rp = *p;
           free(p1);
	}
        
}

/*
** Get the list of scans that belongs to this project
*/
void gcontlist_(actno,maxno,list,return_code)
	int *actno;
	int *maxno;
	int *list;
	int *return_code;
{
	int n;

	req.request_code = GCONTLIST;
	req.max_data_items = *maxno;
	if ((n = talk_to_port((char *)list)) != 0)
	    RETURN(n)
	*actno = reply.actual_data_items;
	*return_code = 0;
}

/*
** Get a particular scan
*/
void gcont_(scan_no,feed_no,actualpoints,maxpoints,record,return_code)
	int *scan_no;
	int *feed_no;
	int *actualpoints;
	int *maxpoints;
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GCONT;
	req.scan_number = *scan_no;
	req.feed_number = *feed_no;
	req.max_data_items = *maxpoints;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	if (*scan_no != record[0]) 
	{
		if ((n = talk_to_port((char *)record)) != 0)
	   	 RETURN(n)
	}

	*return_code = reply.service_return_code;
}

/*
** Get the header for a particular scan
*/
void gchead_(scan_no,feed_no,record,return_code)
	int *scan_no;
	int *feed_no;
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GCHEAD;
	req.scan_number = *scan_no;
	req.feed_number = *feed_no;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	if (*scan_no != record[0]) 
	{
		if ((n = talk_to_port((char *)record)) != 0)
	   	 RETURN(n)
	}

	*return_code = reply.service_return_code;
}

/*
** Get the last completed scan that belongs to this project
*/
void glcont_(feed_no,actualpoints,maxpoints,record,return_code)
	int *feed_no;
	int *actualpoints;
	int *maxpoints;
	float *record;
	int *return_code;
{
	int n;

	req.request_code = GLCONT;
	req.feed_number = *feed_no;
	req.max_data_items = *maxpoints;
	if ((n = talk_to_port((char *)record)) != 0)
	    RETURN(n)
	*return_code = reply.service_return_code;
}

void gselect_(params, actno, maxno, record, return_code)
sel_params *params;
int *actno, *maxno, *return_code;
float *record;
{
	int n, i, nb;
        char *cp;

	req.request_code = GSELECT;
	req.max_data_items = *maxno;
/*		send sizeof(sel_params) via scan number */
	req.scan_number = sizeof(sel_params);
        if ((n = talk_to_port((char *)record)) != 0) 
           RETURN(n)
/*		is it OK to send more ? */
	if (reply.service_return_code != 0) {
           *return_code = reply.service_return_code;
           return;
        }
/*		send params and get response */
        n = sizeof(sel_params);
        cp = (char *)params;
        do 
        {
           i = (n < PACKETSIZE) ? n : PACKETSIZE;
           if ((nb = write(port, cp, i)) == -1) RETURN(1);
           cp += nb;
           n -= nb;
        } while (n > 0);

        if (read(port, &reply, sizeof(reply)) != sizeof(reply))
           RETURN(2);
        if (reply.length_data > 0)
        {
	  cp = (char *)record;
          n = reply.length_data;
          do
          {
             i = (n < PACKETSIZE) ? n : PACKETSIZE;
             if ((nb = read(port, cp, i)) == -1) RETURN(3);
             cp += nb;
             n -= nb;
          } while (n > 0);
        }
        *actno = reply.actual_data_items;
        *return_code = reply.service_return_code;
}

void closeaccess_()
{
	req.request_code = CLOSE;
	write(port,&req,sizeof(req));
}

int useprojcode_()
{
      return(1);
}

int onlinesite_()
{
   return(0);
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
/*		returns the default feed number for the first feed */
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
