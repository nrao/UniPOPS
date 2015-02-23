
/* @(#)access.h	5.2 08/02/94 */

/*
** Communication data structures for data_client.c and data_server.c
*/
# define OPEN 1
# define CHGPRJ 2
# define GSCANLIST 3
# define GSCAN 4
# define GNSCAN 5
# define GPSCAN 6
# define GLSCAN 7
# define GCSCAN 8
# define GCONTLIST 9
# define GCONT 10
# define GLCONT 11
# define CLOSE 12
# define GSPSCAN 13
# define GSHEAD 14
# define GCHEAD 15
# define GSPIR 16
# define GACIR 17
# define SETFMT 18
# define GSELECT 19

# define PACKETSIZE 1024

# define SDDFMT 0
# define IEEEFMT 1

typedef struct
{
	int request_code;
	int scan_number;
	int feed_number;
	int phase_number;
	int record_number;
	int max_data_items;
	char user_code[8];
	char user_name[8];
	char site_name[8];
} ServiceRequest;

typedef struct
{
	int service_return_code;
	int actual_data_items;
	int length_data;
	/* The actual data is returned in packets of 512 bytes. */
} ServiceReply;

typedef struct {
   double def_val;
   char source_name[16];
   char obs_mode[8];
   double scan_min, scan_max;
   double feed_min, feed_max;
   double x_min, x_max;
   double y_min, y_max;
   double lst_min, lst_max;
   double ut_min, ut_max;
   double bw_min, bw_max;
   double f_min, f_max;
   double rate_min, rate_max;
   double int_min, int_max;
} sel_params;

/*
** Data server program number (used by rpc calls)
*/
# define PROG 0x20000001
# define VERS 1
# define PROC 1

/*
** Data server requests
*/
# define SEND_ANALYSIS_DATA 1
# define SEND_ANALYSIS_DATA_2 2

/*
** Data server sub-process names
*/
/*   # ifdef ANALYSIS */

# define SEND_ANALYSIS_PROCESS_2 "/fahd/rmaddale/mac2sun4/bin/data_server_2"
# define DATA_HOST_NAME "fahd.gb.nrao.edu"

/*
** Message queue identification string
*/
# define MSG_QUEUE_ID "MsgQ"
