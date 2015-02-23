
/* @(#)sp_hdr.h	5.1 06/22/94 */

typedef struct Mini_Header
{
	long scan;
	long record_id;
	long phase_id;
	long obs_mode;
	long backend;
	long precis;
	long end_scan;
	char source[16];
	long no_rchan;
	long no_int;
	long no_phases;
	float t_rms;
	long processor_mode;
	double iff;
	float freq_res;
	float bandwidth;
	float int_time;
	float eff_int;
	float t_dur;
	float t_sys;
	float t_ref;
	float phase_time;
	long num_rec;
	float data[2048];
} MiniHDR;

