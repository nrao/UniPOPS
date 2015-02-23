/* ---------------------------------------------------------------------------
 *  @(#)140.h	5.1 06/22/94
 * ---------------------------------------------------------------------------
 *
 * SUN/MASSCOMP POPS SPECTRAL LINE FORMATS
*/

typedef struct spectral_line_disk_header
{
	float		scan_number;
	float		off_scan_number;
	char		source_name[12];
	short		observer_number;
	char		observer_name[18];
	double		julian_date;
	short		day_of_year;
	short		month;
	short		day;
	short		year;
	float		lst;
	float		est;
	short		telescope;
	short		observing_program;
	short		scan_type;
	short		mode;
	short		irec;
	short		lrec;
	short		incr;
	short		istart[4];
	short		istop[4];
	short		vref;
	short		vdef;
	short		position_code;
	short		scan_direction;
	short		unused_1;
	float		scan_duration;
	short		unused_2[2];
	float		integration_time;
	short		unused_3[4];
	float		epoch_of_obs;
	float		h_coord_rate;
	float		v_coord_rate;
	float		ra_ind;
	float		dec_ind;
	float		focus;
	float		orientation;
	float		zenith_distance;
	float		descriptive_origin[3];
	float		ra_pointing_fudge;
	float		dec_pointing_fudge;
	float		pvls_p1;
	float		pvls_p2;
	float		pvls_p3;
	float		ext_mark;
	short		unused_4[2];
	short		environment_values_1[2];
	float		zenith_opacity;
	short		environment_values_2[4];
	float		app_ra[4];
	float		app_dec[4];
	float		epoch_ra[4];
	float		epoch_dec[4];
	float		gal_l[4];
	float		gal_b[4];
	float		obs_h[4];
	float		obs_v[4];
	float		rho[4];
	float		theta[4];
	float		noise_tube[4];
	float		sig_system_temp[4];
	float		center_vel[4];
	float		delta_vel[4];
	float		rvsys[4];
	float		integration[4];
	float		bandwidth[4];
	float		theoretical_rms[4];
	short		unused_5[2];
	double		center_freq[4];
	double		rest_freq[4];
	char		c_f_formula[4][18];
	double		l1;
	double		l1f1;
	double		l1f2;
	double		l2;
	double		l2f1;
	double		l2f2;
	float		la;
	float		lb;
	float		lc;
	float		ld;
	short		a_c_words[22];
	short		error_words[52];
	short		parameters_monitor[35];
	char		user_project_code[6];
	float		ref_sys_temp[4];
	float		refpower_noise_off[4];
	float		refpower_noise_on[4];
	float		sigpower_noise_off[4];
	float		sigpower_noise_on[4];
	float		refchanzero_noise_off;
	float		refchanzero_noise_on;
	float		sigchanzero_noise_off;
	float		sigchanzero_noise_on;
} Line140Header;


typedef struct spectral_line_record
{
	Line140Header hdr;
	float signal[1024];
} Line140;

typedef struct spectral_line_spower_record
{
	Line140Header hdr;
	float signal[1024];
	float reference[1024];
} Line140SPower;

/*
** SUN/MASSCOMP POPS CONTINUUM FORMATS
*/

typedef struct condare_disk_header
{
	float		scan_number;
	short		unused_0[2];
	char		source_name[12];
	short		observer_number;
	char		observer_name[18];
	double		julian_date;
	short		day_of_year;
	short		month;
	short		day;
	short		year;
	float		lst;
	float		est;
	short		telescope;
	short		observing_program;
	short		scan_type;
	short		mode;
	short		first_channel;
	short		number_of_channels;
	short		samples_per_record;
	short		start;
	short		stop;
	short		unused_1[6];
	short		vref;
	short		vdef;
	short		position_code;
	short		scan_direction;
	short		subscan_number;
	short		feed_number;
	short		total_subscans;
	short		unused_2[2];
	float		sample_rate;
	float		lambda;
	short		unused_3[2];
	float		epoch_of_obs;
	float		h_coord_rate;
	float		v_coord_rate;
	float		ra_ind;
	float		dec_ind;
	float		focus;
	float		orientation;
	float		zenith_distance;
	float		descriptive_origin[3];
	float		ra_pointing_fudge;
	float		dec_pointing_fudge;
	float		pvls_p1;
	float		pvls_p2;
	float		pvls_p3;
	float		commanded_h;
	float		commanded_v;
	short		environment_values_1[2];
	float		zenith_opacity;
	short		environment_values_2[4];
	float		epoch_ra;
	float		epoch_dec;
	float		app_ra;
	float		app_dec;
	float		gal_l;
	float		gal_b;
	float		azimuth;
	float		elevation;
	float		horizontal_desc;
	float		vertical_desc;
	float		rho;
	float		theta;
	float		noise_tube;
	float		DCR_scale_factor;
	float		DCR_system_temp;
	short		unused_4;
	short		number_accum;
	float		accum_list[18];
	float 		signal[1];
} Cont140Header;


typedef struct continuum_record
{
	Cont140Header 	hdr;
} Cont140;
