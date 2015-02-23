/* ---------------------------------------------------------------------------
 *  @(#)tuc_online_sdd.h	5.2 09/10/98
 * ---------------------------------------------------------------------------

/*
 * on_line data structure for pdfl files written at the 12-meter
 * This is from pdfl.h version 1.9 (9/24/92) of the cactus system 
 */

typedef struct tuc_sdd_header
{
	short		number_header_class;
        short		class_location[15];

	double		header_length;
	double		data_length;
	double		scan_number;
	char		observer_initials[8];
	char		observer_name[16];
	char		telescope[8];
	char		project_ident[8];
	char		source_name[16];
	char		type_data[8];
	char		front_end_desc[8];
	char		back_end_desc[8];
	char		data_precision[8];

	double		tot_azra_pointing;
	double		tot_eldec_pointing;
	double		user_azra_pointing;
	double		user_eldec_pointing;
	double		pointing_constant[4];
	double		orient_angle;
	double		radial_focus;
	double		north_south_focus;
	double		east_west_focus;
	char		pt_model[8];

	double		ut_date;
	double		utime;
	double		lst;
	double		nb_receiv_channels;
	double		nb_sw_variables;
	double		nb_phases;
	double		length_cycle;
	double		length_sample;
	char		class11_type[8];

	double		epoch;
	double		sourcex;
	double		sourcey;
	double		referencex;
	double		referencey;
	double		epoch_ra;
	double		epoch_dec;
	double		galactic_l;
	double		galactic_b;
	double		azimuth;
	double		elevation;
	double		xposition;
	double		yposition;
	double		desc_origin[3];
	char		coord_sys_code[8];

	double		amb_temp;
        double          pressure;
        double          relative_humidity;
        double          index_refraction;
        double          dew_point;
        double          mm_h2o;

	double		map_scan_angle;
	double		xposition_zero;
	double		yposition_zero;
 	double		deltax_rate;
	double		deltay_rate;
	double		nb_grid_pt;
	double		x_grid_pt;
	double		y_grid_pt;
	double		x_grid_cell_nb;
	double		y_grid_cell_nb;
	char		xy_ref_frame_code[8];

	double		beam;
	double		off_scan_nb;
	double		bad_channel_value;
	double		velocity_correct;
	double		velocity_ref;
	char		velocity_def_ref[8];
	char		type_calibration[8];

	double		antenna_aperature_eff;
        double          antenna_beam_eff;
        double          antenna_gain;
        double          etal;
        double          etalfss;

	double 		synth_freq;
	double		lo_factor;
	double		harmonic;
	double		lo_if;
	double		first_if;
	double		ref_AZ_offset;
	double		ref_EL_offset;
	double		beam_throw;
	double		beam_orient;
	double		bl_offset;
	double		obs_tol;
	double		sideband;
	double		wavelength;
	double		gain_scan;
	double		p_beam[2];
	double		m_beam[2];
	double		RA_DEC_offsets[4];
	double		freq_off_signal;
	double		freq_off_ref_1;
	double		freq_off_ref_2;

	char		open_param_values[80];

	double		nb_sw_variables_fast;
        double		nb_cycles;
        double		nb_cycles_fast;
        double		nb_phases_fast;
        double		length_cycle_fast;
        double		length_sample_fast;
        struct {
           double          variable_value;
           char            variable_desc[8];
           char            phase_table[32];
        } cl11ent[10];
/*		10 used here but only 5 in the original pdfl.h */

	double		obs_freq;
	double		rest_freq;
	double		freq_resolution;
	double		bandwidth;
	double		receiver_temp;
	double		calibration_temp;
	double		source_syst_temp;
	double		ref_sys_temp;
	double		source_temp;
	double		std_deviation_mean;
	double		ref_point_nb;
	double		xvalue_ref_pt;
	double		deltax;
	double		tot_integ_time;
	double		nb_integrations;
	double		starting_pt_nb;
	double		h2o_opacity;
	double		h2o_temp;
	double		o2_opacity;
	double		o2_temp;
	char		polarization[8];
	double		effint;
	char		receiver_info[16];

	double		nb_scans_stacked;
	double		f_scan_stack;
	double		l_scan_stack;
	double		line_amplitude;
	double		line_width;
	double		integrated_line_int;
	double		rms_noise;
	double		save[38];

} tucsddheader;

#ifndef MAX_DATA_POINTS
#define MAX_DATA_POINTS 16284  /* max number of data values */
#endif

typedef struct tuc_sdd_format
{
	tucsddheader header;
	float data[MAX_DATA_POINTS];
} tucsddformat;

/*		the default class pointers for this structure	*/

#define TUC_NUMBER_HEADER_CLASS 13
#define TUC_CLASS_1 5
#define TUC_CLASS_2 19
#define TUC_CLASS_3 32
#define TUC_CLASS_4 41
#define TUC_CLASS_5 58
#define TUC_CLASS_6 64
#define TUC_CLASS_7 75
#define TUC_CLASS_8 82
#define TUC_CLASS_9 87
#define TUC_CLASS_10 112
#define TUC_CLASS_11 122		
#define TUC_CLASS_12 188
#define TUC_CLASS_13 212
#define TUC_CLASS_14 257
#define TUC_CLASS_15 0
