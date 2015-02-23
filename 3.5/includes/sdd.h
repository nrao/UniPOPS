/*
 * ---------------------------------------------------------------------------
 *  @(#)sdd.h	5.4 09/10/98
 * ---------------------------------------------------------------------------
 *
 * SDD DISK FORMAT
 *
 *
 *  If USETUC has been previously definied, class 9 for Tucson will be used
 *  otherwise the Green Bank class 9 will be used.
 *
 *  If USEPAD has been previously definied, classes will be padded out to
 *  25 doubles per class.  This is the internal unipops sdd implementation.
 *
*/

#define NUMBER_HEADER_CLASS 13

/*		class 11 is still in the proto type phase */
/*		for now, a number of the parameters here are */
/*		dependant on each other, modify them carefully */
/*		the size in the sdd_header struct depends on these */

/*	class 11 basic entities */

typedef struct c11_orig_ent {
   double variable_value;
   char   variable_desc[8];
   char   phase_table[8];
} CL11ORIGENT;

typedef struct cl11_proto_ent {
   double variable_value;
   char   variable_desc[8];
   char   phase_table[32];
} CL11PROTOENT;

typedef struct cl11_orig {
   CL11ORIGENT ent[22];
} CL11ORIG;

typedef struct cl11_proto {
   double nb_sw_variables_fast;
   double nb_cycles;
   double nb_cycles_fast;
   double nb_phases_fast;
   double length_cycle_fast;
   double length_sample_fast;
   CL11PROTOENT ent[10];
} CL11PROTO;

#ifdef USEPAD
#define PAD_CLASS_SIZE 25	/* full size when padded */
#endif

typedef struct sdd_header
{
#define CLASS_0 4		/* size of class 0 in R*8 words */
	short		number_header_class;
        short		class_location[15];

#define CLASS_1	17		/* size of class 1 in R*8 words */
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
	double		save_bin;
	double		nb_records;
	double		record_id;
#ifdef USEPAD
#define SPARE_1 (PAD_CLASS_SIZE - CLASS_1)
	double		spares_1[SPARE_1];
#endif

#define CLASS_2 13		/* size of class 2 in R*8 words */
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
#ifdef USEPAD
#define SPARE_2 (PAD_CLASS_SIZE - CLASS_2)
	double		spares_2[SPARE_2];
#endif

#define CLASS_3	10		/* size of class 3 in R*8 words */
	double		ut_date;
	double		utime;
	double		lst;
	double		nb_receiv_channels;
	double		nb_sw_variables;
	double		nb_phases;
	double		length_cycle;
	double		length_sample;
        char		class11_type[8];
	double		phase_id;
#ifdef USEPAD
#define SPARE_3 (PAD_CLASS_SIZE - CLASS_3)
	double		spares_3[SPARE_3];
#endif

#define CLASS_4	17		/* size of class 4 in R*8 words */
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
#ifdef USEPAD
#define SPARE_4 (PAD_CLASS_SIZE - CLASS_4)
	double		spares_4[SPARE_4];
#endif

#define CLASS_5	6		/* size of class 5 in R*8 words */
	double		amb_temp;
	double		pressure;
	double		relative_humidity;
	double		index_refraction;
	double		dew_point;
	double		mm_h2o;
#ifdef USEPAD
#define SPARE_5 (PAD_CLASS_SIZE - CLASS_5)
	double		spares_5[SPARE_5];
#endif

#define CLASS_6	11		/* size of class 6 in R*8 words */
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
#ifdef USEPAD
#define SPARE_6 (PAD_CLASS_SIZE - CLASS_6)
	double		spares_6[SPARE_6];
#endif

#define CLASS_7	7		/* size of class 7 in R*8 words */
	double		beam;
	double		off_scan_nb;
	double		bad_channel_value;
	double		velocity_correct;
	double		velocity_ref;
	char		velocity_def_ref[8];
	char		type_calibration[8];
#ifdef USEPAD
#define SPARE_7 (PAD_CLASS_SIZE - CLASS_7)
	double		spares_7[SPARE_7];
#endif

#define CLASS_8	5		/* size of class 8 in R*8 words */
	double		antenna_aperature_eff;
	double		antenna_beam_eff;
	double		antenna_gain;
	double		etal;
	double		etalfss;
#ifdef USEPAD
#define SPARE_8 (PAD_CLASS_SIZE - CLASS_8)
	double		spares_8[SPARE_8];
#endif

#ifdef USETUC
#define CLASS_9	25		/* size of class 9 in R*8 words */
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
#ifdef USEPAD
#define SPARE_9 0
#endif
#else
#define CLASS_9	18		/* size of class 9 in R*8 words */
	double		l1;
	double		l1f1;
	double		l1f2;
	double		l2;
	double		l2f1;
	double		l2f2;
	double		la;
	double		lb;
	double		lc;
	double		ld;
	double		level_correct;
	double		pointing_fudge[2];
	double		rho;
	double		theta;
	char		center_freq_formula[24];
#ifdef USEPAD
#define SPARE_9 (PAD_CLASS_SIZE - CLASS_9)
	double		spares_9[SPARE_9];
#endif
#endif

#define CLASS_10 10		/* size of class 10 in R*8 words */
	char		open_param_values[80];
#ifdef USEPAD
#define SPARE_10 (PAD_CLASS_SIZE - CLASS_10)
	double		spares_10[SPARE_10];
#endif

#define CLASS_11 66		/* size of class 11 in R*8 words */
	union {
		CL11ORIG	orig;
        	CL11PROTO	proto;
        } cl11;
#ifdef USEPAD
#define SPARE_11 0
#endif
/*		class 11 is not currently padded, its larger than 25 */



#define CLASS_12 24		/* size of class 12 in R*8 words */
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
#ifdef USEPAD
#define SPARE_12 (PAD_CLASS_SIZE - CLASS_12)
	double		spares_12[SPARE_12];
#endif

#define CLASS_13 7		/* size of class 13 in R*8 words */
	double		nb_scans_stacked;
	double		f_scan_stack;
	double		l_scan_stack;
	double		line_amplitude;
	double		line_width;
	double		integrated_line_int;
	double		rms_noise;
#ifdef USEPAD
#define SPARE_13 (PAD_CLASS_SIZE - CLASS_13)
	double		spares_13[SPARE_13];
#endif

#define CLASS_14 0		/* size of class 14 in R*8 words */
#ifdef USEPAD
#define SPARE_14 (PAD_CLASS_SIZE - CLASS_14)
	double		spares_14[SPARE_14];
#endif

#define CLASS_15 0		/* there is no class 15 at this time */
#ifdef USEPAD
#define SPARE_15 0
#endif

} sddheader;


typedef struct class_locations {
	short 	number_header_class;
	short	class_location[15];
	} Class_0;


#ifndef MAX_DATA_POINTS
#define MAX_DATA_POINTS 16384		/* max number of data values */
#endif

typedef struct sdd_format
{
	sddheader header;
	float data[MAX_DATA_POINTS];
} sddformat;


/*		some usefull defines		*/

#define MAX_DATA_LENGTH (MAX_DATA_POINTS * sizeof(float)) 
#define SDDBUFSIZE 512			/* bytes per block */

/*	sdd bootstrap block structure		*/

#define OLDBSZERO 249
typedef struct OLD_SDD_Bootstrap_Block {
	short num_index_rec;		/* # of non-data rec, includes bs */ 
	short num_data_rec;		/* # of data records */
	short bytperrec;		/* # of bytes in 1 record */
	short bytperent;		/* # of bytes in 1 index entry */
	short num_entries_used;		/* # of larged index entry in use */
        short counter;			/* counter used by access routines */
	short typesdd;			/* 0 if data, 1 if Records  */
	short zeros[OLDBSZERO];		/* unused */
	} old_sdd_bs_block;

/*	sdd index entry structure		*/

#define OLDINDZERO 2
typedef struct OLD_SDD_Index_Entry {
	short start_rec, end_rec, magic, pos_code;
	float h_coord, v_coord;
	char source[16];
	float scan, freq_res;
	double rest_freq;
	float lst, UT;
	short obsmode;
        short phase_rec;
        short zeros[OLDINDZERO];
        } old_sdd_index_entry;

#define BSZERO 120
typedef struct SDD_Bootstrap_Block {
	int num_index_rec;		/* # of non-data rec, includes bs */ 
	int num_data_rec;		/* # of data records */
	int bytperrec;			/* # of bytes in 1 record */
	int bytperent;			/* # of bytes in 1 index entry */
	int num_entries_used;		/* # of larged index entry in use */
        int counter;			/* counter used by access routines */
	int typesdd;			/* 0 if data, 1 if Records  */
	int sddversion;			/* 1 for this type */
	int zeros[BSZERO];		/* unused */
	} sdd_bs_block;

/*	sdd index entry structure		*/

#define INDZERO 1
typedef struct SDD_Index_Entry {
	int start_rec, end_rec;
	float h_coord, v_coord;
	char source[16];
	float scan, freq_res;
	double rest_freq;
	float lst, UT;
	short obsmode;
        short phase_rec;
	short pos_code;
        short zeros[INDZERO];
	} sdd_index_entry;
