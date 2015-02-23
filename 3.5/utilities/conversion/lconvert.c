
/* @(#)lconvert.c	5.1 06/22/94 */

#include <string.h>
#include <math.h>
#include <140.h>
#include <sdd.h>


/* Fillarray writes the string st into the array arr from arr[f] */
void fillarray (arr,st,f)
	char arr[];
	char *st;
	int f;
{
	char *u=st;
	int i,lg;
	lg=strlen(st);
	for (i=f;i<(f+lg);i++) {
	arr[i] = *u;
	u++;}
	return;
}

/*
Lconversion takes an input structure of the Line140 format and
turns it into a structure of the sddformat format
*/
void lconversion_(istruct,ostruct,ind,errcode)
	Line140 *istruct;
	sddformat *ostruct;
	int *ind, *errcode;

{
#define in istruct->hdr 
#define out ostruct->header

	int i,j,k,return_code, infeed, scanno, n0;
	int imonth, iyear, iday;
	char s[8];
	double pi=3.141592654;
	double diff = in.istop[*ind-1] - in.istart[*ind-1] + 1;
	double temp, day, hour_angle;
	double lat140 = pi *38.43679111 / 180.;
	extern double jdtodate();

/* test; converts only spectral line data file */
	switch(in.observing_program) {
	   case 1: case 2: break;
	   case 5: case 6: in.mode = 2*in.mode; break ;
	   default: *errcode = 1; return; 
	}

/* tests if the ind value corresponds to the number of one of the 
utilized receivers */
    	if (*ind < 1 || *ind > in.mode ) 
	{
	   *errcode=2; 
	   return;
	}

/*			class 0 information 	*/

	out.number_header_class = NUMBER_HEADER_CLASS;
	out.class_location[0] = CLASS_0 + 1;
	out.class_location[1] = out.class_location[0] + CLASS_1;
	out.class_location[2] = out.class_location[1] + CLASS_2;
	out.class_location[3] = out.class_location[2] + CLASS_3;
	out.class_location[4] = out.class_location[3] + CLASS_4;
	out.class_location[5] = out.class_location[4] + CLASS_5;
	out.class_location[6] = out.class_location[5] + CLASS_6;
	out.class_location[7] = out.class_location[6] + CLASS_7;
	out.class_location[8] = out.class_location[7] + CLASS_8;
	out.class_location[9] = out.class_location[8] + CLASS_9;
	out.class_location[10] = out.class_location[9] + CLASS_10;
	out.class_location[11] = out.class_location[10] + CLASS_11;
	out.class_location[12] = out.class_location[11] + CLASS_12;
	out.class_location[13] = out.class_location[12] + CLASS_13;
	out.class_location[14] = out.class_location[13] + CLASS_14;

/* Header and data lengths */

	out.header_length = sizeof(sddheader);
	out.data_length = diff*4;

/* Basic information */

	out.scan_number = in.scan_number + (*ind) / 100.0;

	i=0;
	do{
	   s[i++]=in.observer_number % 10 + '0';
	} while ((in.observer_number /= 10) > 0);

	for (k=0,j=i-1;j>=0;k++,j--) out.observer_initials[k] = s[j];
	for (i=0;i<14;i++) out.observer_name[i]=in.observer_name[i];

	switch (in.telescope){
	case 140 : fillarray(out.telescope,"NRAO 43M",0);
	           switch (in.observing_program) {
		      case 1: case 2: fillarray(out.back_end_desc,"1024ACIV",0);
				      break;
		      case 5: case 6: fillarray(out.back_end_desc,"SPECPROC",0);
				      break;
		      default: *errcode = 3; return;
		      }
	           break;	
	case 300 : fillarray(out.telescope,"NRAO 93M",0);
	           fillarray(out.back_end_desc,"384ACIII",0);
	           break;
	case 45  : fillarray(out.telescope,"NRAO 43M",0);
	           fillarray(out.back_end_desc,"        ",0);
	           break;
	default :  *errcode = 3; return;
	}

	for (i=0;i<4;i++) out.project_ident[i] = in.observer_name[i+14];
	for(i=0;i<12;i++) out.source_name[i] = in.source_name[i];

	switch(in.scan_type){
	case 0: fillarray(out.type_data,"LINETPOF",0); 	
		out.phase_id = 0;
		out.nb_phases = 1;
		break;
	case 1: fillarray(out.type_data,"LINETPON",0);
		out.phase_id = 0;
 		out.nb_phases = 1;
		break;
	case 2: if (in.l1 == in.l1f1 && in.l1 == in.l1f2) 
		{
			out.phase_id = 0;
			out.nb_phases = 2;
			fillarray(out.type_data,"LINEBMSW",0);
		} 
		else
		{
			out.phase_id = 0;
			out.nb_phases = 2;
			fillarray(out.type_data,"LINEFQSW",0);
		} 
		break;
	default:*errcode = 4; return;
	}

	fillarray(out.data_precision,"R*4     ",0);
	out.nb_records = in.scan_duration/in.integration_time;
	out.record_id = 0;

/* Pointing parameters */

	out.user_azra_pointing = 15*in.ra_pointing_fudge;
	out.user_eldec_pointing = in.dec_pointing_fudge;
	out.pointing_constant[0] = in.pvls_p1;
	out.pointing_constant[1] = in.pvls_p2;
	out.pointing_constant[2] = in.pvls_p3;
	out.orient_angle = (180*in.orientation)/pi;
	out.radial_focus = in.focus;

/* Observing parameters */

	temp=in.julian_date + 2400000;
	jdtodate_(&temp,&iyear,&imonth,&day);
	iday = day;
	out.ut_date = (iday + (imonth*100.) + (iyear*10000.)) / 10000;
	temp = ((in.est*180)/(pi*15)) + 5;
	if(temp > 24)
	   out.utime = temp - 24;
	else
	   out.utime = temp;
	out.lst = ((in.lst*180)/(pi*15));
	out.nb_receiv_channels = in.mode;
	out.nb_sw_variables = 1;
	out.length_cycle = 1;
	out.length_sample = in.integration_time;

/* Positions */

	out.epoch = in.epoch_of_obs;
	out.sourcex = (180*in.obs_h[*ind-1]) / pi;
	out.sourcey = (180*in.obs_v[*ind-1]) / pi;
	out.epoch_ra =(180*in.epoch_ra[*ind-1]) / pi;
	out.epoch_dec = (180*in.epoch_dec[*ind-1]) / pi;
	out.galactic_l = (180*in.gal_l[*ind-1]) / pi;
	out.galactic_b = (180*in.gal_b[*ind-1]) / pi;
	out.xposition = (180*in.ra_ind) / pi;
	out.yposition = (180*in.dec_ind) / pi;

	hour_angle = in.lst-in.app_ra[*ind-1];
	temp = cos(hour_angle)*sin(lat140)-tan(in.app_dec[*ind-1])*cos(lat140);
	out.azimuth = 180.+180.*atan2(sin(hour_angle),temp)/pi;
	temp = sin(in.app_dec[*ind-1])*sin(lat140) + 
		cos(in.app_dec[*ind-1])*cos(lat140)*cos(hour_angle);
	out.elevation = 180*asin(temp) / pi;

	for (i=0;i<3;i++) out.desc_origin[i] = in.descriptive_origin[i];

	switch (in.position_code) {
	case 0: fillarray(out.coord_sys_code,"        ",0); break;
	case 1: fillarray(out.coord_sys_code,"APPHADC ",0); break;
	case 2: fillarray(out.coord_sys_code,"EPOCRADC",0);break;
	case 3: fillarray(out.coord_sys_code,"APPRADC ",0); break;
	case 4: fillarray(out.coord_sys_code,"GALACTIC",0); break;
	case 5: fillarray(out.coord_sys_code,"AZEL    ",0); break;
	case 6: fillarray(out.coord_sys_code,"USERDEF ",0); break;
	default: *errcode = 5; return;
	}

/* Environment */

	out.amb_temp = in.environment_values_1[0] / 128.0;
	out.pressure = in.environment_values_2[2] / 12.80;
	out.dew_point = in.environment_values_2[0] / 128.0;

/* Map parameters */

	out.deltax_rate = 60*in.h_coord_rate*180/pi;
	out.deltay_rate = 60*in.v_coord_rate*180/pi;
	fillarray(out.xy_ref_frame_code,"CARTSCAN",0);

/* Data parameters */

	out.off_scan_nb = in.off_scan_number;
	out.velocity_ref = in.center_vel[*ind-1];
	out.velocity_correct = in.rvsys[*ind-1];

	switch (in.vdef) {
	case 1: fillarray(out.velocity_def_ref,"RADI",0); break;
	case 2: fillarray(out.velocity_def_ref,"OPTL",0); break;
	case 3: fillarray(out.velocity_def_ref,"REVL",0); break;
	default: *errcode = 6; return;
	}

	switch (in.vref) {
	case 1: fillarray(out.velocity_def_ref,"OBS ",4); break;
	case 2: fillarray(out.velocity_def_ref,"LSR ",4); break;
	case 3: fillarray(out.velocity_def_ref,"HELO",4); break;
	case 4: fillarray(out.velocity_def_ref,"EART",4); break;
	default: *errcode = 7; return;
	}

/* Telescope dependent parameters */

	out.l1 = in.l1;
	out.l1f1 = in.l1f1;
	out.l1f2 = in.l1f2;
	out.l2 = in.l2;
	out.l2f1 = in.l2f1;
	out.l2f2 = in.l2f2;
	out.la = in.la;
	out.lb =  in.lb;
	out.lc = in.lc;
	out.ld = in.ld;
	out.rho = (180*in.rho[*ind-1]) / pi;
	out.theta = (180*in.theta[*ind-1]) / pi;
	for (i=0;i<6;i++) out.center_freq_formula[i] = ' ';
	for (i=6;i<24;i++) 
		out.center_freq_formula[i] = in.c_f_formula[*ind-1][i-6];

/* Receiver descriptor block */

	out.obs_freq = in.center_freq[*ind-1];
	out.rest_freq = in.rest_freq[*ind-1];
	out.freq_resolution = - in.bandwidth[*ind-1] / diff; 
	out.bandwidth = in.bandwidth[*ind-1];  
	out.calibration_temp = in.noise_tube[*ind-1];
	out.source_syst_temp = in.sig_system_temp[*ind-1];
	out.ref_sys_temp = in.ref_sys_temp[*ind-1];
	out.std_deviation_mean = in.theoretical_rms[*ind-1];
	out.ref_point_nb = (diff / 2) + 1;
	out.xvalue_ref_pt = in.center_vel[*ind-1];
	out.deltax = in.delta_vel[*ind-1];
	out.tot_integ_time = in.scan_duration;
	out.nb_integrations = diff;
	out.starting_pt_nb = 1;
	out.h2o_opacity = in.zenith_opacity;
	out.effint = in.integration[*ind-1];

/*  Data  */

	switch(in.observing_program) {
	case 1: case 2:
	   j = in.istart[*ind-1] -1;
	   for (i=0;i<diff;i++) ostruct->data[i] = istruct->signal[j++];
	   break;
 	case 5: case 6:
	   /* Request for SP data  */
	   n0 = 0;
	   getsprec_(ostruct,ind,&n0,&n0,errcode);
	   if (*errcode != 0) return;
	   break;
	default: *errcode = 5; return;
	}


/* End of conversion */
	*errcode=0;
	return;
}

/*
Cconversion takes an input structure of the Cont140 format and
turns it into a structure of the sddformat format
*/
void cconversion_(istruct,ostruct,ind,errcode)
	Cont140 *istruct;
	sddformat *ostruct;
	int *ind, *errcode;

{
#define in istruct->hdr 
#define out ostruct->header

	int i,j,k;
	int imonth, iyear, iday;
	char s[8];
	double pi=3.141592654;
	double diff = in.stop - in.start + 1;
	double temp, day, temp2, hour_angle;
	extern double jdtodate();
	double lat140 = pi *38.43679111 / 180.;

/* test; converts only continuum data file */
	switch(in.observing_program) {
	case 3: case 4: fillarray(out.back_end_desc,"STD A/D ",0); break ;
	case 8:         fillarray(out.back_end_desc,"DIGITAL ",0); break ;
	default: *errcode = 1; return;
	}

/*		class 0		*/

	out.number_header_class = NUMBER_HEADER_CLASS;
	out.class_location[0] = CLASS_0 + 1;
	out.class_location[1] = out.class_location[0] + CLASS_1;
	out.class_location[2] = out.class_location[1] + CLASS_2;
	out.class_location[3] = out.class_location[2] + CLASS_3;
	out.class_location[4] = out.class_location[3] + CLASS_4;
	out.class_location[5] = out.class_location[4] + CLASS_5;
	out.class_location[6] = out.class_location[5] + CLASS_6;
	out.class_location[7] = out.class_location[6] + CLASS_7;
	out.class_location[8] = out.class_location[7] + CLASS_8;
	out.class_location[9] = out.class_location[8] + CLASS_9;
	out.class_location[10] = out.class_location[9] + CLASS_10;
	out.class_location[11] = out.class_location[10] + CLASS_11;
	out.class_location[12] = out.class_location[11] + CLASS_12;
	out.class_location[13] = out.class_location[12] + CLASS_13;
	out.class_location[14] = out.class_location[13] + CLASS_14;

/* Header and data lengths */

	out.header_length = sizeof(sddheader);
	out.data_length = diff*4;

/* Basic information */

	if (*ind > 0 && *ind <= 99) out.scan_number = in.scan_number + (*ind) / 100.0;
	else if (*ind == 0) out.scan_number = in.scan_number + .98;
	else out.scan_number = in.scan_number + .99;

	i=0;
	do{
	s[i++]=in.observer_number % 10 + '0';
	} while ((in.observer_number /= 10) > 0);

	for (k=0,j=i-1;j>=0;k++,j--) out.observer_initials[k] = s[j];
	for (i=0;i<14;i++) out.observer_name[i]=in.observer_name[i];

	switch (in.telescope){
	case 140 : fillarray(out.telescope,"NRAO 43M",0);
	           break;	
	case 300 : fillarray(out.telescope,"NRAO 93M",0);
	           break;
	case 45  : fillarray(out.telescope,"NRAO 43M",0);
	           break;
	default :  *errcode = 3; return;
	}

	for (i=0;i<4;i++) out.project_ident[i] = in.observer_name[i+14];
	for(i=0;i<12;i++) out.source_name[i] = in.source_name[i];

	switch(in.scan_type){
	case 0: fillarray(out.type_data,"CONTNOCL",0); break;
	case 1: fillarray(out.type_data,"CONTPLCL",0); break;
	case 2: fillarray(out.type_data,"CONTCAL ",0); break;
	case 3: fillarray(out.type_data,"CONTONOF",0); break;
	case 4: fillarray(out.type_data,"CONTNOCL",0); break;
	default:*errcode = 4; return;
	}

	fillarray(out.data_precision,"R*4     ",0);

/* Pointing parameters */

	out.user_azra_pointing = 15*in.ra_pointing_fudge;
	out.user_eldec_pointing = in.dec_pointing_fudge;
	out.pointing_constant[0] = in.pvls_p1;
	out.pointing_constant[1] = in.pvls_p2;
	out.pointing_constant[2] = in.pvls_p3;
	out.orient_angle = (180*in.orientation)/pi;
	out.radial_focus = in.focus;

/* Observing parameters */

	temp=in.julian_date + 2400000;
	jdtodate_(&temp,&iyear,&imonth,&day);
	iday = day;
	out.ut_date = (iday + (imonth*100.) + (iyear*10000.)) / 10000;
	temp = ((in.est*180)/(pi*15)) + 5;
	if(temp > 24)
	   out.utime = temp - 24;
	else
	   out.utime = temp;
	out.lst = ((in.lst*180)/(pi*15));
	out.nb_receiv_channels = 1;
	out.nb_sw_variables = 1;
	out.nb_phases = 1;
	out.length_cycle = 1;
	out.length_sample = in.sample_rate;

/* Positions */

	out.epoch = in.epoch_of_obs;
	out.epoch_ra =(180*in.epoch_ra) / pi;
	out.epoch_dec = (180*in.epoch_dec) / pi;
	out.galactic_l = (180*in.gal_l) / pi;
	out.galactic_b = (180*in.gal_b) / pi;
	out.xposition = (180*in.ra_ind) / pi;
	out.yposition = (180*in.dec_ind) / pi;

	hour_angle = in.lst-in.app_ra;
	temp = cos(hour_angle)*sin(lat140)-tan(in.app_dec)*cos(lat140);
	out.azimuth = 180.+180.*atan2(sin(hour_angle),temp)/pi;
	temp = sin(in.app_dec)*sin(lat140) + 
		cos(in.app_dec)*cos(lat140)*cos(hour_angle);
	out.elevation = 180*asin(temp) / pi;

	for (i=0;i<3;i++) out.desc_origin[i] = in.descriptive_origin[i];

	switch (in.position_code) {
	case 0: fillarray(out.coord_sys_code,"        ",0); break;
	case 1: fillarray(out.coord_sys_code,"APPHADC ",0); 
		out.sourcex = (180*(in.lst-in.app_ra)) / pi;
		out.sourcey = (180*in.app_dec) / pi;break;
	case 2: fillarray(out.coord_sys_code,"EPOCRADC",0);
		out.sourcex = (180*in.epoch_ra) / pi;
		out.sourcey = (180*in.epoch_dec) / pi;break;
	case 3: fillarray(out.coord_sys_code,"APPRADC ",0);
		out.sourcex = (180*in.app_ra) / pi;
		out.sourcey = (180*in.app_dec) / pi;break;
	case 4: fillarray(out.coord_sys_code,"GALACTIC",0);
		out.sourcex = (180*in.gal_l) / pi;
		out.sourcey = (180*in.gal_b) / pi;break;
	case 5: fillarray(out.coord_sys_code,"AZEL    ",0);
		out.sourcex = (180*in.azimuth) / pi;
		out.sourcey = (180*in.elevation) / pi;break;
	case 6: fillarray(out.coord_sys_code,"USERDEF ",0);
		out.sourcex = (180*in.horizontal_desc) / pi;
		out.sourcey = (180*in.vertical_desc) / pi;break;
	default: *errcode = 5; return;
	}

/* Environment */

	out.amb_temp = in.environment_values_1[0] / 128.0;
	out.pressure = in.environment_values_2[2] / 12.80;
	out.dew_point = in.environment_values_2[0] / 128.0;

/* Map parameters */

	out.deltax_rate = 60*in.h_coord_rate*180/pi;
	out.deltay_rate = 60*in.v_coord_rate*180/pi;
	fillarray(out.xy_ref_frame_code,"CARTSCAN",0);

/* Telescope dependent parameters */

	out.rho = (180*in.rho) / pi;
	out.theta = (180*in.theta) / pi;

/* Receiver descriptor block */

	out.freq_resolution = in.DCR_scale_factor; 
	out.bandwidth = 0.0;
	out.calibration_temp = in.noise_tube;
	out.source_syst_temp = in.DCR_system_temp;
	out.ref_point_nb = 1;
	temp = out.deltax_rate * cos(out.sourcey*pi/180.);
	temp2 = out.deltay_rate;
	out.deltax = in.sample_rate * hypot(temp, temp2) / 3600.;
	out.xvalue_ref_pt = 0.0;
	out.tot_integ_time = in.sample_rate;
	out.nb_integrations = diff;
	out.starting_pt_nb = 1;
	out.h2o_opacity = in.zenith_opacity;

/*  Data  */

	for (i=0;i<diff;i++) ostruct->data[i] = in.signal[i];

/* End of conversion */
	*errcode=0;
	return;
}

