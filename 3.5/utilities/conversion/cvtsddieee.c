
/*   @(#)cvtsddieee.c	5.1 06/22/94   */

#include <string.h>
#include <math.h>
#include <140.h> 
#define USEPAD
#include <sdd.h>

#define NINT(a)		((a) - floor(a) >= 0.5 ? ceil(a) : floor(a))

/*
lcvtsddieee takes an input structure of the sddformat format and
turns it into a structure of the Line140 format
*/
void lcvtsddieee_(istruct,ostruct,ind,errcode)
	sddformat *istruct;
	Line140 *ostruct;
	int *ind, *errcode;  /*  *ind is always 1   */ 

{
#define in istruct->header 
#define out ostruct->hdr

/* Constants */ 

        double pi = 3.141592654; 
	double lat140 = pi *38.43679111 / 180.;

/* Variables */  
 
	int i,j,k;
	int idayofyear,iday,imonth, iyear;
	char s[16];
	double hour_angle, elevation, day, est, tjd;
	extern double jdtodate(); 
        extern int dayofyear(); 
        extern double jd(); 
       
/* Header and data lengths */

        out.istart[*ind-1] = 1;  
        out.istop[*ind-1] = (in.data_length/4);  
	if ( (in.data_length/4) > 1024.) out.istop[*ind-1] = 1024;

/* Basic information */

	out.scan_number = in.scan_number; 
        out.observer_number = atoi(in.observer_initials);
	for (i=0;i<14;i++) out.observer_name[i] = in.observer_name[i]; 
        for (i=0;i<4;i++) out.observer_name[i+14] = in.project_ident[i]; 
  
        for (i=0;i<8;i++) s[i] = in.telescope[i]; 
        if (strncmp(s,"NRAO 43M",8) == 0) 
		out.telescope = 140;
        else if (strncmp(s,"NRAO 93M",8) == 0) 
		out.telescope = 300;
        else {
		*errcode = 3; return;
	} 

	for(i=0;i<12;i++) out.source_name[i] = in.source_name[i]; 
       
        for (i=0;i<8;i++) s[i] = in.type_data[i];
        if (strncmp(s,"LINETLPW",8) == 0){
           out.observing_program = 2;
           out.scan_type = 0; 
        }  
        else if (strncmp(s,"LINEFQSW",8) == 0) {
           out.observing_program = 1;
           out.scan_type = 2;
        }
        else if (strncmp(s,"LINEBMSW",8) == 0) {
           out.observing_program = 1;
           out.scan_type = 2;
	}
        else if (strncmp(s,"LINETPOF",8) == 0) {
           out.observing_program = 2;
           out.scan_type = 0;
	}
        else if (strncmp(s,"LINETPON",8) == 0) {
           out.observing_program = 2;
           out.scan_type = 1;
	}
        else {
	   *errcode = 4; return;
	}  
   
        for (i=0;i<8;i++) s[i] = in.back_end_desc[i];
        if (strncmp(s,"SPECPROC",8) == 0) 
	    out.observing_program = out.observing_program + 3;
	 
/* Pointing parameters */ 

        out.ra_pointing_fudge = in.user_azra_pointing/15; 
        out.dec_pointing_fudge = in.user_eldec_pointing;
        out.pvls_p1 = in.pointing_constant[0];
        out.pvls_p2 = in.pointing_constant[1];  
        out.pvls_p3 = in.pointing_constant[2]; 
        out.orientation = (pi*in.orient_angle)/180; 
        out.focus = in.radial_focus; 

/* Observing parameters */
 
        iyear = in.ut_date; 
        imonth = (in.ut_date - iyear)*100; 
        day = ((in.ut_date - iyear)*100 - imonth)*100; 
        iday = NINT(day); 
        jd_(&iyear,&imonth,&iday,&tjd);
	tjd += in.utime / 24.;
        out.julian_date = tjd - 2400000.; 
        tjd -= 5./24.;  
        est = in.utime - 5.0; 
        if( est < 0.0)  est += 24.0; 
	jdtodate_(&tjd,&iyear,&imonth,&day);
        out.day = day; 
        iday = day;  
        out.month = imonth; 
        out.year = iyear - 1900;     
        dayofyear_(&iyear,&imonth,&iday,&idayofyear); 
        out.day_of_year = idayofyear; 
        out.est = (est*pi*15)/180; 
        out.lst = (in.lst*pi*15)/180;  
        out.integration_time = in.length_sample; 
 
        if ((in.deltax_rate == 0) && (in.deltay_rate == 0))      
            out.scan_direction = 0; 
        else if (in.deltax_rate > 0) 
            out.scan_direction = 1;
        else if (in.deltax_rate < 0) 
            out.scan_direction = -1;
        else if (in.deltay_rate > 0) 
            out.scan_direction = 2;
        else out.scan_direction = -2; 

        out.mode = 1; 
        out.irec = 1;
        out.lrec = 1;
        out.incr = 1;

/* Positions */  
   
        out.epoch_of_obs = in.epoch;
        out.obs_h[*ind-1] = (pi*in.sourcex)/180;
        out.obs_v[*ind-1] = (pi*in.sourcey)/180;  
        out.epoch_ra[*ind-1] = (pi*in.epoch_ra)/180;
        out.epoch_dec[*ind-1] = (pi*in.epoch_dec)/180;
        out.gal_l[*ind-1] = (pi*in.galactic_l)/180;
        out.gal_b[*ind-1] = (pi*in.galactic_b)/180;
        out.ra_ind = (pi*in.xposition)/180;
        out.dec_ind = (pi*in.yposition)/180;
	for (i=0;i<3;i++) out.descriptive_origin[i] = in.desc_origin[i];
        out.zenith_distance = pi*(90.-in.elevation)/180.;    

        hour_angle = pi + atan2(sin(pi*in.azimuth/180), 
	    sin(lat140)*cos(pi*in.azimuth/180) - tan(pi*in.elevation/180)*cos(lat140) ); 
        out.app_ra[*ind-1] = out.lst - hour_angle;
	if (out.app_ra[*ind-1] < 0) 
	    out.app_ra[*ind-1] += 2*pi;
	else if (out.app_ra[*ind-1] >= 2*pi)
	    out.app_ra[*ind-1] -= 2*pi;
        out.app_dec[*ind-1] = asin( sin(pi*in.elevation/180)*sin(lat140) +
	    cos(pi*in.elevation/180)*cos(lat140)*cos(pi*in.azimuth/180) ); 

        for (i=0;i<8;i++) s[i] = in.coord_sys_code[i];
        if (strncmp(s,"        ",8) == 0) 
		out.position_code = 0;
        else if (strncmp(s,"APPHADC ",8) == 0) 
		out.position_code = 1;
        else if (strncmp(s,"EPOCRADC",8) == 0) 
		out.position_code = 2;
        else if (strncmp(s,"APPRADC ",8) == 0) 
		out.position_code = 3;
        else if (strncmp(s,"GALACTIC",8) == 0) 
		out.position_code = 4;
        else if (strncmp(s,"AZEL    ",8) == 0) 
		out.position_code = 5;
        else if (strncmp(s,"USERDEF ",8) == 0) 
		out.position_code = 6;
        else {
		*errcode = 5; return;
	}    

/* Environment */

        out.environment_values_1[0] = 128*in.amb_temp; 
        out.environment_values_2[2] = 12.8*in.pressure;
        out.environment_values_2[0] = 128*in.dew_point;
 
/* Map parameters */

        out.h_coord_rate = (pi*in.deltax_rate)/(180*60); 
        out.v_coord_rate = (pi*in.deltay_rate)/(180*60); 
 
/* Data parameters */

        out.off_scan_number = in.off_scan_nb;
        out.center_vel[*ind-1] = in.velocity_ref; 
        out.rvsys[*ind-1] = in.velocity_correct;  

        for(i=0;i<4;i++) s[i] = in.velocity_def_ref[i]; 
        if (strncmp(s,"RADI",4) == 0) 
		out.vdef = 1;
        else if (strncmp(s,"OPTL",4) == 0) 
		out.vdef = 2;
        else if (strncmp(s,"REVL",4) == 0) 
		out.vdef = 3;
        else {
		*errcode = 6; return;
	} 

        for (i=0;i<4;i++) s[i] = in.velocity_def_ref[i+4];   
        if (strncmp(s,"OBS ",4) == 0) 
		out.vref = 1;
        else if (strncmp(s,"LSR ",4) == 0) 
		out.vref = 2;
        else if (strncmp(s,"HELO",4) == 0) 
		out.vref = 3; 
        else if (strncmp(s,"EART",4) == 0) 
		out.vref = 4; 
        else {
		*errcode = 7; return;
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
        out.rho[*ind-1] = (pi*in.rho)/180;
        out.theta[*ind-1] = (pi*in.theta)/180;
	if (in.center_freq_formula[22] == 'L')
	{   
	   for (i=6;i<24;i++) 
              out.c_f_formula[*ind-1][i-6] = in.center_freq_formula[i]; 
	}
	else
	{
	   for (i=0;i<16;i++) 
              out.c_f_formula[*ind-1][i+2] = in.center_freq_formula[i]; 
	}

/* Receiver descriptor block */
 
        out.center_freq[*ind-1] = in.obs_freq;
        out.rest_freq[*ind-1] = in.rest_freq; 
        out.bandwidth[*ind-1] = in.bandwidth;
        out.noise_tube[*ind-1] = in.calibration_temp;
        out.sig_system_temp[*ind-1] = in.source_syst_temp;
        out.ref_sys_temp[*ind-1] = in.ref_sys_temp; 
        out.theoretical_rms[*ind-1] = in.std_deviation_mean;
        out.center_vel[*ind-1] = in.xvalue_ref_pt;
        out.delta_vel[*ind-1] = in.deltax;
        out.scan_duration = in.tot_integ_time;
        out.zenith_opacity = in.h2o_opacity; 
        out.integration[*ind-1] = in.effint;
 
/*  Data  */

	j = out.istart[*ind-1] - 1; 
	for (i=0;i< (out.istop[*ind-1] - out.istart[*ind-1] + 1);i++) 
            ostruct->signal[j++] = istruct->data[i];

/* End of conversion */
	*errcode=0;
	return;
}
/*
ccvtsddieee takes an input structure of the sddformat format and
turns it into a structure of the Cont140 format
*/
void ccvtsddieee_(istruct,ostruct,ind,errcode)
	sddformat *istruct;
	Cont140 *ostruct;
	int *ind, *errcode;  /*  *ind is always 1   */ 

{
#define in istruct->header 
#define out ostruct->hdr

/* Constants */ 

        double pi = 3.141592654; 
	double lat140 = pi *38.43679111 / 180.;

/* Variables */  
 
	int i,j,k;
	int idayofyear,iday,imonth, iyear;
	char s[16];
	double hour_angle, elevation, day, est, tjd;
	extern double jdtodate(); 
        extern int dayofyear(); 
        extern double jd(); 
       
/* Header and data lengths */

        out.start = 1;  
        out.stop = (in.data_length/4);  
	if ( (in.data_length/4) > 1193.) out.stop = 1193;

/* Basic information */

	out.scan_number = in.scan_number; 
        out.observer_number = atoi(in.observer_initials);
	for (i=0;i<14;i++) out.observer_name[i] = in.observer_name[i]; 
        for (i=0;i<4;i++) out.observer_name[i+14] = in.project_ident[i]; 
  
        for (i=0;i<8;i++) s[i] = in.telescope[i]; 
        if (strncmp(s,"NRAO 43M",8) == 0) 
		out.telescope = 140;
        else if (strncmp(s,"NRAO 93M",8) == 0) 
		out.telescope = 300;
        else {
		*errcode = 3; return;
	} 

	for(i=0;i<12;i++) out.source_name[i] = in.source_name[i]; 
       
        for (i=0;i<8;i++) s[i] = in.back_end_desc[i];
        if (strncmp(s,"STD A/D ",8) == 0) {
           for (i=0;i<8;i++) s[i] = in.type_data[i];
           if (strncmp(s,"CONTNOCL",8) == 0) {
              if ((in.deltax_rate == 0) && (in.deltay_rate == 0)) {     
                 out.observing_program = 3;
                 out.scan_type = 0;   
	      } 
	      else {
                 out.observing_program = 3;
                 out.scan_type = 4;  
	      } 
	   }
           else if (strncmp(s,"CONTPLCL",8) == 0) {
              out.observing_program = 3;
              out.scan_type = 1;
	   }
           else if (strncmp(s,"CONTCAL ",8) == 0) {
              out.observing_program = 3;
              out.scan_type = 2;
	   }
           else if (strncmp(s,"CONTONOF",8) == 0) {
              out.observing_program = 4;
              out.scan_type = 3;
	   }
           else {
	      *errcode = 4; return;
	   }  
        }  
        else if (strncmp(s,"DIGITAL ",8) == 0){
           out.observing_program = 8;
           out.scan_type = 0;   
        }  
        else {*errcode = 1; return;}  

   
/* Pointing parameters */ 

        out.ra_pointing_fudge = in.user_azra_pointing/15; 
        out.dec_pointing_fudge = in.user_eldec_pointing;
        out.pvls_p1 = in.pointing_constant[0];
        out.pvls_p2 = in.pointing_constant[1];  
        out.pvls_p3 = in.pointing_constant[2]; 
        out.orientation = (pi*in.orient_angle)/180; 
        out.focus = in.radial_focus; 

/* Observing parameters */
 
        iyear = in.ut_date; 
        imonth = (in.ut_date - iyear)*100; 
        day = ((in.ut_date - iyear)*100 - imonth)*100; 
        iday = NINT(day); 
        jd_(&iyear,&imonth,&iday,&tjd);
	tjd += in.utime / 24.;
        out.julian_date = tjd - 2400000.; 
        tjd -= 5./24.;  
        est = in.utime - 5.0; 
        if( est < 0.0)  est += 24.0; 
	jdtodate_(&tjd,&iyear,&imonth,&day);
        out.day = day; 
        iday = day;  
        out.month = imonth; 
        out.year = iyear - 1900;     
        dayofyear_(&iyear,&imonth,&iday,&idayofyear); 
        out.day_of_year = idayofyear; 
        out.est = (est*pi*15)/180; 
        out.lst = (in.lst*pi*15)/180;  
        out.sample_rate = in.tot_integ_time; 
 
        if ((in.deltax_rate == 0) && (in.deltay_rate == 0))      
            out.scan_direction = 0; 
        else if (in.deltax_rate > 0) 
            out.scan_direction = 1;
        else if (in.deltax_rate < 0) 
            out.scan_direction = -1;
        else if (in.deltay_rate > 0) 
            out.scan_direction = 2;
        else out.scan_direction = -2; 

        out.mode = 1; 
        out.first_channel = 1;
        out.number_of_channels = 1;
        out.samples_per_record = 1;

/* Positions */  
   
        out.epoch_of_obs = in.epoch;
        out.epoch_ra = (pi*in.epoch_ra)/180;
        out.epoch_dec = (pi*in.epoch_dec)/180;
        out.gal_l = (pi*in.galactic_l)/180;
        out.gal_b = (pi*in.galactic_b)/180;
        out.ra_ind = (pi*in.xposition)/180;
        out.dec_ind = (pi*in.yposition)/180;
        out.azimuth = (pi*in.azimuth)/180;
        out.elevation = (pi*in.elevation)/180; 
	for (i=0;i<3;i++) out.descriptive_origin[i] = in.desc_origin[i];
        out.zenith_distance = pi*(90.-in.elevation)/180.;    
	out.horizontal_desc = (pi*in.sourcex)/180;
	out.vertical_desc = (pi*in.sourcey)/180;

        hour_angle = pi + atan2(sin(pi*in.azimuth/180), 
	    sin(lat140)*cos(pi*in.azimuth/180) - tan(pi*in.elevation/180)*cos(lat140) ); 
        out.app_ra = out.lst - hour_angle;
	if (out.app_ra < 0) 
	    out.app_ra += 2*pi;
	else if (out.app_ra >= 2*pi)
	    out.app_ra -= 2*pi;
        out.app_dec = asin( sin(pi*in.elevation/180)*sin(lat140) +
	    cos(pi*in.elevation/180)*cos(lat140)*cos(pi*in.azimuth/180) ); 

        for (i=0;i<8;i++) s[i] = in.coord_sys_code[i];
        if (strncmp(s,"        ",8) == 0)
		out.position_code = 0;
        else if (strncmp(s,"APPHADC ",8) == 0)
		out.position_code = 1;
        else if (strncmp(s,"EPOCRADC",8) == 0)
		out.position_code = 2;
        else if (strncmp(s,"APPRADC ",8) == 0)
		out.position_code = 3;
        else if (strncmp(s,"GALACTIC",8) == 0)
		out.position_code = 4;
        else if (strncmp(s,"AZEL    ",8) == 0)
		out.position_code = 5;
        else if (strncmp(s,"USERDEF ",8) == 0)
		out.position_code = 6;
        else {
		*errcode = 5; return;
	}    

/* Environment */

        out.environment_values_1[0] = 128*in.amb_temp; 
        out.environment_values_2[2] = 12.8*in.pressure;
        out.environment_values_2[0] = 128*in.dew_point;
 
/* Map parameters */

        out.h_coord_rate = (pi*in.deltax_rate)/(180*60); 
        out.v_coord_rate = (pi*in.deltay_rate)/(180*60); 
  
/* Telescope dependent parameters */

        out.rho = (pi*in.rho)/180;
        out.theta = (pi*in.theta)/180;

/* Receiver descriptor block */
 
        out.DCR_scale_factor = in.freq_resolution;
        out.noise_tube = in.calibration_temp;
        out.DCR_system_temp = in.source_syst_temp;
        out.zenith_opacity = in.h2o_opacity; 
 
/*  Data  */

	j = out.start - 1; 
	for (i=0;i< (out.stop - out.start + 1);i++) 
           out.signal[j++] = istruct->data[i];


/* End of conversion */
	*errcode=0;
	return;
}
/*
Lineorcont takes an input structure of the sddformat format and
checks whether it is a line or continuum structure
*/
void lineorcont_(istruct,retncode)

	sddformat *istruct;
	int *retncode;  

{
#define in istruct->header 

	int i;
	char s[4];

        for (i=0;i<4;i++) s[i] = in.type_data[i];

        if (strncmp(s,"CONT",4) == 0)
           *retncode = 0;
        else if (strncmp(s,"LINE",4) == 0)
           *retncode = 1;
        else *retncode = -1;

	return;  
}

