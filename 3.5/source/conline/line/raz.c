
/* @(#)raz.c	5.2 08/12/98 */

/* For non-USETUC use : */
/* If USEPAD is definied before compilation, raz_ results */
/* If USEPAD is UNDEFINIED -> unpadraz_ results */

/* For USETUC use : */
/* USETUC implies NO padding (USEPAD undefined) */
/* tucraz_ is generated */

/* undefine USEPAD if USETUC is definied, if necessary */

#ifdef USETUC
#  undef USEPAD
#endif

#include <sdd.h>

#ifdef USETUC
void tucraz_(ostruct)
#else
#  ifdef USEPAD 
void raz_(ostruct)
#  else
void unpadraz_(ostruct)
#  endif
#endif
sddformat *ostruct;

{
#define O ostruct->header
int i,j;
double dinf;
float rinf;

long int *xx;
long int *yy;
xx = (long *) &dinf;
xx[0] = 0x7FF00000;
xx[1] = 0x00000000; 
/* xx[0] = 0x7FFFFFFF;
xx[1] = 0x00000001; */
yy = (long *) &rinf;
yy[0] = 0x7F800000; 
/* yy[0] = 0x7FFFFFFF; */

O.number_header_class = NUMBER_HEADER_CLASS;
O.class_location[0] = CLASS_0 + 1;
O.class_location[1] = O.class_location[0] + CLASS_1;
O.class_location[2] = O.class_location[1] + CLASS_2;
O.class_location[3] = O.class_location[2] + CLASS_3;
O.class_location[4] = O.class_location[3] + CLASS_4;
O.class_location[5] = O.class_location[4] + CLASS_5;
O.class_location[6] = O.class_location[5] + CLASS_6;
O.class_location[7] = O.class_location[6] + CLASS_7;
O.class_location[8] = O.class_location[7] + CLASS_8;
#ifdef USEPAD
O.class_location[9] = O.class_location[8] + CLASS_9 + SPARE_9;
#else
O.class_location[9] = O.class_location[8] + CLASS_9;
#endif
O.class_location[10] = O.class_location[9] + CLASS_10;
O.class_location[11] = O.class_location[10] + CLASS_11;
O.class_location[12] = O.class_location[11] + CLASS_12;
O.class_location[13] = O.class_location[12] + CLASS_13;
O.class_location[14] = O.class_location[13] + CLASS_14;

O.header_length = (O.class_location[14] + CLASS_15  - 1) * 8;
O.data_length = 0;
O.scan_number = dinf;
for (i=0;i<8;O.observer_initials[i++]=' ');
for (i=0;i<16;O.observer_name[i++]=' ');
for (i=0;i<8;O.telescope[i++]=' ');
for (i=0;i<8;O.project_ident[i++]=' ');
for (i=0;i<16;O.source_name[i++]=' ');
for (i=0;i<8;O.type_data[i++]=' ');
for (i=0;i<8;O.front_end_desc[i++]=' ');
for (i=0;i<8;O.back_end_desc[i++]=' ');
O.data_precision[0]='R';
O.data_precision[1]='*';
O.data_precision[2]='4';
for (i=3;i<8;O.data_precision[i++]=' ');
O.save_bin = dinf;
O.nb_records = dinf;
O.record_id = 0.;

O.tot_azra_pointing = dinf;
O.tot_eldec_pointing = dinf;
O.user_azra_pointing= dinf;
O.user_eldec_pointing = dinf;
for (i=0;i<4;O.pointing_constant[i++]=dinf);
O.orient_angle = dinf;
O.radial_focus = dinf;
O.north_south_focus = dinf;
O.east_west_focus = dinf;
for (i=0;i<8;O.pt_model[i++]=' ');

O.ut_date = dinf;
O.utime = dinf;
O.lst = dinf;
O.nb_receiv_channels = dinf;
O.nb_sw_variables = dinf;
O.nb_phases = dinf;
O.length_cycle = dinf;
O.length_sample = dinf;
for (i=0;i<8;O.class11_type[i++]=' ');
O.phase_id = 0.;

O.epoch = dinf;
O.sourcex = dinf;
O.sourcey = dinf;
O.referencex = dinf;
O.referencey = dinf;
O.epoch_ra = dinf;
O.epoch_dec = dinf;
O.galactic_l = dinf;
O.galactic_b= dinf;
O.azimuth = dinf;
O.elevation = dinf;
O.xposition = dinf;
O.yposition = dinf;
for (i=0;i<3;O.desc_origin[i++]=dinf);
for (i=0;i<8;O.coord_sys_code[i++]=' ');

O.amb_temp = dinf;
O.pressure = dinf;
O.relative_humidity = dinf;
O.index_refraction = dinf;
O.dew_point = dinf;
O.mm_h2o = dinf;

O.map_scan_angle = dinf;
O.xposition_zero = dinf;
O.yposition_zero = dinf;
O.deltax_rate = dinf;
O.deltay_rate = dinf;
O.nb_grid_pt = dinf;
O.x_grid_pt = dinf;
O.y_grid_pt = dinf;
O.x_grid_cell_nb = dinf;
O.y_grid_cell_nb = dinf;
for (i=0;i<8;O.xy_ref_frame_code[i++] = ' ');

O.beam=dinf;
O.off_scan_nb = dinf;
O.bad_channel_value = dinf;
O.velocity_correct = dinf;
O.velocity_ref = dinf;
for (i=0;i<8;O.velocity_def_ref[i++]=' ');
for (i=0;i<8;O.type_calibration[i++]=' ');

O.antenna_aperature_eff = dinf;
O.antenna_beam_eff = dinf;
O.antenna_gain = dinf;
O.etal = dinf;
O.etalfss = dinf;

#ifdef USETUC
   O.synth_freq = dinf;
   O.lo_factor = dinf;
   O.harmonic = dinf;
   O.lo_if = dinf;
   O.first_if = dinf;
   O.ref_AZ_offset = dinf;
   O.ref_EL_offset = dinf;
   O.beam_throw = dinf;
   O.beam_orient = dinf;
   O.bl_offset = dinf;
   O.obs_tol = dinf;
   O.sideband = dinf;
   O.wavelength = dinf;
   O.gain_scan = dinf;
   for (i=0;i<2;O.p_beam[i++] = dinf);
   for (i=0;i<2;O.m_beam[i++] = dinf);
   for (i=0;i<4;O.RA_DEC_offsets[i++] = dinf);
   O.freq_off_signal = dinf;
   O.freq_off_ref_1 = dinf;
   O.freq_off_ref_2 = dinf;
#else
   O.l1 = dinf;
   O.l1f1 = dinf;
   O.l1f2= dinf;
   O.l2 = dinf;
   O.l2f1 = dinf;
   O.l2f2 = dinf;
   O.la = dinf;
   O.lb = dinf;
   O.lc = dinf;
   O.ld = dinf;
   O.level_correct = dinf;
   for (i=0;i<2;O.pointing_fudge[i++] = dinf);
   O.rho = dinf;
   O.theta = dinf;
   for (j=0;j<24;j++)
   O.center_freq_formula[j]=' ';
#endif

for (i=0;i<80;O.open_param_values[i++]=' ');

/*	the original class 11 is the default */

for (j=0;j<22;j++) {
   O.cl11.orig.ent[j].variable_value = dinf;
   for (i=0;i<8;O.cl11.orig.ent[j].variable_desc[i++]=' ');
   for (i=0;i<8;O.cl11.orig.ent[j].phase_table[i++]=' ');
}

O.obs_freq = dinf;
O.rest_freq = dinf;
O.freq_resolution = dinf;
O.bandwidth = dinf;
O.receiver_temp = dinf;
O.calibration_temp = dinf;
O.source_syst_temp = dinf;
O.ref_sys_temp = dinf;
O.source_temp = dinf;
O.std_deviation_mean = dinf;
O.ref_point_nb= 0.;
O.xvalue_ref_pt = 0.;
O.deltax = 0.;
O.tot_integ_time = dinf;
O.nb_integrations = 0.;
O.starting_pt_nb = 1.;
O.h2o_opacity = dinf;
O.h2o_temp= dinf;
O.o2_opacity = dinf;
O.o2_temp = dinf;
for (i=0;i<8;O.polarization[i++]=' ');
O.effint = dinf;
for (i=0;i<16;O.receiver_info[i++]=' ');

O.nb_scans_stacked = 0.;
O.f_scan_stack = dinf;
O.l_scan_stack = dinf;
O.line_amplitude = dinf;
O.line_width = dinf;
O.integrated_line_int = dinf;
O.rms_noise = dinf;

for (i=0;i<MAX_DATA_POINTS;ostruct->data[i++] = rinf); 

#ifdef USEPAD
for (i=0;i<SPARE_1;O.spares_1[i++]=dinf);
for (i=0;i<SPARE_2;O.spares_2[i++]=dinf);
for (i=0;i<SPARE_3;O.spares_3[i++]=dinf);
for (i=0;i<SPARE_4;O.spares_4[i++]=dinf);
for (i=0;i<SPARE_5;O.spares_5[i++]=dinf);
for (i=0;i<SPARE_6;O.spares_6[i++]=dinf);
for (i=0;i<SPARE_7;O.spares_7[i++]=dinf);
for (i=0;i<SPARE_8;O.spares_8[i++]=dinf);
#ifndef USETUC
for (i=0;i<SPARE_9;O.spares_9[i++]=dinf);
#endif
for (i=0;i<SPARE_10;O.spares_10[i++]=dinf);
for (i=0;i<SPARE_12;O.spares_12[i++]=dinf);
for (i=0;i<SPARE_13;O.spares_13[i++]=dinf);
for (i=0;i<SPARE_14;O.spares_14[i++]=dinf);
#endif

return;
}
