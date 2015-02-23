
/* @(#)tuc2uni.c	5.1 06/22/94 */

/* Converts from the on_line sdd format to the internal unipops format */
/* sdd_struct should probably have been appropriately initialized first */
/* on_line_struct should be aligned appropriately but still in vax binary */


#include <string.h>
#include <math.h>
#include <ctype.h>

#define USETUC
#include <sdd.h>
#include <tuc_online_sdd.h>

void tuc2uni(on_line_struct, sdd_struct, size, feed_number)

tucsddformat *on_line_struct;
sddformat *sdd_struct;
int *size;
int *feed_number;

{
#define O on_line_struct->header
#define S sdd_struct->header
   int i, j, num_pt, start_pt, iscan, datalen;
   int noswvar, noswvarf;
   unsigned int opch;
   float feed;

   double vax2dbl_();
   float vax2flt_();

   feed = *feed_number;
   S.scan_number = vax2dbl_(&(O.scan_number));
   iscan = S.scan_number;
   S.scan_number = iscan + feed/100.;
   for (i=0;i<8;i++) S.observer_initials[i] = O.observer_initials[i];
   for (i=0;i<16;i++) S.observer_name[i] = O.observer_name[i];
   for (i=0;i<8;i++) S.telescope[i] = O.telescope[i];
   for (i=0;i<8;i++) S.project_ident[i] = O.project_ident[i];
   for (i=0;i<16;i++) S.source_name[i] = O.source_name[i];
   for (i=0;i<8;i++) S.type_data[i] = O.type_data[i];
   for (i=0;i<8;i++) S.front_end_desc[i] = O.front_end_desc[i];
   for (i=0;i<8;i++) S.back_end_desc[i] = O.back_end_desc[i];
   for (i=0;i<8;i++) S.data_precision[i] = O.data_precision[i];

   S.tot_azra_pointing = vax2dbl_(&(O.tot_azra_pointing));
   S.tot_eldec_pointing = vax2dbl_(&(O.tot_eldec_pointing));
   S.user_azra_pointing = vax2dbl_(&(O.user_azra_pointing));
   S.user_eldec_pointing = vax2dbl_(&(O.user_eldec_pointing));
   for (i=0;i<4;i++) S.pointing_constant[i] = vax2dbl_(&(O.pointing_constant[i]));
   S.orient_angle = vax2dbl_(&(O.orient_angle));
   S.radial_focus = vax2dbl_(&(O.radial_focus));
   S.north_south_focus = vax2dbl_(&(O.north_south_focus));
   S.east_west_focus = vax2dbl_(&(O.east_west_focus));
   for (i=0;i<8;i++) S.pt_model[i] = O.pt_model[i];

   S.ut_date = vax2dbl_(&(O.ut_date));
   S.utime = vax2dbl_(&(O.utime));
   S.lst = vax2dbl_(&(O.lst));
   S.nb_receiv_channels = vax2dbl_(&(O.nb_receiv_channels));
   S.nb_sw_variables = vax2dbl_(&(O.nb_sw_variables));
   S.nb_phases = vax2dbl_(&(O.nb_phases));
   S.length_cycle = vax2dbl_(&(O.length_cycle));
   S.length_sample = vax2dbl_(&(O.length_sample));
   for (i=0;i<8;i++) S.class11_type[i] = O.class11_type[i];

   S.epoch = vax2dbl_(&(O.epoch));
   S.sourcex = vax2dbl_(&(O.sourcex));
   S.sourcey = vax2dbl_(&(O.sourcey));
   S.referencex = vax2dbl_(&(O.referencex));
   S.referencey = vax2dbl_(&(O.referencey));
   S.epoch_ra = vax2dbl_(&(O.epoch_ra));
   S.epoch_dec = vax2dbl_(&(O.epoch_dec));
   S.galactic_l = vax2dbl_(&(O.galactic_l));
   S.galactic_b = vax2dbl_(&(O.galactic_b));
   S.azimuth = vax2dbl_(&(O.azimuth));
   S.elevation = vax2dbl_(&(O.elevation));
   S.xposition = vax2dbl_(&(O.xposition));
   S.yposition = vax2dbl_(&(O.yposition));
   for (i=0;i<3;i++) S.desc_origin[i] = vax2dbl_(&(O.desc_origin[i]));
   for (i=0;i<8;i++) S.coord_sys_code[i] = O.coord_sys_code[i];

   S.amb_temp = vax2dbl_(&(O.amb_temp));
   S.pressure = vax2dbl_(&(O.pressure));
   S.relative_humidity = vax2dbl_(&(O.relative_humidity));
   S.index_refraction = vax2dbl_(&(O.index_refraction));
   S.dew_point = vax2dbl_(&(O.dew_point));
   S.mm_h2o = vax2dbl_(&(O.mm_h2o));

   S.map_scan_angle = vax2dbl_(&(O.map_scan_angle));
   S.xposition_zero = vax2dbl_(&(O.xposition_zero));
   S.yposition_zero = vax2dbl_(&(O.yposition_zero));
   S.deltax_rate = vax2dbl_(&(O.deltax_rate));
   S.deltay_rate = vax2dbl_(&(O.deltay_rate));
   S.nb_grid_pt = vax2dbl_(&(O.nb_grid_pt));
   S.x_grid_pt = vax2dbl_(&(O.x_grid_pt));
   S.y_grid_pt = vax2dbl_(&(O.y_grid_pt));
   S.x_grid_cell_nb = vax2dbl_(&(O.x_grid_cell_nb));
   S.y_grid_cell_nb = vax2dbl_(&(O.y_grid_cell_nb));
   for (i=0;i<8;i++) S.xy_ref_frame_code[i] = O.xy_ref_frame_code[i];

   S.beam = vax2dbl_(&(O.beam));
   S.off_scan_nb = vax2dbl_(&(O.off_scan_nb));
   S.bad_channel_value = vax2dbl_(&(O.bad_channel_value));
   S.velocity_correct = vax2dbl_(&(O.velocity_correct));
   S.velocity_ref = vax2dbl_(&(O.velocity_ref));
   for (i=0;i<8;i++) S.velocity_def_ref[i] = O.velocity_def_ref[i];
   for (i=0;i<8;i++) S.type_calibration[i] = O.type_calibration[i];

   S.antenna_aperature_eff = vax2dbl_(&(O.antenna_aperature_eff));
   S.antenna_beam_eff = vax2dbl_(&(O.antenna_beam_eff));
   S.antenna_gain = vax2dbl_(&(O.antenna_gain));
   S.etal = vax2dbl_(&(O.etal));
   S.etalfss = vax2dbl_(&(O.etalfss));

   S.synth_freq = vax2dbl_(&(O.synth_freq));
   S.lo_factor = vax2dbl_(&(O.lo_factor));
   S.harmonic = vax2dbl_(&(O.harmonic));
   S.lo_if = vax2dbl_(&(O.lo_if));
   S.first_if = vax2dbl_(&(O.first_if));
   S.ref_AZ_offset = vax2dbl_(&(O.ref_AZ_offset));
   S.ref_EL_offset = vax2dbl_(&(O.ref_EL_offset));
   S.beam_throw = vax2dbl_(&(O.beam_throw));
   S.beam_orient = vax2dbl_(&(O.beam_orient));
   S.bl_offset = vax2dbl_(&(O.bl_offset));
   S.obs_tol = vax2dbl_(&(O.obs_tol));
   S.sideband = vax2dbl_(&(O.sideband));
   S.wavelength = vax2dbl_(&(O.wavelength));
   S.gain_scan = vax2dbl_(&(O.gain_scan));
   for (i=0;i<2;i++) S.p_beam[i] = vax2dbl_(&(O.p_beam[i]));
   for (i=0;i<2;i++) S.m_beam[i] = vax2dbl_(&(O.m_beam[i]));
   for (i=0;i<4;i++) S.RA_DEC_offsets[i] = vax2dbl_(&(O.RA_DEC_offsets[i]));
   S.freq_off_signal = vax2dbl_(&(O.freq_off_signal));
   S.freq_off_ref_1 = vax2dbl_(&(O.freq_off_ref_1));
   S.freq_off_ref_2 = vax2dbl_(&(O.freq_off_ref_2));

   for (i=0;i<80;i++) {
      if (isalpha(O.open_param_values[i]))
         S.open_param_values[i] = O.open_param_values[i];
   }

   if (strncmp(S.class11_type, "PROTO12M", 8) == 0) {
      S.cl11.proto.nb_sw_variables_fast = vax2dbl_(&(O.nb_sw_variables_fast));
      S.cl11.proto.nb_cycles = vax2dbl_(&(O.nb_cycles));
      S.cl11.proto.nb_cycles_fast = vax2dbl_(&(O.nb_cycles_fast));
      S.cl11.proto.nb_phases_fast = vax2dbl_(&(O.nb_phases_fast));
      S.cl11.proto.length_cycle_fast = vax2dbl_(&(O.length_cycle_fast));
      S.cl11.proto.length_sample_fast = vax2dbl_(&(O.length_sample_fast));
      noswvar = S.nb_sw_variables;
      noswvarf = S.cl11.proto.nb_sw_variables_fast;
      for (j=0;j<(noswvar+noswvarf);j++) {
         S.cl11.proto.ent[j].variable_value = 
                              vax2dbl_(&(O.cl11ent[j].variable_value));
         for (i=0;i<8;i++) S.cl11.proto.ent[j].variable_desc[i] = 
                                         O.cl11ent[j].variable_desc[i];
         for (i=0;i<32;i++) S.cl11.proto.ent[j].phase_table[i] = 
                                         O.cl11ent[j].phase_table[i];
      }
      for (j=(noswvar+noswvarf);j<10;j++) {
         S.cl11.proto.ent[j].variable_value = 0.0;
         for (i=0;i<8;S.cl11.proto.ent[j].variable_desc[i++]=' ');
         for (i=0;i<32;S.cl11.proto.ent[j].phase_table[i++]=' ');
      }
   }
/*		tucson has never used the original class 12 */

   S.obs_freq = vax2dbl_(&(O.obs_freq));
   S.rest_freq = vax2dbl_(&(O.rest_freq));
   S.freq_resolution = vax2dbl_(&(O.freq_resolution));
   S.bandwidth = vax2dbl_(&(O.bandwidth));
   S.receiver_temp = vax2dbl_(&(O.receiver_temp));
   S.calibration_temp = vax2dbl_(&(O.calibration_temp));
   S.source_syst_temp = vax2dbl_(&(O.source_syst_temp));
   S.ref_sys_temp = vax2dbl_(&(O.ref_sys_temp));
   S.source_temp = vax2dbl_(&(O.source_temp));
   S.std_deviation_mean = vax2dbl_(&(O.std_deviation_mean));
   S.ref_point_nb = vax2dbl_(&(O.ref_point_nb));
   S.xvalue_ref_pt = vax2dbl_(&(O.xvalue_ref_pt));
   S.deltax = vax2dbl_(&(O.deltax));
   S.tot_integ_time = vax2dbl_(&(O.tot_integ_time));
   S.nb_integrations = vax2dbl_(&(O.nb_integrations));
   S.starting_pt_nb = vax2dbl_(&(O.starting_pt_nb));
   S.h2o_opacity = vax2dbl_(&(O.h2o_opacity));
   S.h2o_temp = vax2dbl_(&(O.h2o_temp));
   S.o2_opacity = vax2dbl_(&(O.o2_opacity));
   S.o2_temp = vax2dbl_(&(O.o2_temp));
   for (i=0;i<8;i++) S.polarization[i] = O.polarization[i];
   S.effint = vax2dbl_(&(O.effint));
   for (i=0;i<16;i++) S.receiver_info[i] = O.receiver_info[i];

   S.nb_scans_stacked = vax2dbl_(&O.nb_scans_stacked);
   S.f_scan_stack = vax2dbl_(&(O.f_scan_stack));
   S.l_scan_stack = vax2dbl_(&(O.l_scan_stack));
   S.line_amplitude = vax2dbl_(&(O.line_amplitude));
   S.line_width = vax2dbl_(&(O.line_width));
   S.integrated_line_int = vax2dbl_(&(O.integrated_line_int));
   S.rms_noise = vax2dbl_(&(O.rms_noise));

   S.number_header_class = NUMBER_HEADER_CLASS;
   S.class_location[0] = CLASS_0 + 1;
   S.class_location[1] = S.class_location[0] + CLASS_1;
   S.class_location[2] = S.class_location[1] + CLASS_2;
   S.class_location[3] = S.class_location[2] + CLASS_3;
   S.class_location[4] = S.class_location[3] + CLASS_4;
   S.class_location[5] = S.class_location[4] + CLASS_5;
   S.class_location[6] = S.class_location[5] + CLASS_6;
   S.class_location[7] = S.class_location[6] + CLASS_7;
   S.class_location[8] = S.class_location[7] + CLASS_8;
   S.class_location[9] = S.class_location[8] + CLASS_9;
   S.class_location[10] = S.class_location[9] + CLASS_10;
   S.class_location[11] = S.class_location[10] + CLASS_11;
   S.class_location[12] = S.class_location[11] + CLASS_12;
   S.class_location[13] = S.class_location[12] + CLASS_13;
   S.class_location[14] = S.class_location[13] + CLASS_14;

/*		set the sense of the frequency axis based on deltax */
/*		if deltax is 0, don't change freq_resolution */

   if (strncmp(S.type_data, "LINE", 4) == 0) {
      if (S.deltax < 0) {
         S.freq_resolution = fabs(S.freq_resolution);
      } else if (S.deltax > 0) {
         S.freq_resolution = - fabs(S.freq_resolution);
      }
/*		also, for LINE data, use nb_integrations and starting_pt_nb */
/*		to get the data for the appropriate receiver block */
/*		For CONT data, just get everything */
      num_pt = S.nb_integrations;
      start_pt = S.starting_pt_nb;
   } else {
      datalen = vax2dbl_(&(O.data_length));
      num_pt = (datalen % sizeof(float)) ?
                  ((datalen/sizeof(float)) + 1) : (datalen/sizeof(float));
      start_pt = 1;
   }

   S.header_length = sizeof(sddheader);

   num_pt = (num_pt < MAX_DATA_POINTS) ? num_pt : MAX_DATA_POINTS;

   S.data_length = num_pt * sizeof(float);
   *size = S.header_length + S.data_length;

   for (i=0;i<num_pt;i++) {
      sdd_struct->data[i] = vax2flt_(&(on_line_struct->data[i + start_pt - 1]));
   }

   S.starting_pt_nb = 1;
   S.ref_point_nb = S.ref_point_nb - start_pt + 1;

   return;
}
