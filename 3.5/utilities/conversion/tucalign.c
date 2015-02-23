
/* @(#)tucalign.c	5.1 06/22/94 */
#include <malloc.h>
#include <tuc_online_sdd.h>

int tucalign(ibuff, feed_number)
int *feed_number;
double *ibuff;

{
   int i, j, norchan;
   double *tbuff, dscan;
   int hdrlen, datalen, iptr, tptr, feedtmp, cl12_size, c3nrch;
   int hdrelem, dataelem, maxelem, elemin, elemout, doagain;
   int scan, subscan, c1scn;
   tucsddheader *TH;
   tucsddformat *TF;

   short shortswap_();
   double vax2dbl_();
   union {
      short class_0[16];
      double d[4];
   } u;
   short def_class_0[16];

   for(i=0;i<4;i++) u.d[i] = *(ibuff+i);
   for(i=0;i<16;i++) {
      u.class_0[i] = shortswap_(u.class_0[i]);
      if (u.class_0[i] < 0) return(1);
      if (i>0) {
         --u.class_0[i];
      } else {
         if (u.class_0[0] > 15) return(1);
      }
   }

   hdrlen = vax2dbl_((ibuff + u.class_0[1]));
   datalen = vax2dbl_((ibuff + u.class_0[1] + 1));

   hdrelem =  (hdrlen % sizeof(double)) ? 
             ((hdrlen/sizeof(double)) + 1) : (hdrlen/sizeof(double));

   dataelem = (datalen % sizeof(double)) ? 
             ((datalen/sizeof(double)) + 1) : (datalen/sizeof(double));

   tbuff = (double *)malloc (sizeof(tucsddformat));
   if (tbuff == 0) {
      perror("Align:");
      return(1);
   }
   maxelem = sizeof(tucsddformat) / sizeof(double);
   maxelem = (maxelem*sizeof(double) == sizeof(tucsddformat)) ? 
             maxelem  : (maxelem + 1);
/*	and zero out this allocated array  */
   for(i=0;i<maxelem;i++) *(tbuff+i)=0;

/*	make sure that the class pointers are > zero   */
   u.class_0[u.class_0[0]+1] = hdrelem;
   doagain = 1;
   while (doagain) {
      doagain = 0;
      for(i=1;i<u.class_0[0]+1;i++) {
         if (u.class_0[i] <= 0) {
            u.class_0[i] = u.class_0[i+1];
            doagain = 1;
         }
      }
   }

   def_class_0[0] = TUC_NUMBER_HEADER_CLASS;
   def_class_0[1] = TUC_CLASS_1 - 1;
   def_class_0[2] = TUC_CLASS_2 - 1;
   def_class_0[3] = TUC_CLASS_3 - 1;
   def_class_0[4] = TUC_CLASS_4 - 1;
   def_class_0[5] = TUC_CLASS_5 - 1;
   def_class_0[6] = TUC_CLASS_6 - 1;
   def_class_0[7] = TUC_CLASS_7 - 1;
   def_class_0[8] = TUC_CLASS_8 - 1;
   def_class_0[9] = TUC_CLASS_9 - 1;
   def_class_0[10] = TUC_CLASS_10 - 1;
   def_class_0[11] = TUC_CLASS_11 - 1;
   def_class_0[12] = TUC_CLASS_12 - 1;
   def_class_0[13] = TUC_CLASS_13 - 1;
   def_class_0[14] = TUC_CLASS_14 - 1;

/*	select class 12 start based on value of feed_number  */
/*	first, get number of receiver changes */
   c1scn = u.class_0[1]+2;
   dscan = vax2dbl_(&(ibuff[c1scn]));
   scan = dscan;
   subscan = (int)((dscan - scan) * 100.0 + 0.5);
   c3nrch = u.class_0[3] + 3;
   norchan = vax2dbl_(&(ibuff[c3nrch]));
   if (norchan <= 0) norchan = 1;
   if (subscan <= 0) subscan = 1;
   feedtmp = *feed_number - subscan + 1;
   if (feedtmp > 10) feedtmp = feedtmp - 10;
   if (feedtmp > norchan) {
      free(tbuff);
      return(2);
   }
/*	Now figure out the class_12 size */
   cl12_size = (u.class_0[13] - u.class_0[12]) / norchan;
   if (cl12_size < 21) cl12_size = 21;
/*		that should catch old style class 12s */
/*	now set the appropriate class_12 */
   for (i=0;i<(feedtmp - 1);i++) 
                  u.class_0[12] = u.class_0[12] + cl12_size;
/*	check that class 12 is not larger than class 13 */
   if (u.class_0[12] > u.class_0[13]) {
      free(tbuff);
      return(1);
   }

/*		set the default header values in tbuff */
/*		since this will eventually get translated from VAX to IEEE */
/*		and since VAX doesn't have any representation for inf */
/*		the best we can do is set the values to 0 */
/*		characters, however, can be set to spaces */

   TH = (tucsddheader *)tbuff;
   TF = (tucsddformat *)tbuff;
   TH->scan_number = 0.0;
   for (i=0;i<8;TH->observer_initials[i++]=' ');
   for (i=0;i<16;TH->observer_name[i++]=' ');
   for (i=0;i<8;TH->telescope[i++]=' ');
   for (i=0;i<8;TH->project_ident[i++]=' ');
   for (i=0;i<16;TH->source_name[i++]=' ');
   for (i=0;i<8;TH->type_data[i++]=' ');
   for (i=0;i<8;TH->front_end_desc[i++]=' ');
   for (i=0;i<8;TH->back_end_desc[i++]=' ');
   for (i=0;i<8;TH->data_precision[i++]=' ');

   TH->tot_azra_pointing = 0.0;
   TH->tot_eldec_pointing = 0.0;
   TH->user_azra_pointing = 0.0;
   TH->user_eldec_pointing = 0.0;
   for (i=0;i<4;TH->pointing_constant[i++] = 0.0);
   TH->orient_angle = 0.0;
   TH->radial_focus = 0.0;
   TH->north_south_focus = 0.0;
   TH->east_west_focus = 0.0;
   for (i=0;i<8;TH->pt_model[i++]=' ');

   TH->ut_date = 0.0;
   TH->utime = 0.0;
   TH->lst = 0.0;
   TH->nb_receiv_channels = 0.0;
   TH->nb_sw_variables = 0.0;
   TH->nb_phases = 0.0;
   TH->length_cycle = 0.0;
   TH->length_sample = 0.0;
   for (i=0;i<8;TH->class11_type[i++]=' ');

   TH->epoch = 0.0;
   TH->sourcex = 0.0;
   TH->sourcey = 0.0;
   TH->referencex = 0.0;
   TH->referencey = 0.0;
   TH->epoch_ra = 0.0;
   TH->epoch_dec = 0.0;
   TH->galactic_l = 0.0;
   TH->galactic_b = 0.0;
   TH->azimuth = 0.0;
   TH->elevation = 0.0;
   TH->xposition = 0.0;
   TH->yposition = 0.0;
   for (i=0;i<3;TH->desc_origin[i++] = 0.0);
   for (i=0;i<8;TH->coord_sys_code[i++]=' ');

   TH->amb_temp = 0.0;

   TH->map_scan_angle = 0.0;
   TH->xposition_zero = 0.0;
   TH->yposition_zero = 0.0;
   TH->deltax_rate = 0.0;
   TH->deltay_rate = 0.0;
   TH->nb_grid_pt = 0.0;
   TH->x_grid_pt = 0.0;
   TH->y_grid_pt = 0.0;
   TH->x_grid_cell_nb = 0.0;
   TH->y_grid_cell_nb = 0.0;
   for (i=0;i<8;TH->xy_ref_frame_code[i++]=' ');

   TH->beam = 0.0;
   TH->off_scan_nb = 0.0;
   TH->bad_channel_value = 0.0;
   TH->velocity_correct = 0.0;
   TH->velocity_ref = 0.0;
   for (i=0;i<8;TH->velocity_def_ref[i++]=' ');
   for (i=0;i<8;TH->type_calibration[i++]=' ');

   TH->antenna_aperature_eff = 0.0;

   TH->synth_freq = 0.0;
   TH->lo_factor = 0.0;
   TH->harmonic = 0.0;
   TH->lo_if = 0.0;
   TH->first_if = 0.0;
   TH->ref_AZ_offset = 0.0;
   TH->ref_EL_offset = 0.0;
   TH->beam_throw = 0.0;
   TH->beam_orient = 0.0;
   TH->bl_offset = 0.0;
   TH->obs_tol = 0.0;
   TH->sideband = 0.0;
   TH->wavelength = 0.0;
   TH->gain_scan = 0.0;
   for (i=0;i<2;TH->p_beam[i++] = 0.0);
   for (i=0;i<2;TH->m_beam[i++] = 0.0);
   for (i=0;i<2;TH->RA_DEC_offsets[i++] = 0.0);
   TH->freq_off_signal = 0.0;
   TH->freq_off_ref_1 = 0.0;
   TH->freq_off_ref_2 = 0.0;

   for (i=0;i<80;TH->open_param_values[i++]=' ');

   TH->nb_sw_variables_fast = 0.0;
   TH->nb_cycles = 0.0;
   TH->nb_cycles_fast = 0.0;
   TH->nb_phases_fast = 0.0;
   TH->length_cycle_fast = 0.0;
   TH->length_sample_fast = 0.0;
   for (i=0;i<10;i++) {
      TH->cl11ent[i].variable_value = 0.0;
      for (j=0;j<8;TH->cl11ent[i].variable_desc[j++]=' ');
      for (j=0;j<32;TH->cl11ent[i].phase_table[j++]=' ');
   }

   TH->obs_freq = 0.0;
   TH->rest_freq = 0.0;
   TH->freq_resolution = 0.0;
   TH->bandwidth = 0.0;
   TH->receiver_temp = 0.0;
   TH->calibration_temp = 0.0;
   TH->source_syst_temp = 0.0;
   TH->ref_sys_temp = 0.0;
   TH->source_temp = 0.0;
   TH->std_deviation_mean = 0.0;
   TH->ref_point_nb = 0.0;
   TH->xvalue_ref_pt = 0.0;
   TH->deltax = 0.0;
   TH->tot_integ_time = 0.0;
   TH->nb_integrations = 0.0;
   TH->starting_pt_nb = 0.0;
   TH->h2o_opacity = 0.0;
   TH->h2o_temp = 0.0;
   TH->o2_opacity = 0.0;
   TH->o2_temp = 0.0;
   for (i=0;i<8;TH->polarization[i++]=' ');
   TH->effint = 0.0;
   for (i=0;i<16;TH->receiver_info[i++]=' ');

   TH->nb_scans_stacked = 0.0;
   TH->f_scan_stack = 0.0;
   TH->l_scan_stack = 0.0;
   TH->line_amplitude = 0.0;
   TH->line_width = 0.0;
   TH->integrated_line_int = 0.0;
   TH->rms_noise = 0.0;
   for (i=0;i<47;TH->save[i++] = 0.0);

   for (i=0;i<MAX_DATA_POINTS;TF->data[i++]=0.0);

/*		ok, now move it from ibuff to tbuff */

   for(i=1;i<(u.class_0[0]+1) && i<(def_class_0[0]+1);i++) {
      tptr = def_class_0[i];
      iptr = u.class_0[i];
      for(;iptr<u.class_0[i+1] && tptr<def_class_0[i+1]; iptr++, tptr++) {
         if (i==12 && ((iptr >= (cl12_size + u.class_0[i])) ||
                       (tptr >= (cl12_size + def_class_0[i])))) break;
         *(tbuff+tptr) = *(ibuff+iptr);
      }
   }

   for(iptr=hdrelem,tptr=def_class_0[14];
       iptr<(dataelem+hdrelem) && tptr<maxelem; 
       *(tbuff + tptr++) = *(ibuff + iptr++));

   hdrelem = def_class_0[14];
   for(iptr=0,tptr=0;iptr<(dataelem+hdrelem) && tptr<maxelem;
       *(ibuff + iptr++) = *(tbuff + tptr++));

   free(tbuff);

   return(0);
}
