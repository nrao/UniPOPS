
/* @(#)gbrecio.c	5.1 06/22/94 */

/*		ensure that this uses the unpadded sddformat */
#undef USEPAD
#include <sdd.h>
#include <sp_hdr.h>

/*  Get an  AC scan, feed, phase, and record  */
void getacrec_(ostruct,ind,phase,rec,errcode)
	sddformat *ostruct;
	int *ind, *phase, *rec, *errcode;

#define out ostruct->header
{
	int i, j, infeed, scanno;
	MiniHDR mini_hdr;

	   scanno = out.scan_number;
	   infeed = *ind;

	   gacir_(&scanno, &infeed, phase, rec, &mini_hdr, errcode);

	   if (*errcode != 0) { 
		*errcode = 5; return;
		}

	   for (i=0;i<out.nb_integrations;i++) 
		ostruct->data[i] = mini_hdr.data[i];

	   out.data_length = 4*out.nb_integrations;
	   out.source_syst_temp = mini_hdr.t_sys;
	   out.std_deviation_mean = mini_hdr.t_rms;
	   out.effint = mini_hdr.eff_int;
	   out.tot_integ_time = mini_hdr.t_dur;
	   out.ref_sys_temp = mini_hdr.t_ref;
	   out.nb_phases = mini_hdr.no_phases;	   
	   out.nb_records = mini_hdr.num_rec;	   
	   out.phase_id = mini_hdr.phase_id;	   
	   out.record_id = mini_hdr.record_id;	   
}

/*  Get an  SP scan, feed, phase, and record  */
void getsprec_(ostruct,ind,phase,rec,errcode)
	sddformat *ostruct;
	int *ind, *phase, *rec, *errcode;

#define out ostruct->header
{
	int i, j, infeed, scanno;
	MiniHDR mini_hdr;

	   infeed = *ind - 1;
	   scanno = out.scan_number;

	   if (*phase == 0 && *rec == 0) 
		gspscan_(&scanno, &infeed, &mini_hdr, errcode);
	   else
		gspir_(&scanno, &infeed, phase, rec, &mini_hdr, errcode);

	   if (*errcode != 0) { 
		*errcode = 5; return;
		}
	   out.nb_integrations = mini_hdr.no_int;
	   out.data_length = 4*out.nb_integrations;

	   /* May need to flip sense of data  */

	   if (mini_hdr.freq_res < 0.) {  
	     out.ref_point_nb = (out.nb_integrations / 2) + 1;
	     out.freq_resolution = mini_hdr.freq_res; 
	     for (i=0;i<out.nb_integrations;i++) 
		ostruct->data[i] = out.calibration_temp*mini_hdr.data[i];
	     }
	   else {
	     out.ref_point_nb = (out.nb_integrations / 2);
	     out.freq_resolution = -mini_hdr.freq_res; 
	     for (i=0, j=out.nb_integrations-1; i<out.nb_integrations; i++, j--) 
		ostruct->data[i] = out.calibration_temp*mini_hdr.data[j];
	     }

	   /* NOTE:  All temperatures must be scaled by TCAL; Assume that some
		frequency information is correct in Modcomp and SP header -- no
		consistency checking!  */

	   out.std_deviation_mean = out.calibration_temp*mini_hdr.t_rms;
	   out.la = mini_hdr.iff;
	   out.lb = mini_hdr.iff;
	   out.lc = mini_hdr.iff;
	   out.ld = mini_hdr.iff;
	   out.bandwidth = mini_hdr.bandwidth;
	   out.source_syst_temp = out.calibration_temp*mini_hdr.t_sys;
	   out.ref_sys_temp = out.calibration_temp*mini_hdr.t_ref;
	   out.tot_integ_time = mini_hdr.t_dur;
	   out.effint = mini_hdr.eff_int;
	   out.length_sample = mini_hdr.int_time;
	   out.length_cycle = mini_hdr.phase_time;	   
	   out.nb_receiv_channels = mini_hdr.no_rchan;
	   out.nb_phases = mini_hdr.no_phases;	   
	   out.phase_id = mini_hdr.phase_id;	   
	   out.nb_records = mini_hdr.num_rec;	   
	   out.record_id = mini_hdr.record_id;	   
}

/*	getrecio_ chooses the correct one of the above to call */
/*	using the information in ostruct */

#include <string.h>
void getrecio_(phase, record, feed, ostruct, irtn)
	int *phase, *record, *feed, *irtn;
	sddformat *ostruct;

#define out ostruct->header
{
        void getacrec_(), getsprec_();

        if (strncmp(out.back_end_desc,"1024ACIV", 8) == 0) {
           getacrec_(ostruct, feed, phase, record, irtn);
           if (*irtn != 0) *irtn = -360;
	}else if (strncmp(out.back_end_desc,"SPECPROC", 8) == 0) {
           getsprec_(ostruct, feed, phase, record, irtn);
           if (*irtn != 0) *irtn = -360;
        } else {
           *irtn = -241;
        }
}
