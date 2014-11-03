pro mc_calc_errors $
   , in_file = in_file $
   , in_sim = in_sim $
   , idl_file = idl_file
   
;+
; mc_calc_errors
;
; Purpose: calculate errors based on monte carlo simulation generated
; by mc_mom_errors.
;
; Input: 
;       in_sim: idl save file for simulation
;
; Output:
;
;       out_props_err: partial props structure with errors as values.
;
; Notes:
;       
;       Right now I'm not calculating the errors on everything, just
;       on a few crucial values. I may go back and calculate things
;       more completely and put NaNs for values that don't have
;       errors from the monte carlo (e.g., ellfit, etc).
;
;       I'm also just using the standard variation to calculate the errors.
;
; Date          Programmer              Description of Changes
; ----------------------------------------------------------------------
; 10/13/2014     A.A. Kepley            Original Code
; 10/28/2014     A.A. Kepley            Updating to add err parameters
;                                       directly to cprops structure.
;-

; check the inputs
  if n_elements(in_file) le 0 then begin
     message,/info,'Please input the original cprops structure'
     return
  endif
  
  if n_elements(in_sim) le 0 then begin
     message,/info,'Please input monte carlo simulation output from mc_mom_errors'
     return
  endif
  
; Read in the original cprops structure
  restore,/verb,filename=in_file

  props_size = size(props)

; read in the simulations
  restore,/verb,filename=in_sim

  props_sim_size = size(props_sim)
  nassign = props_sim_size[1]
  nsim = props_sim_size[2]

  
 
; initialize the output structure
  for i = 0, nassign - 1 do begin
     this_props_err = add_error_fields(props[i])    
     this_props_err = alphabetize_struct(this_props_err)
     if i eq 0 then  begin
        props_err = this_props_err
     endif else begin
        props_err = [props_err,this_props_err]
     endelse
  endfor

; calculate the errors
  for i = 0, nassign - 1 do begin

     props_err[i].majrms_err = stddev(props_sim[i,*].majrms,/double)
     props_err[i].majrms_extrap_err = stddev(props_sim[i,*].majrms_extrap,/double)
     props_err[i].majrms_extrap_deconv_err = stddev(props_sim[i,*].majrms_extrap_deconv,/double)
     props_err[i].majrms_gcorr_err = stddev(props_sim[i,*].majrms_gcorr,/double)
     props_err[i].majrms_gcorr_deconv_err = stddev(props_sim[i,*].majrms_gcorr_deconv,/double)

     props_err[i].minrms_err = stddev(props_sim[i,*].minrms,/double)
     props_err[i].minrms_extrap_err = stddev(props_sim[i,*].minrms_extrap,/double)
     props_err[i].minrms_extrap_deconv_err = stddev(props_sim[i,*].minrms_extrap_deconv,/double)
     props_err[i].minrms_gcorr_err = stddev(props_sim[i,*].minrms_gcorr,/double)
     props_err[i].minrms_gcorr_deconv_err = stddev(props_sim[i,*].minrms_gcorr_deconv,/double)

     props_err[i].posang_extrap_deconv_err = stddev(props_sim[i,*].posang_extrap_deconv,/double)
     props_err[i].posang_gcorr_deconv_err = stddev(props_sim[i,*].posang_gcorr_deconv,/double)

     props_err[i].flux_err = stddev(props_sim[i,*].flux,/double)
     props_err[i].flux_extrap_err = stddev(props_sim[i,*].flux_extrap,/double)
     props_err[i].flux_gcorr_err = stddev(props_sim[i,*].flux_gcorr,/double)

     props_err[i].mass_err = stddev(props_sim[i,*].mass,/double)
     props_err[i].mass_extrap_err = stddev(props_sim[i,*].mass_extrap,/double)
     props_err[i].mass_gcorr_err = stddev(props_sim[i,*].mass_gcorr,/double)

     props_err[i].lum_err = stddev(props_sim[i,*].lum,/double)
     props_err[i].lum_extrap_err = stddev(props_sim[i,*].lum_extrap,/double)
     props_err[i].lum_gcorr_err = stddev(props_sim[i,*].lum_gcorr,/double)
    
     props_err[i].radrms_extrap_deconv_err = stddev(props_sim[i,*].radrms_extrap_deconv,/double)
     props_err[i].radrms_gcorr_deconv_err = stddev(props_sim[i,*].radrms_gcorr_deconv,/double)

     props_err[i].virmass_extrap_deconv_err = stddev(props_sim[i,*].virmass_extrap_deconv,/double)
     props_err[i].virmass_gcorr_deconv_err = stddev(props_sim[i,*].virmass_gcorr_deconv,/double)

     props_err[i].vrms_err =  stddev(props_sim[i,*].vrms,/double)
     props_err[i].vrms_extrap_err =  stddev(props_sim[i,*].vrms_extrap,/double)
     props_err[i].vrms_extrap_deconv_err =  stddev(props_sim[i,*].vrms_extrap_deconv,/double)
     props_err[i].vrms_gcorr_err =  stddev(props_sim[i,*].vrms_gcorr,/double)
     props_err[i].vrms_gcorr_deconv_err =  stddev(props_sim[i,*].vrms_gcorr_deconv,/double)
   
     props_err[i].xrms_err =  stddev(props_sim[i,*].xrms,/double)
     props_err[i].xrms_extrap_err =  stddev(props_sim[i,*].xrms_extrap,/double)
     props_err[i].xrms_extrap_deconv_err =  stddev(props_sim[i,*].xrms_extrap_deconv,/double)
     props_err[i].xrms_gcorr_err =  stddev(props_sim[i,*].xrms_gcorr,/double)
     props_err[i].xrms_gcorr_deconv_err =  stddev(props_sim[i,*].xrms_gcorr_deconv,/double)

     props_err[i].yrms_err =  stddev(props_sim[i,*].yrms,/double)
     props_err[i].yrms_extrap_err =  stddev(props_sim[i,*].yrms_extrap,/double)
     props_err[i].yrms_extrap_deconv_err =  stddev(props_sim[i,*].yrms_extrap_deconv,/double)
     props_err[i].yrms_gcorr_err =  stddev(props_sim[i,*].yrms_gcorr,/double)
     props_err[i].yrms_gcorr_deconv_err =  stddev(props_sim[i,*].yrms_gcorr_deconv,/double)
endfor

; write out the error structure

  props = props_err

  save, /verb, props, filename=idl_file

end
