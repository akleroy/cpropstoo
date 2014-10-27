pro mc_calc_errors $
   , in_sim = in_sim $
   , out_props_err = out_props_err
   
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
; 10/13/2014     A.A. Kepley             Original Code
;-

; check the inputs
  if n_elements(in_sim) le 0 then begin
     message,/info,'Please input monte carlo simulation output from mc_mom_errors'
     return
  endif
  
; read in the simulations
  restore,/verb,filename=in_sim
  

; create structure for output
  props_err = reform(props_sim[*,0])

  props_size = size(props_sim)
  nassign = props_size[1]
  nsim = props_size[2]

; calculate the errors
  for i = 0, nassign - 1 do begin

     props_err[i].majrms = stddev(props_sim[i,*].majrms,/double)
     props_err[i].majrms_extrap = stddev(props_sim[i,*].majrms_extrap,/double)
     props_err[i].majrms_extrap_deconv = stddev(props_sim[i,*].majrms_extrap_deconv,/double)
     props_err[i].majrms_gcorr = stddev(props_sim[i,*].majrms_gcorr,/double)
     props_err[i].majrms_gcorr_deconv = stddev(props_sim[i,*].majrms_gcorr_deconv,/double)

     props_err[i].minrms = stddev(props_sim[i,*].minrms,/double)
     props_err[i].minrms_extrap = stddev(props_sim[i,*].minrms_extrap,/double)
     props_err[i].minrms_extrap_deconv = stddev(props_sim[i,*].minrms_extrap_deconv,/double)
     props_err[i].minrms_gcorr = stddev(props_sim[i,*].minrms_gcorr,/double)
     props_err[i].minrms_gcorr_deconv = stddev(props_sim[i,*].minrms_gcorr_deconv,/double)

     props_err[i].posang_extrap_deconv = stddev(props_sim[i,*].posang_extrap_deconv,/double)
     props_err[i].posang_gcorr_deconv = stddev(props_sim[i,*].posang_gcorr_deconv,/double)

     props_err[i].flux = stddev(props_sim[i,*].flux,/double)
     props_err[i].flux_extrap = stddev(props_sim[i,*].flux_extrap,/double)
     props_err[i].flux_gcorr = stddev(props_sim[i,*].flux_gcorr,/double)

     
     props_err[i].mass = stddev(props_sim[i,*].mass,/double)
     props_err[i].mass_extrap = stddev(props_sim[i,*].mass_extrap,/double)
     props_err[i].mass_gcorr = stddev(props_sim[i,*].mass_gcorr,/double)

     props_err[i].lum = stddev(props_sim[i,*].lum,/double)
     props_err[i].lum_extrap = stddev(props_sim[i,*].lum_extrap,/double)
     props_err[i].lum_gcorr = stddev(props_sim[i,*].lum_gcorr,/double)
    
     props_err[i].radrms_extrap_deconv = stddev(props_sim[i,*].radrms_extrap_deconv,/double)
     props_err[i].radrms_gcorr_deconv= stddev(props_sim[i,*].radrms_gcorr_deconv,/double)

     props_err[i].virmass_extrap_deconv = stddev(props_sim[i,*].virmass_extrap_deconv,/double)
     props_err[i].virmass_gcorr_deconv = stddev(props_sim[i,*].virmass_gcorr_deconv,/double)

     props_err[i].vrms =  stddev(props_sim[i,*].vrms,/double)
     props_err[i].vrms_extrap =  stddev(props_sim[i,*].vrms_extrap,/double)
     props_err[i].vrms_extrap_deconv =  stddev(props_sim[i,*].vrms_extrap_deconv,/double)

     props_err[i].vrms =  stddev(props_sim[i,*].vrms,/double)
     props_err[i].vrms_gcorr =  stddev(props_sim[i,*].vrms_gcorr,/double)
     props_err[i].vrms_gcorr_deconv =  stddev(props_sim[i,*].vrms_gcorr_deconv,/double)
   
     props_err[i].xrms =  stddev(props_sim[i,*].xrms,/double)
     props_err[i].xrms_extrap =  stddev(props_sim[i,*].xrms_extrap,/double)
     props_err[i].xrms_extrap_deconv =  stddev(props_sim[i,*].xrms_extrap_deconv,/double)

     props_err[i].xrms =  stddev(props_sim[i,*].xrms,/double)
     props_err[i].xrms_gcorr =  stddev(props_sim[i,*].xrms_gcorr,/double)
     props_err[i].xrms_gcorr_deconv =  stddev(props_sim[i,*].xrms_gcorr_deconv,/double)

     props_err[i].yrms =  stddev(props_sim[i,*].yrms,/double)
     props_err[i].yrms_extrap =  stddev(props_sim[i,*].yrms_extrap,/double)
     props_err[i].yrms_extrap_deconv =  stddev(props_sim[i,*].yrms_extrap_deconv,/double)

     props_err[i].yrms =  stddev(props_sim[i,*].yrms,/double)
     props_err[i].yrms_gcorr =  stddev(props_sim[i,*].yrms_gcorr,/double)
     props_err[i].yrms_gcorr_deconv =  stddev(props_sim[i,*].yrms_gcorr_deconv,/double)
endfor

; write out the error structure

  save, /verb, props_err,filename=out_props_err

end
