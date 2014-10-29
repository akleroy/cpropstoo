function add_error_fields $
   , prev_struct

;+
;
; NAME:
;
;   add_error_fields
;
; PURPOSE:
;
; Adds error fields to a CPROPS structure. Needs a seed structure
; Returns a new structure with properties fields in place.
;
; NOTE:
;
; This is an infrastructure routine and not documented for general
; use.
;
;-

; FOR CONVENIENCE, ASSIGN "not-a-number" TO THE nan VARIABLE
  nan = !values.f_nan

; major axis rms errors
  empty_struct = create_struct(prev_struct, "majrms_err", nan)
  empty_struct = create_struct(empty_struct, "majrms_extrap_err", nan)
  empty_struct = create_struct(empty_struct, "majrms_extrap_deconv_err", nan)
  empty_struct = create_struct(empty_struct, "majrms_gcorr_err", nan)
  empty_struct = create_struct(empty_struct, "majrms_gcorr_deconv_err", nan)
                                
; minor axis rms errors
  empty_struct = create_struct(empty_struct, "minrms_err", nan)
  empty_struct = create_struct(empty_struct, "minrms_extrap_err", nan)
  empty_struct = create_struct(empty_struct, "minrms_extrap_deconv_err", nan)
  empty_struct = create_struct(empty_struct, "minrms_gcorr_err", nan)
  empty_struct = create_struct(empty_struct, "minrms_gcorr_deconv_err", nan)

; position angle errors
  empty_struct = create_struct(empty_struct, "posang_extrap_deconv_err", nan)
  empty_struct = create_struct(empty_struct, "posang_gcorr_deconv_err", nan)
  
; flux errors
  empty_struct = create_struct(empty_struct, "flux_err", nan)
  empty_struct = create_struct(empty_struct, "flux_extrap_err", nan)
  empty_struct = create_struct(empty_struct, "flux_gcorr_err", nan)
  
; luminosity errors
  empty_struct = create_struct(empty_struct, "lum_err", nan)
  empty_struct = create_struct(empty_struct, "lum_extrap_err", nan)
  empty_struct = create_struct(empty_struct, "lum_gcorr_err", nan)

; mass errors
  empty_struct = create_struct(empty_struct, "mass_err", nan)
  empty_struct = create_struct(empty_struct, "mass_extrap_err", nan)
  empty_struct = create_struct(empty_struct, "mass_gcorr_err", nan)

; virial mass errors
  empty_struct = create_struct(empty_struct, "virmass_extrap_deconv_err", nan)
  empty_struct = create_struct(empty_struct, "virmass_gcorr_deconv_err", nan)

; radius errors 
  empty_struct = create_struct(empty_struct, "radrms_extrap_deconv_err", nan)
  empty_struct = create_struct(empty_struct, "radrms_gcorr_deconv_err", nan)

; xrms errors
  empty_struct = create_struct(empty_struct, "xrms_err", nan)
  empty_struct = create_struct(empty_struct, "xrms_extrap_err", nan)
  empty_struct = create_struct(empty_struct, "xrms_extrap_deconv_err", nan)
  empty_struct = create_struct(empty_struct, "xrms_gcorr_err", nan)
  empty_struct = create_struct(empty_struct, "xrms_gcorr_deconv_err", nan)

; yrms errors
  empty_struct = create_struct(empty_struct, "yrms_err", nan)
  empty_struct = create_struct(empty_struct, "yrms_extrap_err", nan)
  empty_struct = create_struct(empty_struct, "yrms_extrap_deconv_err", nan)
  empty_struct = create_struct(empty_struct, "yrms_gcorr_err", nan)
  empty_struct = create_struct(empty_struct, "yrms_gcorr_deconv_err", nan)

; vrms errors
  empty_struct = create_struct(empty_struct, "vrms_err", nan)
  empty_struct = create_struct(empty_struct, "vrms_extrap_err", nan)
  empty_struct = create_struct(empty_struct, "vrms_extrap_deconv_err", nan)
  empty_struct = create_struct(empty_struct, "vrms_gcorr_err", nan)
  empty_struct = create_struct(empty_struct, "vrms_gcorr_deconv_err", nan)


  return, empty_struct

end
