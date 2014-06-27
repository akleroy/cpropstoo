function add_prop_to_struct $
   , struct $
   , prop_name $
   , unit=unit $
   , errors=errors

;+
;
; NAME:
;
;   empty_props_struct
;
; PURPOSE:
;
;-

; FOR CONVENIENCE, ASSIGN "not-a-number" TO THE nan VARIABLE
  nan = !values.f_nan

; DEFAULT THE UNIT TO AN EMPTY STRING
  if n_elements(unit) eq 0 then $
     unit = ""

; UNITS
  new_struct = $
     create_struct( $
     struct $
     , prop_name+"_unit" $
     , unit)

; MEASUREMENT
  new_struct = $
     create_struct( $
     new_struct $
     , prop_name+"_meas" $
     , nan)

  new_struct = $
     create_struct( $
     new_struct $
     , prop_name+"_meas_err" $
     , nan)

; DATA-DRIVEN EXTRAPOLATION
  new_struct = $
     create_struct( $
     new_struct $
     , prop_name+"_extrap" $
     , nan)

  new_struct = $
     create_struct( $
     new_struct $
     , prop_name+"_extrap_err" $
     , nan)

; GAUSSIAN APERTURE CORRECTION
  new_struct = $
     create_struct( $
     new_struct $
     , prop_name+"_gcorr" $
     , nan)
  
  new_struct = $
     create_struct( $
     new_struct $
     , prop_name+"_gcorr_err" $
     , nan)

; BEST ESTIMATE
  new_struct = $
     create_struct( $
     new_struct $
     , prop_name $
     , nan)

  new_struct = $
     create_struct( $
     new_struct $
     , prop_name+"_err" $
     , nan)
  
  return, new_struct

end                             
