function empty_prop_struct $
   , unit=unit

;+
;
; NAME:
;
;   empty_props_struct
;
; PURPOSE:
;
;   
;
;
;-

; FOR CONVENIENCE, ASSIGN "not-a-number" TO THE nan VARIABLE
  nan = !values.f_nan

  if n_elements(unit) eq 0 then $
     unit = ""

; CREATE A STRUCTURE THAT WILL HOLD THE MEASURED GMC PROPERTIES
  empty_struct = { $
                 unit: unit, $
                 val: nan, $           
                 err: nan, $        
                 val_extrap: nan, $   
                 err_extrap: nan, $   
                 val_meas: nan, $   
                 err_meas: nan $
                 }

  return, empty_struct

end                             
