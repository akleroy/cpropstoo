function empty_moment_struct

;+
;
; NAME:
;
;   empty_moment_struct
;
; PURPOSE:
; 
; Return a structure holding the first few common fields present in
; any CPROPS moment measurement.
;
; NOTE:
;
; This is purely an infrastructure routine and not heavily documented.
;
;-

; FOR CONVENIENCE, ASSIGN "not-a-number" TO THE nan VARIABLE
  nan = !values.f_nan

  empty_struct = { $
                 peaknum: 0L, $
                 npix: 0L, $
                 minval: nan, $
                 minval_unit: "K", $
                 maxval: nan, $
                 maxval_unit: "K", $                
                 noise: nan, $
                 noise_unit: "K", $
                 clipped: 0B, $
                 clipval: nan, $
                 clipval_unit: "K" $
                 }
  
  return, empty_struct

end                             
