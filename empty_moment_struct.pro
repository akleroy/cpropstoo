function empty_moment_struct

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
