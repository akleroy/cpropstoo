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
                 mom0: empty_prop_struct(unit="K*pix^3"), $
                 mom1_x: empty_prop_struct(unit="pix"), $
                 mom1_y: empty_prop_struct(unit="pix"), $
                 mom1_v: empty_prop_struct(unit="pix"), $
                 mom2_x: empty_prop_struct(unit="pix"), $
                 mom2_y: empty_prop_struct(unit="pix"), $
                 mom2_v: empty_prop_struct(unit="pix"), $
                 posang: empty_prop_struct(unit="rad"), $
                 mom2_maj: empty_prop_struct(unit="pix"), $
                 mom2_min: empty_prop_struct(unit="pix"), $                 
                 area: empty_prop_struct(unit="pix^2"), $
                 area_halfmax: empty_prop_struct(unit="pix^2"), $
                 ell_maj: empty_prop_struct(unit="pix"), $
                 ell_min: empty_prop_struct(unit="pix"), $
                 ell_pa:  empty_prop_struct(unit="rad"), $
                 ell_maj_halfmax: empty_prop_struct(unit="pix"), $
                 ell_min_halfmax: empty_prop_struct(unit="pix"), $
                 ell_pa_halfmax:  empty_prop_struct(unit="rad"), $
                 deltav: empty_prop_struct(unit="pix"), $
                 deltav_halfmax: empty_prop_struct(unit="pix"), $
                 covar_xy: empty_prop_struct(unit=""), $
                 npix: empty_prop_struct(unit=""), $
                 maxval: empty_prop_struct(unit="K"), $
                 minval: empty_prop_struct(unit="K"), $
                 clipval: empty_prop_struct(unit="K"), $
                 noise: empty_prop_struct(unit="K") $
                 }

  return, empty_struct

end                             
