function empty_cloud_struct

;+
;
; NAME:
;
;   empty_cloud_struct
;
; PURPOSE:
;
;   
;
;
;-

; FOR CONVENIENCE, ASSIGN "not-a-number" TO THE nan VARIABLE
  nan = !values.f_nan

; CREATE CLOUD STRUCTURE
  empty_struct = { $
                 id : long(0), $
                 tag : "", $
                 gal : "", $
                 line : "", $
                 filename : "", $
                 dist_pc: nan, $                 
                 beamfwhm_pix: nan, $                 
                 beamfwhm_deg: nan, $                 
                 beamfwhm_pc: nan, $
                 degperpix: nan, $
                 pcperpix: nan, $
                 pixperbeam: nan, $
                 srperbeam: nan, $
                 pc2perbeam: nan, $
                 chanwidth_kms: nan, $
                 chan_to_sig: nan, $
                 rms_to_rad: nan, $
                 xco: nan, $
                 vircoeff: nan, $
                 moments: empty_moment_struct(), $
                 flux: empty_prop_struct(unit="K*km/s*as^2"), $
                 lum: empty_prop_struct(unit="K*km/s*pc^2"), $
                 mass: empty_prop_struct(unit="Msun"), $
                 xpos: empty_prop_struct(unit="deg"), $
                 ypos: empty_prop_struct(unit="deg"), $
                 vpos: empty_prop_struct(unit="km/s"), $
                 xmom: empty_prop_struct(unit="pc"), $
                 ymom: empty_prop_struct(unit="pc"), $
                 vmom: empty_prop_struct(unit="km/s"), $
                 mom_maj: empty_prop_struct(unit="pc"), $
                 mom_min: empty_prop_struct(unit="pc"), $                 
                 mom_posang: empty_prop_struct(unit="deg"), $
                 mom_unresolved: 0B, $
                 ell_hm_maj: empty_prop_struct(unit="pc"), $
                 ell_hm_min: empty_prop_struct(unit="pc"), $
                 ell_hm_posang: empty_prop_struct(unit="deg"), $
                 ell_hm_unresolved: 0B, $
                 area_hm_maj: empty_prop_struct(unit="pc"), $
                 area_hm_unresolved: 0B, $
                 area: empty_prop_struct(unit="pc^2"), $
                 deltav: empty_prop_struct(unit="km/s"), $
                 rad_mom: empty_prop_struct(unit="pc"), $
                 rad_ell: empty_prop_struct(unit="pc"), $
                 rad_area: empty_prop_struct(unit="pc"), $
                 virmass: empty_prop_struct(unit="Msun") $
                 }

  return, empty_struct

end                             
