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
                 peak_to_edge: nan, $
                 gcorr_1d: nan, $
                 gcorr_area: nan, $
                 gcorr_delta: nan, $
                 gcorr_flux: nan $
                 }
  
  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "flux" $
                 , unit="K*km/s*as^2")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "lum" $
                 , unit="K*km/s*pc^2")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mass" $
                 , unit="Msun")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "xpos" $
                 , unit="deg")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ypos" $
                 , unit="deg")
  
  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "vpos" $
                 , unit="km/s")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "xmom" $
                 , unit="pc")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ymom" $
                 , unit="pc")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "vmom" $
                 , unit="km/s")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mommaj" $
                 , unit="pc")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mommin" $
                 , unit="pc")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "momposang" $
                 , unit="deg")

  empty_struct = create_struct(empty_struct, "mom_unresolved", 0B)

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ellmajhalfmax" $
                 , unit="pc")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ellminhalfmax" $
                 , unit="pc")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ellposanghalfmax" $
                 , unit="deg")

  empty_struct = create_struct(empty_struct, "ellhalfmax_unresolved", 0B)

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "areamajhalfmax" $
                 , unit="pc")
     
  empty_struct = create_struct(empty_struct, "areahalfmax_unresolved", 0B)

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "area" $
                 , unit="pc^2")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "deltav" $
                 , unit="km/s")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "radmom" $
                 , unit="pc")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "radell" $
                 , unit="pc")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "radarea" $
                 , unit="pc")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "virmass" $
                 , unit="Msun")

  return, empty_struct

end                             
