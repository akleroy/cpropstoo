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
                 peaknum: 0L $
                 }

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mom0" $
                 , unit="K*pix^3")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mom1x" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mom1y" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mom1v" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mom2x" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mom2y" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mom2v" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mom2maj" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "mom2min" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "momposang" $
                 , unit="rad")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "covar_xy" $
                 , unit="")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "area" $
                 , unit="pix^2")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "areahalfmax" $
                 , unit="pix^2")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ellmaj" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ellmin" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ellposang" $
                 , unit="rad")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ellmajhalfmax" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ellminhalfmax" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "ellposanghalfmax" $
                 , unit="rad")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "deltav" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "deltavhalfmax" $
                 , unit="pix")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "npix" $
                 , unit="")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "maxval" $
                 , unit="K")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "minval" $
                 , unit="K")

  empty_struct = create_struct(empty_struct, "clipped", 0B)
  
  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "clipval" $
                 , unit="K")

  empty_struct = add_prop_to_struct( $
                 empty_struct $
                 , "noise" $
                 , unit="K")
  
  return, empty_struct

end                             
