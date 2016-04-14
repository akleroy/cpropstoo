function add_band_to_struct $
   , struct=prev_struct $
   , band=band_name $
   , unit=unit_string $
   , desc=desc_string

;+
;
; NAME:
;
; add_band_to_struct
;
; PURPOSE:
;
; Adds several standard fields associated with a band name to a
; structure, returning a new structure. Utility for building an
; intensity mapping database.
;
; USAGE
;
; new_struct = add_band_to_struct(struct=old_struct, band="bandname")
;
; NOTE:
;
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Create an empty structure if one is not supplied.
  if n_elements(prev_struct) eq 0 then begin
     prev_struct = {}
  endif

; Default to an empty description
  if n_elements(desc_string) eq 0 then begin
     desc_string = ""
  endif

; Default to an empty unit
  if n_elements(unit_string) eq 0 then begin
     unit_string = ""
  endif

; For convenience, assign "not-a-number" to the nan variable
  nan = !values.f_nan

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  int_name = "INT_VAL_"+strupcase(band_name)
  new_struct = create_struct(prev_struct, int_name, nan)

  uc_name = "INT_UC_"+strupcase(band_name)
  new_struct = create_struct(new_struct, uc_name, nan)

  cov_name = "INT_COV_"+strupcase(band_name)
  new_struct = create_struct(new_struct, cov_name, nan)

  unit_name = "INT_UNIT_"+strupcase(band_name)
  new_struct = create_struct(new_struct, unit_name, unit_string)

  desc_name = "INT_DESC_"+strupcase(band_name)
  new_struct = create_struct(new_struct, desc_name, desc_string)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, new_struct

end
