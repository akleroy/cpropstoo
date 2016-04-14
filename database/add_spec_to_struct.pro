function add_spec_to_struct $
   , struct=prev_struct $
   , line=line_name $
   , unit=unit_string $
   , desc=desc_string $
   , n_chan = n_chan

;+
;
; NAME:
;
; add_band_to_struct
;
; PURPOSE:
;
; Adds several standard fields associated with a cube name to a
; structure, returning a new structure. Utility for building an
; spectroscopic mapping database.
;
; IDL is inflexible with regard to arrays in structures and backwards
; compatbility is an issue for us. As a stopgap, we will adopt the
; not-great approach of assuming that 500 elements is enough for most
; extragalactic cases. This number can be modified.
;
; USAGE
;
; new_struct = add_cube_to_struct(struct=old_struct, band="bandname")
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

; Default to 500 channels
  if n_elements(n_chan) eq 0 then begin
     n_chan = 500L
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

  int_name = "SPEC_VAL_"+strupcase(line_name)
  empty_spec = fltarr(n_chan)*nan
  new_struct = create_struct(prev_struct, int_name, empty_spec)

  vchan0_name = "SPEC_VCHAN0_"+strupcase(line_name)
  new_struct = create_struct(new_struct, vchan0_name, nan)

  deltav_name = "SPEC_DELTAV_"+strupcase(line_name)
  new_struct = create_struct(new_struct, deltav_name, nan)

  uc_name = "SPEC_UC_"+strupcase(line_name)
  new_struct = create_struct(new_struct, uc_name, nan)

  cov_name = "SPEC_COV_"+strupcase(line_name)
  new_struct = create_struct(new_struct, cov_name, nan)

  unit_name = "SPEC_UNIT_"+strupcase(line_name)
  new_struct = create_struct(new_struct, unit_name, unit_string)

  desc_name = "SPEC_DESC_"+strupcase(line_name)
  new_struct = create_struct(new_struct, desc_name, desc_string)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, new_struct

end
