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

; Check if the field already exists
  tags = tag_names(prev_struct)

  empty_spec = fltarr(n_chan)*nan

  int_name = "SPEC_VAL_"+strupcase(line_name)
  vchan0_name = "SPEC_VCHAN0_"+strupcase(line_name)
  deltav_name = "SPEC_DELTAV_"+strupcase(line_name)
  uc_name = "SPEC_UC_"+strupcase(line_name)
  cov_name = "SPEC_COV_"+strupcase(line_name)
  res_name = "SPEC_RES_"+strupcase(line_name)
  unit_name = "SPEC_UNIT_"+strupcase(line_name)
  desc_name = "SPEC_DESC_"+strupcase(line_name)

  if total(tags eq int_name) gt 0 then begin
;    ... this is risky. Delete and replace instead?
     message, "Band "+int_name+" already in structure.", /info
     message, "Resetting values and returning.", /info
     new_struct = prev_struct
     new_struct.(where(tags eq int_name)) = empty_spec
     new_struct.(where(tags eq vchan0_name)) = nan
     new_struct.(where(tags eq deltav_name)) = nan
     new_struct.(where(tags eq uc_name)) = nan
     new_struct.(where(tags eq cov_name)) = nan
     new_struct.(where(tags eq res_name)) = nan
     new_struct.(where(tags eq unit_name)) = unit_string
     new_struct.(where(tags eq desc_name)) = desc_string
  endif else begin
     if n_elements(prev_struct) gt 0 then begin
        is_array = 1B
     endif else begin
        is_array = 0B
     endelse
     new_struct = create_struct(prev_struct[0], int_name, empty_spec)
     new_struct = create_struct(new_struct, vchan0_name, nan)
     new_struct = create_struct(new_struct, deltav_name, nan)
     new_struct = create_struct(new_struct, uc_name, nan)
     new_struct = create_struct(new_struct, cov_name, nan)
     new_struct = create_struct(new_struct, res_name, nan)
     new_struct = create_struct(new_struct, unit_name, unit_string)
     new_struct = create_struct(new_struct, desc_name, desc_string)
     if is_array then begin
        new_struct = replicate(new_struct, n_elements(prev_struct))
        struct_assign, prev_struct, new_struct
     endif
  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, new_struct

end
