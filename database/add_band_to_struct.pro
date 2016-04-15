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
; ADD THE FIELDS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Check if the field already exists
  tags = tag_names(prev_struct)
 
  int_name = "INT_VAL_"+strupcase(band_name)
  uc_name = "INT_UC_"+strupcase(band_name)
  cov_name = "INT_COV_"+strupcase(band_name)
  res_name = "INT_RES_"+strupcase(band_name)
  unit_name = "INT_UNIT_"+strupcase(band_name)
  desc_name = "INT_DESC_"+strupcase(band_name)

  if total(tags eq int_name) gt 0 then begin
;    ... this is risky. Delete and replace instead?
     message, "Band "+int_name+" already in structure.", /info
     message, "Resetting values and returning.", /info
     new_struct = prev_struct
     new_struct.(where(tags eq int_name)) = nan
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
     new_struct = create_struct(prev_struct[0], int_name, nan)
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
