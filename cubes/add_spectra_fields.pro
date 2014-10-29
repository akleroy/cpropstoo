function add_spectra_fields $
   , prev_struct $
   , nchan

;+
;
; NAME:
;
;   add_spectra_fields
;
; PURPOSE:
;
; Adds spectra fields to a CPROPS structure. Needs a seed structure
; and a number of channels.  Returns a new structure with properties
; fields in place.
;
; NOTE:
;
; This is an infrastructure routine and not documented for general
; use.
;
;-

; FOR CONVENIENCE, ASSIGN "not-a-number" TO THE nan VARIABLE
  nan = !values.f_nan

; major axis rms errors
  empty_struct = create_struct(prev_struct, "spectra", dblarr(nchan))
  empty_struct = create_struct(empty_struct, "velocity", dblarr(nchan))

  return, empty_struct

end
