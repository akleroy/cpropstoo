function add_props_fields $
   , prev_struct

;+
;
; NAME:
;
;   add_props_fields
;
; PURPOSE:
;
; Adds fields to a structure that are expected in any CPROPS property
; measurement. Accepts a seed structure. If one is not supplied, it
; generates an empty moment strucutre. Returns a new structure with
; properties fields in place.
;
; NOTE:
;
; This is an infrastructure routine and not documented for general
; use.
;
;-

; FOR CONVENIENCE, ASSIGN "not-a-number" TO THE nan VARIABLE
  nan = !values.f_nan

; DEFAULT TO AN EMPTY BARE-BONES MOMENT STRUCTURE
  if n_elements(prev_struct) eq 0 then $
     prev_struct = empty_moment_struct()

; ADD IDENTIFIER FIELDS
  empty_struct = create_struct(prev_struct, "tag", "")
  empty_struct = create_struct(empty_struct, "gal", "")
  empty_struct = create_struct(empty_struct, "filename", "")

; DISTANCE
  empty_struct = create_struct(empty_struct, "dist_pc", nan)

; THE BEAM (2D)
  empty_struct = create_struct(empty_struct, "bmaj_deg", nan)
  empty_struct = create_struct(empty_struct, "bmin_deg", nan)
  empty_struct = create_struct(empty_struct, "bpa_deg", nan)

; THE BEAM (1D)
  empty_struct = create_struct(empty_struct, "beamfwhm_pix", nan)
  empty_struct = create_struct(empty_struct, "beamfwhm_deg", nan)
  empty_struct = create_struct(empty_struct, "beamfwhm_pc", nan)

  empty_struct = create_struct(empty_struct, "pixperbeam", nan)
  empty_struct = create_struct(empty_struct, "srperbeam", nan)
  empty_struct = create_struct(empty_struct, "pc2perbeam", nan)

; PIXEL SCALE
  empty_struct = create_struct(empty_struct, "degperpix", nan)
  empty_struct = create_struct(empty_struct, "pcperpix", nan)

; CHANNEL WIDTH
  empty_struct = create_struct(empty_struct, "chanwidth_kms", nan)

; CONSTANTS
  empty_struct = create_struct(empty_struct, "chantosig", nan)
  empty_struct = create_struct(empty_struct, "rmstorad", nan)
  empty_struct = create_struct(empty_struct, "vircoeff", nan)
  empty_struct = create_struct(empty_struct, "sig_to_fwhm", nan)
  empty_struct = create_struct(empty_struct, "ell_to_sig_half", nan)

; MOLECULAR LINE
  empty_struct = create_struct(empty_struct, "line", "")
  empty_struct = create_struct(empty_struct, "alpha", nan)

  return, empty_struct

end
