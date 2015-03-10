function twod_head, orig_hdr

;+
; NAME:
;
; twod_head
;
; PURPOSE:
;
; silly little program to hack a header from XXX to 2 dimensions. useful for
; stuff like hastrom that require specifically two dimensions to function.
;
; CATEGORY:
;
; glorified string command
;
; CALLING SEQUENCE:
;
; hacked_header = twod_head(real_header)
;
; -or-
;
; new_cube = cube_hastrom(cube_in = cube_in, hdr_in=hdr_in, 
;                         , target_hdr = target_hdr)
;
; INPUTS:
;
; A fits header?
;
; OPTIONAL INPUTS:
;
; nichts
;
; KEYWORD PARAMETERS:
;
; nada
;
; OUTPUTS:
; 
; The two-d header.
;
; OPTIONAL OUTPUTS:
;
; zilch
;
; COMMON BLOCKS:
;
; zero
;
; SIDE EFFECTS:
;
; in a miniscule fraction of users, river blindness
;
; RESTRICTIONS:
;
; please do not use this in space
;
; PROCEDURES USED:
;
;
; MODIFICATION HISTORY:
;
; written - 11 apr 08 leroy@mpia.de
;
;-

; COPY THE ORIGINAL
  hdr_copy = orig_hdr

; HOW MANY AXES?
  naxis = sxpar(orig_hdr,'NAXIS')

; NOW YOU THINK IT'S TWO!
  sxaddpar, hdr_copy, 'NAXIS', 2 

; ZAP ALL THOSE LYING EXTRA AXES
  if naxis gt 2 then begin
      for i = 3, naxis do begin
          sxdelpar, hdr_copy, 'NAXIS'+string(i,format='(I1)')
      endfor
  endif

; RETURN THE FAKE HEADER
  return, hdr_copy
end
