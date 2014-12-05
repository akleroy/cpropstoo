function cube_hrot $
   , file_in = file_in $
   , file_out = file_out $
   , cube_in = input $
   , hdr_in = orig_hdr $
   , hdr_out = hdr_out $
   , angle=angle $
   , xc = xc $
   , yc = yc $
   , pivot = pivot $
   , interp = interp $
   , missing = missing $
   , cubic = cubic $
   , quiet=quiet $
   , noreturn=noreturn $
   , _extra = _extra

;+
; NAME:
;
; cube_hrot
;
; PURPOSE:
;
; Accept a file name or a cube + header run hrot plane by plane.
;
; CATEGORY:
;
; Science program (astrometry utility).
;
; CALLING SEQUENCE:
;
; dummy = cube_hrot()
;
; -or-
;
; new_cube = cube_hrot()
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
; 
;
; OPTIONAL OUTPUTS:
;
;
; COMMON BLOCKS:
;
; None.
;
; SIDE EFFECTS:
;
;
; RESTRICTIONS:
;
; Corrective lenses.
;
; PROCEDURES USED:
;
;
; MODIFICATION HISTORY:
;
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(interp) eq 0 then $
     interp=2

  if n_elements(cubic) eq 0 then $
     cubic = -0.5

  if n_elements(missing) eq 0 then $
     missing = !values.f_nan

  if n_elements(xc) eq 0 then $
     xc = -1

  if n_elements(yc) eq 0 then $
     yc = -1

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; HANDLE DATA INPUT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(file_in) gt 0 then begin
     input = readfits(file_in,orig_hdr)
  endif
  
; COPY THE ORIGINAL AND TARGET HEADERS SO WE CAN FUTZ WITH THEM
  hdr_copy = gls_to_sfl(twod_head(orig_hdr))
  
; GET THE SIZES OF THE INPUT DATA SET
  nz = (size(input))[3]

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RUN ONCE TO SET UP OUTPUT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; NOT NECESSARY FOR HROT, BUT FOLLOWS PATTERN OF MY OTHER CUBE WRAPPERS

  plane_0 = input[*,*,0]
  hrot, plane_0, hdr_copy, new_plane, new_hdr, angle, xc, yc, interp, pivot=pivot $
        , missing=missing, cubic=cubic

  nx_out = sxpar(new_hdr, 'NAXIS1')
  ny_out = sxpar(new_hdr, 'NAXIS2')
  
; INITIALIZE AN OUTPUT CUBE
  output = fltarr(nx_out,ny_out,nz)*!values.f_nan
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; LOOP OVER THE CUBE AND USE HEXTRACT ON EACH PLANE 
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  for k = 0, nz-1 do begin
      if not keyword_set(quiet) then $
        counter, k+1, nz, 'CUBE_HROT: plane '
      new_copy = hdr_copy
      plane = input[*,*,k]
      hrot, plane, new_copy, new_plane, new_copy, angle, xc, yc, interp $
        , missing=missing, cubic=cubic
      output[*,*,k] = new_plane
  endfor

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; MAKE AN UPDATED HEADER ... PULL ASTROMETRY FROM LAST HASTROM
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  hdr_out = orig_hdr
  extast, new_hdr, astr
  putast, hdr_out, astr

; IF REQUESTED, WRITE TO DISK
  if n_elements(file_out) gt 0 then begin
      writefits,file_out,output,hdr_out
  endif

; RETURN THE CUBE (UNLESS THE USER REQUESTS NOT)
  if keyword_set(noreturn) then begin
      return, -1
  endif else begin
      return, output
  endelse

end
