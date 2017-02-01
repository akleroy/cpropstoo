function cube_hextract, file_in = file_in $
                        , file_out = file_out $
                        , cube_in = input $
                        , hdr_in = orig_hdr $
                        , hdr_out = hdr_out $
                        , x0 = x0 $
                        , x1 = x1 $
                        , y0 = y0 $
                        , y1 = y1 $
                        , quiet=quiet $
                        , noreturn=noreturn $
                        , _extra = _extra
;+
; NAME:
;
; cube_hextract
;
; PURPOSE:
;
; Accept a file name or a cube + header run hextract plane by plane.
;
; CATEGORY:
;
; Science program (astrometry utility).
;
; CALLING SEQUENCE:
;
; dummy = cube_hextract(file_in = file_in, file_out=file_out 
;                      , x0=x0, x1=x1, y0=y0, y1=y1, /quiet, /noreturn)
;
; -or-
;
; new_cube = cube_hextract(cube_in = cube_in, hdr_in=hdr_in, 
;                         , x0=x0, x1=x1, y0=y0, y1=y1)
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

; READ FILE FROM DISK IF FILENAME SUPPLIED
  if n_elements(file_in) gt 0 then begin
      input = readfits(file_in,orig_hdr)
  endif

; COPY THE ORIGINAL AND TARGET HEADERS SO WE CAN FUTZ WITH THEM
  hdr_copy = gls_to_sfl(twod_head(orig_hdr))
  
; GET THE SIZES OF THE INPUT DATA SET
  nz = (size(input))[3]

  plane_0 = input[*,*,0]
  hextract, plane_0, hdr_copy, new_plane, new_hdr, x0, x1, y0, y1, /silent

  nx_out = sxpar(new_hdr, 'NAXIS1')
  ny_out = sxpar(new_hdr, 'NAXIS2')
  
; INITIALIZE AN OUTPUT CUBE
  output = fltarr(nx_out,ny_out,nz)*!values.f_nan
  
; LOOP OVER THE CUBE AND USE HEXTRACT ON EACH PLANE 

  for k = 0, nz-1 do begin
      if not keyword_set(quiet) then $
        counter, k+1, nz, 'CUBE_HEXTRACT: plane '
      new_copy = hdr_copy
      plane = input[*,*,k]
      hextract, plane, new_copy, new_plane, new_copy $
                , x0, x1, y0, y1, /silent
      output[*,*,k] = new_plane
  endfor

; MAKE AN UPDATED HEADER FOR THE ORIGINAL CUBE ... SHOULD BE OKAY TO USE THE
; LAST HASTROMed COPY AND THEN JUST PATCH THE NAXIS, NAXIS3, AND NAXIS4
; KEYWORDS BACK IN...

  hdr_out = new_copy
  sxaddpar, hdr_out, 'NAXIS', sxpar(orig_hdr,'NAXIS')
  n3 = sxpar(orig_hdr,'NAXIS3',count=ct)
  if (ct gt 0) then sxaddpar,hdr_out,'NAXIS3',n3,after='NAXIS2'
  n4 = sxpar(orig_hdr,'NAXIS4',count=ct)
  if (ct gt 0) then sxaddpar,hdr_out,'NAXIS4',n4,after='NAXIS3'

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
