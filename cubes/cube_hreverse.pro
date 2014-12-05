function cube_hreverse, file_in = file_in $
                        , file_out = file_out $
                        , cube_in = input $
                        , hdr_in = orig_hdr $
                        , hdr_out = new_hdr $
                        , subs=subs $
                        , quiet=quiet $
                        , noreturn=noreturn
;+
; NAME:
;
; cube_hreverse
;
; PURPOSE:
;
; Accept a file name or a cube + header and an axis index. Apply the goddard
; routine HREVERSE to the cube plane-by-plane.
;
; CATEGORY:
;
; Science program (astrometry utility).
;
; CALLING SEQUENCE:
;
; dummy = cube_hreverse(file_in = file_in, file_out=file_out 
;                      , sub=sub, /quiet, /noreturn)
;
; -or-
;
; new_cube = cube_hreverse(cube_in = cube_in, hdr_in=hdr_in, 
;                         , sub=sub)
;
; INPUTS:
;
; The program requires a header (hdr_in) and cube (cube_in) to be changed and
; a "sub" value indicating the index to be reversed. The rest is gravy.
;
; OPTIONAL INPUTS:
;
; CUBE_REVERSE will read the cube+header from file_in and will write a new
; file and header to file_out.
;
; KEYWORD PARAMETERS:
;
; If you don't want the cube (sometimes large) returned then flip the
; /noreturn flag. If you don't want to see the progress counter, flip the
; /quiet flag.
;
; OUTPUTS:
; 
; The newly-flipped cube.
;
; OPTIONAL OUTPUTS:
;
; An updated header (hdr_out) and a file written to disk (file_out).
;
; COMMON BLOCKS:
;
; None.
;
; SIDE EFFECTS:
;
; A profound sense of moral superiority. Fight this.
;
; RESTRICTIONS:
;
; Corrective lenses.
;
; PROCEDURES USED:
;
; hreverse, counter (could be commented out w/ no problem), readfits,
; writefits, sx---par, twod_head
;
; MODIFICATION HISTORY:
;
; documented - 09 apr 08 leroy@mpia.de
;
;-

; READ FILE FROM DISK IF FILENAME SUPPLIED
  if n_elements(file_in) gt 0 then begin
      input = readfits(file_in,orig_hdr)
  endif

  if n_elements(subs) eq 0 then begin
      message, 'Need SUBS keyword.'
  endif

; COPY THE ORIGINAL AND TARGET HEADERS SO WE CAN FUTZ WITH THEM
  hdr_copy = twod_head(orig_hdr)
  
; GET THE SIZES OF THE INPUT DATA SET
  nz = (size(input))[3]
  nx = sxpar(hdr_copy,'NAXIS1')
  ny = sxpar(hdr_copy,'NAXIS2')

; INITIALIZE AN OUTPUT CUBE
  output = fltarr(nx,ny,nz)*!values.f_nan
  
; LOOP OVER THE CUBE AND USE HASTROM TO ALIGN EACH PLANE 

; (N.B. A FASTER WAY TO DO THIS WOULD BE TO SWIPE THE INTERPOLATION CODE FROM
; FROM hastrom. THIS WOULD AVOID REPEATING THE SAME CALCULATION FOR EVERY
; PLANE OF THE CUBE. THIS WAY IS A BIT CLEARER AND DOESN'T CARE IF hastrom
; CHANGES UNDERNEATH IT (AS IT SOMETIMES DOES!)

  for k = 0, nz-1 do begin
      if not keyword_set(quiet) then $
        counter, k+1, nz, 'CUBE_HREVERSE: plane '
      new_copy = hdr_copy
      plane = input[*,*,k]
      hreverse, plane, new_copy, subs, /silent
      output[*,*,k] = plane
  endfor

; MAKE AN UPDATED HEADER FOR THE ORIGINAL CUBE ... SHOULD BE OKAY TO USE THE
; LAST HREVERSEd COPY AND THEN JUST PATCH THE NAXIS, NAXIS3, AND NAXIS4
; KEYWORDS BACK IN...

  hdr_out = new_copy
  sxaddpar, hdr_out, 'NAXIS',sxpar(orig_hdr,'NAXIS')
  n3 = sxpar(orig_hdr,'NAXIS3',count=ct)
  if (ct gt 0) then sxaddpar,hdr_out,'NAXIS3',n3,after='NAXIS2'
  n4 = sxpar(orig_hdr,'NAXIS4',count=ct)
  if (ct gt 0) then sxaddpar,hdr_out,'NAXIS4',n4,after='NAXIS3'

; IF REQUESTED, WRITE TO DISK
  if n_elements(file_out) gt 0 then begin
      writefits,file_out,output,hdr_out
  endif

  new_hdr = hdr_out

; RETURN THE CUBE (UNLESS THE USER REQUESTS NOT)
  if keyword_set(noreturn) then begin
      return, -1
  endif else begin
      return, output
  endelse

end
