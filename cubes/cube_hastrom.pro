function cube_hastrom, file_in = file_in $
                       , file_out = file_out $
                       , cube_in = input $
                       , hdr_in = orig_hdr $
                       , hdr_out = hdr_out $
                       , target_hdr = target_hdr $
                       , quiet=quiet $
                       , noreturn=noreturn $
                       , cubic=cubic $
                       , _extra = _extra
;+
; NAME:
;
; cube_hastrom
;
; PURPOSE:
;
; Accept a file name or a cube + header and a target header containing
; WCS astrometry. Align the input cube to the target astrometry and either
; return the cube or write it to a new file.
;
; CATEGORY:
;
; Science program (astrometry utility).
;
; CALLING SEQUENCE:
;
; dummy = cube_hastrom(file_in = file_in, file_out=file_out 
;                      , target_hdr=target_hdr, /quiet, /noreturn)
;
; -or-
;
; new_cube = cube_hastrom(cube_in = cube_in, hdr_in=hdr_in, 
;                         , target_hdr = target_hdr)
;
; INPUTS:
;
; The program requires a header (hdr_in) and cube (cube_in) to be changed and
; a "target" header (target_hdr) containing the desired astrometry. The rest
; is gravy.
;
; OPTIONAL INPUTS:
;
; CUBE_HASTROM will read the cube+header from file_in and will write a new
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
; The newly-aligned cube.
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
; A profound sense of moral superiority.
;
; RESTRICTIONS:
;
; Corrective lenses.
;
; PROCEDURES USED:
;
; hastrom, counter (could be commented out w/ no problem), readfits,
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

; COPY THE ORIGINAL AND TARGET HEADERS SO WE CAN FUTZ WITH THEM
  hdr_copy = twod_head(orig_hdr)
  target_copy = twod_head(target_hdr)

; DEAL WITH THE FUCKING SFL/GLS THING (UPDATE PROJECTION NAME)
  if (strpos(sxpar(hdr_copy,'CTYPE1'),'GLS') ne -1) then begin
      sxaddpar,hdr_copy,'CTYPE1','RA---SFL','Replaced GLS'
      sxaddpar,hdr_copy,'CTYPE2','DEC--SFL','Replaced GLS'
  endif

  if (strpos(sxpar(target_copy,'CTYPE1'),'GLS') ne -1) then begin
      sxaddpar,target_copy,'CTYPE1','RA---SFL','Replaced GLS'
      sxaddpar,target_copy,'CTYPE2','DEC--SFL','Replaced GLS'
  endif

  
; GET THE SIZES OF THE INPUT DATA SET
  nz = (size(input))[3]
  nx = sxpar(target_copy,'NAXIS1')
  ny = sxpar(target_copy,'NAXIS2')

; INITIALIZE AN OUTPUT CUBE
  output = fltarr(nx,ny,nz)*!values.f_nan
  
; LOOP OVER THE CUBE AND USE HASTROM TO ALIGN EACH PLANE 

; (N.B. A FASTER WAY TO DO THIS WOULD BE TO SWIPE THE INTERPOLATION CODE FROM
; FROM hastrom. THIS WOULD AVOID REPEATING THE SAME CALCULATION FOR EVERY
; PLANE OF THE CUBE. THIS WAY IS A BIT CLEARER AND DOESN'T CARE IF hastrom
; CHANGES UNDERNEATH IT

  for k = 0, nz-1 do begin
      if not keyword_set(quiet) then $
        counter, k+1, nz, 'CUBE_HASTROM: plane '
      new_copy = hdr_copy
      plane = input[*,*,k]
      if keyword_set(cubic) then begin
          hastrom, plane, new_copy, target_copy, missing=!values.f_nan $
            , interp=2, cubic=-0.5, _extra = _extra
      endif else begin
          hastrom, plane, new_copy, target_copy, missing=!values.f_nan $
                   , _extra = _extra
      endelse
      output[*,*,k] = plane
  endfor

; MAKE AN UPDATED HEADER FOR THE ORIGINAL CUBE ... SHOULD BE OKAY TO USE THE
; LAST HASTROMed COPY AND THEN JUST PATCH THE NAXIS, NAXIS3, AND NAXIS4
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

; RETURN THE CUBE (UNLESS THE USER REQUESTS NOT)
  if keyword_set(noreturn) then begin
      return, -1
  endif else begin
      return, output
  endelse

end
