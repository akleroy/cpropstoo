pro cube_hrebin $
;  Input and output
   , data = data_in $
   , hdr_in = hdr_in $
   , outfile = file_out $
   , outcube = output $
   , outhdr = hdr_out $
;  Factor by which to downsample
   , factor = factor $
   , quiet=quiet $
   , _extra = _extra
  
;+
; NAME:
;
; cube_hrebin
;
; PURPOSE:
;
; CATEGORY:
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; COMMON BLOCKS:
;
; SIDE EFFECTS:
;
; RESTRICTIONS:
;
; PROCEDURES USED:
;
; Needs IDLAstro installed.
;
; MODIFICATION HISTORY:
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if size(data_in, /type) eq size("hello", /type) then begin
     fname = data_in
     fits_read, fname, data, hdr
  endif else begin
     data = data_in
     if n_elements(hdr_in) gt 0 then begin
        hdr = hdr_in 
     endif else begin
        message, "Alignment requires coordinate information"+$
                 ". Returning.", /info
     endelse
  endelse

  if n_elements(factor) eq 0 then begin
     message, "Defaulting to a factor of two.", /info
     factor = 2
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; REBIN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  sz = size(data)
  newx = floor(sz[1]/factor)
  xupper = newx*factor-1
  newy = floor(sz[2]/factor)
  yupper = newy*factor-1

  hdr_copy = twod_head(hdr)

; Work out the target
  plane_0 = data[*,*,0]
  hextract $
     , plane_0, hdr_copy $
     , new_plane, new_hdr, 0, xupper, 0, yupper, /silent
  hrebin $
     , new_plane, new_hdr $
     , final_plane, final_hdr $
     , newx, newy
  output = fltarr(newx,newy,sz[3])*!values.f_nan  

; Make an update header
  hdr_out = final_hdr
  sxaddpar, hdr_out, 'NAXIS',sxpar(hdr,'NAXIS')
  n3 = sxpar(hdr,'NAXIS3',count=ct)
  if (ct gt 0) then sxaddpar,hdr_out,'NAXIS3',n3,after='NAXIS2'  
  n4 = sxpar(hdr,'NAXIS4',count=ct)
  if (ct gt 0) then sxaddpar,hdr_out,'NAXIS4',n4,after='NAXIS3'       

; Loop over planes
  for kk = 0, sz[3]-1 do begin    
     if keyword_set(quiet) eq 0 then $
        counter, kk+1, sz[3], 'CUBE_HREBIN: plane '
     this_plane = data[*,*,kk]
     output[*,*,kk] = rebin(this_plane[0:xupper, 0:yupper], newx, newy)
  endfor

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; OUTPUT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; If requrested, write the cube to disk

  if n_elements(file_out) gt 0 then begin

     writefits,file_out,output,hdr_out

  endif

end
