function sample_at_res $
   , data = in_data $
   , hdr = in_hdr $
   , ra_samp = ra_samp $
   , dec_samp = dec_samp $
   , target_res_as = target_res_as $
   , target_hdr = target_hdr $
   , show=show
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if (n_elements(ra_samp) eq 0) or $
     (n_elements(dec_samp) eq 0) or $
     (n_elements(dec_samp) ne n_elements(ra_samp)) then begin
     message, 'Need matched RA, Dec vectors. Check input. Returning.', /info
     return, !values.f_nan
  endif

; Attempt to smartly detect whether the file is a string (FITS file
; name) or data. Error catch the need for a header.

  if size(in_data, /type) eq size("hello", /type) then begin
     fname = in_data
     if file_test(fname) then $
        fits_read, fname, data, hdr $
     else begin
        message, 'File '+fname+' not found. Returning.', /info
        return, ra_samp*!values.f_nan
     endelse
  endif else begin
     data = in_data     
     if n_elements(in_hdr) gt 0 then $
        hdr = in_hdr
  endelse

  if n_elements(target_res_as) eq 0 then begin
     message, 'No target resolution specified.'+$
              'Taking this as zero and sampling at the native resolution.', /info
     target_res_as = 0
  endif 

  if n_elements(target_hdr) eq 0 then begin
     message, 'No target header. Will use native astrometry', /info
  endif 

  sz = size(data)
  if sz[0] eq 3 then $
     is_cube = 1B $
  else $
     is_cube = 0B

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CONVOLVE AND ALIGN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Convolve (if needed)

  current_bmaj = sxpar(hdr,"BMAJ")
  if current_bmaj lt target_res_as/3600. then begin
     conv_with_gauss, $
        data = data $
        , hdr = hdr $
        , out_data = data_out $
        , out_hdr = hdr_out $
        , target_beam = target_res_as*[1,1,0] $
        , /quiet
     data = data_out
     hdr = hdr_out
  endif else begin     
     message, "Already at target resolution.", /info
  endelse

; Align (if needed)

  if n_elements(target_hdr) ne 0 then begin
     if is_cube then begin
        cube_hastrom $
           , data=data $
           , hdr=hdr $
           , target_hdr =target_hdr $
           , op='POS' $
           , outcube = data_out $
           , outhdr = hdr_out
        data = data_out
        hdr = hdr_out
     endif else begin
        hastrom, data, hdr, target_hdr $
                 , cubic=-0.5, interp=2, missing=!values.f_nan
     endelse
  endif else begin
     message, "No alignment because no target header supplied.", /info
  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SAMPLE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  adxy, target_hdr, ra_samp, dec_samp, x_samp, y_samp
  n_pts = n_elements(x_samp)
  sz = size(data)

  if keyword_set(show) then begin
     loadct, 33
     if is_cube then begin
        disp, max(data, dim=3), /sq
     endif else begin
        disp, data, /sq
     endelse
     oplot, [x_samp], [y_samp], ps=1, thick=3, color=cgcolor('white')
  endif

  if is_cube then begin     
     result = fltarr(n_pts, sz[3])*!values.f_nan
  endif else begin
     result = fltarr(n_pts)*!values.f_nan
  endelse
  
  in_map = $
     where((x_samp gt 0) and (x_samp lt sz[1]) and $
           (y_samp gt 0) and (y_samp lt sz[2]), in_map_ct)
  if in_map_ct gt 0 then begin 
     if is_cube then begin     
        for kk = 0, in_map_ct-1 do $
           result[in_map[kk],*] = data[x_samp[in_map[kk]], y_samp[in_map[kk]], *]
     endif else begin
        result[in_map] = data[x_samp[in_map], y_samp[in_map]]
     endelse
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, result

end
