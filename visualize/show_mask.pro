pro show_mask $
   , infile = infile $
   , indata = cube $
   , mask = mask $
   , inmask = inmask $
   , rms = rms
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN THE DATA
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(infile) gt 0 then begin
     file_data = file_search(infile, count=file_ct)
     if file_ct eq 0 then begin
        message, "Data not found.", /info
        return
     endif else begin
        data = readfits(file_data, hdr)
     endelse
  endif

  if n_elements(mask) eq 0 then begin
     if n_elements(inmask) gt 0 then begin
        file_mask = file_search(inmask, count=file_ct)
        if file_ct eq 0 then begin
           message, "Mask file not found.", /info
           return
        endif else begin
           mask = readfits(file_mask, mask_hdr)
        endelse
     endif else begin
        message, "Defaulting to a mask of finite elements.", /info
        mask = finite(data)
     endelse
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SHOW THE PROJECTIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  szdata = size(data)

  loadct, 33

  rms = mad(data)

  if szdata[0] eq 3 then begin
     !p.multi = [0,2,2]

     disp, max(data, dim=3, /nan), max=10*rms $
           , reserve=50
     contour, max(mask, dim=3), lev=[1], /overplot

     disp, max(data, dim=2, /nan), max=10*rms $
           , reserve=50
     contour, max(mask, dim=2), lev=[1], /overplot

     disp, max(data, dim=1, /nan), max=10*rms $
           , reserve=50
     contour, max(mask, dim=1), lev=[1], /overplot
  endif

  if szdata[0] eq 2 then begin
     !p.multi = [0]

     disp, data, max=10*rms, /sq
     contour, mask, lev=[1], /overplot
  endif

end                             ; of show_mask
