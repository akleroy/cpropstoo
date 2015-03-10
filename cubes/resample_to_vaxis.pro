pro resample_to_vaxis $
   , data=data $
   , hdr=hdr $
   , vaxis=vaxis_old $
   , target_hdr=target_hdr $
   , target_vaxis=vaxis_new $
   , out_data = out_data $
   , out_hdr = out_hdr $
   , missing = missing_val

  if n_elements(missing_val) eq 0 then $
     missing_val = !values.f_nan

;  ADD STRING/DATA READ HERE

   if n_elements(hdr) ne 0 then $
      make_axes, hdr, vaxis=vaxis_old, /vonly
   
   if n_elements(target_hdr) ne 0 then $
      make_axes, target_hdr, vaxis=vaxis_new, /vonly
   new_sz3 = n_elements(vaxis_new)

; MAKE THE OUTPUT
   sz = size(data, /dim)
   out_data = fltarr(sz[0],sz[1],new_sz3)

   deltav = abs(vaxis_old[1] - vaxis_old[0])
   bad_ind = where(vaxis_new lt min(vaxis_old-deltav/2.,/nan) or $
                   vaxis_new gt max(vaxis_old+deltav/2.,/nan), bad_ct)

;  COULD ADD A FAST MODE FOR LINEAR OR NEAREST NEIGHBOR THAT OPERATES
;  PER CHANNEL - ONLY ONE AXIS TO LOOP OVER

   for i = 0, sz[0]-1 do begin
      for j = 0, sz[1]-1 do begin
         old_spec = data[i,j,*]
;        IMPLEMENT INTERPOLATION OPTIONS HERE
         new_spec = interpol(old_spec, vaxis_old, vaxis_new)
         if bad_ct gt 0 then new_spec[bad_ind] = missing_val
         out_data[i,j,*] = new_spec
      endfor
   endfor

   if n_elements(hdr) gt 0 then begin
;     ASSUME RADIO EQUAL FREQUENCY SPACING
      out_hdr = hdr
      sxaddpar, out_hdr, 'NAXIS3', n_elements(vaxis_new)
      sxaddpar, out_hdr, 'CRPIX3', 1
      sxaddpar, out_hdr, 'CDELT3', vaxis_new[1] - vaxis_new[0]
      sxaddpar, out_hdr, 'CRVAL3', vaxis_new[0]
      if n_elements(target_hdr) gt 0 then begin
         sxaddpar, out_hdr, 'CTYPE3', sxpar(target_hdr, 'CTYPE3')
         sxaddpar, out_hdr, 'CUNIT3', sxpar(target_hdr, 'CUNIT3')
      endif
   endif
;  DO BETTER WITH VELOCITY KEYWORDS - ALTRVAL, ETC.

end
