function calc_pixperbeam $
   , hdr=hdr $
   , bmaj=bmaj $
   , bmin=bmin $
   , pix_scale=pix_scale $
   , aips=aips $
   , fwhm_pix=beamfwhm_pix

;
; Document later. Helper function.
;

  if n_elements(hdr) gt 0 then begin
     if keyword_set(aips) then begin
        getaipsbeam, hdr, bmaj=bmaj, bmin=bmin
     endif else begin
        if n_elements(bmaj) eq 0 then $
           bmaj = sxpar(hdr, 'BMAJ')
        if n_elements(bmin) eq 0 then $
           bmin = sxpar(hdr, 'BMIN')
     endelse
     
     if n_elements(pix_scale) eq 0 then begin
        xyad, hdr, 0, 0, ra0, dec0
        xyad, hdr, 1, 0, ra1, dec1
        pix_scale = sphdist(ra0, dec0, ra1, dec1, /deg)
     endif
  endif

  beamfwhm_deg = sqrt(bmaj*bmin)
  beamfwhm_pix = beamfwhm_deg/pix_scale
  pixperbeam = (beamfwhm_pix/2.0)^2*!pi/alog(2)
  
  return, pixperbeam

end
