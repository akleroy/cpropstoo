function gls_to_sfl, orig_hdr, update=update

;+
; NAME:
;
; gls_to_sfl
;
; PURPOSE:
;
; silly little program to hack a header and change the name of the projection
; from GLS to SFL. Please be careful, though, this is correct only for the new
; GLS. 
;
; For the old GLS there's an AIPS memo from the mid 80s that describes
; what you need to do, which is basically setting the reference
; position to sit on the equator.
;
; CATEGORY:
;
; glorified string command
;
; CALLING SEQUENCE:
;
; hacked_header = gls_to_sfl(real_header)
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
; The SFL header.
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
; yup
;
; RESTRICTIONS:
;
; redacted
;
; PROCEDURES USED:
;
;
; MODIFICATION HISTORY:
;
; written - 17 nov 08 leroy@mpia.de
;
;-

; COPY THE ORIGINAL
  hdr_copy = orig_hdr

  if keyword_set(update) then begin
; CHECK IF CROTA IS 0
     if sxpar(hdr_copy,'CROTA1') ne 0 or $
        sxpar(hdr_copy,'CROTA2') ne 0 then begin
        message, "Warning! Your image has a rotation, "+ $
                 "which is problematic for GLS->SFL", /info
     endif
     
; CHECK IF CRVAL IS 0
     crval1 = sxpar(hdr_copy, 'CRVAL1')
     crpix1 = sxpar(hdr_copy, 'CRPIX1')
     cdelt1 = sxpar(hdr_copy, 'CDELT1')
     
     crval2 = sxpar(hdr_copy, 'CRVAL2')
     crpix2 = sxpar(hdr_copy, 'CRPIX2')
     cdelt2 = sxpar(hdr_copy, 'CDELT2')

     if crval1 ne 0 or crval2 ne 0 then begin

        new_crpix1 = crpix1 - crval1/cdelt1
        sxaddpar, hdr_copy, 'CRPIX1', new_crpix1, 'UPDATED'
        sxaddpar, hdr_copy, 'CRVAL1', 0.0, 'UPDATED'
        
        new_crpix2 = crpix2 - crval2/cdelt2
        sxaddpar, hdr_copy, 'CRPIX2', new_crpix2, 'UPDATED'
        sxaddpar, hdr_copy, 'CRVAL2', 0.0, 'UPDATED'
     endif

  endif


  ctype1 = sxpar(hdr_copy,'CTYPE1')
  if strcompress(ctype1,/remove_all) eq 'RA---GLS' then $
    sxaddpar, hdr_copy, 'CTYPE1', 'RA---SFL', 'REPLACED GLS'

  ctype2 = sxpar(hdr_copy,'CTYPE2')
  if  strcompress(ctype2,/remove_all) eq 'DEC--GLS' then $
    sxaddpar, hdr_copy, 'CTYPE2', 'DEC--SFL', 'REPLACED GLS'

; RETURN THE FAKE HEADER
  return, hdr_copy
end
