function stringad $
   , ra_string, dec_string, split=split $
   , quiet=quiet

;+
;
; Convert a string to a decimal ra, dec pair.
;
;-
  
  if n_elements(split) eq 0 then $
     split = ' '

  ra_tokens = strsplit(ra_string, split, /extract)
  
  dec_tokens = strsplit(dec_string, split, /extract)
  
  if n_elements(ra_tokens) ne 3 or $
     n_elements(dec_tokens) ne 3 then begin
     if keyword_set(quiet) eq 0 then $
        message, ra_string+" "+dec_string+" did not parse.", /info
     return, !values.f_nan*[1,1]
  endif

  ra_deg = (float(ra_tokens[0])+ $
            float(ra_tokens[1])/60.+ $
            float(ra_tokens[2])/3600.)*15.0

  if strmid(strcompress(dec_string,/rem),0,1) eq '-' then begin
     dec_sign = -1.0
  endif else begin
     dec_sign = +1.0
  endelse

  dec_deg = (abs(float(dec_tokens[0]))+ $
             dec_tokens[1]/60.+ $
             dec_tokens[2]/3600.)*dec_sign
             
  return, [ra_deg, dec_deg]

end
