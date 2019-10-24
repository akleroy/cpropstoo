function calc_jtok, hdr=hdr $
                    , bmaj=bmaj $
                    , bmin=bmin $
                    , freq=freq $
                    , restfreq=restfreq $
                    , aips=aips $
                    , pixels_per_beam = pixels_per_beam

;+
;
; NAME:
;
;   CALC_JTOK()
;
; PURPOSE:
;   Helper function to turn cube in units of Jy/Beam into units of K.      
;
;
; CALLING SEQUENCE:
;   jtok = calc_jtok(hdr=hdr [,bmaj=bmaj, bmin=bmin,restfreq=restfreq,aips=aips]) 
;
; INPUTS:
;    HDR -- .fits header as an array of strings.
;    BMAJ -- (optional) Specify beam major axis (required without
;            header).
;    BMIN -- (optional) Specify beam minor axis (required without
;            header). 
;    restfreq -- (optional) The restfrequency of the observation
;                (required without header).  
;             
; KEYWORD PARAMETERS:
;    AIPS -- USE AIPS HEADER PARAMETERS
;
; OUTPUTS:
;   JTOK -- The conversion from Jansky/Beam to Kelvin
; MODIFICATION HISTORY:
;
;       Documentated -- Mon Nov 25  2013  Stephen Pardy 
;                     <spardy@astro.wisc.edu>
; 
;-
                                ; PHYSICAL CONSTANTS
  c = 2.99792458d10             ; speed of light CGS
  h = 6.6260755d-27             ; Planck's constant CGS
  kb = 1.380658d-16             ; Boltzmann's const CGS

  if n_elements(restfreq) gt 0 then begin
     message, "WARNING! Use FREQ and not RESTFREQ.", /info
     message, "Returning not-a-number. Fix and rerun.", /info
     return, !values.f_nan
  endif

  if n_elements(hdr) gt 0 then begin

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; GET THE BEAM
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     if keyword_set(aips) then begin
        getaipsbeam, hdr, bmaj=bmaj, bmin=bmin
     endif else begin
        if n_elements(bmaj) eq 0 then $
           bmaj = sxpar(hdr, 'BMAJ')
        if n_elements(bmin) eq 0 then $
           bmin = sxpar(hdr, 'BMIN')
     endelse

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; GET THE FREQUENCY
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     if n_elements(freq) eq 0 then begin

        ctype3 = strcompress(sxpar(hdr, 'CTYPE3',count=found_ctype3),/rem)
        if found_ctype3 then begin
           if ctype3 eq 'FREQ' then begin
              make_axes, hdr, vaxis=faxis, /vonly
              freq=mean(faxis,/nan)
           endif
           if ctype3 eq 'VRAD' then begin
              make_axes, hdr, vaxis=vaxis, /vonly
              unit = sxpar(hdr, 'CUNIT3', count=found_cunit3)
              if found_cunit3 eq 0 then begin
                 if abs(vaxis[1]-vaxis[0]) gt 1d2 then begin
                    print, "Guessing V units to be M/S."
                    vunit = "M/S"
                 endif else begin
                    print, "Guessing V units to be KM/S."
                    vunit = "KM/S"
                 endelse
              endif else begin
                 vunit = strupcase(strcompress(unit,/rem))
              endelse
              meanv = mean(vaxis)
              if vunit eq 'M/S' then begin
                 meanv *= 1d2 
              endif else if vunit eq 'KM/S' then begin
                 meanv *= 1d5
              endif else begin
                 print, "Velocity unit not recognized."
                 meanv = !values.f_nan
              endelse

              restfreq = sxpar(hdr, 'RESTFRQ', count=found_rf)
              if found_rf eq 0 then $
                 restfreq = sxpar(hdr, 'RESTFREQ', count=found_rf)
              if found_rf eq 0 then begin
                 message, 'NO REST FREQUENCY!', /info              
                 restfreq = !values.f_nan
              endif

              freq = restfreq*(1-meanv/c)
              
           endif
           
        endif

     endif

  endif
     
  res_deg = sqrt(bmaj*bmin)
  beaminster = !pi*(res_deg*!dtor/2.)^2/alog(2.)
  
  jtok = c^2/beaminster/1.d23/(2.*kb*freq^2.)
  
  return, jtok
  
end
