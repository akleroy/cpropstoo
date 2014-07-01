function calc_props_from_moments $
   , moments $
   , empty_props = empty_props $
   , modules = extra_modules $
   , hdr = hdr $
   , astr = astr $
   , vaxis = vaxis $
   , dist_pc = dist_pc $
   , alpha = alpha $
   , bmaj = bmaj $
   , bmin = bmin $
   , bpa = bpa $
   , rmstorad = rmstorad $
   , chantosig = chantosig $
   , vircoeff = vircoeff $
   , _extra = extra

  compile_opt idl2
     
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFINE THE SET OF MODULES THAT WE WILL WORK WITH
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; Each modules adds fields and calculations to the moment structure.

  modules = ["classic" $
             , "area" $
             , "gausscorr"]
  if n_elements(extra_modules) gt 0 then begin
     modules = [modules, extra_modules]
  endif
  n_mod = n_elements(modules)
  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; INITIALIZE THE OUTPUT STRUCTURE
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; Call the individual modules to initialize the fields that they will
; need. this step is skipped if we already have an empty property structure.
  
  if n_elements(empty_props) eq 0 then begin

     empty_props = add_props_fields(moments)

     for i = 0, n_mod-1 do begin
        empty_props = $
           call_function("moments_"+modules[i] $
                         , empty_props $
                         , /define_field $
                         , /properties $
                         , _extra=extra)
     endfor
  endif
  props = empty_props

; Assign values from the moments 
  struct_assign, moments, props, /nozero

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; FILL IN META DATA
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  nan = !values.f_nan

; NUMERICAL CONSTANTS
  props.sig_to_fwhm = 2.354
  props.ell_to_sig_half = 1.69536

; DISTANCE
  if n_elements(dist_pc) eq 0 then begin
     message, "Need a distance.", /info        
     return, props
  endif else begin
     props.dist_pc = dist_pc
  endelse

; ASTROMETRY
  if n_elements(astr) eq 0 then begin
     if n_elements(hdr) eq 0 then begin
        message, "Need a header or a velocity axis.", /info        
        return, props
     endif else begin
        extast, hdr, astr
     endelse
  endif
; ... DISTANCE BETWEEN FIRST TWO PIXELS (ASSUME SQUARE)
  xy2ad, [0,1], [0,0], astr, ra, dec
  props.degperpix = sphdist(ra[0], dec[0], ra[1], dec[1], /deg)
  props.pcperpix = props.degperpix*!dtor*props.dist_pc

; VELOCITY
  if n_elements(vaxis) eq 0 then begin
     if n_elements(hdr) eq 0 then begin
        message, "Need a header or a velocity axis.", /info        
        return, props
     endif else begin
        make_axes, hdr, vaxis=vaxis, /vonly
     endelse
  endif
  props.chanwidth_kms = abs(vaxis[1]-vaxis[0])

; LINE
  if n_elements(line) eq 0 then $
     line = "CO"
  props.line = line

; CONVERSION FACTOR
  if n_elements(alpha) eq 0 then $
     alpha = 4.35         ; Msun / (K km s^-1) / pc^2
  props.alpha = alpha

; CONVERSION FROM MOMENT TO RADIUS
  if n_elements(rmstorad) eq 0 then $
     rmstorad = 1.91
  props.rmstorad = rmstorad

; CONVERSION FROM TOPHAT CHANNEL TO SIGMA
  if n_elements(chantosig) eq 0 then $
     chantosig = 1.0/sqrt(2.0*!pi)
  props.chantosig = chantosig

; COEFFICIENT FOR VIRIAL MASS CALCULATION
  if n_elements(vircoeff) eq 0 then $
     vircoeff = 1040.0
  props.vircoeff = vircoeff

; BEAM SIZES
  if n_elements(bmaj) eq 0 then begin
     if n_elements(hdr) gt 0 then begin
        props.bmaj_deg = sxpar(hdr, "BMAJ")
        props.bmin_deg = sxpar(hdr, "BMIN")
        props.bpa_deg = sxpar(hdr, "BPA")
     endif else begin
        message, "No header and no beam information.", /info
        message, "Beam taken to be zero.", /info
        props.bmaj_deg = 0.0
        props.bmin_deg = 0.0
        props.bpa_deg = 0.0
     endelse
  endif
  
; DEFAULT TO A SYMMETRIC BEAM IF NO MINOR AXIS IS SUPPLIED
  if n_elements(bmin) eq 0 then begin
     props.bmin_deg = props.bmaj_deg
  endif
  
  props.beamfwhm_deg = sqrt(props.bmaj_deg*props.bmin_deg)
  props.beamfwhm_pix = props.beamfwhm_deg / props.degperpix
  props.beamfwhm_pc = props.beamfwhm_deg*!dtor*props.dist_pc

  props.pixperbeam = (props.beamfwhm_pix/2.0)^2*!pi/alog(2)
  props.srperbeam = (props.beamfwhm_deg*!dtor/2.0)^2*!pi/alog(2)
  props.pc2perbeam = (props.beamfwhm_deg*!dtor/2.0*props.dist_pc)^2*!pi/alog(2)

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; LOOP OVER THE MODULES REQUESTED
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
  
  for i = 0, n_mod-1 do begin
     props = $
        call_function("moments_"+modules[i] $
                      , props $
                      , /calculate $
                      , /properties $
                      , hdr=hdr $
                      , astr=astr $
                      , vaxis=vaxis $                      
                      , _extra=extra)
  endfor

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; RETURN
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  return, props

end                             ; OF CALC_PROPS_FROM_MOMENTS
