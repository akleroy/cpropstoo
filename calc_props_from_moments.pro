function calc_props_from_moments $
   , moments $
   , hdr = hdr $
   , astr = astr $
   , vaxis = vaxis $
   , dist = dist $
   , xco = xco $
   , bmaj = bmaj $
   , bmin = bmin $
   , bpa = bpa $
   , rmstorad = rmstorad $
   , chantosig = chantosig $
   , vircoeff = vircoeff

  compile_opt idl2

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFAULTS & DEFINITIONS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; INITIALIZE
  props = empty_cloud_struct()
  props.moments = moments

; PHYSICAL CONSTANTS
  mh = 1.673534d-24             ; hydrogen mass CGS
  ms = 1.98900d+33              ; solar mass CGS
  pc = 3.0857d18                ; parsec CGS

; DISTANCE
  if n_elements(dist) eq 0 then begin
     message, "Need a distance.", /info        
     return, props
  endif

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
  degperpix = sphdist(ra[0], dec[0], ra[1], dec[1], /deg)
  pcperpix = degperpix*!dtor*dist

; VELOCITY
  if n_elements(vaxis) eq 0 then begin
     if n_elements(hdr) eq 0 then begin
        message, "Need a header or a velocity axis.", /info        
        return, props
     endif else begin
        make_axes, hdr, vaxis=vaxis, /vonly
     endelse
  endif
  channum = findgen(n_elements(vaxis))
  chanwidth_kms = abs(vaxis[1]-vaxis[0])

; CO-TO-H2 CONVERSION FACTOR
  if n_elements(xco) eq 0 then $
    xco = 2.d20                 ; H2 cm^-2/K km s^-1

; CONVERSION FROM MOMENT TO RADIUS
  if n_elements(rmstorad) eq 0 then $
     rmstorad = 1.91

; CONVERSION FROM TOPHAT CHANNEL TO SIGMA
  if n_elements(chantosig) eq 0 then $
     chantosig = 1.0/sqrt(2.0*!pi)

; COEFFICIENT FOR VIRIAL MASS CALCULATION
  if n_elements(vircoeff) eq 0 then $
     vircoeff = 1040.0

; BEAM SIZES
  if n_elements(bmaj) eq 0 then begin
     if n_elements(hdr) gt 0 then begin
        bmaj = sxpar(hdr, "BMAJ")
        bmin = sxpar(hdr, "BMIN")
        bpa = sxpar(hdr, "BPA")
     endif else begin
        message, "No header and no beam information. Beam will be taken to be zero.", /info
        bmaj = 0.0
        bmin = 0.0
        bpa = 0.0
     endelse
  endif
  
  if n_elements(bmin) eq 0 then begin
     bmin = bpa
  endif
  
  beamfwhm_deg = sqrt(bmaj*bmin)
  beamfwhm_pix = beamfwhm_deg / degperpix
  beamfwhm_pc = beamfwhm_deg*!dtor*dist

  beam_pix = (beamfwhm_pix/2.0)^2*!pi/alog(2)
  beam_sr = (beamfwhm_deg*!dtor/2.0)^2*!pi/alog(2)
  beam_pc2 = (beamfwhm_deg*!dtor/2.0*dist)^2*!pi/alog(2)
  
; RECORD CHOICES IN STRUCTURE
  props.degperpix = degperpix
  props.pcperpix = pcperpix
  props.dist_pc = dist
  props.beamfwhm_pix = beamfwhm_pix
  props.beamfwhm_deg = beamfwhm_deg
  props.beamfwhm_pc = beamfwhm_pc
  props.pixperbeam = beam_pix
  props.srperbeam = beam_sr
  props.pc2perbeam = beam_pc2
  props.xco = xco
  props.rms_to_rad = rmstorad
  props.chanwidth_kms = chanwidth_kms
  props.chan_to_sig = chantosig

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; MOMENT 0
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; FLUX
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  mom_to_flux = chanwidth_kms*(degperpix*3600.)^2
  props.flux.val_meas = props.moments.mom0.val_meas*mom_to_flux
  props.flux.err_meas = props.moments.mom0.val_meas*mom_to_flux
  props.flux.val_extrap = props.moments.mom0.val_extrap*mom_to_flux
  props.flux.err_extrap = props.moments.mom0.err_extrap*mom_to_flux
  props.flux.val = props.moments.mom0.val*mom_to_flux
  props.flux.err = props.moments.mom0.val*mom_to_flux

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; LUMINOSITY
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  mom_to_lum = chanwidth_kms*(pcperpix)^2
  props.lum.val_meas = props.moments.mom0.val_meas*mom_to_lum
  props.lum.err_meas = props.moments.mom0.val_meas*mom_to_lum
  props.lum.val_extrap = props.moments.mom0.val_extrap*mom_to_lum
  props.lum.err_extrap = props.moments.mom0.err_extrap*mom_to_lum
  props.lum.val = props.moments.mom0.val*mom_to_lum
  props.lum.err = props.moments.mom0.val*mom_to_lum

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; LUMINOUS MASS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  mom_to_mass = chanwidth_kms*(pcperpix)^2*(2.0*mh*1.36*pc*pc/ms*xco)
  props.mass.val_meas = props.moments.mom0.val_meas*mom_to_mass
  props.mass.err_meas = props.moments.mom0.val_meas*mom_to_mass
  props.mass.val_extrap = props.moments.mom0.val_extrap*mom_to_mass
  props.mass.err_extrap = props.moments.mom0.err_extrap*mom_to_mass
  props.mass.val = props.moments.mom0.val_extrap*mom_to_mass
  props.mass.err = props.moments.mom0.val_extrap*mom_to_mass

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; LOCATION / MOMENT 1
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  xy2ad, props.moments.mom1_x.val_meas, props.moments.mom1_y.val_meas $
         , astr, ra_mom1, dec_mom1
  props.xpos.val_meas = ra_mom1
  props.xpos.val = props.xpos.val_meas
  props.ypos.val_meas = dec_mom1
  props.ypos.val = props.ypos.val_meas

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; VELOCITY
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  props.vpos.val_meas = interpol(vaxis, channum, props.moments.mom1_v.val_meas)
  props.vpos.val = props.vpos.val_meas

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; SIZE / MOMENT 2
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; X AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION
  props.moments.mom2_x.val = $
     sqrt(props.moments.mom2_x.val_extrap^2 - (beamfwhm_pix/2.354)^2)

; ... CONVERSION TO PC
  props.xmom.val_meas = props.moments.mom2_x.val_meas*pcperpix
  props.xmom.err_meas = props.moments.mom2_x.val_meas*pcperpix
  props.xmom.val_extrap = props.moments.mom2_x.val_extrap*pcperpix
  props.xmom.err_extrap = props.moments.mom2_x.err_extrap*pcperpix
  props.xmom.val = props.moments.mom2_x.val*pcperpix
  props.xmom.err = props.moments.mom2_x.val*pcperpix

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Y AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION
  props.moments.mom2_y.val = $
     sqrt(props.moments.mom2_y.val_extrap^2 - (beamfwhm_pix/2.354)^2)

; ... CONVERSION TO PC
  props.ymom.val_meas = props.moments.mom2_y.val_meas*pcperpix
  props.ymom.err_meas = props.moments.mom2_y.val_meas*pcperpix
  props.ymom.val_extrap = props.moments.mom2_y.val_extrap*pcperpix
  props.ymom.err_extrap = props.moments.mom2_y.err_extrap*pcperpix
  props.ymom.val = props.moments.mom2_y.val*pcperpix
  props.ymom.err = props.moments.mom2_y.val*pcperpix

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; MAJOR AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION
  props.moments.mom2_maj.val = $
     sqrt(props.moments.mom2_maj.val_extrap^2 - (beamfwhm_pix/2.354)^2)

; ... CONVERSION TO PC
  props.majmom.val_meas = props.moments.mom2_maj.val_meas*pcperpix
  props.majmom.err_meas = props.moments.mom2_maj.val_meas*pcperpix
  props.majmom.val_extrap = props.moments.mom2_maj.val_extrap*pcperpix
  props.majmom.err_extrap = props.moments.mom2_maj.err_extrap*pcperpix
  props.majmom.val = props.moments.mom2_maj.val*pcperpix
  props.majmom.err = props.moments.mom2_maj.val*pcperpix

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; MINOR AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION
  props.moments.mom2_min.val = $
     sqrt(props.moments.mom2_min.val_extrap^2 - (beamfwhm_pix/2.354)^2)

; ... CONVERSION TO PC
  props.minmom.val_meas = props.moments.mom2_min.val_meas*pcperpix
  props.minmom.err_meas = props.moments.mom2_min.val_meas*pcperpix
  props.minmom.val_extrap = props.moments.mom2_min.val_extrap*pcperpix
  props.minmom.err_extrap = props.moments.mom2_min.err_extrap*pcperpix
  props.minmom.val = props.moments.mom2_min.val*pcperpix
  props.minmom.err = props.moments.mom2_min.val*pcperpix

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; V AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION
  props.moments.mom2_v.val = $
     sqrt(props.moments.mom2_v.val_extrap^2 - (1.0*chantosig)^2)

; ... CONVERSION TO KM/S
  props.vmom.val_meas = props.moments.mom2_v.val_meas*chanwidth_kms
  props.vmom.err_meas = props.moments.mom2_v.val_meas*chanwidth_kms
  props.vmom.val_extrap = props.moments.mom2_v.val_extrap*chanwidth_kms
  props.vmom.err_extrap = props.moments.mom2_v.err_extrap*chanwidth_kms
  props.vmom.val = props.moments.mom2_v.val*chanwidth_kms
  props.vmom.err = props.moments.mom2_v.val*chanwidth_kms

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; RADIUS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  props.rad.val_meas = rmstorad*sqrt(props.majmom.val_meas*props.minmom.val_meas)
  props.rad.val_extrap = rmstorad*sqrt(props.majmom.val_extrap*props.minmom.val_extrap)
  props.rad.val = rmstorad*sqrt(props.majmom.val*props.minmom.val)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; VIRIAL MASS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  props.virmass.val_meas = vircoeff*props.rad.val_meas*props.vmom.val_meas^2
  props.virmass.val_extrap = vircoeff*props.rad.val_extrap*props.vmom.val_extrap^2
  props.virmass.val = vircoeff*props.rad.val*props.vmom.val^2

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; RETURN
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  return, props

end                             ; OF CALC_PROPS_FROM_MOMENTS
