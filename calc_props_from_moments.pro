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

; Error calculation is still TBD via Monte Carlo

; Consider naming convention shift ... ORIG/CORR/EXTRAP/DECONV?
; README. Fill in.

  compile_opt idl2

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFAULTS & DEFINITIONS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  nan = !values.f_nan

; INITIALIZE
  props = empty_cloud_struct()

; PHYSICAL CONSTANTS
  mh = 1.673534d-24             ; hydrogen mass CGS
  ms = 1.98900d+33              ; solar mass CGS
  pc = 3.0857d18                ; parsec CGS

; MATHEMATICAL CONSTANTS
  sig_to_fwhm = 2.354
  ell_to_sig_half = 1.69536

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
     xco = 2.d20                ; H2 cm^-2/K km s^-1

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
  
; DEFAULT TO A SYMMETRIC BEAM IF NO MINOR AXIS IS SUPPLIED
  if n_elements(bmin) eq 0 then begin
     bmin = bmaj
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
; LOOK UP GAUSSIAN CORRECTION FACTOR
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  props.peak_to_edge = moments.maxval_meas / moments.minval_meas

  calc_gauss_corr, props.peak_to_edge $
                   , corr_1d=gcorr_sig, corr_delta=gcorr_delta, corr_area=gcorr_area, corr_flux=gcorr_flux
  
  props.gcorr_1d = gcorr_sig
  props.gcorr_area = gcorr_area
  props.gcorr_delta = gcorr_delta
  props.gcorr_flux = gcorr_flux


; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; MOMENT 0
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; FLUX
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  mom_to_flux = chanwidth_kms*(degperpix*3600.)^2

  props.flux_meas = moments.mom0_meas*mom_to_flux
  props.flux_meas_err = moments.mom0_meas_err*mom_to_flux

  props.flux_extrap = moments.mom0_extrap*mom_to_flux
  props.flux_extrap_err = moments.mom0_extrap_err*mom_to_flux

  props.flux = moments.mom0*mom_to_flux
  props.flux_err = moments.mom0_err*mom_to_flux

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; LUMINOSITY
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  mom_to_lum = chanwidth_kms*(pcperpix)^2

  props.lum_meas = moments.mom0_meas*mom_to_lum
  props.lum_meas_err = moments.mom0_meas_err*mom_to_lum

  props.lum_extrap = moments.mom0_extrap*mom_to_lum
  props.lum_extrap_err = moments.mom0_extrap_err*mom_to_lum

  props.lum = moments.mom0*mom_to_lum
  props.lum_err = moments.mom0_err*mom_to_lum

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; LUMINOUS MASS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  mom_to_mass = chanwidth_kms*(pcperpix)^2*(2.0*mh*1.36*pc*pc/ms*xco)

  props.mass_meas = moments.mom0_meas*mom_to_mass
  props.mass_meas_err = moments.mom0_meas_err*mom_to_mass

  props.mass_extrap = moments.mom0_extrap*mom_to_mass
  props.mass_extrap_err = moments.mom0_extrap_err*mom_to_mass

  props.mass = moments.mom0*mom_to_mass
  props.mass_err = moments.mom0_err*mom_to_mass

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; LOCATION / MOMENT 1
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  xy2ad, moments.mom1x_meas, moments.mom1y_meas $
         , astr, ra_mom1, dec_mom1

  props.xpos_meas = ra_mom1
  props.xpos = props.xpos_meas

  props.ypos_meas = dec_mom1
  props.ypos = props.ypos_meas

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; VELOCITY
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  props.vpos_meas = interpol(vaxis, channum, moments.mom1v_meas)
  props.vpos = props.vpos_meas

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; SIZE ::: MOMENT 2
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; X AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION
  moments.mom2x = $
     sqrt(moments.mom2x_extrap^2 - (beamfwhm_pix/sig_to_fwhm)^2)

; ... CONVERSION TO PC
  props.xmom_meas = moments.mom2x_meas*pcperpix
  props.xmom_meas_err = moments.mom2x_meas*pcperpix

  props.xmom_extrap = moments.mom2x_extrap*pcperpix
  props.xmom_extrap_err = moments.mom2x_extrap_err*pcperpix

  props.xmom = moments.mom2x*pcperpix
  props.xmom_err = nan

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; Y AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION
  moments.mom2y = $
     sqrt(moments.mom2y_extrap^2 - (beamfwhm_pix/sig_to_fwhm)^2)

; ... CONVERSION TO PC
  props.ymom_meas = moments.mom2y_meas*pcperpix
  props.ymom_meas_err = moments.mom2y_meas_err*pcperpix

  props.ymom_extrap = moments.mom2y_extrap*pcperpix
  props.ymom_extrap_err = moments.mom2y_extrap_err*pcperpix

  props.ymom = moments.mom2y*pcperpix
  props.ymom_err = moments.mom2y_err*pcperpix

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; MAJOR AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... CONVERSION TO PC
  props.mommaj_meas = moments.mom2maj_meas*pcperpix
  props.mommaj_meas_err = moments.mom2maj_meas_err*pcperpix

  props.mommaj_extrap = moments.mom2maj_extrap*pcperpix
  props.mommaj_extrap_err = moments.mom2maj_extrap_err*pcperpix

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; MINOR AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... CONVERSION TO PC
  props.mommin_meas = moments.mom2min_meas*pcperpix
  props.mommin_meas_err = moments.mom2min_meas_err*pcperpix

  props.mommin_extrap = moments.mom2min_extrap*pcperpix
  props.mommin_extrap_err = moments.mom2min_extrap_err*pcperpix
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; FULL DECONVOLUTION
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; DO THE TWO-D DECONVOLUTION (FACTORS MATCH SIGMA TO FWHM)
  deconvolve_gauss $
     , meas_maj = props.mommaj_extrap*sig_to_fwhm $
     , meas_min = props.mommin_extrap*sig_to_fwhm $
     , meas_pa = moments.momposang/!dtor $ ; INPUT AS DEGREES
     , beam_maj = bmaj*!dtor*dist $
     , beam_min = bmin*!dtor*dist $
     , beam_pa = bpa $
     , src_maj = src_maj $
     , src_min = src_min $
     , src_pa = src_pa $
     , worked = worked $
     , point = point
  src_pa *= !dtor               ; SAVE AS RADIANS

  if worked then begin
     props.mommaj = src_maj/sig_to_fwhm
     props.mommin = src_min/sig_to_fwhm
     props.momposang = src_pa
  endif else begin
;     if point then begin
     props.mom_unresolved = 1B
;     endif
  endelse

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; SIZE ::: ELLIPSE
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; ELLIPSE FIT AT HALF MAX
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... CONVERSION TO PC
  
; MAJOR
  props.ellmajhalfmax_meas = $
     moments.ellmajhalfmax_meas*pcperpix*ell_to_sig_half

; MINOR
  props.ellminhalfmax_meas = $
     moments.ellminhalfmax_meas*pcperpix*ell_to_sig_half

; DO THE TWO-D DECONVOLUTION (FACTORS MATCH SIGMA TO FWHM)
  deconvolve_gauss $
     , meas_maj = props.ellmajhalfmax_meas*sig_to_fwhm $
     , meas_min = props.ellminhalfmax_meas*sig_to_fwhm $
     , meas_pa = moments.ellposanghalfmax $
     , beam_maj = bmaj*!dtor*dist $
     , beam_min = bmin*!dtor*dist $
     , beam_pa = bpa $
     , src_maj = src_maj $
     , src_min = src_min $
     , src_pa = src_pa $
     , worked = worked $
     , point = point

  if worked then begin
     props.ellmajhalfmax = src_maj/sig_to_fwhm
     props.ellminhalfmax = src_min/sig_to_fwhm
     props.ellposanghalfmax = src_pa
  endif else begin
;     if point then begin
     props.ellhalfmax_unresolved = 1B
;     endif
  endelse

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; ELLIPSE FIT
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION

; ... CONVERSION TO PC
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; AREA AT HALF MAX
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... CONVERSION FROM AREA TO AN EFFECTIVE SIGMA IN PC
  props.areamajhalfmax_meas = $
     sqrt(moments.areahalfmax_meas/!pi)*2.0*pcperpix/sig_to_fwhm
  
; ... ONE-D DECONVOLUTION   
  if props.areamajhalfmax_meas gt beamfwhm_pix/sig_to_fwhm*pcperpix then begin
     props.areamajhalfmax = $
        sqrt(props.areamajhalfmax_meas^2 - (beamfwhm_pix/sig_to_fwhm*pcperpix)^2)
  endif else begin
     props.areahalfmax_unresolved = 1B
  endelse
  
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; FULL AREA
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION

; ... CONVERSION TO PC

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; V AXIS SIZE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; ... DECONVOLUTION
  moments.mom2v = $
     sqrt(moments.mom2v_extrap^2 - (1.0*chantosig)^2)
  
; ... CONVERSION TO KM/S
  props.vmom_meas = moments.mom2v_meas*chanwidth_kms
  props.vmom_meas_err = moments.mom2v_meas_err*chanwidth_kms

  props.vmom_extrap = moments.mom2v_extrap*chanwidth_kms
  props.vmom_extrap_err = moments.mom2v_extrap_err*chanwidth_kms

  props.vmom = moments.mom2v*chanwidth_kms
  props.vmom_err = moments.mom2v_err*chanwidth_kms

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; RADIUS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; FROM MOMENTS
  props.radmom_meas = $
     rmstorad*sqrt(props.mommaj_meas*props.mommin_meas)
  props.radmom_extrap = $
     rmstorad*sqrt(props.mommaj_extrap*props.mommin_extrap)
  props.radmom = $
     rmstorad*sqrt(props.mommaj*props.mommin)

; FROM ELLIPSE
  props.radell_meas = $
     rmstorad*sqrt(props.ellmajhalfmax_meas*props.ellminhalfmax_meas)
  props.radell_extrap = $
     nan
  props.radell = $
     rmstorad*sqrt(props.ellmajhalfmax_meas*props.ellminhalfmax_meas)

; FROM AREA
  props.radarea_meas = $
     rmstorad*props.areamajhalfmax_meas
  props.radarea_extrap = $
     rmstorad*props.areamajhalfmax_extrap
  props.radarea = $
     rmstorad*props.areamajhalfmax

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; VIRIAL MASS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  props.virmass_meas = vircoeff*props.radmom_meas*props.vmom_meas^2
  props.virmass_extrap = vircoeff*props.radmom_extrap*props.vmom_extrap^2
  props.virmass = vircoeff*props.radmom*props.vmom^2

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; RETURN
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  return, props

end                             ; OF CALC_PROPS_FROM_MOMENTS
