function moments_gausscorr $
   , props $
   , xin = x $
   , yin = y $
   , vin = v $
   , tin = t $
   , define_fields = define_fields $   
   , calculate = calculate $
   , moments = moments $
   , properties = properties $
   , hdr=hdr $
   , astr=astr $
   , vaxis=vaxis $
   , extrap = do_extrap $
   , extarg = targett $
   , fluxlin = fluxlin
  
;+
;
; Moment calculation module. Will either define fields, calculate
; moments, or convert moments into properties, depending on the
; calling syntax.
;
; This version implements a Gaussian correction based on the
; peak_to_edge (min-to-max) ratio (see, e.g., Bolatto et al. 2003,
; Rosolowsky & Blitz 2005). The corrections are based on tabulated
; corrections derived from modeling and written into a routine that
; does the interpolation.
;
;-

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFINE FIELDS 
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; Add the necessary fields to the properties structure

; FOR CONVENIENCE, ASSIGN "not-a-number" TO THE nan VARIABLE
  nan = !values.f_nan

  if keyword_set(define_fields) then begin

;    ... ADD FIELDS FOR MOMENTS
     if keyword_set(moments) then begin

;       GAUSSIAN CORRECTION FACTOR
        props = create_struct(props, "peak_to_edge", nan)
        props = create_struct(props, "gcorr_1d", nan)
        props = create_struct(props, "gcorr_flux", nan)

;       MOMENT 0  
        props = create_struct(props, "mom0_gcorr", nan)
        
;       MOMENT 2
        props = create_struct(props, "mom2x_gcorr", nan)
        props = create_struct(props, "mom2y_gcorr", nan)
        props = create_struct(props, "mom2v_gcorr", nan)

;       PRINCIPLE AXES CALCULATION
        props = create_struct(props, "mom2maj_gcorr", nan)
        props = create_struct(props, "mom2min_gcorr", nan)

        return, props

     endif

;    ... ADD FIELDS FOR PROPERTIES
     if keyword_set(properties) then begin

;       FLUX-DERIVED QUANTITIES
        props = create_struct(props, "flux_gcorr", nan)
        props = create_struct(props, "lum_gcorr", nan)
        props = create_struct(props, "mass_gcorr", nan)
        
;       SIZE QUANTITIES
        props = create_struct(props, "xrms_gcorr", nan)
        props = create_struct(props, "xrms_gcorr_deconv", nan)

        props = create_struct(props, "yrms_gcorr", nan)
        props = create_struct(props, "yrms_gcorr_deconv", nan)

        props = create_struct(props, "vrms_gcorr", nan)
        props = create_struct(props, "vrms_gcorr_deconv", nan)

;       PRINCIPLE AXIS QUANTITIES
        props = create_struct(props, "majrms_gcorr", nan)
        props = create_struct(props, "majrms_gcorr_deconv", nan)

        props = create_struct(props, "minrms_gcorr", nan)
        props = create_struct(props, "minrms_gcorr_deconv", nan)

        props = create_struct(props, "posang_gcorr_deconv", nan)

        props = create_struct(props, "resolved_gcorr", 0B)

;       PHYSICAL QUANTITIES
        props = create_struct(props, "radrms_gcorr_deconv", nan)
        props = create_struct(props, "virmass_gcorr_deconv", nan)

        return, props

     endif

  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CALCULATE MOMENTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

;+ 
;
; Carry out the original CPROPS moment calculations.
;
;-

  if keyword_set(calculate) and keyword_set(moments) then begin
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; LOOK UP THE GAUSSIAN CORRECTION FACTOR
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=     
     
     props.peak_to_edge = props.maxval / props.minval

     if props.peak_to_edge gt 1.0 then begin
;    USE THE LOOK-UP FUNCTION
        calc_gauss_corr $
           , props.peak_to_edge $
           , corr_1d=gcorr_sig $
           , corr_flux=gcorr_flux
     endif else begin
        gcorr_sig = 1.0
        gcorr_flux = 1.0
     endelse

;    RECORD THE CORRECTIONS
     props.gcorr_1d = gcorr_sig
     props.gcorr_flux = gcorr_flux

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; APPLY IT TO MOMENTS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=     

     props.mom0_gcorr = props.mom0*props.gcorr_flux

     props.mom2x_gcorr = props.mom2x*props.gcorr_1d
     props.mom2y_gcorr = props.mom2y*props.gcorr_1d
     props.mom2v_gcorr = props.mom2v*props.gcorr_1d

     props.mom2maj_gcorr = props.mom2maj*props.gcorr_1d
     props.mom2min_gcorr = props.mom2min*props.gcorr_1d

     return, props

  endif
  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CONVERT MOMENTS TO PROPERTIES
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

;+ 
;
; Convert from moments to properties. At this point we can assume a
; user-defined alpha and distance and header information, so that we
; know the astromery and velocity.
;
;-

  if keyword_set(calculate) and keyword_set(properties) then begin

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; FLUX-LIKE QUANTITIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     mom_to_flux = props.chanwidth_kms*(props.degperpix*3600.)^2
     props.flux_gcorr = props.mom0_gcorr*mom_to_flux

     mom_to_lum = props.chanwidth_kms*(props.pcperpix)^2
     props.lum_gcorr = props.mom0_gcorr*mom_to_lum

     mom_to_mass = props.chanwidth_kms*(props.pcperpix)^2*props.alpha
     props.mass_gcorr = props.mom0_gcorr*mom_to_mass
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; SIZE-LIKE QUANTITIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     props.xrms_gcorr = props.mom2x_gcorr*props.pcperpix
     props.xrms_gcorr_deconv = $
        sqrt(props.mom2x_gcorr^2 - (props.beamfwhm_pix/props.sig_to_fwhm)^2) * $
        props.pcperpix
     
     props.yrms_gcorr = props.mom2y_gcorr*props.pcperpix
     props.yrms_gcorr_deconv = $
        sqrt(props.mom2y_gcorr^2 - (props.beamfwhm_pix/props.sig_to_fwhm)^2) * $
        props.pcperpix

     props.vrms_gcorr = props.mom2v_gcorr*props.chanwidth_kms
     props.vrms_gcorr_deconv = $
        sqrt(props.mom2v_gcorr^2 - (1.0*props.chantosig)^2)*props.chanwidth_kms

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PRINCIPLE AXIS QUANTITIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     props.majrms_gcorr = props.mom2maj_gcorr*props.pcperpix
     props.minrms_gcorr = props.mom2min_gcorr*props.pcperpix

;    DO THE TWO-D DECONVOLUTION (FACTORS MATCH SIGMA TO FWHM)
     deconvolve_gauss $
        , meas_maj = props.majrms_gcorr*props.sig_to_fwhm $
        , meas_min = props.minrms_gcorr*props.sig_to_fwhm $
        , meas_pa = props.momposang/!dtor $ ; INPUT AS DEGREES
        , beam_maj = props.bmaj_deg*!dtor*props.dist_pc $
        , beam_min = props.bmin_deg*!dtor*props.dist_pc $
        , beam_pa = props.bpa_deg $
        , src_maj = src_maj $
        , src_min = src_min $
        , src_pa = src_pa $
        , worked = worked $
        , point = point
     src_pa *= !dtor            ; SAVE AS RADIANS

     if worked then begin
        props.majrms_gcorr_deconv = src_maj/props.sig_to_fwhm
        props.minrms_gcorr_deconv = src_min/props.sig_to_fwhm
        props.posang_gcorr_deconv = src_pa
        props.resolved_gcorr = 1B
     endif else begin
        props.resolved_gcorr = 0B
     endelse

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PHYSICAL QUANTITIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     props.radrms_gcorr_deconv = $
        props.rmstorad*sqrt(props.majrms_gcorr_deconv* $
                            props.minrms_gcorr_deconv)

     props.virmass_gcorr_deconv = $
        props.vircoeff*props.radrms_gcorr_deconv* $
        props.vrms_gcorr_deconv^2
     
     return, props

  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; SHOULD NOT GET HERE
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  message, "No calculation carried out. Check calling sequence.", /info
  return, nan

end
   
