function moments_area $
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
; This version implements area-oriented calculations.
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


        props = create_struct(props, "area", nan)
        props = create_struct(props, "area_halfmax", nan)
        props = create_struct(props, "area_unit", "pix^2")

        props = create_struct(props, "deltav", nan)
        props = create_struct(props, "deltav_halfmax", nan)
        props = create_struct(props, "deltav_unit", "pix")

        props = create_struct(props, "ellfitmaj", nan)
        props = create_struct(props, "ellfitmaj_halfmax", nan)
        props = create_struct(props, "ellfitmaj_unit", "pix")

        props = create_struct(props, "ellfitmin", nan)
        props = create_struct(props, "ellfitmin_halfmax", nan)
        props = create_struct(props, "ellfitmin_unit", "pix")

        props = create_struct(props, "ellfitposang", nan)
        props = create_struct(props, "ellfitposang_halfmax", nan)
        props = create_struct(props, "ellfitposang_unit", "rad")

        return, props

     endif

;    ... ADD FIELDS FOR PROPERTIES
     if keyword_set(properties) then begin

        props = create_struct(props, "deltav_phys", nan)
        props = create_struct(props, "deltav_phys_halfmax", nan)
        props = create_struct(props, "deltav_phys_halfmax_deconv", nan)
        
        props = create_struct(props, "areamaj_phys", nan)
        props = create_struct(props, "areamaj_phys_halfmax", nan)
        props = create_struct(props, "areamaj_phys_halfmax_deconv", nan)

        props = create_struct(props, "ellmaj", nan)
        props = create_struct(props, "ellmaj_halfmax", nan)
        props = create_struct(props, "ellmaj_halfmax_deconv", nan)
        props = create_struct(props, "ellmaj_unit", "pc")

        props = create_struct(props, "ellmin", nan)
        props = create_struct(props, "ellmin_halfmax", nan)
        props = create_struct(props, "ellmin_halfmax_deconv", nan)
        props = create_struct(props, "ellmin_unit", "pc")

        props = create_struct(props, "ellposang_halfmax", nan)

        props = create_struct(props, "radarea_deconv", nan)
        props = create_struct(props, "radell_deconv", nan)

        props = create_struct(props, "virmass_radell_deconv", nan)
        props = create_struct(props, "virmass_radarea_deconv", nan)
        
        props = create_struct(props, "resolved_ellfit", 0B)

        return, props

     endif

  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CALCULATE MOMENTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

;+ 
;
; Carry out area-oriented calculations.
;
;-

  if keyword_set(calculate) and keyword_set(moments) then begin
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; CALCULATE TWOD INDICES AND FIND POINTS ABOVE HALF MAX
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;    (Note that there is a subtley here. We keep all points above half
;    max but one might want only connected points above half
;    max. Right now, I think the current way is more consistent with
;    the overall assignment definition.)

;    ... WORK OUT A SET OF UNIQUE TWO-D INDICES
     twod_id = x + y*(max(x)+1)  
     twod_ind = uniq(twod_id, sort(twod_id))
     twod_x = x[twod_ind]
     twod_y = y[twod_ind]

;    ... WORK OUT INDICES WHERE INTENSITY > 0.5 MAX
     halfmax_ind = where(t gt 0.5*props.maxval, halfmax_ct) 
     if halfmax_ct gt 0 then begin
        halfmax_twod_ind = uniq(twod_id[halfmax_ind], sort(twod_id[halfmax_ind]))
        halfmax_twod_x = x[halfmax_twod_ind]
        halfmax_twod_y = y[halfmax_twod_ind]
     endif
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; CALCULATE AREA AND EXTENT
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;    AREA 
     props.area = n_elements(twod_ind)*1.0
     if halfmax_ct gt 0 then $    
        props.area_halfmax = n_elements(halfmax_twod_ind)*1.0 

;    DELTAV
     props.deltav = max(v) - min(v)
     if halfmax_ct gt 0 then $    
        props.deltav_halfmax = max(v[halfmax_ind]) - min(v[halfmax_ind])
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; ELLIPSE FITS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Unweighted (that works but would need a different call) two
; dimensional ellipse fits.

     if props.npix gt 1 then begin

        ellfit, x=twod_x, y=twod_y $
                , maj=ell_maj, min=ell_min, posang=ell_pa        
        
        props.ellfitmaj = ell_maj
        props.ellfitmin = ell_min
        props.ellfitposang = ell_pa
        
        if halfmax_ct gt 0 then begin
           
           ellfit, x=halfmax_twod_x, y=halfmax_twod_y $
                   , maj=half_ell_maj, min=half_ell_min, posang=half_ell_pa           

           props.ellfitmaj_halfmax = half_ell_maj
           props.ellfitmin_halfmax = half_ell_min
           props.ellfitposang_halfmax = half_ell_pa     
           
        endif

     endif
     
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
; SIZE-LIKE QUANTITES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     props.deltav_phys = $
        props.deltav*props.chanwidth_kms
     props.deltav_phys_halfmax = $
        props.deltav_halfmax*props.chanwidth_kms

     props.ellmaj = $
        props.ellfitmaj*props.pcperpix*props.ell_to_sig_half
     props.ellmaj_halfmax = $
        props.ellfitmaj_halfmax*props.pcperpix*props.ell_to_sig_half

     props.ellmin = $
        props.ellfitmin*props.pcperpix*props.ell_to_sig_half
     props.ellmin_halfmax = $
        props.ellfitmin_halfmax*props.pcperpix*props.ell_to_sig_half

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DECONVOLUTION
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;    AREA CONVERSION TO RADIUS
     props.areamaj_phys = $
        sqrt(props.area/!pi)*2.0*props.pcperpix/props.sig_to_fwhm

     props.areamaj_phys_halfmax = $
        sqrt(props.area_halfmax/!pi)*2.0*props.pcperpix/props.sig_to_fwhm

     props.areamaj_phys_halfmax_deconv = $
        sqrt(props.areamaj_phys_halfmax^2 - $
             (props.beamfwhm_pix/props.sig_to_fwhm*props.pcperpix)^2)


;    CHANNEL WIDTH DECONVOLUTION
;    props.deltav_phys_halfmax_deconv = $
;       sqrt(props.deltav_halfmax^2 - (1.0*props.chantosig)^2) * $
;       props.chanwidth_kms
     props.deltav_phys_halfmax_deconv = $
        sqrt(props.deltav_halfmax^2 - 1.0) * $
        props.chanwidth_kms

;    DO THE TWO-D DECONVOLUTION (FACTORS MATCH SIGMA TO FWHM)
     deconvolve_gauss $
        , meas_maj = props.ellmaj_halfmax*props.sig_to_fwhm $
        , meas_min = props.ellmin_halfmax*props.sig_to_fwhm $
        , meas_pa = props.ellfitposang_halfmax/!dtor $ ; INPUT AS DEGREES
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
        props.ellmaj_halfmax_deconv = src_maj/props.sig_to_fwhm
        props.ellmin_halfmax_deconv = src_min/props.sig_to_fwhm
        props.ellposang_halfmax = src_pa
        props.resolved_ellfit = 1B
     endif else begin
        props.resolved_ellfit = 0B
     endelse

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PHYSICAL QUANTITIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     props.radell_deconv = $
        props.rmstorad*sqrt(props.ellmaj_halfmax_deconv* $
                            props.ellmin_halfmax_deconv)

     props.virmass_radell_deconv = $
        props.vircoeff*props.radell_deconv* $
        props.vrms_gcorr_deconv^2

     props.radarea_deconv = $
        props.rmstorad*props.areamaj_phys_halfmax_deconv 

     props.virmass_radarea_deconv = $
        props.vircoeff*props.radarea_deconv* $
        props.deltav_phys_halfmax_deconv^2
     
     return, props

  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; SHOULD NOT GET HERE
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  message, "No calculation carried out. Check calling sequence.", /info
  return, nan

end
   
