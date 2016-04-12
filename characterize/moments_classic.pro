function moments_classic $
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
; This version implements the "classic" CPROPS prescription of
; Rosolowsky & Leroy (2006).
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
        
;       MOMENT 0  
        props = create_struct(props, "mom0", nan)
        props = create_struct(props, "mom0_extrap", nan)
        props = create_struct(props, "mom0_unit", "K*pix^3")
        
;       MOMENT 1
        props = create_struct(props, "mom1x", nan)
        props = create_struct(props, "mom1x_unit", "pix")
        
        props = create_struct(props, "mom1y", nan)
        props = create_struct(props, "mom1y_unit", "pix")

        props = create_struct(props, "mom1v", nan)
        props = create_struct(props, "mom1v_unit", "pix")
        
;       MOMENT 2
        props = create_struct(props, "mom2x", nan)
        props = create_struct(props, "mom2x_extrap", nan)
        props = create_struct(props, "mom2x_unit", "pix")
        
        props = create_struct(props, "mom2y", nan)
        props = create_struct(props, "mom2y_extrap", nan)
        props = create_struct(props, "mom2y_unit", "pix")
        
        props = create_struct(props, "mom2v", nan)
        props = create_struct(props, "mom2v_extrap", nan)
        props = create_struct(props, "mom2v_unit", "pix")

;       PRINCIPAL AXES CALCULATION
        props = create_struct(props, "mom2maj", nan)
        props = create_struct(props, "mom2maj_extrap", nan)
        props = create_struct(props, "mom2maj_unit", "pix")

        props = create_struct(props, "mom2min", nan)
        props = create_struct(props, "mom2min_extrap", nan)
        props = create_struct(props, "mom2min_unit", "pix")

        props = create_struct(props, "momposang", nan)
        props = create_struct(props, "momposang_unit", "rad")

        props = create_struct(props, "covar_xy", nan)
        
;       VELOCITY GRADIENT CALCULATION (ADDED DEC 4, 2014)
        props = create_struct(props, "velgrad", nan)
        props = create_struct(props, "velgrad_unit", "pix/pix")
        props = create_struct(props, "velposang", nan)
        props = create_struct(props, "velposang_unit", "rad")
        props = create_struct(props, "veldisp", nan)
        props = create_struct(props, "veldisp_unit", "pix")

        return, props

     endif

;    ... ADD FIELDS FOR PROPERTIES
     if keyword_set(properties) then begin

;       FLUX-DERIVED QUANTITIES
        props = create_struct(props, "flux", nan)
        props = create_struct(props, "flux_extrap", nan)
        props = create_struct(props, "flux_unit", "K*km/s*as^2")

        props = create_struct(props, "lum", nan)
        props = create_struct(props, "lum_extrap", nan)
        props = create_struct(props, "lum_unit", "K*km/s*pc^2")

        props = create_struct(props, "mass", nan)
        props = create_struct(props, "mass_extrap", nan)
        props = create_struct(props, "mass_unit", "Msun")

;       POSITIONAL QUANTITIES
        props = create_struct(props, "xpos", nan)
        props = create_struct(props, "xpos_unit", "deg")

        props = create_struct(props, "ypos", nan)
        props = create_struct(props, "ypos_unit", "deg")

        props = create_struct(props, "vpos", nan)
        props = create_struct(props, "vpos_unit", "km/s")
  
;       SIZE QUANTITIES
        props = create_struct(props, "xrms", nan)
        props = create_struct(props, "xrms_extrap", nan)
        props = create_struct(props, "xrms_extrap_deconv", nan)
        props = create_struct(props, "xrms_unit", "pc")

        props = create_struct(props, "yrms", nan)
        props = create_struct(props, "yrms_extrap", nan)
        props = create_struct(props, "yrms_extrap_deconv", nan)
        props = create_struct(props, "yrms_unit", "pc")

        props = create_struct(props, "vrms", nan)
        props = create_struct(props, "vrms_extrap", nan)
        props = create_struct(props, "vrms_extrap_deconv", nan)
        props = create_struct(props, "vrms_unit", "km/s")

;       PRINCIPAL AXIS QUANTITIES
        props = create_struct(props, "majrms", nan)
        props = create_struct(props, "majrms_extrap", nan)
        props = create_struct(props, "majrms_extrap_deconv", nan)
        props = create_struct(props, "majrms_unit", "pc")

        props = create_struct(props, "minrms", nan)
        props = create_struct(props, "minrms_extrap", nan)
        props = create_struct(props, "minrms_extrap_deconv", nan)
        props = create_struct(props, "minrms_unit", "pc")

        props = create_struct(props, "posang_extrap_deconv", nan)
        props = create_struct(props, "posang_unit", "rad")

        props = create_struct(props, "resolved_extrap", 0B)

;       PHYSICAL QUANTITIES
        props = create_struct(props, "radrms_extrap_deconv", nan)
        props = create_struct(props, "radrms_unit", "pc")

        props = create_struct(props, "virmass_extrap_deconv", nan)
        props = create_struct(props, "virmass_unit", "Msun")

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
; DEFAULTS & DEFINITIONS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;    SET THE FUNCTIONAL FORM FOR THE FLUX EXTRAPOLATION
     if n_elements(fluxlin) eq 0 then $
        square = 1 $
     else $
        square = 0

;    SET THE FUNCTIONAL FORM FOR THE FLUX EXTRAPOLATION
     if keyword_set(do_extrap) then $
        do_extrap = 1 $
     else $
        do_extrap = 0

;    SET THE TARGET FOR THE CURVE OF GROWTH
     if n_elements(targett) eq 0 then $
        targett = 0
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; CUMULATIVE CALCULATIONS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Calculate cumulative moments for the provided x, y, v, t vector.

;    ZEROTH MOMENT
     mom0t = total(t, cumul=do_extrap)

;    FIRST MOMENT
     mom1x = total(x*t, cumul=do_extrap)/total(t, cumul=do_extrap)
     mom1y = total(y*t, cumul=do_extrap)/total(t, cumul=do_extrap)
     mom1v = total(v*t, cumul=do_extrap)/total(t, cumul=do_extrap)

;    SECOND MOMENT
     term1x = total(t*double(x)^2., cumul=do_extrap)
     term2x = (total(t*double(x), cumul=do_extrap))^2./mom0t
     mom2x = sqrt((term1x - term2x)/mom0t)
     zeroind = where(abs(term1x - term2x) lt 1.d-10, num)
     if (num gt 0) then $
        mom2x[zeroind] = 0.0
     
     term1y = total(t*double(y)^2., cumul=do_extrap)
     term2y = (total(t*double(y), cumul=do_extrap))^2./mom0t
     mom2y = sqrt((term1y - term2y)/mom0t)
     zeroind = where(abs(term1y - term2y) lt 1.d-10, num)
     if (num gt 0) then $
        mom2y[zeroind] = 0.0  

     term1v = total(t*double(v)^2., cumul=do_extrap)
     term2v = (total(t*double(v), cumul=do_extrap))^2./mom0t
     mom2v = sqrt((term1v - term2v)/mom0t)
     zeroind = where(abs(term1v - term2v) lt 1.d-10, num)
     if (num gt 0) then $
        mom2v[zeroind] = 0.0

;    SAVE TO STRUCTURE
     props.mom0 = mom0t[n_elements(mom0t)-1]

     props.mom1x = mom1x[n_elements(mom1x)-1]
     props.mom1y = mom1y[n_elements(mom1y)-1]
     props.mom1v = mom1v[n_elements(mom1v)-1]

     props.mom2x = mom2x[n_elements(mom2x)-1]
     props.mom2y = mom2y[n_elements(mom2y)-1]
     props.mom2v = mom2v[n_elements(mom2v)-1]
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PRINCIPAL AXES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Perform the principal axes decomposition and record those values.

     ellfit, x=x, y=y, wt=t, posang=posang

;    CALCULATE THE ROTATED X AND Y
     xrot = x*cos(posang)+y*sin(posang)
     yrot = -x*sin(posang)+y*cos(posang)    

;    FIRST MOMENT
     mom1xrot = total(xrot*t, cumul=do_extrap)/total(t, cumul=do_extrap)
     mom1yrot = total(yrot*t, cumul=do_extrap)/total(t, cumul=do_extrap)

;    SECOND MOMENT
     term1xrot = total(t*double(xrot)^2., cumul=do_extrap)
     term2xrot = (total(t*double(xrot), cumul=do_extrap))^2./mom0t
     mom2xrot = sqrt((term1xrot - term2xrot)/mom0t)
     zeroind = where(abs(term1xrot - term2xrot) lt 1.d-10, num)
     if (num gt 0) then $
        mom2xrot[zeroind] = 0.0
     
     term1yrot = total(t*double(yrot)^2., cumul=do_extrap)
     term2yrot = (total(t*double(yrot), cumul=do_extrap))^2./mom0t
     mom2yrot = sqrt((term1yrot - term2yrot)/mom0t)
     zeroind = where(abs(term1yrot - term2yrot) lt 1.d-10, num)
     if (num gt 0) then $
        mom2yrot[zeroind] = 0.0    
     
;    SAVE TO STRUCTURE
     props.momposang = posang     

     props.mom2maj = mom2xrot[n_elements(mom2xrot)-1]
     props.mom2min = mom2yrot[n_elements(mom2yrot)-1]

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; CURVE OF GROWTH EXTRAPOLATION
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Perform the curve-of-growth extrapolation.

     if do_extrap then begin

        ex_mom2x = extrap(t, mom2x, targett = targett, /fast $
                          , scatter = e_mom2x, /weight)
        
        ex_mom2y = extrap(t, mom2y, targett = targett, /fast $
                          , scatter = e_mom2y, /weight)
        
        ex_mom2v = extrap(t, mom2v, targett = targett, /fast $
                          , scatter = e_mom2v, /weight)

        ex_mom2xrot = extrap(t, mom2xrot, targett = targett, /fast $
                             , scatter = e_mom2xrot, /weight)
        
        ex_mom2yrot = extrap(t, mom2yrot, targett = targett, /fast $
                             , scatter = e_mom2yrot, /weight)
        
        ex_mom0t = extrap(t, mom0t, targett = targett, /fast, square = square, $
                          scatter = e_mom0t, /weight)

        if (ex_mom0t lt max(mom0t, /NAN)) then begin
           help, calls = calls
           if n_elements(calls) eq 2 then begin
              print, 'ERROR in EXTRAPOLATION!!! Extrapolated flux is LESS THAN measured flux. That is BAD!'
              print, 'Defaulting to linear extrapolation...'
           endif
           ex_mom0t = extrap(t, mom0t, targett = targett, /fast)
        endif

;       ASSIGN TO STRUCTURE     
        props.mom0_extrap = ex_mom0t

        props.mom2x_extrap = ex_mom2x
        props.mom2y_extrap = ex_mom2y
        props.mom2v_extrap = ex_mom2v

        props.mom2maj_extrap = ex_mom2xrot
        props.mom2min_extrap = ex_mom2yrot

     endif

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; VELOCITY GRADIENT AXIS (added on Dec 4, 2014 by schruba@mpe.mpg.de)
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     cubify $
        , x=x, y=y, v=v, t=t $
        , cube = minicube $
        , pad = 1 $
        , location = location $
        , /silent

     l3d = array_indices(minicube, location)

     sz = size(minicube)
     xaxis = (indgen(sz[1])+min(x)-1)
     yaxis = (indgen(sz[2])+min(y)-1)
     vaxis = (indgen(sz[3])+min(v)-1)
     xmap = (intarr(sz[2])+1) ## xaxis
     ymap = yaxis ## (intarr(sz[1])+1)
     vcube = rebin(reform(vaxis,1,1,sz[3]), sz[1:3])

     mom0 = total(minicube,3,/nan)
     mom1 = total(vcube*minicube,3,/nan) / mom0

;    ESTIMATE UNCERTAINTY IN VELOCITY FIELD
     errcube = (vcube - rebin(mom1, sz[1:3]))^2
     mom1err = props.noise/mom0 * sqrt(total(errcube,3,/nan))

;    FIT A PLANE TO THE MOMENT MAP (REQ FREUDENREICH ROUTINES)
;    resid : cloud velocity dispersion after gradient correction
     if (keyword_set(robust)) then begin  
        fitmap = ROB_MAPFIT(mom1, 1, coef, resid)
     endif else begin
        mom1vec = reform(mom1[l3d[0,*],l3d[1,*]])
        weights = 1.0/(mom1err^2)
        wt = reform(weights[l3d[0,*],l3d[1,*]])
        if total(finite(wt)) ge 10 then begin
           coef = PLANEFIT(x, y, mom1vec, wt, vfit)
           resid = stddev(vfit - mom1vec)
        endif else begin
           coef = fltarr(4) * !values.f_nan
           resid = !values.f_nan
        endelse
     endelse

;    CALCULATE GRADIENT AND POSITION ANGLE
     velgrad = sqrt(coef[1]^2+coef[2]^2) ; gradient in pixel units
     velposang = atan(coef[2],coef[1])   ; angle from x-axis (increasing R.A.)
                                         ; to y-axis (increasing DEC)

;    SAVE RESULTS
     props.velgrad = velgrad
     props.velgrad_unit = 'pix/pix'
     props.velposang = velposang
     props.velposang_unit = 'rad'
     props.veldisp = resid
     props.veldisp_unit = 'pix'

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; BE DONE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

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
     props.flux = props.mom0*mom_to_flux
     props.flux_extrap = props.mom0_extrap*mom_to_flux

     mom_to_lum = props.chanwidth_kms*(props.pcperpix)^2
     props.lum = props.mom0*mom_to_lum
     props.lum_extrap = props.mom0_extrap*mom_to_lum

     mom_to_mass = props.chanwidth_kms*(props.pcperpix)^2*props.alpha
     props.mass = props.mom0*mom_to_mass
     props.mass_extrap = props.mom0_extrap*mom_to_mass

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; POSITIONAL QUANTITIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     xy2ad, props.mom1x, props.mom1y, astr, ra_mom1, dec_mom1
     props.xpos = ra_mom1
     props.ypos = dec_mom1

     channum = findgen(n_elements(vaxis))
     props.vpos = interpol(vaxis, channum, props.mom1v)
     
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; SIZE-LIKE QUANTITIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     props.xrms = props.mom2x*props.pcperpix
     props.xrms_extrap = props.mom2x_extrap*props.pcperpix
     props.xrms_extrap_deconv = $
        sqrt(props.mom2x_extrap^2 - (props.beamfwhm_pix/props.sig_to_fwhm)^2) * $
        props.pcperpix
     
     props.yrms = props.mom2y*props.pcperpix
     props.yrms_extrap = props.mom2y_extrap*props.pcperpix
     props.yrms_extrap_deconv = $
        sqrt(props.mom2y_extrap^2 - (props.beamfwhm_pix/props.sig_to_fwhm)^2) * $
        props.pcperpix

     props.vrms = props.mom2v*props.chanwidth_kms
     props.vrms_extrap = props.mom2v_extrap*props.chanwidth_kms
     props.vrms_extrap_deconv = $
        sqrt(props.mom2v_extrap^2 - (1.0*props.chantosig)^2)*props.chanwidth_kms

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PRINCIPAL AXIS QUANTITIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     props.majrms = props.mom2maj*props.pcperpix
     props.majrms_extrap = props.mom2maj_extrap*props.pcperpix

     props.minrms = props.mom2min*props.pcperpix
     props.minrms_extrap = props.mom2min_extrap*props.pcperpix

;    DO THE TWO-D DECONVOLUTION (FACTORS MATCH SIGMA TO FWHM)
     deconvolve_gauss $
        , meas_maj = props.majrms_extrap*props.sig_to_fwhm $
        , meas_min = props.minrms_extrap*props.sig_to_fwhm $
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
        props.majrms_extrap_deconv = src_maj/props.sig_to_fwhm
        props.minrms_extrap_deconv = src_min/props.sig_to_fwhm
        props.posang_extrap_deconv = src_pa
        props.resolved_extrap = 1B
     endif else begin
        props.resolved_extrap = 0B
     endelse

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; VELOCITY GRADIENT AXIS IN PHYSICAL UNITS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

   props.velgrad *= props.chanwidth_kms / props.pcperpix
   props.veldisp *= props.chanwidth_kms
   props.velgrad_unit = 'km/s/pc'
   props.veldisp_unit = 'km/s'

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PHYSICAL QUANTITIES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     props.radrms_extrap_deconv = $
        props.rmstorad*sqrt(props.majrms_extrap_deconv* $
                            props.minrms_extrap_deconv)

     props.virmass_extrap_deconv = $
        props.vircoeff*props.radrms_extrap_deconv* $
        props.vrms_extrap_deconv^2
     
     return, props

  endif

; SHOULD NOT GET HERE
  message, "No calculation carried out. Check calling sequence.", /info
  return, nan

end
   
