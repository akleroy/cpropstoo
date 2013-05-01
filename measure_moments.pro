function measure_moments $
   , x = xin $
   , y = yin $
   , v = vin $
   , t = tin $
   , extrap = do_extrap $
   , extarg = targett $
   , fluxlin = fluxlin $
   , clip = do_clip $
   , edge = edge

;+
;
;-
  
  compile_opt idl2

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFAULTS & DEFINITIONS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; INITIALIZE OUTPUT
  props = empty_moment_struct()

; SET THE FUNCTIONAL FORM FOR THE LUMINOSITY EXTRAPOLATION
  if n_elements(fluxlin) eq 0 then $
     square = 1 $
  else $
     square = 0

; SET THE FUNCTIONAL FORM FOR THE LUMINOSITY EXTRAPOLATION
  if keyword_set(do_extrap) then $
     do_extrap = 1 $
  else $
     do_extrap = 0

; SET THE TARGET FOR THE CURVE OF GROWTH
  if n_elements(targett) eq 0 then $
     targett = 0

; COPY THE INPUT ARRAYS
  x = xin
  y = yin
  v = vin
  t = tin
  
; SORT BY DECREASING ANTENNA TEMPERATURE AND REORDER ALL OF THE
; ARRAYS. THE BRIGHTEST PIXEL IS NOW ELEMENT 0.
  sort_t = reverse(sort(t))
  x = x[sort_t]
  y = y[sort_t]
  v = v[sort_t]
  t = t[sort_t]  

; RECORD BASICS
  props.npix.val = n_elements(t)
  props.minval.val = min(t)
  props.maxval.val = max(t)

; IF REQUESTED, CLIP THE DATA
  if keyword_set(do_clip) then begin
     
;    DEFAULT TO CLIP AT THE EDGE VALUE OF THE CLOUD
     if n_elements(clipval) eq 0 then $
        clipval = props.minval.val
     
;    REMOVE THE CLIPPING VALUE FROM THE INTENSITY
     t = (t - clipval) > 0

  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CALCULATE CUMULATIVE MOMENTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; ZEROTH MOMENT
  mom0t = total(t, cumul=do_extrap)

; FIRST MOMENT
  mom1x = total(x*t, cumul=do_extrap)/total(t, cumul=do_extrap)
  mom1y = total(y*t, cumul=do_extrap)/total(t, cumul=do_extrap)
  mom1v = total(v*t, cumul=do_extrap)/total(t, cumul=do_extrap)

; SECOND MOMENT
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
  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; AREA, VELOCITY WIDTH, AND ELLIPSE FIT CALCULATIONS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; ID POINTS ABOVE HALF MAXIMUM

; (Note that there is a subtley here. We keep all points above half
; max but one might want only connected points above half max. Right
; now, I think the current way is more consistent with the overall
; assignment definition.)

; ... WORK OUT A SET OF UNIQUE TWO-D INDICES
  twod_id = x + y*(max(x)+1)  
  twod_ind = uniq(twod_id, sort(twod_id))
  twod_x = x[twod_ind]
  twod_y = y[twod_ind]
  
; ... WORK OUT INDICES WHERE INTENSITY > 0.5 MAX
  halfmax_ind = where(t gt 0.5*props.maxval.val) 
  halfmax_twod_ind = uniq(twod_id[halfmax_ind], sort(twod_id[halfmax_ind]))
  halfmax_twod_x = x[halfmax_twod_ind]
  halfmax_twod_y = y[halfmax_twod_ind]

; AREA 
  area = n_elements(twod_ind)*1.0
  area_halfmax = n_elements(halfmax_twod_ind)*1.0

; DELTAV
  deltav = max(v) - min(v)
  deltav_halfmax = max(v[halfmax_ind]) - min(v[halfmax_ind])

; ELLIPSE FITS

; ... AT HALF-MAX, UNWEIGHTED (TWOD ONLY)
  ellfit, x=halfmax_twod_x, y=halfmax_twod_y $
          , maj=half_ell_maj, min=half_ell_min, posang=half_ell_pa

; ... TOTAL, UNWEIGHTED (TWOD ONLY)
  ellfit, x=twod_x, y=twod_y $
          , maj=ell_maj, min=ell_min, posang=ell_pa

; ... TOTAL, WEIGHTED
  ellfit, x=x, y=y, wt=t, posang=posang
  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CALCULATE MOMENTS ALONG THE NATURAL MAJOR/MINOR AXIS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; CALCULATE THE ROTATED X AND Y
  xrot = x*cos(posang)+y*sin(posang)
  yrot = -x*sin(posang)+y*cos(posang)    

; FIRST MOMENT
  mom1xrot = total(xrot*t, cumul=do_extrap)/total(t, cumul=do_extrap)
  mom1yrot = total(yrot*t, cumul=do_extrap)/total(t, cumul=do_extrap)

; SECOND MOMENT
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

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; ASSIGN VALUES TO THE OUTPUT STRUCTURE
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  props.area.val = area
  props.area_halfmax.val = area_halfmax
  props.deltav.val = deltav
  props.deltav_halfmax.val = deltav_halfmax

  props.ell_maj.val = ell_maj
  props.ell_min.val = ell_min
  props.ell_pa.val = ell_pa
  props.ell_maj_halfmax.val = half_ell_maj
  props.ell_min_halfmax.val = half_ell_min
  props.ell_pa_halfmax.val = half_ell_pa

  props.posang.val = posang     
  props.mom0.val_meas = mom0t[n_elements(mom0t)-1]
  props.mom1_x.val_meas = mom1x[n_elements(mom1x)-1]
  props.mom1_y.val_meas = mom1y[n_elements(mom1y)-1]
  props.mom1_v.val_meas = mom1v[n_elements(mom1v)-1]
  props.mom2_x.val_meas = mom2x[n_elements(mom2x)-1]
  props.mom2_y.val_meas = mom2y[n_elements(mom2y)-1]
  props.mom2_v.val_meas = mom2v[n_elements(mom2v)-1]

  props.mom2_maj.val_meas = mom2xrot[n_elements(mom2xrot)-1]
  props.mom2_min.val_meas = mom2yrot[n_elements(mom2yrot)-1]

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CURVE OF GROWTH EXTRAPOLATION
; %&%&%&%&%&%&%&%&%&%&%&%&% &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

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

;    AREA

;    DELTAV

;    ASSIGN TO STRUCTURE
     
     props.mom0.val_extrap = ex_mom0t
     props.mom2_x.val_extrap = ex_mom2x
     props.mom2_y.val_extrap = ex_mom2y
     props.mom2_v.val_extrap = ex_mom2v
     props.mom2_maj.val_extrap = ex_mom2xrot
     props.mom2_min.val_extrap = ex_mom2yrot

  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; RETURN
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
  
  return, props
  
end                             ; OF MEASURE_MOMENTS



