function measure_moments $
   , xin = xin $
   , yin = yin $
   , vin = vin $
   , tin = tin $
   , empty_props = empty_props $
   , modules = extra_modules $
   , do_clip = do_clip $
   , clipval = clipval $
   , just_empty = just_empty $
   , rms = rms $
   , _extra = extra

;+
;
;-
  
  compile_opt idl2
   
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFINE THE SET OF MODULES THAT WE WILL WORK WITH
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; Each modules adds fields and calculations to the moment structure.

  modules = [ "classic" $
            , "gausscorr" $
            , "area" ]

  if n_elements(extra_modules) gt 0 then begin
     modules = [modules, extra_modules]
  endif
  n_mod = n_elements(modules)
  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; INITIALIZE THE OUTPUT STRUCTURE
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; Call the individual modules to initialize the fields that they will
; need. this step is skipped if we already have an empty property structure.
  
  if n_elements(empty_props) eq 0 or keyword_set(just_empty) then begin
     empty_props = empty_moment_struct()
     for i = 0, n_mod-1 do begin
        empty_props = $
           call_function("moments_"+modules[i] $
                         , empty_props $
                         , /define_field $
                         , /moments $
                         , _extra=extra)
     endfor
     if keyword_set(just_empty) then begin
        return, empty_props
     endif
  endif

  props = empty_props

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; HANDLE THE DATA
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; We organize into a sorted, potentially clipped, array of values that
; will be used by the individual moments for further calculation.

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

; RECORD BASIC PIXEL VALUES
  props.npix = n_elements(t)
  props.minval = min(t)
  props.maxval = max(t)

; IF REQUESTED, CLIP THE DATA
  if keyword_set(do_clip) then begin
     
;    DEFAULT TO CLIP AT THE EDGE VALUE OF THE CLOUD
     if n_elements(clipval) eq 0 then $
        clipval = props.minval_meas
     
;    REMOVE THE CLIPPING VALUE FROM THE INTENSITY
     t = (t - clipval) > 0
     props.clipped = 1B
     props.clipval_meas = clipval

;    REMEASURE THE MIN AND MAX AFTER CLIPPING
     props.minval_meas = min(t)
     props.maxval_meas = max(t)
  endif

; SAVE RMS NOISE VALUE AT PEAK LOCATION
if n_elements(rms) gt 0 then begin
   if n_elements(rms) eq 1 then begin
      props.noise = rms
      endif else begin
         dummy = max(t, ind)
            props.noise = rms[x[ind],y[ind],v[ind]]
      endelse
endif
  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; LOOP OVER THE MODULES REQUESTED
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
  
  for i = 0, n_mod-1 do begin
     props = $
        call_function("moments_"+modules[i] $
                      , props $
                      , xin=x, yin=y, vin=v, tin=t $
                      , /calculate $
                      , /moments $
                      , _extra=extra)
  endfor
  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; RETURN
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  return, props

end                             ; OF MEASURE_MOMENTS



