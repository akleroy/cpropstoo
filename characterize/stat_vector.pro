function stat_vector $
   , x_in, e_in $
   , oversamp=oversamp $
   , nan=nan $
   , skip_perc=skip_perc

;+
; NAME:
;
;  stat_vector
;
; PURPOSE:
;
; Quickly characterize a distribution of values, optionally with
; associated errors.
;
; CATEGORY:
;
;  Science tool.
;
; CALLING SEQUENCE:
;
; output_structure = stat_vector(data, error, oversamp = 10, /nan)
;
; INPUTS:
;
; data : the input data vector
;
; OPTIONAL INPUTS:
;
; err : error vector
;
; KEYWORD PARAMETERS:
;
; skip_perc : skip the percentile calculation, which is expensive
; (i.e., avoid a sort)
;  
; oversamp : the degree to which the data are oversampled
;
; nan : pare not-a-number values before calculating
;
; OPTIONAL OUTPUTS:
;
; 
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
; TO DO:
;
; MODIFICATION HISTORY:
;
;
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; INITIALIZE AN EMPTY STRUCTURE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  

; Percentiles corresponding to {1,2,3} sigma.  
  perc_vec = $
     [0.0013498980316291513 $
      , 0.022750131948099109 $
      , 0.15865525393148883 $
      , 0.84134474606851117 $
      , 0.97724986805190084 $
      , 0.99865010196837090]

; Count thresholds to expect a random deviate
  perc_thresh = $
     [741L, 44L, 6L, 6L, 44L, 741L]

; Build a structure that will hold the measurements 
  nan = !values.f_nan
  meas = $
     { $
     , counts: 0L $
     , min: nan $     
     , max: nan $
     , mean: nan $
     , meanlog: nan $
     , med: nan $
     , std: nan $
     , stdlog: nan $
     , mad: nan $
     , madlog: nan $
     , frac_std: nan $
     , frac_mad: nan $     
     , perc: perc_vec $
     , perc_val: nan*perc_vec $
     }     
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ERROR CHECKING AND SET DEFAULTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Check existence of data  
  if n_elements(x_in) eq 0 then begin
     message, "No data. Returning empty structure.", /informational
     return, meas
  endif

; Check size of error vector  
  if n_elements(e_in) eq 0 then begin
     e = nan * x_in
  endif else if  n_elements(e_in) eq 1 then begin
     e = 0.0*x_in + e_in
  endif else if n_elements(e_in) eq n_elements(x_in) then begin
     e = e_in
  endif else begin
     message, 'Error must have 0, 1, or matched set of errors.', /info
     return, meas
  endelse
  
; Default to independent data
  if n_elements(oversamp) eq 0 then begin
     oversamp = 1.
  endif

; If requested, pare non-finite data from the input vectors
  if keyword_set(nan) then begin
     keep = where(finite(x_in))
     x = x_in[keep]
     e = e[keep]
  endif else begin
     if finite(x_in) ne n_elements(x_in) then begin
        message, "Non-finite data supplied without /nan. Returning.", /info
        return, meas
     endif
     x = x_in
  endelse

; Set a threshold in minimum counts to estimate scatter
  if n_elements(thresh_for_scatt) eq 0 then begin
     thresh_for_scatt = 6L
  endif
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CALCULATIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
     
  meas.counts = n_elements(x)
  meas.min = min(x) 
  meas.max = max(x)
  meas.mean = mean(x)
  meas.meanlog = mean(alog10(x))
  meas.med = median(x, /even)

  if meas.counts gt thresh_for_scatt then begin
     
     meas.std = stddev(x)
     meas.stdlog = stddev(alog10(x))
     meas.mad = mad(x)
     meas.madlog = mad(alog10(x))
     meas.frac_std = stddev(x)/meas.mean
     meas.frac_mad = mad(x)/meas.median     
     
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN RESULTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, meas
  
end                             ; of stat_vector
