function base_fit $
   , xaxis = xaxis_in $
   , yaxis = yaxis_in $
   , flag = flag $
   , degree = degree $
   , orig_y = orig_y $
   , show = show $
   , status = status $
   , _extra = extra
  
;+
; NAME:
;
; base_fit
;
; PURPOSE:
;
; A pared-down polynomial baseline fitter. Doesn't have to be the
; bee's knees, just has to be FAST. A more general, robust baseline
; fitter is a horse of a different color (e.g., my old fit_baseline).
;
; CATEGORY:
;
; Data reduction tool.
;
; CALLING SEQUENCE:
;
; full docs to follow
;
; INPUTS:
;
;
;
; OPTIONAL INPUTS:
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
; none
;
; SIDE EFFECTS:
;
; none
;
; RESTRICTIONS:
;
; none
;
; PROCEDURE:
;
; Uses SVDFIT with the /legendre keyword
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; PARSE THE INPUTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(xaxis_in) eq 0 then begin
     xaxis_in = findgen(n_elements(yaxis_in))
  endif

  if n_elements(flag) eq 0 then begin
     flag = finite(xaxis_in) and finite(yaxis_in)
  endif

; NUMBER OF ELEMENTS IN THE SPECTRUM
  nspec = n_elements(spec)
  
; DEFAULT TO A LINEAR BASELINE
  if n_elements(degree) eq 0 then $
     degree = 1

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; COPY INPUT ARRAYS, WHICH WE WILL MANGLE HORRIBLY
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  xaxis = xaxis_in
  yaxis = yaxis_in

  coeffs = fltarr(degree+1)*!values.f_nan

  fit = xaxis*!values.f_nan

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; EXTRACT THE DATA THAT WE INTEND TO FIT WITH
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  use = finite(xaxis) and finite(yaxis) and (flag eq 1)

; MARKER FOR THE FITTING
  fit:

  if total(use) eq 0 then begin
     status = -1
     return, coeffs
  endif

  use_ind = where(use, use_ct)
  xaxis = xaxis_in[use_ind]
  yaxis = yaxis_in[use_ind]

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DO THE FIT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; POLYNOMIAL FIT OF REQUESTED DEGREE
  if keyword_set(lad) and degree eq 1 then begin
     coefs = ladfit(xaxis, yaxis)
     ybase = coefs[0] + xaxis*coefs[1]
  endif else begin
     median_y = median(yaxis)
     yaxis -= median_y
     if degree eq 0 then begin
;       ... JUST USE THE MEDIAN FOR DEGREE 0?
        coefs = [0.0]
        yfit = 0.0 * yaxis
     endif else begin
        coefs = $
           ROBUST_POLY_FIT(xaxis,yaxis,degree,yfit)
     endelse
     yaxis += median_y
     coefs[0] += median_y
     yfit += median_y
     ybase = yfit
  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; PLUG IN THE BASELINE FIT TO THE LARGER AXIS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  fit[use_ind] = ybase

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; PLOT IF REQUESTED
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(show) then begin
     if n_elements(orig_y) gt 0 then $
        plot, xaxis_in, orig_y, ps=1, _extra = extra $
     else $
        plot, xaxis_in, yaxis_in, ps=1, _extra = extra
     oplot, xaxis, yaxis, ps=1, color=getcolor('blue')
     oplot, xaxis, ybase, color=getcolor('red'), thick=3
  endif

; AND WE ARE ... OUT!
  status=1
  return, coefs

end
