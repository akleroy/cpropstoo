pro ellfit $
   , x = x $
   , y = y $
   , wt = wt $
   , major = major $
   , minor = minor $
   , posang = posang

;+
; NAME:
;   
; ELLFIT
;
; PURPOSE:
;
; Calculate the best fit ellipse for an X and Y distribution, allowing
; for weighting.
;
; CALLING SEQUENCE:
;
;   ELLFIT(x=x, y=y, wt=wt, major = major, minor = minor, posange=posang)
;
; INPUTS:
;
;   X - x position of data
;
;   Y - y position of data
;
;   WT - weighting of data (defaults to 1)
;
; OUTPUTS:
;
;   MAJOR - major axis in same units as x and y
;
;   MINOR - minor axis in same units as x and y
;
;   POSANG - the position angle CCW from the X=0 line of the coordinates used.
;
; NOTES:
;
;   The intensity weighted major and minor values are equal to the
;   second moment. For equal weighting by pixel (of the sort that
;   might be done for blob analysis) the ellipse fit to the
;   half-maximum area will have semimajor axis equal to 1./1.69536 the
;   second moment. For the quarter maximum surface this is 1./1.19755.
;
; MODIFICATION HISTORY:
;
;   Adapted from CPROPS pa_moments and D. Fanning webpages.
;
;-

  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFAULTS AND DEFINITIONS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; DEFAULT WEIGHTS TO 1
  if n_elements(wt) eq 0 then begin
     wt = x*0.0 + 1.0
  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CALCULATE MATRIX AND EIGENVALUES
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&  

; WEIGHTED X AND Y CENTERS
  x_ctr = total(wt*x)/total(wt)
  y_ctr = total(wt*y)/total(wt)

; BUILD THE MATRIX
  tot_wt = total(wt)
  i11 = total(wt*(x-x_ctr)^2)/tot_wt
  i22 = total(wt*(y-y_ctr)^2)/tot_wt
  i12 = total(wt*(x-x_ctr)*(y-y_ctr))/tot_wt
  mat = [[i11, i12], $
         [i12, i22]]

; CATCH THE CASE OF MATRIX WITH NANs  
  if finite(total(mat)) eq 0 then begin
     major = !values.f_nan
     minor = !values.f_nan
     posang = !values.f_nan
     return
  endif

; CATCH THE CASE OF ZERO DETERMINANT  
  if determ(mat, /check) eq 0 then begin
     major = !values.f_nan
     minor = !values.f_nan
     posang = !values.f_nan
     return
  endif

; WORK OUT THE EIGENVALUES
  evals = eigenql(mat, eigenvectors=evec, /double)

; PICK THE MAJOR AXIS
  major = max(abs(evals), maj_ind)
  major_vec = float(evec[*, maj_ind])
  min_ind = (maj_ind eq 1) ? 0 : 1

; WORK OUT THE ORIENTATION OF THE MAJOR AXIS
  posang = atan(major_vec[1], major_vec[0])
  posang = posang+!pi*(posang lt 0)
  posang = float(posang)

; MAJOR AND MINOR AXIS SIZES
  major = float(sqrt(evals[maj_ind]))
  minor = float(sqrt(evals[min_ind]))

  return

end
