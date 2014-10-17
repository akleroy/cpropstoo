function sig_n_outliers $
   , n_data $
   , n_outliers=n_outliers $
   , pos_only=pos_only

;+ 
;
; Use the cumulative distribution function of the normal distribution
; to solve for the effective number of "sigmas" that would be needed
; to expect *n_outliers* random deviate outliers in a vector of
; *n_data*.
;
;-

; Default to considering only positive random outliers.
  if n_elements(pos_only) eq 0 then $
     pos_only = 1B

; Default to requesting only one outlier
  if n_elements(n_outliers) eq 0 then $
     n_outliers = 1.0

; Calculate the location in the CDF that we want to solve for
  perc = float(n_outliers)/n_data

  if pos_only eq 0B then $
     perc *= 2.0

  target_cdf = 1d0 - perc
  
; Now following that the CDF = 1/2*(1+erf(x/sqrt(2))) we need to solve
; for:
  target = (2.0*target_cdf - 1.)

  soln = bisection(3., 'erf0', erftarg = target, /double)  
  thresh = soln*sqrt(2.)
  return, thresh

end
