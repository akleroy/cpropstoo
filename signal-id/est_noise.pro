function est_noise $
   , data $
   , mask=mask $
   , stddev=use_stddev $
   , negative=negative $
   , iterate = iterate $
   , thresh = thresh $
   , noutlier = noutlier

;+
; NAME:
;
; est_noise
;
; PURPOSE:
;
; Bootstrap the rms noise from a vector of data. Can be coarsely used
; to get a number to characterize a whole data set or applied to
; individual lines of sight or planes to make noise more sophisticated
; measurements.
;
; CATEGORY:
;
; Analysis tool
;
; CALLING SEQUENCE:
;
; noise_rms = est_noise(data, method=method)
;
; INPUTS:
;
; A vector containing data that is dominated by noise.
;
; OPTIONAL INPUTS:
;
; mask: a mask used to identify signal that will be ignored when
; fitting the noise. Anything that is 0 in the mask is assumed to be
; noise and is used to fit. So this is a *signal* mask.
;
; thresh: hard threshold, in units of sigma, used for outlier
; rejection. The default if "iterate" is set and nothing else is
; specified, is to clip at 3 sigma.
;
; noutlier: acceptable number of expected outliers used as an
; alternative way to specify the threshold for outlier rejection. This
; is compared to the cumulative normal distribution and the number of
; data to calculate a threshold. For comparison ~1 outlier is expected
; for about 750 samples and this is default behavior above, so this is
; more of a refinement option.
;
; iterate: apply iterative outlier rejection to refine the
; estimate. Useful in the case where no a priori masking has been done
; and bright signal is expected. The numerical value of the keyword
; iterate is the number of recursive iterations of noise estimation
; that will be applied. Be careful with very large data sets here (or
; if its a recurring issue, we can change the code from the recursive
; model).
;
; KEYWORD PARAMETERS:
;
; stddev: use the standard deviation instead of the default median
; absolute deviation to calculate the width of the noise distribution
;
; negative: use only the negatives (assumed to have a symmetric
; positive counterpart distribution) to estimate the noise.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; N/A
;
; COMMON BLOCKS:
;
; N/A
;
; SIDE EFFECTS:
;
; N/A
;
; RESTRICTIONS:
;
; N/A
;
; PROCEDURE:
;
; N/A
;
; EXAMPLE:
;
; N/A
;
; MODIFICATION HISTORY:
; 
; 17-Oct-14 : Separated out from other programs in make_noise_cube.
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; COPY THE DATA - INEFFICIENT BUT CLEAN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  vec = data

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; MAKE SURE THAT A MASK EXISTS AND BLANKS NON-FINITE DATA
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(mask) eq 0 then $
     mask = finite(data) eq 0 $
  else $
     mask = mask or (finite(data) eq 0)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; IDENTIFY THE DATA TO USE IN THE ESTIMATE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(negative) then begin
     use = where((mask eq 0) and vec le 0.0)
     vec = [vec[use], -1*vec[use]]
  endif else begin
     use = where(mask eq 0)
     vec = vec[use]
  endelse
     
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CALCULATE THE ESTIMATE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(use_stddev) then $
     sigma = stddev(vec) $
  else $
     sigma = mad(vec)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RECURSIVELY ITERATE IF REQUESTED
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(iterate) gt 0 then begin

     if iterate gt 0 then begin
     
        iterate = iterate - 1

        if n_elements(thresh) eq 0 and $
           n_elements(noutlier) eq 0 then $
              thresh = 3.0
        
        if n_elements(thresh) gt 0 then $
           reject_val = thresh*sigma $
        else if n_elements(noutlier) gt 0 then begin
           nval = total(mask eq 0)
           reject_thresh = sig_n_outliers( $
                           nval $
                           , n_outliers=noutlier)
           reject_val = reject_thresh*sigma
        endif

        mask = mask or $
               (abs(data) gt reject_val)

        sigma = est_noise( $
                data $
                , mask=mask $
                , stddev=stddev $
                , negative=negative $
                , iterate = iterate $
                , thresh = thresh $
                , noutlier = noutlier)
                
     endif

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, sigma
          
end
