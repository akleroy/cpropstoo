function bin_data $
   , x_in, y_in $
   , xerr = x_err $
   , yerr = yerr_in $
   , xmin = xmin_in $
   , xmax = xmax_in $
   , binsize = binsize_in $
   , irregular = irregular $
   , oversamp=oversamp $
   , n_monte = n_monte $
   , nan=nan $
   , thresh_for_scatt=thresh_for_scatt

;+
; NAME:
;
;  bin_data
;
; PURPOSE:
;
; Bin the supplied data, returning a structure describing the
; statistical properties of each bin.
;
; CATEGORY:
;
;  Science tool.
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
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
; Interface with the contour_values program to allow natural bin
; determination.
;
; Implement monte carlo error determination.
;
; Allow an easy application to a single vector of data, i.e., a single
; bin, without requiring a fiducial x-vector.
;
; MODIFICATION HISTORY:
;
;
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ERROR CHECKING AND SET DEFAULTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Check size and existence of data
  if (n_elements(y_in) ne n_elements(x_in)) or $
     (n_elements(x_in) eq 0) then begin
     message, "Mismatched or missing data. Returning.", /informational
     return, !values.f_nan
  endif

; Check the existence and size of the error vector  
  if n_elements(yerr_in) gt 0 then begin
     if n_elements(yerr_in) ne 1 and $
        n_elements(yerr_in) ne n_elements(x_in) then begin
        message, "Mismatched uncertainty length. Returning.", /informational
        return, !values.f_nan
     endif

;    If a single value of the uncertainty is supplied then explode it
;    into a vector.
     if n_elements(yerr_in) eq 1 then begin
        yerr_in = yerr_in + 0.0*y_in
     endif
     
     have_err = 1B
  endif else begin
     have_err = 0B
  endelse
  
; Default to independent data
  if n_elements(oversamp) eq 0 then begin
     oversamp = 1.
  endif

; If requested, pare non-finite data from the input vectors
  if keyword_set(nan) then begin
     keep = where(finite(x_in) and finite(y_in))
     x = x_in[keep]
     y = y_in[keep]
     if have_err then e = yerr_in[keep]
  endif else begin
     if total(finite(x_in) and finite(y_in)) ne n_elements(x_in) then begin
        message, "Non-finite data supplied without /nan. Returning.", /info
        return, !values.f_nan
     endif
     x = x_in
     y = y_in
     if have_err then e = yerr_in
  endelse

; Default to no Monte Carlo treatment
  if n_elements(n_monte) eq 0 then begin
     n_monte = 0L
  endif else begin
     message, "Monte carlo requested but not yet implemented.", /info
  endelse

; Default the percentile measurements to consider the one-sigma range
  if n_elements(perc) eq 0 then begin
     perc = 0.15
  endif

; Set a threshold in minimum counts to estimate scatter
  if n_elements(thresh_for_scatt) eq 0 then begin
     thresh_for_scatt = 6L
  endif
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; AN EMPTY STRUCTURE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  

  nan = !values.f_nan
  empty_bin = $
     { $
     xmin: nan $
     , xmax: nan $
     , xmid: nan $
     , xwidth: nan $
     , xgeo: nan $
     , xmean: nan $
     , xmed: nan $
     , counts: nan $
     , ysum: 0.0 $
     , e_ysum: nan $
     , yvary: nan $
     , e_yvary: nan $     
     , ymean: nan $
     , e_ymean: nan $
     , ymed: nan $
     , e_ymed: nan $
     , ymax: nan $
     , ymin: nan $
     , perc: nan $
     , ylo_perc: nan $
     , yhi_perc: nan $
     , ymad: nan $
     , ymad_frac: nan $
     , ymad_log: nan $     
     , ystd: nan $
     , ystd_frac: nan $
     , ystd_log: nan $     
     }     
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CONSTRUCT THE BINS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
  if n_elements(xmax_in) eq 0 then begin
      xmax = max(x, /nan)
  endif else begin
      xmax = xmax_in
  endelse

  if n_elements(xmin_in) eq 0 then begin
      if keyword_set(irregular) then begin
          message, 'Bin minima required with /IRREGULAR flag. Returning.', /info
          return, !values.f_nan
      endif
      message, 'No minimum specified. Defaulting to data.', /informational
      xmin = min(x, /nan)
  endif else begin
      xmin = xmin_in
  endelse

  if keyword_set(irregular) eq 0 then begin
      deltax = (xmax - xmin)
      
      if n_elements(binsize_in) eq 0 then begin
          message, "No binsize specified. Making 10 bins.", /info
          binsize = deltax / 10.
      endif else begin
          binsize = binsize_in
      endelse

;     MAKE THE BINS
      nbins = ceil(deltax / binsize) 
      xmin_bin = findgen(nbins)*binsize+xmin 
      xmax_bin = xmin_bin+binsize < xmax 
      xmid_bin = (xmin_bin+xmax_bin)*0.5      
      
  endif else begin
      
      nbins = n_elements(xmax_in)
      xmin_bin = xmin
      xmax_bin = xmax
      xmid_bin = (xmin_bin+xmax_bin)*0.5
      xgeo_bin = sqrt(xmin_bin*xmax_bin)

  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; INITIALIZE PROFILES
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  bins = replicate(empty_bin, nbins)
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; LOOP OVER BINS AND CALCULATE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  for ii = 0L, nbins-1 do begin

     this_bin = empty_bin
          this_bin.xmin = xmin_bin[ii]
     this_bin.xmax = xmax_bin[ii]
     this_bin.xmid = (xmax_bin[ii]+xmin_bin[ii])*0.5
     this_bin.xwidth = (this_bin.xmax - this_bin.xmin)
     this_bin.xgeo = sqrt(xmax_bin[ii]*xmin_bin[ii])

;    Find the pixels in the present bin. Use a greater than or equal
;    value for the lower edge and a less than for the upper edge.
     
     ind = where(x ge xmin_bin[ii] AND $
                 x lt xmax_bin[ii], ct)

     this_bin.counts = ct

     if ct eq 0 then begin
        bins[ii] = this_bin
        continue
     endif
     
     this_x = x[ind]
     this_y = y[ind]
     if have_err then this_e = e[ind]
     
;    Sort the data. This step could be made optional as it accounts
;    for a large fraction of the computation required for the bin.     
     
     sort_ind = sort(this_y)
     this_x = this_x[sort_ind]
     this_y = this_y[sort_ind]
     if have_err then this_e = this_e[sort_ind]

;    Record the properties of the bin in the independent variable
     
     this_bin.xmean = mean(this_x)
     this_bin.xmed = median(this_x,/even)
     
;    Record the properties of the bin in the dependent values
     
     this_bin.ysum = total(this_y)
     this_bin.ymean = mean(this_y)
     this_bin.ymed = median(this_y, /even)
     this_bin.ymin = min(this_y)
     this_bin.ymax = max(this_y)

;    Use the sorted data to work out percentiles. Skip this
;    step if we don't have enough counts in the bin.

     if ct gt (1./perc*2) then begin
        this_bin.perc = perc
        lo_ind = ceil(ct*(0.5-perc/2.))-1
        this_bin.ylo_perc = this_y[lo_ind]
        hi_ind = ceil(ct*(0.5+perc/2.))-1
        this_bin.yhi_perc = this_y[hi_ind]
     endif
     
;    Measure the scatter in the data in various ways

     if ct gt thresh_for_scatt then begin
        this_bin.ymad = mad(this_y)
        this_bin.ymad_frac = mad(this_y/this_bin.ymed)
        this_bin.ymad_log = mad(alog10(this_y))
        this_bin.ystd = stddev(this_y)
        this_bin.ystd_frac = stddev(this_y/this_bin.ymean)
        this_bin.ystd_log = stddev(alog10(this_y))
     endif

;     Note the uncertainty in the mean and median

     if have_err then begin        
        this_bin.e_yvary = sqrt(total(this_e^2) / (1.0*ct))
        if ct gt oversamp then $           
           this_bin.e_ymean = this_bin.e_yvary / sqrt(1.0*ct/oversamp) $
        else $
           this_bin.e_ymean = this_bin.e_yvary
        this_bin.e_ymed = 1.25*this_bin.e_ymean
     endif
     
;     Save this in the larger structure of bins

      bins[ii] = this_bin
      
  endfor

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN RESULTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, bins
  
end                             ; of bin_datax
