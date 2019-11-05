function shuffle $
   , spec = spec $
   , vaxis = orig_vaxis $
   , zero = zero $
   , target_vaxis = new_vaxis $
   , target_hdr = new_hdr $
   , target_naxis = new_naxis $
   , target_crval = new_crval $
   , target_crpix = new_crpix $
   , target_cdelt = new_cdelt $
   , interp=interp $
   , missing=missing $
   , fft = fft $
   , quiet = quiet

;+
;
; SHUFFLE 
;
;Uses interpolation to rearrange a spectrum, array of spectra, or
;spectral cube to sit on a new velocity grid. Optionally, specify a
;new zero point, either for each input spectrum or for each location
;in the cube.
;
; CALLING SEQUENCE
;
; new_spectrum = shuffle(spec = spec, vaxis = orig_vaxis $
;   , zero = zero, target_vaxis = new_vaxis, target_hdr = new_hdr $
;   , target_crval = new_crval, target_crpix = new_crpix $
;   , target_cdelt = new_cdelt, interp=interp $
;   , missing=missing, fft = fft, quiet = quiet)
;
; REQUIRED INPUTS
;
; spec - the spectrum to regrid
; 
; vaxis - the original velocity axis
;
; some combination of target_hdr, target_vaxis, and keywords to make a
; new velocity axis
;
; OPTIONAL INPUTS
;
; zero - a local or global offset applied to the velocity before
;        regridding.
; 
; interp - degree of interpolation
;
; missing - value of missing data. Generall NaN or maybe zero.
;
;-

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; ERROR CHECKING AND INPUT PROCESSING
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

; Require an input velocity axis and spectrum.

  if n_elements(orig_vaxis) eq 0 and n_elements(spec) eq 0 then begin

     if keyword_set(quiet) eq 0 then $
        message, 'Requires input spectrum and velocity'+ $
                 ' information. Returning', /info
     return, !values.f_nan

  endif

; Build the output velocity axis if only keywords (spacing, center,
; etc.) and not a vector are supplied

  if n_elements(new_vaxis) eq 0 then begin

     if n_elements(new_hdr) gt 0 then begin

;       Build the target velocity axis from the header

        if keyword_set(quiet) eq 0 then $
           message, 'Extracting new velocity axis from header.', /info
        make_axes, new_hdr, vaxis=new_vaxis, /vonly

     endif else begin

;       Build the target velocity axis from keywords

        message, 'Attempting to build a new axis from keywords.', /info
        
        if n_elements(new_cdelt) eq 0 then begin
           message, '... defaulting to original channel width.', /info
           new_cdelt = orig_vaxis[1] - orig_vaxis[0]
        endif

        if n_elements(new_crval) eq 0 or $
           n_elements(new_crpix) eq 0 then begin
           message, '... defaulting to original reference value.', /info
           new_crval = orig_vaxis[0]
           new_crpix = 1
        endif

        if n_elements(new_naxis) eq 0 then begin
           message, '... defaulting to original axis length.', /info
           new_naxis = n_elements(orig_vaxis)
        endif        

        new_vaxis = (findgen(new_naxis) - (new_crpix-1.))*new_cdelt + new_crval
        
     endelse

  endif

; If the new and old velocity axes are identical then throw an
; informational error and return.

  if total(new_vaxis ne orig_vaxis) eq 0 then begin

     if keyword_set(quier) eq 0 then $
        message, 'WARNING! Your new and old velocity '+ $
                 'axes are identical. '+ $
                 'Is this really what you want?', /info

     return, spec

  endif

; Note the number of channels.
  
  n_chan = n_elements(new_vaxis)

; Record the size of the spectrum to figure out if it's an array of
; spectra or cube with a plane of spectra.

  sz = size(spec)

  if sz[0] eq 2 then begin
     shape = 'ARRAY'
     n_spec = sz[1]
  endif else if sz[0] eq 3 then begin
     shape = 'CUBE'
     n_spec = sz[1]*sz[2]
  endif else begin
     shape = 'SPEC'
     n_spec = 1
  endelse

; Default to no spectral shift. That is, the default behavior is a
; simple regrid.

  if n_elements(zero) eq 0 then begin

     message, 'Defaulting to regridding mode .', /info
     zero = 0.0

  endif
  
; Trap error in which zero is a mismatched size

  if n_elements(zero) ne 1 and $
     n_elements(zero) ne n_spec then begin

     message, 'The zero point vector should have'+$
              ' either 1 or n_spec elements. Returning.', /info
     return, !values.f_nan

  endif

; Note the number of channels in the old and new axis.

  orig_nchan = n_elements(orig_vaxis)
  orig_deltav = (orig_vaxis[1] - orig_vaxis[0])
  orig_chan = findgen(orig_nchan)

  new_nchan = n_elements(new_vaxis)  
  new_deltav = (new_vaxis[1] - new_vaxis[0])
  new_chan = findgen(new_nchan)

; Default missing and interpolation

  if n_elements(missing) eq 0 then $
     missing = !values.f_nan

  if n_elements(interp) eq 0 then $
     interp = 1
    
  valid_interp = [0,1,2]
  if total(interp eq valid_interp) eq 0 then begin
     if keyword_set(quiet) eq 0 then $
        message, "Invalid interpolation selection. Defaulting to linear." $
                 , /info
     interp = 1
  endif

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; INITIALIZE THE OUTPUT
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

  if sz[0] eq 1 then begin
     output = fltarr(n_chan)+missing
  endif

  if sz[0] eq 2 then begin
     output = fltarr(sz[1], n_chan)+missing
  endif

  if sz[0] eq 3 then begin
     output = fltarr(sz[1], sz[2], n_chan)+missing
  endif

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; LOOP OVER SPECTRA AND DO THE INTERPOLATION
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

  no_overlap_ct = 0L

  for ii = 0L, n_spec-1 do begin

     if keyword_set(quiet) eq 0 then $
        if ii mod 100 eq 0 then $
           counter, ii+1, n_spec, 'SHUFFLE: spectrum '
 

;    Retrieve the current spectrum into its own array

     if sz[0] eq 3 then begin
        yy = ii / sz[1]
        xx = ii mod sz[1]
        this_spec = reform(spec[xx, yy, *])
        if n_elements(zero) gt 1 then $
           this_zero = zero[xx,yy] $
        else this_zero = zero
     endif else begin
        this_spec = reform(spec[ii,*])
        if n_elements(zero) gt 1 then $
           this_zero = zero[ii] $
        else this_zero = zero
     endelse

;    Recenter the current spectrum (this may be trivial for
;    regridding)

     this_vaxis = orig_vaxis - this_zero

;    Check overlap of the recentered spectrum

     max_this_vaxis = max(this_vaxis)
     min_this_vaxis = min(this_vaxis)
        
;    Initialize a new spectrum and fill it with our "missing" value.

     new_spec = fltarr(new_nchan) + missing

;    Find overlap

     channel_mapping = interpol(orig_chan, this_vaxis, new_vaxis)
     
     overlap = where(channel_mapping ge 0.0 and $
                     channel_mapping le orig_nchan-1, overlap_ct)

     if overlap_ct eq 0 then begin
        no_overlap_ct += 1
        continue
     endif

;    Do the interpolation, with the method depending on the user
;    input.

     if interp eq 0 then begin

;       Nearest neighbor interpolation

        new_spec[overlap] = this_spec[round(channel_mapping[overlap])]

     endif else if interp eq 1 then begin

;       Linear interpolation

        new_spec[overlap] = $
           interpol(this_spec, this_vaxis, new_vaxis[overlap])

     endif else begin

;       Quadratic interpolation

;       ... pad here with two channels of zeroes and even steps in
;       velocity to avoid instability from the quadratic
;       interpolation.

        padded_spec = [0,0,this_spec,0,0]
        padded_vaxis = [this_vaxis[0]-2*orig_deltav $
                        , this_vaxis[0]-orig_deltav $
                        , this_vaxis $
                        , this_vaxis[orig_nchan-1]+orig_deltav $
                        , this_vaxis[orig_nchan-1]+2*orig_deltav]

        new_spec[overlap] = $
           interpol(padded_spec, padded_vaxis $
                    , new_vaxis[overlap], /lsquadratic)

     endelse

;    ---------------------------------------------------------------
;    ----------------------------- TEST ----------------------------
;    ---------------------------------------------------------------

;    A. Schruba's implementation of the FFT approach. Needs to
;    be reworked to deal with the refctor.

     if keyword_set(fft) then begin 

        if keyword_set(quiet) eq 0 then $
           message, "FFT shifting is currently not working.", /info

;        results = poly_fit((findgen(orig_n_chan))[interp_ind], $
;                           sample_chan[interp_ind],1,chisq=chisq)

;        if chisq gt 1e-2 then begin

;           if keyword_set(quiet) eq 0 then $
;              message,'Requested velocity axis is non-linear.' $
;                      + ' FFT method cannot be used.',/con
;           new_spec[interp_ind] = !value.f_nan
;           continue

;        endif

;        meanshift = -results[0]/results[1]
;        scalefac = 1/results[1]
;        fftspec = fft(orig)
;        frequency = shift(findgen(n_elements(fftspec))-n_elements(fftspec)/2.0 $
;                          ,n_elements(fftspec)/2.0) ;/n_elements(fftspec) 

;       Phase sampling needs to be shifted to the new frequencies
;        phase = exp(2*!pi*frequency*complex(0,1)*(meanshift))
;        new_spec[interp_ind] = $
;           ((float(fft(fftspec*phase/scalefac,/inverse)))*abs(scalefac))

     endif

;    ---------------------------------------------------------------

;    Save the result in an output array.

     if sz[0] eq 3 then begin
        output[xx,yy,*] = new_spec
     endif else if sz[0] eq 2 then begin
        output[ii,*] = new_spec
     endif else begin
        output = new_spec
     endelse
     
  endfor

; Return

  return, output
  
end
