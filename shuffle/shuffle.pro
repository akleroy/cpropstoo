function shuffle $
   , spec = spec $
   , vaxis = vaxis $
   , zero = zero $
   , new_vaxis = new_vaxis $
   , new_hdr = new_hdr $
   , new_crval = new_crval $
   , new_crpix = new_crpix $
   , new_cdelt = new_cdelt $
   , fft = fft
;+
;
; SHUFFLE 
;
; Uses linear interpolation to rearrange a spectrum or array of
; spectra along the velocity.
;
; INPUT PARAMETERS:
;
; CHECKED:
;
; TO BE DONE:
;
;-



; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; ERROR CHECKING
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

; REQUIRE AN INPUT VELOCITY AXIS
  if n_elements(vaxis) eq 0 then begin
     message, 'Require a velocity axis.', /info
     return, !values.f_nan
  endif

; BUILD THE OUTPUT AXIS IF IT IS NOT SUPPLIED
  if n_elements(new_vaxis) eq 0 then begin
     if n_elements(new_hdr) gt 0 then begin
        message, 'Extracting new velocity axis from header.', /info
        make_axes, new_hdr, vaxis=vaxis, /vonly
     endif else begin
        message, 'Attempting to build a new axis from keywords.', /info
        
        if n_elements(new_cdelt) eq 0 then begin
           message, '... defaulting to original channel width.', /info
           new_cdelt = vaxis[1] - vaxis[0]
        endif

        if n_elements(new_crval) eq 0 or $
           n_elements(new_crpix) eq 0 then begin
           message, '... defaulting to original reference value.', /info
           new_crval = vaxis[0]
           new_crpix = 1
        endif

        if n_elements(new_naxis) eq 0 then begin
           message, '... defaulting to original axis length.', /info
           new_naxis = n_elements(vaxis)
        endif

        new_vaxis = (findgen(new_naxis) - (new_crpix-1.))*new_cdelt + new_crval
        
     endelse
  endif

; ERROR IF THE VELOCITIES ARE NOT MATCHED
  if total(new_vaxis ne vaxis) eq 0 then begin
     message, 'WARNING! Your new and old velocity axes are identical. '+ $
              'Is this really what you want?', /info
  endif

; CHECK SIZE OF ZERO POINT
  sz = size(spec)
  if n_elements(zero) eq 0 then begin
     message, 'Defaulting to zero point of 0 (i.e., regridding mode only).', /info
     zero = 0.0
  endif
  if sz[0] eq 2 then begin
     n_spec = sz[2]     
     if n_elements(zero) ne 1 and $
        n_elements(zero) ne n_spec then begin
        message, 'The zero point vector should have either 1 or n_spec elements. Returning.', /info
        return, !values.f_nan
     endif
  endif else begin
     n_spec = 1
  endelse
  n_chan = n_elements(new_vaxis)

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; LOOP SPECTRUM BY SPECTRUM AND INTERPOLATE TO THE NEW AXIS
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
;
; Now we have the new velocity axis, the local zero point (defined by the
; velocity field), and the old velocity axis (pulled out of the header). The
; next step is to move each spectrum from the old velocity axis to the new
; velocity axis. We do that here by looping over the cube, grabbing each
; spectrum and interpolating it to the new velocity axis. There is an option
; to hanning smooth first, which is useful if you are moving things to a
; velocity spacing that is much coarser than the original cube (in this case
; interpolation will sample the original data, yielding no improvement in
; signal-to-noise; smoothing lets you average first).
;

; INITIALIZE THE OUTPUT
  if n_spec eq 1 then begin
     output = fltarr(n_chan)*!values.f_nan
  endif else begin
     output = fltarr(n_chan, n_spec)*!values.f_nan
  endelse

; AXIS OF CHANNEL NUMBER FOR THE ORIGINAL VELOCITY AXIS
  orig_chan = findgen(n_elements(vaxis))
  orig_n_chan = n_elements(orig_chan)

; LOOP OVER SPECTRA
  for ii = 0, n_spec-1 do begin

;   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;   RECENTER VELOCITY AXIS
;   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;    ... WE CAN'T SHUFFLE WITHOUT A ZERO POINT
     if finite(zero[ii]) eq 0 then continue

;    ... GET THE ORIGINAL SPECTRUM AT THIS LOCATION
     if n_spec eq 1 then begin
        this_spec = spec
     endif else begin
        this_spec = reform(spec[*,ii])
     endelse

;    ... RECENTER THE ORIGINAL VELOCITY AXIS AT THE NEW ZERO POINT
     this_vaxis = vaxis - zero[ii]
     
;       ... IF REQUESTED, FIRST HANNING SMOOTH THE SPECTRUM
;     if n_elements(hans_first) gt 0 then begin
;        this_spec = hans(hans_first, this_spec)
;        this_vaxis = hans(hans_first, this_vaxis)
;     endif

;    ... GET THE VEL. RANGE COVERED BY THE CURRENT SPEC. IN THE NEW SYSTEM
     max_this_vaxis = max(this_vaxis)
     min_this_vaxis = min(this_vaxis)
        
;    THE INTERPOLATION: the way we do this may seem weird at first,
;    but it helps when the ranges of the new and old spectra don't
;    overlap perfectly (which is often the case). What we do is this:
;    first, use linear interpolation to figure out the fractional
;    channel number for each new velocity channel in the old spectrum
;    (e.g., "new channel 45 corresponds to old channel 38.7 for this
;    spectrum"). Then we get only the subset of channels that actually
;    lie inside the old spectrum (so, e.g., if channel 3 in the new
;    axis corresponded to channel -1.1 in the old spectrum then
;    channel 3 would remain empty in the new spectrum). These valid
;    points are filled in with simple linear interpolation.

;   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;   FIND OVERLAP
;   -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;    ... WORK OUT OLD V. AXIS <-> NEW V. AXIS CORRESPONDENCE
     sample_chan = interpol(orig_chan,this_vaxis,new_vaxis)
        
;    ... NOTE WHICH CASES ACTUALLY LIE IN A RANGE WHERE WE HAVE DATA
     interp_ind = where((sample_chan ge 0) and $
                        (sample_chan le (orig_n_chan-1)) $ ; Should this be orig_n_chan not n_chan
                        , interp_ct)
;    ... KEEP LOOPING IF THERE'S NOTHING TO DO
     if interp_ct eq 0 then $
        continue
     
;    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;    INTERPOLATE FOR THE CHANNELS WHERE WE HAVE DATA
;    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;    ... INITIALIZE THE NEW SPECTRUM
;    Why is there an if then else block here?
     new_spec = fltarr(n_chan)*!values.f_nan

;    ... FIGURE OUT THE CHANNEL JUST ABOVE EACH NEW SPEC CHANNEL IN
;    THE OLD SPECTRUM (e.g. 38.25 BECOMES 39)
     chan_hi = ceil(sample_chan[interp_ind])

;    ... FIGURE OUT THE CHANNEL JUST BELOW EACH NEW SPEC CHANNEL IN
;    THE OLD SPECTRUM (e.g. 38.25 BECOMES 38)
     chan_lo = floor(sample_chan[interp_ind])                

;     ... COMBINE WITH APPROPRIATE WEIGHTING:

;    1) FIGURE THE SLOPE OF THE OLD SPECTRUM FROM THE LOW TO HIGH CHANNEL
     slope = ((this_spec[chan_hi] - this_spec[chan_lo]) $
              / (this_vaxis[chan_hi] - this_vaxis[chan_lo]))
     
;    2) GET THE VELOCITY OFFSET BETWEEN THE NEW POINT AND OLD POINT
     voffset = (new_vaxis[interp_ind] - this_vaxis[chan_lo])
     
;    3) NOTE THE ORIGINAL SPECTRUM VALUE AT THE AT LOW CHANNEL
     orig = this_spec[chan_lo]
     if keyword_set(fft) then begin 
        results = poly_fit((findgen(orig_n_chan))[interp_ind],sample_chan[interp_ind],1,chisq=chisq)
        if chisq gt 1e-2 then begin
           message,'Requested velocity axis is non-linear.  FFT method cannot be used.',/con
           new_spec[interp_ind] = !value.f_nan
           continue
        endif
        meanshift = -results[0]/results[1]
        scalefac = 1/results[1]
        fftspec = fft(orig)
        frequency = shift(findgen(n_elements(fftspec))-n_elements(fftspec)/2.0,n_elements(fftspec)/2.0);/n_elements(fftspec) 
; Phase sampling needs to be shifted to the new frequencies
        phase = exp(2*!pi*frequency*complex(0,1)*(meanshift))
        new_spec[interp_ind] = ((float(fft(fftspec*phase/scalefac,/inverse)))*abs(scalefac))
     endif else begin
;    ... DO THE INTERPOLATION
        new_spec[interp_ind] = orig + slope * voffset
;    CATCH THE CASE WHERE WE ARE RIGHT ON A SAMPLING POINT
        eq_ind = where(chan_hi eq chan_lo, eq_ct)
        if eq_ct gt 0 then $
           new_spec[interp_ind[eq_ind]] = this_spec[chan_lo[eq_ind]]
     endelse
;    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=     
;    SAVE THE RESULT IN THE OUTPUT ARRAY
;    -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

     if n_spec eq 1 then begin
        output = new_spec
     endif else begin
        output[*,ii] = new_spec
     endelse
     
  endfor

  return, output
  
end
