pro make_noise_cube $
   , cube_file = cube_file $
   , cube_in = cube $
   , cube_hdr = cube_hdr $
   , out_cube = noise_cube $
   , out_file = noise_file $
   , mask_file = mask_file $
   , mask_in = mask_in $
   , box = box $
   , spec_box = spec_box $
   , poly = poly $
   , iterate = iterate $
   , zero_only = zero_only $
   , oned_only = oned_only $
   , twod_only = twod_only $
   , fast_twod = fast_twod $
   , roll_fast_twod = roll_fast_twod $
   , collapse = collapse $
   , show = show
  

; --- CHECK ON NORMALIZATION OF SPECTRAL AXIS
; --- CHECK ON HANDLING OF EDGES IN CASE OF "BOX"
; --- ADD LABELS TO PLOTS!!!
; --- NORMALIZE INFILE, INDATA TO MATCH OTHER PROGRAMS
; --- ALLOW THE USER TO SPECIFY A Z RANGE?

;+
; NAME:
;
; make_noise_cube
;
; PURPOSE:
;
; Bootstrap the noise from empty regions of a data cube and produce a
; matched-astrometry "noise cube" containing the pixelwise noise
; estimates.
;
; The calculation assumes that the spectral and spatial dimensions are
; separable and can operate in 0, 1, 2, or 3 dimensions. By default,
; the program operates pixel-by-pixel and plane-by-plane but keywords
; allow it to aggregate data over a larger area to produce a smoother
; estimate. 
;
; Optionally, the program will perform iterative outlier rejection and
; accept a mask that defines regions to avoid.
;
; CATEGORY:
;
; Analysis tool
;
; CALLING SEQUENCE:
;
;  make_noise_cube $
;   , cube_file = cube_file $
;   , cube_in = cube $
;   , cube_hdr = cube_hdr $
;   , out_cube = noise_cube $
;   , out_file = noise_file $
;   , mask_file = mask_file $
;   , mask_in = mask_in $
;   , box = box $
;   , spec_box = spec_box $
;   , poly = poly $
;   , iterate = iterate $
;   , zero_only = zero_only $
;   , oned_only = oned_only $
;   , twod_only = twod_only $
;   , collapse = collapse $
;   , show = show
; 
;
; INPUTS:
;
; CUBE_FILE -or- CUBE_IN+CUBE_HDR : the data to bootstrap
;
;
; OPTIONAL INPUTS:
;
; MASK_FILE -or- MASK_IN : regions to avoid during calculation
; BOX                    : edge size of region to spatially average
; SPEC_BOX               : edge size of region to spectrally average
;
;
; KEYWORD PARAMETERS:
;
; ITERATE                : perform iterative outlier rejection
; ZERO_ONLY              : perform only a cube-average calculation
; ONED_ONLY              : perform only a spectral calculation
; TWOD_ONLY              : perform only a map-wise calculation
; FAST_TWOD              : perform only a map-wise calculation using
; differences along the spectral axis as a measure of the noise.
; ROLL_FAST_TWOD         : The fast 2D algorithm uses the differences
; bewteen spectral channels.  This is the distance between channels to
; consider.  It should be larger than the correlation of the
; spectrometer but as small as possible.  Default = 2.
; COLLAPSE               : collapse the output (only the variables) if 0d, 1d, or 2d set
; SHOW                   : visualize the results
;
; POLY                   : carry out a polynomial fit to the RMS vs channel
;
; OUTPUTS:
;
; OUT_CUBE : the output noise cube
; OUT_FILE : the noise cube written to a .FITS file
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
; Nov 12 - adapted from IRAM pipeline.
; Dec 12 - documented
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN THE DATA
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; READ THE CUBE FROM DISK
  if (n_elements(cube) eq 0 and $
      n_elements(cube_file) gt 0) then $
         cube = readfits(cube_file, cube_hdr)

; ERROR CATCH LACK OF CUBE
  if n_elements(cube) eq 0 then begin
     message, "Cube missing. Returning.", /info
     return
  endif

; ERROR CATCH LACK OF HEADER
  if n_elements(cube_hdr) eq 0 then begin
     message, "Header missing. Output files will be compromised.", /info
  endif else begin
     noise_hdr = cube_hdr
     sxdelpar, noise_hdr, "HISTORY"
     sxaddpar, noise_hdr $
               , "HISTORY" $
               , "Now holds estimates of local RMS noise."
  endelse

; INITIALIZE PLOTTING
  if keyword_set(show) then begin
     loadct, 0
     reversect
     !p.multi=[0,2,2]
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; NOTE DIMENSIONS
  sz = size(cube)
  chan = findgen(sz[3])

; A MASK THAT IDENTIFIED THE REGIONS TO CONSIDER AS NOISE

; GET THE MASK, 1=FIT, 0=IGNORE
  if n_elements(mask_in) eq n_elements(cube) then begin
;    "INVERT" THE USER SUPPLIED MASK
     mask = (mask_in eq 0) and finite(cube)
  endif else begin
     if n_elements(mask_file) eq 0 then $
        mask = finite(cube) $
     else begin
;       READ IN THE MASK
        mask = readfits(mask_file, mask_hdr) 
        
;       "INVERT" THE MASK TO GET WHERE ITS OKAY TO FIT
        mask = (mask eq 0) and finite(cube)
     endelse
  endelse

; DEFAULT TO NO SLIDING BOX
  if n_elements(box) eq 0 then $
     box = 0

  if n_elements(spec_box) eq 0 then $
     spec_box = 0

; NOTE REGIONS WHERE THE CUBE IS EMPTY
  nan_cube = finite(cube) eq 0
  nan_ind = where(nan_cube, nan_ct)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; MEASURE A SINGLE SIGMA FOR THE WHOLE CUBE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(zero_only) then begin
     
;    ID DATA TO USE
     noise_ind = where(mask, noise_ct)
     data = cube[noise_ind]
     sigma = mad(data)
     
     if keyword_set(iterate) then begin
        
;       FIRST ESTIMATE (FROM ONLY THE NEGATIVES)
        neg_ind = where(data lt 0, neg_ct)
        if (neg_ct gt 25) then begin
           sigma = mad([data[neg_ind], -1.*data[neg_ind]])

;          IDENTIFY A REASONABLE OUTLIER CRITERIA
;          (HONESTLY 3 SIGMA REJECTION WOULD PROBABLY BE FINE)
           sig_false = $
              bisection(3., 'erf0', erftarg = (1d0-(5d-1)/sz[3]), /double)           
           
;          FINAL ESTIMATE (ALL DATA WITH OUTLIER REJECTION)
           use_ind = where(abs(data) lt sig_false*sigma)
;        noise = mad(data[use_ind]) ; wrong? cf. line 226
           sigma = mad(data[use_ind]) ; want to put result into sigma? cf line 226

        endif
     endif

;    EXPLODE INTO THE MOST BORING CUBE EVER
     noise_cube = finite(cube)*0.0 + sigma
     if nan_ct gt 0 then $
        noise_cube[nan_ind] = !values.f_nan

;    SHOW IF REQUESTED
     if keyword_set(show) then begin
        fasthist, cube/noise_cube, /ylog
        al_legend, /top, /left, box=0, clear=0 $
                   , lines=[-99] $
                   , str(mad(cube/noise_cube),format='(G10.3)')
        !p.multi=0
     endif

;    WRITE TO DISK IF REQUESTED     
     if n_elements(noise_file) gt 0 then begin
        writefits, noise_file, noise_cube, noise_hdr
     endif

;    COLLAPSE TO A SINGLE VALUE
     if keyword_set(collapse) then begin
        noise_cube = median(noise_cube,/even)
     endif

     return

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; MEASURE POISITIONALLY DEPENDENT NOISE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; SKIP THIS STEP IF DOING ONE-D ONLY
  if keyword_set(oned_only) eq 0 then begin

;    INITIALIZE OUTPUT
     noise_map = fltarr(sz[1],sz[2])*!values.f_nan

;    IDENTIFY A REASONABLE OUTLIER CRITERIA
;    (HONESTLY 3 SIGMA REJECTION WOULD PROBABLY BE FINE)
     sig_false = $
        bisection(3., 'erf0', erftarg = (1d0-(5d-1)/sz[3]), /double)
     
     ct = 0L

;    WORK OUT STEP SIZES
     if box eq 0 then begin
        step = 1
     endif else begin
        step = floor(box / 2.5) > 1
     endelse
     xsteps = ceil(sz[1]/step/1.0)
     ysteps = ceil(sz[2]/step/1.0)

     outof = long(xsteps*ysteps)

     if keyword_set(fast_twod) then begin
        message,'This is an experimental, fast 2D approach.'+$
                ' I too like to live dangerously.', /con
        if n_elements(roll_fast_twod) ne 1 then roll_fast_twod = 2 
        noise_map = mad(cube - shift(cube, 0, 0, roll_fast_twod), $
                        dimension=3) / sqrt(2)
     endif else begin
        
;    LOOP OVER PIXELS AND WORK OUT NOISE FOR EACH SPECTRUM
        for i = 0, xsteps-1 do begin
           for j = 0, ysteps-1 do begin

              ct += 1
              counter, ct, outof, "Line of sight "

              xctr = i*step
              yctr = j*step

              if box eq 0 then begin
                 noise_ind = where(mask[i,j,*], noise_ct)
              endif else begin
                 lox = ((xctr - box) > 0) < (sz[1]-box-1)
                 hix = ((xctr + box) > box) < (sz[1]-1)
                 loy = ((yctr - box) > 0) < (sz[2]-box-1)
                 hiy = ((yctr + box) > box) < (sz[2]-1)
                 noise_ind = where(mask[lox:hix,loy:hiy,*], noise_ct)
              endelse

              if noise_ct lt 30 then $
                 continue

              if box eq 0 then begin
                 data = (cube[xctr,yctr,*])[noise_ind]
              endif else begin
                 data = (cube[lox:hix,loy:hiy,*])[noise_ind]
              endelse

;          DEFAULT ESTIMATE IS M.A.D. BASED RMS OF THE DATA
              noise = mad(data)
              
              if keyword_set(iterate) then begin
                 
;             FIRST ESTIMATE (FROM ONLY THE NEGATIVES)
                 neg_ind = where(data lt 0, neg_ct)
                 if (neg_ct lt 25) then continue
                 sigma = mad([data[neg_ind], -1.*data[neg_ind]])

;             IDENTIFY A REASONABLE OUTLIER CRITERIA
;             (HONESTLY 3 SIGMA REJECTION WOULD PROBABLY BE FINE)
                 sig_false = $
                    bisection(3., 'erf0', erftarg = (1d0-(5d-1)/sz[3]), /double)           

;             FINAL ESTIMATE (ALL DATA WITH OUTLIER REJECTION)
                 use_ind = where(abs(data) lt sig_false*sigma)           
                 noise = mad(data[use_ind])
              endif

              if box eq 0 then begin
                 noise_map[i,j] = noise
              endif else begin
                 lofill_x = (xctr-ceil(step/2)) > 0 
                 hifill_x = (xctr+ceil(step/2)) < (sz[1]-1)
                 lofill_y = (yctr-ceil(step/2)) > 0 
                 hifill_y = (yctr+ceil(step/2)) < (sz[2]-1)
                 noise_map[lofill_x:hifill_x,lofill_y:hifill_y] = noise
              endelse

           endfor
        endfor
     endelse
     if keyword_set(show) then begin
        disp, noise_map, /sq, title="Noise Map"
        contour, noise_map, lev=median(noise_map)*[0.5,1.0,2.0], /overplot
     endif

;    INITIALIZE THE NOISE CUBE
     noise_cube = finite(cube)*!values.f_nan
     for i = 0, sz[3]-1 do $
        noise_cube[*,*,i] = noise_map

;    EXIT IF ONLY TWO DIMENSIONS ARE REQUESTED
     if keyword_set(twod_only) then begin
        if keyword_set(show) then begin
           fasthist, cube/noise_cube, /ylog
           al_legend, /top, /left, box=0, clear=0 $
                      , lines=[-99] $
                      , str(mad(cube/noise_cube),format='(G10.3)')
        endif

        if n_elements(noise_file) gt 0 then begin           
           writefits, noise_file, noise_cube, noise_hdr
        endif

;       COLLAPSE TO TWO DIMENSIONS
        if keyword_set(collapse) then begin
           noise_cube = median(noise_cube, dimension=3, /even)
        endif

        return
     endif

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; MEASURE NOISE SPECTRUM - NORMALIZED IF IN THREE-D CASE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; INITIALIZE OUTPUT
  noise_spec = fltarr(sz[3])*!values.f_nan

; WORK OUT STEP SIZES
  if SPEC_box eq 0 then begin
     step = 1
  endif else begin
     step = floor(box / 2.5) > 1
  endelse
  zsteps = ceil(sz[3]/step)

; LOOP OVER PLANES
  for i = 0, zsteps-1 do begin

     counter, i, zsteps, "Plane "

     zctr = i*step

     if spec_box eq 0 then begin
        noise_ind = where(mask[*,*,i], noise_ct) 
     endif else begin
        loz = ((zctr - spec_box) > 0) < (sz[3]-spec_box-1)
        hiz = ((zctr + spec_box) > (spec_box)) < (sz[3]-1)
        noise_ind = where(mask[*,*,loz:hiz], noise_ct)
     endelse

     if noise_ct lt 50 then $
        continue
     
     if spec_box eq 0 then begin
        if keyword_set(oned_only) then begin
           data = (cube[*,*,i])[noise_ind]
        endif else begin
           data = (cube[*,*,i]/noise_cube[*,*,i])[noise_ind]
        endelse
     endif else begin
        if keyword_set(oned_only) then begin
           data = (cube[*,*,loz:hiz])[noise_ind]
        endif else begin
           data = (cube[*,*,loz:hiz]/noise_cube[*,*,loz:hiz])[noise_ind]
        endelse
     endelse

;    DEFAULT ESTIMATE IS M.A.D. BASED RMS OF THE DATA
     noise = mad(data)

     if keyword_set(iterate) then begin
;    IDENTIFY A REASONABLE OUTLIER CRITERIA
;    (HONESTLY 3 SIGMA REJECTION WOULD PROBABLY BE FINE)
        sig_false = bisection(3., 'erf0' $
                              , erftarg = (1d0-(5d-1)/noise_ct), /double)
        
;    FIRST ESTIMATE (FROM ONLY THE NEGATIVES)
        neg_ind = where(data lt 0, neg_ct)
        if (neg_ct lt 25) then continue
        sigma = mad([data[neg_ind], -1.*data[neg_ind]])

;    FINAL ESTIMATE (ALL DATA WITH OUTLIER REJECTION)
        use_ind = where(abs(data) lt sig_false*sigma)
        noise = mad(data[use_ind])
     endif

     if spec_box eq 0 then begin
        noise_spec[i] = noise
     endif else begin
        lofill_z = (zctr-ceil(step/2)) > 0 
        hifill_z = (zctr+ceil(step/2)) < (sz[1]-1)
        noise_spec[lofill_z:hifill_z] = noise
     endelse

  endfor

  if keyword_set(show) then begin
     circ = FINDGEN(17) * (!PI*2/16.)
     USERSYM, COS(circ), SIN(circ), /FILL
     plot, chan, noise_spec, ps=8 $
           , xtitle="Channel", ytitle="Noise Spectrum"
  endif

; IF DESIRED, EXIT WITH ONLY THE ONE-D (SPECTRAL) NOISE MEASURED
  if keyword_set(oned_only) then begin
     noise_cube = finite(cube)*!values.f_nan
     for i = 0, sz[3]-1 do $
        noise_cube[*,*,i] = noise_spec[i]

     if keyword_set(show) then begin
        fasthist, cube/noise_cube
        al_legend, /top, /left, box=0, clear=0 $
                   , lines=[-99] $
                   , str(mad(cube/noise_cube),format='(G10.3)')
     endif

     if n_elements(noise_file) gt 0 then begin
        writefits, noise_file, noise_cube, noise_hdr
     endif

;    COLLAPSE TO ONE DIMENSION
     if keyword_set(collapse) then begin
        noise_cube = median(median(noise_cube, dimension=1,/even),dimension=1,/even)
     endif

     return
  endif
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; FIT A POLYNOMIAL TO THE RMS VS. CHANNEL
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  fit_ind = where(finite(noise_spec))
  if keyword_set(poly) then begin
     coeffs = robust_poly_fit(chan[fit_ind] $
                              , noise_spec[fit_ind], 2, yfit)
  endif  

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; BUILD A NOISE CUBE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; 
; Combine the polynomial fit and the position-by-position noise
; estimates to make a noise cube. At the same time, fill in any empty
; spectra with the appropriate value for that channel.
;

  noise_cube = cube*!values.f_nan
  med_noise_spec = median(noise_spec)

  for i = 0, sz[3]-1 do begin

     if keyword_set(poly) then begin     
        fit_rms = coeffs[0] + (i*1.0)*coeffs[1] + (i*1.0)^2*coeffs[2]
        noise_cube[*,*,i] = fit_rms * noise_map / med_noise
     endif else begin
        noise_cube[*,*,i] = noise_spec[i] / med_noise_spec * noise_map
     endelse

  endfor

  if keyword_set(show) then begin
     fasthist, cube/noise_cube
     al_legend, /top, /left, box=0, clear=0 $
                , lines=[-99] $
                , str(mad(cube/noise_cube),format='(G10.3)')
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; WRITE TO DISK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(noise_file) gt 0 then begin

     writefits, noise_file, noise_cube, noise_hdr

  endif

end                             ; of make_noise_cube
