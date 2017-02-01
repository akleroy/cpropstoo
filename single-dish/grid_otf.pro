pro grid_otf $
   , data = data_in $
   , ra = ra $
   , dec = dec $
   , weight = weight_in $
   , target_hdr = target_hdr $
   , out_root = out_root $
   , out_cube = out_cube $
   , show=show $
   , beam_fwhm = beam_fwhm $
   , gauss_kern = gauss_kern $
   , gauss_fwhm = gauss_fwhm $
   , apodize = apodize $
   , apod_fwhm = apod_fwhm $
   , nan=nan $
   , scale=scale $
   , pad=pad $
   , noclip=noclip $
   , clipval=clipval

;+
; NAME:
;
; grid_otf
;
; PURPOSE:
;
; Grid individual spectra onto a specified regular grid following the
; recommendations of Mangum et al. (2007). This is written to be of general
; use gridding OTF data (i.e., not telescope specific). To apply this to a
; .fits binary table from IRAM use the IRAM_TABLE_TO_CUBE wrapper.
;
; CATEGORY:
;
; Reduction tool.
;
; CALLING SEQUENCE:
;
; grid_otf, data = data $
;         , ra = ra, dec = dec, weight = weight $
;         , target_hdr = target_hdr $
;         , out_root = 'output' $
;         , beam_fwhm = beam_fwhm $
;         , /gauss_kern $
;         , gauss_fwhm = gauss_fwhm $
;         , /median $         
;         , /show
;
; INPUTS:
;
; The spectra to be gridded and a header defining the target grid. This is
; provided as vectors:
;
; data - an nspec x nchan size two-dimensional array containing the spectra to
;        be gridded.
; ra   - an nspec long array of right ascencions in degrees
; dec  - an nspec long array of declinations in degrees
;
; also needs a target astrometry:
;
; target_hdr - a .fits header (string array) containing the target astromtery
;
; OPTIONAL INPUTS:
;
; weight - an nspec long vector containing weights to be applied to the
;          individual spectra. If not supplied, the porgram assumes equal
;          weights for all data.
;
; beam_fwhm - the fwhm in decimal degrees of the telescope beam. If not
;             supplied, then the program looks for this in the
;             target_header. If it isn't in either place, the program crashes. 
;
; gauss_fwhm - the fwhm in decimal degrees of the gaussian beam to use as a
;              convolution kernel. Requires /GAUSS_KERN to be set.
;
; apod_fwhm - the fwhm in decimal degrees of the gaussian apodization kernel
;             used to fill in missing data and smooth edges.
;
;
; KEYWORD PARAMETERS:
;
; gauss_kern - use a Gaussian convolution kernel. Defaults to FWHM = 1/3
;              BEAM_FWHM, the default convolution kernel used by CLASS.
;
; apodize - as a final step, replace missing data that are near valid
;           measurements with values obtained by smoothing the cube. Default
;           FWHM is 30" but this can be adjusted with the APOD_FWHM parameter.
;
; [ WARNING! Apodization still a work in progress]
;
; OUTPUTS:
;
; out_root+".fits" - a .fits file containing the synthesized cube
;
; out_root+".coverage.fits" - a .fits file containing the coverage of the data
;                             at each pixel
;
; OPTIONAL OUTPUTS:
;
; none
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
; Takes the irregularly spaced data and moves them onto the regular .fits grid
; defined by the target header. The procedure involves a convolution. The
; default kernel is a Bessel function tapered by a Gaussian the beam
; width. This is a compromise between practicality and correctness: it
; preserves most spatial scales while being computable in a reasonable amount
; of time. Alternatively, the user can ask the program to use a Gaussian and
; specify a size (default FWHM 1/3 the beam size).
;
; Credit to CLASS's xy_map, E. Rosolowsky, Mangum+ 07.
;
; MODIFICATION HISTORY:
;
; 17dec08 - written following methodology from E. Rosolowsky & Mangum+ 07
;           leroy@mpia.de
;
; 13apr09 - tried my best to speed things up - leroy@mpia.de
;
; 03jun09 - updated documentation and adjusted the final beam size
;           calculations - leroy@mpia.de
;
; 20jul09 - polished, added apodization, updated documentation.
;
; spring14 - some cleanup on pixel size calculation
;
; IF YOU USE THIS AND FIND ERRORS:
;
; email to leroy.42@osu.edu (no promises, but I want to know)
; 
; TO DO:
;
; Apodization - implement creation of a smoother version of the cube
;               to round out poorly covered edges or missing patches.
;
; Mangum kernel - currently the generation of Mangum kernel has a bug
;                 near the center. Fix this.
;
;-

;  on_error, 2

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS, ERROR-CHECKING, AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
  message, 'Error checking.', /info

; Check that we have out basic input 
  if ((n_elements(data_in) eq 0) or $
      (n_elements(ra) eq 0) or $
      (n_elements(dec) eq 0)) then $
         message, 'Requires data, ra, and dec vectors.'

; Copy the input data
  data = data_in

; Require a target header
  if (n_elements(target_hdr) eq 0) then $
     message, 'Requires a target header (TARGET_HDR = ).'
  
; Require a beam size either from the header or a keyword
  if (n_elements(beam_fwhm) eq 0) then begin
     beam_fwhm = sxpar(target_hdr,'BMAJ',count=ct)     
     if ct eq 0 then $
        message, 'Requires the beam size via keyword or header.'     
  endif

; Generate astrometry from the target header for future use
  make_axes, target_hdr, ri = ri, di = di, vaxis=vaxis, astrom=astr

; Note the pixel scale
  pix_scl = sphdist(ri[1,0], di[1,0], ri[0,0], di[0,0], /deg)  

; Send a warning if the cell size is not less than a third of a beam
  if abs(pix_scl) gt beam_fwhm/3.0 then $
     message, 'Warning! Cell size seems to be greater than 1/3 beam.', /info

; Deal with the case of a two-d data set (not a cube)
;  nspec = (size(data))[1]
;  if (size(data))[0] eq 1 then begin
;     data = 
;  endif

; Check that basic dimensionality matches
  nspec = (size(data))[1]
  nchan = (size(data))[2]
  if (n_elements(vaxis) ne nchan) then begin
     message, 'Velocity info in header and spectra size do not match.', /info
     stop
     return
  endif

; Set the size of the apodization kernel
  if n_elements(apod_fwhm) eq 0 then begin
     apod_fwhm = 30./3600.     
  endif  

; Set options related to the FFT
  if keyword_set(pad) then begin
     no_pad = 0B
  endif else begin
     no_pad = 1B
  endelse

; Set the factor by which the pixel grid will be dilated for gridding
; purposes. Spectrum locations are quantized at this gradiation for
; gridding purposes, so it's ideal to have the pixel scale
; divided by the scale factor less than the pointing uncertainty.

  if n_elements(scale) eq 0 then $
     scale = 3

  if n_elements(clipval) eq 0 then $
     clipval = 0.25

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEAL WITH ASTROMETRIC PRE-CALCULATIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  message, 'Astrometric pre-calculations.', /info

  sz = size(ri,/dim)

; FIGURE THE SIZES OF THE OUTPUT
  nx = sz[0]
  ny = sz[1]

; CREATE A BIGGER IMAGE FOR USE IN CALCULATIONS
  dummy_image = fltarr(nx,ny)
  nx_big = scale*nx
  ny_big = scale*ny
  hrebin, dummy_image, twod_head(target_hdr), big_dummy, big_hdr, nx_big, ny_big

; THE SIZE OF A PIXEL IN THE BIG IMAGE
  xyad, big_hdr, [0,1], [0,0], dummy_ra, dummy_dec
  pix_scl_big = sphdist(dummy_ra[0], dummy_dec[0], dummy_ra[1], dummy_dec[1], /deg)
  if abs((pix_scl / pix_scl_big) - scale) gt 1e-3 then begin
     message, "Weird.", /info
     stop
  endif
  
; CALCULATE THE X AND Y COORDINATE FOR EACH SPECTRUM IN THE BIG IMAGE
  adxy, big_hdr, ra, dec, x_pix_big, y_pix_big
  x_pix_big = round(x_pix_big)
  y_pix_big = round(y_pix_big)
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFINE THE CONVOLUTION FUNCTION
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  message, 'Building convolution function.', /info

  if keyword_set(gauss_kern) then begin
     if n_elements(gauss_fwhm) eq 0 then $
        gauss_fwhm = beam_fwhm / 3.0
     r_support = 3.0 * gauss_fwhm
     max_conv_fn = 0.5
     cutoff_conv_fn = 0.25 * max_conv_fn
     scale_fwhm = sqrt(beam_fwhm^2 + gauss_fwhm^2)/beam_fwhm
  endif else begin
;    MANGUM, EMERSON, AND GREISEN (2007) ... MODULO TYPOS
     a = 1.55 * (beam_fwhm) / 3.0
     b = 2.52 * (beam_fwhm) / 3.0
     r_support = beam_fwhm
     max_conv_fn = 0.5
     cutoff_conv_fn = 0.25 * max_conv_fn
     scale_fwhm = 1.09          ; APPROXIMATE
  endelse

; BUILD THE CONVOLUTION KERNEL AS AN ARRAY USING THE PIXEL SIZE FOR
; THE EXPANDED IMAGE.

  r_pix = ceil(r_support / pix_scl_big)
  nx_kern = 2*(r_pix)+1
  ny_kern = nx_kern
  conv_kern = fltarr(nx_kern, ny_kern)
  ximg_kern = findgen(nx_kern) # (fltarr(ny_kern)+1.)
  ximg_kern -= r_pix
  yimg_kern = (fltarr(nx_kern)+1.) # findgen(ny_kern)
  yimg_kern -= r_pix
  rimg_kern = sqrt(ximg_kern^2 + yimg_kern^2)
  dist_kern = rimg_kern*pix_scl_big

  if keyword_set(gauss_kern) then begin
     conv_kern = max_conv_fn * $
                 exp(-0.5 * (dist_kern/(gauss_fwhm/2.354))^2)
  endif else begin
     conv_kern = $
        beselj(!pi*dist_kern/a, 1) / $
        (!pi * dist_kern/a) * exp(-1.0 * (dist_kern/b)^2)     
  endelse  
  conv_kern[where(dist_kern gt r_support)] = 0.0
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; INITIALIZE WEIGHTING
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  message, 'Sanitizing weighting.', /info

; IF THERE IS NO WEIGHTING ARRAY, MAKE A DEFAULT (ALL 1s) VERSION
  if n_elements(weight_in) eq 0 then begin
     weight = fltarr(nspec) + 1.0
  endif else begin
     if (n_elements(weight_in) ne long(nspec)) and $
        (n_elements(weight_in) ne long(nspec)*long(nchan)) $
     then begin
        message, 'Weights in do not match number of spectra or number of spectra*channels.', /info
     endif
     weight = weight_in
  endelse

; IF WEIGHTS ARE FED IN AS A ONE-D ARRAY MAKE THEM TWO-D
  if (size(weight))[0] eq 1 then begin
     weight_in = weight
     weight = fltarr(nspec, nchan)
     for j = 0, nchan-1 do weight[*, j] = weight_in
  endif

; SET THE WEIGHTS OF NOT-A-NUMBER DATA TO ZERO
  if keyword_set(nan) then begin
     nan_ind = where(finite(data) eq 0, nan_ct)
     if nan_ct gt 0 then $
        data[nan_ind] = 0.0
        weight[nan_ind] = 0.0
  endif else begin
     if total(finite(data) eq 0) gt 0 then begin
        message, "Not-a-number present but /nan not set.", /info
        return
     endif
  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEAL WITH ANY DOUBLE-VALUED LOCATIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Our method places spectra into planes based on indexing, so we need
; to make the mapping of spectrum -> pixel single-valued at this point
; for fast computation later. We use histogram to find all
; double-values two-d indices and pile their values into the first
; spectrum.

  message, 'Searching for duplicate spectra locations.', /info
  
  twod_ind = long(x_pix_big) + long(y_pix_big)*long(nx_big)
  ind_hist = histogram(twod_ind, reverse=ri)
  mult_ind = where(ind_hist ge 2, multi_ct)

  if multi_ct gt 0 then begin

     message, "Found some multi-valued pixels. Consolidating.", /info

     for ii = 0L, multi_ct-1 do begin
        counter, ii, multi_ct, " Duplicate "

        first = ri[mult_ind[ii]]
        last = ri[mult_ind[ii]+1]-1
        ind = ri[first:last]

        this_data = data[ind,*]
        this_wt = weight[ind,*]

        data[ri[first],*] = total(this_data*this_wt,1,/nan)/total(this_wt,1,/nan)
        
        weight[ind,*] = 0.0
        weight[ri[first],*] = total(this_wt,1)

     endfor
     
  endif
    
; Remove double values
  ind = where(total(weight,2) gt 0.0)

  data = data[ind,*]
  weight = weight[ind,*]
  x_pix_big = x_pix_big[ind]
  y_pix_big = y_pix_big[ind]

  twod_ind = long(x_pix_big) + long(y_pix_big)*long(nx_big)
  ind_hist = histogram(twod_ind, reverse=ri)
  if total(ind_hist gt 1) ne 0 then begin
     message, "Duplicates persist. Stopping for diagnosis.", /info
     stop
  endif
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; LOOP OVER EACH SPECTRUM
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  message, 'Building cube.', /info

; MAKE TWO EMPTY CUBES: (1) DATA, (2) COVERAGE

  data_cube = fltarr(nx, ny, nchan)
  weight_cube = fltarr(nx,ny,nchan)
    
  for cc = 0L, nchan-1 do begin

     counter, cc, nchan, " Plane "
     
     plane_data = fltarr(nx_big, ny_big)
     plane_weight = fltarr(nx_big, ny_big)

     plane_data[x_pix_big, y_pix_big] = data[*,cc]*weight[*,cc]
     plane_weight[x_pix_big, y_pix_big] = weight[*,cc]

     data_conv = convolve(plane_data, conv_kern, ft_psf = conv_kern_ft, no_pad=no_pad)
     weight_conv = convolve(plane_weight, conv_kern, ft_psf = conv_kern_ft, no_pad=no_pad)

     hastrom, data_conv, big_hdr, data_plane, dummy_hdr, twod_head(target_hdr), interp=2
     hastrom, weight_conv, big_hdr, weight_plane, dummy_hdr, twod_head(target_hdr), interp=2

     data_cube[*,*,cc] = data_plane
     weight_cube[*,*,cc] = weight_plane

;     data_cube[*,*,cc] = rebin(data_conv,nx,ny)
;     weight_cube[*,*,cc] = rebin(weight_conv,nx,ny)

;     hastrom, data_conv, big_hdr, 
;     hrebin, dummy_image, twod_head(target_hdr), big_dummy, big_hdr, nx_big, ny_big

     if keyword_set(show) then begin
        disp, data_cube[*,*,cc], /sq
     endif

  endfor

; Divide out the weights  
  data_cube = data_cube/weight_cube
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CLIP THE CUBE AT SOME COVERAGE LEVEL (OPTIONAL)
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
; For a final cube, it will often be desirable to clip the map at some
; sensitivity threshold. You don't want to do this at this
; stage if you intend to stash the cube and weights to add more data
; later. But if you want a final science map, the program will default
; to clipping all data below 0.25 times the maximum sensitivity
; (defined as coverage times weight).

  if keyword_set(noclip) eq 0 then begin
     
     message, "Clipping data below specified threshold.", /info

     clip_ind = where(weight_cube lt clipval*max(weight_cube,/nan), clip_ct)
     if clip_ct gt 0 then begin
        data_cube[clip_ind] = !values.f_nan
     endif

  endif
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; APODIZATION
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; UPDATE HEADER AND WRITE THE DATA TO DISK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  message, 'Updating header and writing to disk.', /info

; INCREASE THE BEAM SIZE TO REFLECT THE CONVOLUTION
  beam_fwhm = scale_fwhm*beam_fwhm

; GENERATE THE OUTPUT HEADER
  hdr_out = target_hdr
  sxaddpar, hdr_out, 'HISTORY', 'GRID_OTF (IDL): converted spectra to cube.'
  if keyword_set(gauss_kern) then $
     sxaddpar, hdr_out, 'HISTORY', 'Convolved with Gaussian convolution function.' $
  else $
     sxaddpar, hdr_out, 'HISTORY', 'Convolved with optimized kernel from Mangum+07.'

  if keyword_set(apodize) then $
     sxaddpar, hdr_out, 'HISTORY', 'Missing data and edges apodized.'

  if keyword_set(gauss_kern) then begin
     sxaddpar, hdr_out, 'BMAJ', beam_fwhm
     sxaddpar, hdr_out, 'BMIN', beam_fwhm     
  endif else begin
     sxaddpar, hdr_out, 'BMAJ', beam_fwhm, '*But* not Gaussian.'
     sxaddpar, hdr_out, 'BMIN', beam_fwhm, '*But* not Gaussian.'
  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; WRITE TO DISK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if (n_elements(out_root) gt 0) then begin
;     message, 'Requires an output file root  (OUT_ROOT = ).'
   
;    WRITE THE DATA TO DISK
     cube_file = out_root + '.fits'
     writefits, cube_file, data_cube, hdr_out
     
;    WRITE THE COVERAGE TO DISK
     coverage_file = out_root + '.coverage.fits'
     sxaddpar, hdr_out, 'BUNIT', 'COVERAGE' ; CHANGE FROM K -> COVERAGE
     writefits, coverage_file, weight_cube, hdr_out

  endif else begin
     out_cube = data_cube
  endelse
  
end                             ; of grid_otf
