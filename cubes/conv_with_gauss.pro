pro conv_with_gauss $
   , data=in_data $
   , hdr=in_hdr $
   , start_beam=start_beam $
   , pix_deg=pix_deg $
   , target_beam=target_beam $
   , out_file=out_file $
   , out_data=data $
   , out_hdr = hdr $
   , no_ft=no_ft $
   , weight=in_weight $
   , out_weight_data=weight $
   , out_weight_file=out_weight_file $
   , uncertainty = unc $
   , perbeam = perbeam $
   , quiet=quiet $
   , no_pad = no_pad

;+
; NAME:
;
; conv_with_gauss
;
; PURPOSE:
;
; Wrapper to "convolve" (FFT) or "convol" (direct kernel
; multiplication) that will generate an elliptical gaussian PSF and
; then convolve it with the target image. Works on cubes and has some
; ability to handle not-a-number values.
;
; CATEGORY:
;
; Image manipulation.
;
; CALLING SEQUENCE:
;
;pro conv_with_gauss $
;   , data=in_data $
;   , hdr=in_hdr $
;   , start_beam=start_beam $
;   , pix_deg=pix_deg $
;   , target_beam=target_beam $
;   , out_file=out_file $
;   , out_data = data $
;   , out_hdr = hdr $
;   , no_ft=no_ft $
;   , uncertainty = unc $
;   , perbeam = perbeam $
;   , quiet=quiet $
;  , pad = pad
;
; INPUTS:
;
; data: accepts either a cube or a FITS file name.
;
; hdr: an input header (not needed if a FITS file is supplied or if
; both a starting beam size and a pixel scale are supplied).
;
; target_beam: the target PSF measured in ARCSECONDS as a three
; element array in the form [major, minor, position angle] with
; position angle assumed to measured north through east and east
; assumed to be low x values (standard astronomical conventions).
;
; OPTIONAL INPUTS:
;
; start_beam: the current FWHM of the image PSF, if this is not
; supplied then conv_with_gauss tries to look for the BMAJ and BMIN
; keywords in the header. Set this to 0 if you want to force
; convolution with the target_beam size.
;
; out_file: name of the output file (FITS). Only works if a header is
; also supplied, in which case the header is updated to reflect the
; new beam size.
;
; pix_deg: the pixel scale of the data in degrees. Useful in order to
; avoid having to pass a header (and so allows the program to be used
; as a general convolution routine).
;
; weight: perform a weighted convolution, multiplying the cube or map
; by the weight map before hand.
;
; unc: tell the program to treat the data as an "uncertainty" map and
; so square the data before convolution then scale the result by the
; sqrt of the increase in beam area. Appropriate for propogating
; uncertainties with convolution (e.g., convolve a noise cube).
;
; perbeam: apply a correction that handles units in the case of "per
; beam" units. This is almost always Jy/beam or MJy/beam. In this
; case, the program scales by the ratio of beam areas at the end to
; account for the new definition of the beam.
;
; KEYWORD PARAMETERS:
;
; quiet: if thrown, suppresses the report on flux conservation and
; kernel calculations.
;                     
; no_ft: passed directly to 'convolve.' Tells the program to use the IDL
; 'convol' function instead of a Fourier transform.
;
; OUTPUTS:
;
; a fits file written to 'out_file' if that parameter is specified and
; a header is supplied
;
; out_data: the convolved data as a variable
;
; out_hdr: the updated header (requires a header to be passed in)
;
; MODIFICATION HISTORY:
;
; documented - leroy@mpia.de 17 nov 08
; modified to do elliptical gaussians - karin 26 jun 11
; AKL - cleaned up and folded in to cprops. May have broken PA
;       convention.
; caught asymmetric kernel bug - dec 14
;
;-

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; READ IN OR COPY DATA
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Attempt to smartly detect whether the file is a string (FITS file
; name) or data. Error catch the need for a header.

  if size(in_data, /type) eq size("hello", /type) then begin
     fname = in_data
     fits_read, fname, data, hdr
  endif else begin
     data = in_data     
     if n_elements(in_hdr) gt 0 then $
        hdr = in_hdr
  endelse

  if n_elements(in_weight) gt 0 then begin
     if size(in_weight, /type) eq size("hello", /type) then begin
        weight_fname = in_weight
        fits_read, weight_fname, weight, weight_hdr
     endif else begin
        weight = in_weight
     endelse

     sz_wt = size(weight)
     sz_data = size(data)

     if (sz_wt[1] ne sz_data[1]) or $
        (sz_wt[2] ne sz_data[2]) then begin
        message, "Weight image must match data in X and Y size. Returning.", /info
        return
     endif
  endif

  if n_elements(target_beam) eq 1 then begin
     target_beam = [target_beam, target_beam, 0.0]     
  endif else if n_elements(target_beam) eq 2 then begin
     target_beam = [target_beam[0], target_beam[1], 0.0]
  endif

  if n_elements(hdr) eq 0 then begin
     if n_elements(pix_deg) eq 0 or n_elements(start_beam) eq 0 then begin
        message, "Requires a header OR a beam (start_beam=) and a plate scale (pix_deg=). Returning.", /info
        return
     endif
  endif

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; PROCESS DATA BEFORE CONVOLUTION
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Save total flux.
  flux_before = total(data,/nan)

; NaNs cause problems for the FFT - set them to zero
  nan_ind = where(finite(data) eq 0, nanct)
  if (nanct gt 0) then data[nan_ind] = 0.
  
; Measure the pixel size
  if n_elements(pix_deg) eq 0 then $
     pix_deg = get_pixel_scale(hdr)
  as_per_pix = pix_deg*3600.

; If we are treating the data as an uncertainty-type map (i.e., one
; that convolves as error propogation), then we need to square it
; before convolution. We will take the square root later.
  if keyword_set(unc) then begin
     data = data^2
  endif

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; WORK OUT THE SIZE OF THE CONVOLUTION KERNEL AND BUILD IT
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Identify the size of the beam that already described the data
; set. Allow the user-supplied size (given by start_beam) to override
; the header value.
  if n_elements(start_beam) gt 0 then begin
     current_beam = start_beam
     if n_elements(current_beam) eq 1 then $
        current_beam = [current_beam, current_beam, 0.0]
     if n_elements(current_beam) eq 2 then $
        current_beam = [current_beam[0], current_beam[1], 0.0]
  endif else begin
     bmaj = sxpar(hdr,'BMAJ')*3600.
     bmin = sxpar(hdr,'BMIN')*3600.
     bpa = sxpar(hdr,'BPA')
     current_beam = [bmaj,bmin,bpa]
  endelse
 
; Now "deconvolve" the starting beam from the target beam to figure
; out what the convolution kernel should look like for this case.
  deconvolve_gauss $
     , meas_maj = target_beam[0] $
     , meas_min = target_beam[1] $
     , meas_pa = target_beam[2] $
     , beam_maj = current_beam[0] $
     , beam_min = current_beam[1] $
     , beam_pa = current_beam[2] $
     , src_maj = kernel_bmaj $
     , src_min = kernel_bmin $
     , src_pa = kernel_bpa $
     , worked = worked $
     , point = point $
     , verbose = verbose
  
; Catch pathological cases: the deconvolution fails or yields a very
; small beam because target ~ starting beam.
  if worked eq 0 then begin
     message,'Cannot get kernel for this combination of beams.', /info
     return
  endif

  if point eq 1 then begin
     message,'WARNING. The target and starting beam are very close.', /info
  endif

; Figure out an appropriate size for the convolution kernel (forcing odd)
  minsize = long(6.*round(kernel_bmaj/as_per_pix) + 1.)
  kern_size = minsize

; You get problems if the PSF is bigger than the image if you are
; using the IDLAstro convolve. Set to the minimum dimension
  sz = size(data)
  if kern_size gt sz[1] or kern_size gt sz[2] then begin
     message, "Warning! PSF is very big compared to image.", /info
     kern_size = floor((sz[1] < sz[2])/2-2)*2 + 1
  endif 

; Check that the kernel is odd. You can make a symmetric kernel.
  if kern_size mod 2 eq 0 then begin
     message, "Kernel has even dimensions. Program needs to be tweaked to work in this case. You got here first, so...", /info
     stop
  endif
  
; Build the PSF based on the kernel calculation. Note that the units
; are pixels and the rotation associated with the position is taken to
; be counterclockwise from up-down (hence the !pi/2). There's an
; implict assumption that east is to the left in the image.

  my_gauss2d $
     , npix=kern_size $
     , a = [0., 1., kernel_bmaj/as_per_pix, kernel_bmin/as_per_pix, 0., 0. $
            , !pi/2.+kernel_bpa*!dtor] $ ; note my_gauss2d wants radians - fix?
     , /center $
     , /normalize $
     , output=kernel  

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DO THE CONVOLUTION
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Convolve with the PSF. If the data have three dimensions, do the
; convolution for each plane. Otherwise convolve the image itself. In
; order to speed up plane-by-plane convolution, we save the Fourier
; transform of the PSF and turn off padding.

  if n_elements(in_weight) eq 0 then begin

;    This is the case where we do not weight the image. We just
;    convolve with our normalized kernel.
  
     sz = size(data)
     if sz[0] eq 3 then begin
        new_data = data
        for ii = 0, sz[3]-1 do $
           new_data[*,*,ii] = $
           convolve(data[*,*,ii] $
                    , kernel $
                    , no_ft=no_ft $
                    , FT_PSF=psf_ft $
                    , no_pad=no_pad)
        data = new_data
     endif else begin
        data = convolve(data, kernel, no_ft=no_ft $
                        ,no_pad=no_pad)
     endelse
  
  endif else begin

;    In this case we do weight the image. We convolve both the image
;    and the weights, which can be 2d or 3d. Then we divide the final
;    convolved image by the weights.
     
     sz = size(data)
     sz_wt = size(weight)
     if sz[0] eq 3 then begin

;       The weights can be an image or a cube. Take the appropriate
;       case and multiply them by the data.
        
        if sz_wt[0] eq 2 then begin
           weighted_data = data
           for ii = 0, sz[3]-1 do $
              weighted_data[*,*,ii] = data[*,*,ii]*weight           
        endif else if sz_wt[0] eq 3 then begin
           weighted_data = data*weight
        endif else begin
           message, "Weights can only be image or cube. Returning.", /info
           return
        endelse

;       Convolve the weighted data to the new resolution plane by plane

        new_data = weighted_data
        for ii = 0, sz[3]-1 do $
           new_data[*,*,ii] = $
           convolve(weighted_data[*,*,ii] $
                    , kernel $
                    , no_ft=no_ft $
                    , FT_PSF=psf_ft $
                    , no_pad=(keyword_set(pad) eq 0))
        weighted_data = new_data
        
;       Convolve the weights, taking into account their shape, and
;       then divide the convolved weighted cube by the convolved
;       weight cube.

        if sz_wt[0] eq 2 then begin
           weight = $
              convolve(weight, kernel, no_ft=no_ft $
                       ,no_pad=(keyword_set(pad) eq 0))
           data = weighted_data
           for ii = 0, sz[3]-1 do $
              data[*,*,ii] = weighted_data[*,*,ii]/weight
        endif else begin
           new_weight = weight
           for ii = 0, sz[3]-1 do $
              new_weight[*,*,ii] = $
              convolve(weight[*,*,ii] $
                       , kernel $
                       , no_ft=no_ft $
                       , FT_PSF=psf_ft $
                       , no_pad=(keyword_set(pad) eq 0))
           weight = new_weight
           data = weighted_data / weight
        endelse
        
     endif else begin

;       The two-d only case

        weighted_data = weight*data
        weighted_data = $
           convolve(weighted_data, kernel, no_ft=no_ft $
                    ,no_pad=(keyword_set(pad) eq 0))
        weight = $
           convolve(weight, kernel, no_ft=no_ft $
                    ,no_pad=(keyword_set(pad) eq 0))
        data = weighted_data / weight        
     endelse

;    So at the end "weight" holds the convolved weights and "data"
;    holds the result of the weighted convolution.

  endelse

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; CLEAN UP AFTER THE CONVOLUTION
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; In these two cases we will need to know the beam sizes before and
; after convolution.

  if keyword_set(unc) or keyword_set(perbeam) then begin

     if n_elements(in_weight) ne 0 then begin
        message, "WARNING! Interaction of weighting with perbeam and unc is not clear.", /info
        message, "WARNING! Proceed at your own risk here.", /info
     endif

;    Work out the pixels per beam at the start of the convolution
     if n_elements(current_beam) eq 3 then begin
        current_fwhm = sqrt(current_beam[0]*current_beam[1])
        ppbeam_start = $
           ((current_fwhm / as_per_pix / 2.)^2 / alog(2) * !pi )
     endif else begin
        ppbeam_start = 1.0
     endelse
     
;    Work out the pixels per beam at the end of the convolution
     target_fwhm = sqrt(target_beam[0]*target_beam[1])
     ppbeam_final = $
        ((target_fwhm / as_per_pix / 2.)^2 / alog(2) * !pi)
  endif

; If the data were treated as uncertainty-like and so squared before
; convolution then we undo that here. In this case, we also scale by
; the square root of the amount of averaging. The idea here is to
; mimic the procedure of averaging independent noise measurements.

  if keyword_set(unc) then begin
;    Take the square root of the data.
     data = sqrt(data)
     
;    The scale factor is sqrt(N_before/N_after)
     scale_fac = sqrt(ppbeam_start/ppbeam_final)
     
;    Scale the data
     data *= scale_fac

  endif

; If we switched not-a-number values to zeros, then set them back to
; NaNs in this step.

  if (nanct gt 0) then $
     data[nan_ind] = !values.f_nan
  
; If we are requested to treat the units as "per beam" then adjust the
; final map by the ratio of beam areas (final beam/original beam).

  if keyword_set(perbeam) then begin

     scale_fac = ppbeam_final / ppbeam_start
     
     data *= scale_fac

  endif

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; REPORT ON THE CALCULATION (UNLESS SUPPRESSED)
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if keyword_set(quiet) eq 0 then begin
     
     flux_after = total(data,/nan)
     flux_ratio = flux_after/flux_before

     message, 'Pixel Scale [as] = '+string(as_per_pix), /info
     message, 'Starting beam [as] = '+string(current_beam,format='(3f10.3)'), /info
     message, 'Target FWHM [as] = '+string(target_beam,format='(3f10.3)'), /info

     kern_beam = [kernel_bmaj, kernel_bmin, kernel_bpa]
     message, 'Convolution Kernel [as] = '+string(kern_beam,format='(3f10.3)'), /info

     if n_elements(kern_size) eq 2 then $
        kern_size=kern_size[0]

     message, 'PSF Grid Size [pix] = '+string(kern_size), /info
     message, 'Flux Ratio = '+string(flux_ratio), /info
     message, 'Treated as uncertainty = '+(keyword_set(unc) ? 'yes':'no'), /info
     message, 'Corrected per beam units = '+(keyword_set(perbeam) ? 'yes':'no'), /info

  endif

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; UPDATE THE HEADER AND WRITE THE OUTPUT
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  sxaddpar,hdr,'BMAJ',target_beam[0]/3600.,'FWHM BEAM IN DEGREES'
  sxaddpar,hdr,'BMIN',target_beam[1]/3600.,'FWHM BEAM IN DEGREES'
  sxaddpar,hdr,'BPA',target_beam[2],'POSITION ANGLE IN DEGREES'

  sxaddpar,hdr,'HISTORY' $
           ,'IDL CONV_WITH_GAUSS: convolved with '+ $
           string([kernel_bmaj, kernel_bmin, kernel_bpa],format='(3f10.3)')+' pixel gaussian'
  if keyword_set(unc) then $
     sxaddpar,hdr,'HISTORY' $
              ,'IDL CONV_WITH_GAUSS: treated as an uncertainty map'

  if n_elements(out_file) gt 0 then begin
     writefits, out_file, data, hdr
  endif

  if n_elements(in_weight) ne 0 then begin

     if n_elements(out_weight_file) ne 0 then begin

        sz_wt = size(weight)
        sz_data = size(data)

        if sz_data[0] eq 2 and sz_wt eq 2 then begin
           writefits, out_weight_file, weight, hdr
        endif

        if sz_data[0] eq 3 and sz_wt eq 2 then begin
           writefits, out_weight_file, weight, twod_head(hdr)
        endif

        if sz_data[0] eq 3 and sz_wt eq 3 then begin
           writefits, out_weight_file, weight, hdr
        endif
        
     endif
     
  endif

end
