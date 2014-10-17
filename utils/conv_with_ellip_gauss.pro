pro conv_with_ellip_gauss $
;  INPUT/OUTPUT
   , infile=infile $
   , in_data = in_data $
   , in_hdr = in_hdr $
   , outfile=outfile $
   , out_data = map $
   , out_hdr = hdr $
   , cube=cube $
;  DETAILS OF CONVOLUTION
   , target_beam=target_beam $
   , start_beam=start_beam $
;   , force_fwhm=force_fwhm $
   , grow_nan = grow_nan $
   , nan_to_zero = nan_to_zero $
   , no_ft=no_ft $
   , uncertainty = unc $
   , asperbeam = asperbeam $
;  FEEDBACK CONTROLS
   , quiet=quiet $
   , show=show 

;+
; NAME:
;
; conv_with_gauss
;
; PURPOSE:
;
; Wrapper to "convolve" (FFT) or "convol" (direct kernel multiplication) that
; will generate a gaussian PSF and then convolve it with the target image.
;
; CATEGORY:
;
; Image manipulation.
;
; CALLING SEQUENCE:
;
; conv_with_gauss, infile='input.fits', outfile='output.fits' $
;                , target_beam=target_beam, start_beam=start_beam $
;                , quiet=quiet, cube=cube
;
; INPUTS:
;
; infile: the input file (fits)
;
; outfile: the output file (fits)
;
; target_beam: the target full width at half max of the image PSF
;
; OPTIONAL INPUTS:
;
; start_beam: the current FWHM of the image PSF, if this is not supplied then
; conv_with_gauss tries to look for the BMAJ and BMIN keywords in the header.
;
; KEYWORD PARAMETERS:
;
; quiet: tells the program that you don't want to hear any more of its insipid
; blather.
;
; cube: tells the program that the input is a cube and to convolve each plane
; separately. Assumes axis 3 is the velocity / wavelength axis.
;                     
; no_ft: passed directly to 'convolve.' Tells the program to use the IDL
; 'convol' function instead of a Fourier transform.
;
; OUTPUTS:
;
; a fits file written to 'outfile'
;
; MODIFICATION HISTORY:
;
; documented - leroy@mpia.de 17 nov 08
; modified to do elliptical gaussians - karin 26 jun 11
;
;-

; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$
; READ IN OR COPY DATA
; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$

  if n_elements(in_data) eq 0 or $
     n_elements(in_hdr) eq 0 then begin
     fits_read, infile, map, hdr    
  endif else begin
     map = in_data
     hdr = in_hdr
  endelse

; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$
; PROCESS DATA BEFORE CONVOLUTION
; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$

  if keyword_set(show) then begin
      pmulti_old = !p.multi
      !p.multi = [0,2,1]
      disp, map, title='BEFORE'
  endif

; NOTE THE FLUX BEFORE THE CONVOLUTION
  flux_before = total(map,/nan)

; SAVE THE LOCATION OF NANS IN THE CUBE
  finite_mask = finite(map)

; REMOVE THESE NANS FROM THE CUBE AND TREAT THEM AS 0S IN THE CONVOLUTION
; (NOT IDEAL! A BETTER TREATMENT WOULD BE TO ONLY DO THIS WHEN USING THE FFT)
  nan_ind = where(finite(map) eq 0, nanct)
  if (nanct gt 0) then $
    map[nan_ind] = 0.

; EXTRACT ASTROMETRY
  extast, hdr, astr

; MEASURE THE NUMBER OF ARCSECONDS PER PIXEL
  if (astr.cdelt[0] eq 1.0) then begin
     as_per_pix = (abs(astr.cd[0,0]) > abs(astr.cd[1,0]))*3600. 
  endif else begin
     as_per_pix = abs(astr.cdelt[0])*3600.
  endelse

; IF TREATING AS UNCERTAINTY SQUARE THE MAP
  if keyword_set(unc) then begin
     map = map^2
  endif

; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$
; WORK OUT THE CONVOLUTION KERNEL
; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$

; IF THE USER FORCES THE FWHM TO A PARTICULAR VALUE, USE THAT
;  if n_elements(force_fwhm) gt 0 then begin
;     kern_beam = [force_fwhm/as_per_pix,force_fwhm/as_per_pix,0.0]  
;  endif else begin

; OTHERWISE WORK OUT THE KERNEL SIZE NEEDED TO REACH THE TARGET 

;    PREFER THE USER-SUPPLIED BEAM SIZE... 
     if n_elements(start_beam) gt 0 then begin
        current_beam = start_beam
     endif else begin
;    ... ELSE USE THE BEAM SIZE FROM THE HEADER
        bmaj = sxpar(hdr,'BMAJ')*3600.
        bmin = sxpar(hdr,'BMIN')*3600.
		bpa = sxpar(hdr,'BPA')
        current_beam = [bmaj,bmin,bpa]
     endelse
 
;    CHECK IF WE ARE ALREADY AT THE TARGET RESOLUTION
;     if current_fwhm ge target_fwhm then begin
;        message $
;           , 'Current beam already matches or exceeds target beam.' $
;           , /info
;        message $
;           , 'Writing input image to output with NO convolution.' $
;           , /info
;        sxaddpar, hdr, 'HISTORY' $
;                  , 'CONV_WITH_GAUSS: no convolution b/c beam'+$
;                  ' already >= target.'
        
;        if n_elements(outfile) gt 0 then begin
;           writefits, outfile, map, hdr
;        endif

;        return
;     endif
     
;   TAKE THE DIFFERENCE IN QUADRATURE AND DIVIDE BY THE PIXEL SCALE TO GET THE
;   SIZE OF THE CONVOLUTION KERNEL IN PIXELS
;     kern_fwhm = sqrt(target_fwhm^2 - current_fwhm^2) / as_per_pix  
	  get_ellip_beam,bmaj1=target_beam[0],bmin1=target_beam[1],bpa1=target_beam[2],$
	  	bmaj2=current_beam[0],bmin2=current_beam[1],bpa2=current_beam[2],$
		outbmaj=ker_bmaj,outbmin=ker_bmin,outbpa=ker_bpa,ifail=ifail
	  if ifail eq 2 then message,'Cannot get kernel for this combination of beams.'
	  kern_beam = [ker_bmaj, ker_bmin, ker_bpa]
;  endelse  

; FIGURE OUT THE TARGET SIZE (GO WITH 3 FWHM AS A SAFE BET)
  minsize = 6.*kern_beam[0]/as_per_pix + 1.
  if minsize mod 2 eq 0 then size = minsize+1 else size = minsize
  
; MAKE THE KERNEL
;  kernel = psf_gaussian(npixel = size $
;                        , fwhm = kern_fwhm $
;                        , /double, /normalize)

   my_gauss2d,$
   		npix=size,$
        a = [0., 1., kern_beam[0]/as_per_pix, kern_beam[1]/as_per_pix, 0., 0., kern_beam[2]],$
		/center,/normalize,$
		output=kernel

; NORMALIZE (JUST IN CASE)
  kernel /= total(kernel)

; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$
; DO THE CONVOLUTION
; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$

  if keyword_set(cube) then begin
     newmap = map
     sz = size(map)
     for i = 0, sz[3]-1 do $
        newmap[*,*,i] = convolve(map[*,*,i] $
                                 , kernel $
                                 , no_ft=no_ft $
                                 , FT_PSF=psf_ft $ ; save the PSF
                                 , /no_pad)        ; debateable 
     map = newmap
  endif else begin
     map = convolve(map, kernel, no_ft=no_ft)
  endelse
  
; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$
; CLEAN UP AFTERWARDS 
; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$

; IF IT'S AN UNCERTAINTY MAP THEN CLEAN UP
  if keyword_set(unc) then begin
     map = sqrt(map)
     if n_elements(current_beam) eq 3 then begin
        current_fwhm = sqrt(current_beam[0]*current_beam[1])
		ppbeam_start = $
           ((current_fwhm / as_per_pix / 2.)^2 / alog(2) * !pi )
     endif else begin
        ppbeam_start = 1.0
     endelse
     
	 target_fwhm = sqrt(target_beam[0]*target_beam[1])
     ppbeam_final = $
        ((target_fwhm / as_per_pix / 2.)^2 / alog(2) * !pi)
     
     scale_fac = sqrt(ppbeam_start/ppbeam_final)

     map *= scale_fac
  endif

; REPLACE NANS 
; (THIS STEP IS DEBATEABLE, BUT USUALLY RIGHT FOR MY PURPOSES)

; TWO CASES: 

; .... FIRST JUST PUT NANS WHERE THEY WERE TO START WITH

  if n_elements(grow_nan) eq 0 then begin
     if (nanct gt 0) and keyword_set(nan_to_zero) eq 0 then $
        map[nan_ind] = !values.f_nan
  endif ;else begin

; ... SECOND ALLOW THE CONVOLUTION TO GROW INTO THE NON-FINITE REGIONS BY 1/2
; THE FWHM OF THE CONVOLUTION KERNEL

;     if keyword_set(cube) then begin
;        new_finite_mask = finite_mask
;        sz = size(finite_mask)
;        for i = 0, sz[3]-1 do begin
;           nans_here = total(finite_mask[*,*,i] eq 0)
;           if nans_here eq 0 or nans_here eq sz[1]*sz[2] then $
;              continue
;           new_finite_mask[*,*,i] = $
;              exp_mask(finite_mask[*,*,i], radius = kern_fwhm / 2.)
;        endfor
;        finite_mask = new_finite_mask
;     endif else begin
;        finite_mask = exp_mask(finite_mask, radius = kern_fwhm / 2.)
;     endelse
;        
;     nan_ind = where(finite_mask eq 0, nanct)
;     if (nanct gt 0) then $
;        map[nan_ind] = !values.f_nan
;  endelse

; CHECK THE FLUX
  flux_after = total(map,/nan)
  flux_ratio = flux_after/flux_before

; WORK OUT THE FINAL FWHM
;  if n_elements(force_fwhm) gt 0 then begin
;     bmaj = sxpar(hdr,'BMAJ')*3600.
;     final_fwhm = sqrt(bmaj^2 + force_fwhm^2)
  
;  endif else begin
;    final_fwhm = target_fwhm
;  endelse

; THE INSIPID BLATHER
  if keyword_set(quiet) eq 0 then begin
     message, 'Pixel Scale [as] = '+string(as_per_pix), /info
;     if n_elements(force_fwhm) gt 0 then begin
;        message, 'Forced beam [as] = '+string(force_fwhm), /info
;     endif else begin
        message, 'Current beam [as] = '+string(current_beam,format='(3f10.3)'), /info
        message, 'Target FWHM [as] = '+string(target_beam,format='(3f10.3)'), /info
;     endelse

     message, 'Convolution Kernel [as] = '+string(kern_beam,format='(3f10.3)'), /info

;    CATCH OLD PSF GAUSSIAN BUG
     if n_elements(size) eq 2 then size=size[0]
     message, 'PSF Grid Size [pix] = '+string(size), /info
     message, 'Flux Ratio = '+string(flux_ratio), /info
  endif

  if keyword_set(show) then begin
      disp, map, title='AFTER'
      !p.multi = pmulti_old
  endif

; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$
; UPDATE THE HEADER AND WRITE THE OUTPUT
; %$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$%$

; UPDATE THE HEADER WITH A NEW BEAM SIZE AND HISTORY
  sxaddpar,hdr,'BMAJ',target_beam[0]/3600.,'FWHM BEAM IN DEGREES'
  sxaddpar,hdr,'BMIN',target_beam[1]/3600.,'FWHM BEAM IN DEGREES'
  sxaddpar,hdr,'BPA',target_beam[2],'POSITION ANGLE IN DEG'
  sxaddpar,hdr,'HISTORY' $
    ,'IDL CONV_WITH_GUASS: convolved with '+string(kern_beam,format='(3f10.3)')+' pixel gaussian'
  if keyword_set(unc) then $
     sxaddpar,hdr,'HISTORY' $
              ,'IDL CONV_WITH_GAUSS: treated as an uncertainty map'

  if n_elements(outfile) gt 0 then begin
     writefits, outfile, map, hdr
  endif

end
