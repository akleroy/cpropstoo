pro downsample_cube $
   , in_file = in_file $
   , out_file = out_file $
   , iterations_hans = hans_iter $
   , binfac = binfac

;+
; NAME:
;
;    downsample_cube
;
; PURPOSE:
;
;    Lower the velocity resolution of a cube without sacrificing
;    signal-to-noise by either hanning smoothing and resampling or
;    rebinning. Extra channels are trimmed from the upper edge of the
;    cube.
;
; CATEGORY:
;
;
;
; CALLING SEQUENCE:
;
;    downsample_cube, in_file = in_file, out_file = out_file $
;                   , iterations_hans = iterations_hans, binfac = binfac
;
; INPUTS:
;
; in_file : input FITS file name
;
; OPTIONAL INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
; (only one of these two modes works)
;
; iterations_hans : the number of Hanning smoothing iterations to
; perform. Each operation convolves with a kernel = [0.25, 0.5, 0.25]
; then downsamples to every other channel. (N.B. that the result has
; some channel-to-channel correlation).
;
; binfac : the factor by which to combine channels and
; downsample. E.g., if this is set to 5 then 5 channels in the old
; cube are combined into 1 channel in the new cube.
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
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
;
; MODIFICATION HISTORY:
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND ERROR CATCHING
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(hans_iter) gt 0 and $
     n_elements(binfac) gt 0 then begin
     message, "Only one mode (hanning or binning) will work at once. Returning.", /info
     return
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ FILES, WORK OUT ASTROMETRY
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; READ THE DATA
  cube = readfits(in_file, hdr)

; WORK OUT ASTROMETRY
  make_axes, hdr, vaxis=vaxis, /vonly

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; HANNING SMOOTHING
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(hans_iter) gt 0 then begin

;    NOTE THE ORIGIN OF THE VELOCITY AXIS
     v0 = vaxis[0]
     
     for k = 0, hans_iter-1 do begin

;       THE SIZE OF THE CURRENT CUBE
        sz = size(cube)

;       THE NEW CUBE (HALF THE SIZE OF THE ORIGINAL)
        new_cube = dblarr(sz[1],sz[2],sz[3]/2)

;       LOOP OVER 2D PIXELS
        for i = 0, sz[1]-1 do begin
           
           for j = 0, sz[2]-1 do begin
              
;             EXTRACT THE SPECTRUM
              spec = reform(cube[i,j,*])

;             HANNING SMOOTH
              spec = hans(3, spec)

;             DOWNSAMPLE INTO THE NEW CUBE
              new_cube[i,j,*] = spec[2*indgen(sz[3]/2)+1]

           endfor

        endfor

;       SET THE CUBE EQUAL TO THE NEW CUBE
        cube = new_cube

;       DOWNSAMPLE THE VELOCITY AXIS IN THE SAME WAY
        vaxis = vaxis[2*indgen(sz[3]/2)+1]

     endfor

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; BINNING
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(binfac) gt 0 then begin

;    THE SIZE OF THE CURRENT CUBE
     sz = size(cube)

;    FIGURE OUT THE NEW VAXIS SIZE
     new_sz3 = floor(sz[3]/binfac)
     
;    REBIN THE CUBE
     cube = rebin(cube[*,*,0:(new_sz3*binfac-1)], sz[1], sz[2], new_sz3)
     
;    REBIN THE VELOCITY AXIS
     vaxis = rebin(vaxis[0:(new_sz3*binfac-1)], new_sz3)
     
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; WRITE TO DISK, UPDATING THE VELOCITY AXIS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
; UPDATE THE HEADER
  sxaddpar,hdr, 'CDELT3', vaxis[1]-vaxis[0], 'Updated by downsample_cube.pro'
  sxaddpar,hdr, 'CRPIX3', 1, 'Updated by downsample_cube.pro'
  sxaddpar,hdr, 'CRVAL3', vaxis[0], 'Updated by downsample_cube.pro'
  sxaddpar,hdr, 'NAXIS3', n_elements(vaxis), 'Updated by downsample_cube.pro'

; WRITE TO DISK
  writefits, out_file, cube, hdr

end
