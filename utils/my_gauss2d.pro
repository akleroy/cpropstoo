pro my_gauss2d $
   , npix=npix $
   , a = a $ 
   , center=center $
   , normalize=normalize $
   , output = output
  
;+
;
; Create a two dimensional rotated Gaussian appropriate for use as a
; point spread function in convolution.
;
; my_gauss2d, npix, a, /center, /normalize, output=kernel
; 
; Where:
;
; npix = number of pixels in the kernel (ideally odd), either n or [nx,ny]
;
; a = Gaussian shape parameters. Defined as an array where:
;
; a[0] = constant offset (zero level)
; a[1] = peak of Gaussian
; a[2] = full width at half max major axis
; a[3] = full width at half max minor axis 
; a[4] = center in the x coordinate
; a[5] = center in the y coordinate
; a[6] = rotation in radians of x-axis CCW
;
; center = flag telling the program to center the image
;
; normalize = flag telling the program to normalize the PSF
;
; output = the output Gaussian array
;
;-

  on_error,0

  if n_elements(npix) eq 2 then BEGIN 
     nx = npix[0] & ny = npix[1]
  endif

  if n_elements(npix) eq 1 then BEGIN
     nx = npix & ny = npix
  endif

  if n_elements(npix) gt 2 or n_elements(npix) lt 1 then BEGIN
     message,'No valid 2d array size.'
  endif

  xarr = rebin(findgen(nx),nx,ny)
  yarr = rebin(reform(findgen(ny),1,ny),nx,ny)

  if keyword_set(center) then BEGIN
     cenx = (nx-1.)/2.
     ceny = (ny-1.)/2.
  endif else BEGIN
     cenx = a[4]
     ceny = a[5]
  endelse

  fac = 2d*sqrt(2d*alog(2d))

  ang = a[6]
  const = a[0]
  peak = a[1]
  widthx = a[2]/fac
  widthy = a[3]/fac

  s = sin(ang) & c = cos(ang)

  xarr = xarr-cenx
  yarr = yarr-ceny
  
  t =  xarr * (c/widthx) + yarr * (s/widthx)
  yarr = xarr * (s/widthy) - yarr * (c/widthy)
  xarr = temporary(t)
  u = exp(-0.5 * (xarr^2 + yarr^2))
  output = (const + peak * u)

  if keyword_set(normalize) then BEGIN
     tot = total(output)
     output /= tot
  endif

end
