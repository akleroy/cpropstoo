pro test_grid_otf

; GENERATE A FAKE HEADER

  naxis_vec = [257, 257,10]
  mkhdr, hdr, 3, naxis_vec
  sxaddpar, hdr, 'CTYPE1', 'RA---TAN'
  sxaddpar, hdr, 'CRVAL1', 15.0
  sxaddpar, hdr, 'CRPIX1', 1
  sxaddpar, hdr, 'CDELT1', 1./3600.

  sxaddpar, hdr, 'CTYPE2', 'DEC--TAN'
  sxaddpar, hdr, 'CRVAL2', 15.0
  sxaddpar, hdr, 'CRPIX2', 1
  sxaddpar, hdr, 'CDELT2', 1./3600.

  sxaddpar, hdr, 'CTYPE3', 'VELOCITY--DUMMY'
  sxaddpar, hdr, 'CRVAL3', 0.
  sxaddpar, hdr, 'CRPIX3', 1.
  sxaddpar, hdr, 'CDELT3', 10.

  sxaddpar, hdr, 'EQUINOX', 2000.
  sxaddpar, hdr, 'RESTFRQ', 230.538d9

; MAKE A FAKE IMAGE

  beam_fwhm = 2460. / (sxpar(hdr,'RESTFRQ')/1d9) / 3600.
  beam_grid = sqrt(beam_fwhm^2 + (8./3600)^2)
  ximg = findgen(257) # (fltarr(257)+1.)
  yimg = (fltarr(257)+1.) # findgen(257)
  ximg -= mean(ximg)
  yimg -= mean(yimg)
  rimg = sqrt(ximg^2+yimg^2)
  timg = atan(yimg, ximg)

  plane = ((rimg gt 100 and rimg lt 120) + $
           (ximg lt 10 and ximg gt -10) + $
           (yimg lt 10 and yimg gt -10))*1.0
  image = fltarr(257,257,10)
  for i = 0, 9 do image[*,*,i] = plane

  sxaddpar, hdr, 'BMAJ', 0.0
  sxaddpar, hdr, 'BMIN', 0.0

  conv_with_gauss $
     , data=image $
     , hdr=hdr $
     , target_beam=beam_fwhm*3600. $
     , out_data = image_tel $
     , out_hdr = hdr_tel

  conv_with_gauss $
     , data=image_tel $
     , hdr=hdr_tel $
     , target_beam=beam_grid*3600. $
     , out_data = image_grid $
     , out_hdr = hdr_grid

; TRY A SIMPLE CASE OF GRIDDING
  data = fltarr(257L*257L,10)
  for i = 0, 9 do data[*,i] = reform(image_tel[*,*,i])
  make_axes, hdr, ri=ri, di=di
  ra = reform(ri, n_elements(ri))
  dec = reform(di, n_elements(di))

  grid_otf_2 $
     , data = data $
     , ra = ra $
     , dec = dec $
     , target_hdr = hdr_tel $
     , out_cube = out_cube_2 $
     , /gauss_kern $
     , gauss_fwhm = 8.0/3600 $
     , scale=3

  grid_otf $
     , data = data $
     , ra = ra $
     , dec = dec $
     , target_hdr = hdr_tel $
     , out_cube = out_cube $
     , /gauss_kern $
     , gauss_fwhm = 8.0/3600

; GRID TO AN IMAGE
  plane = image[*,*,0]
  tel_plane = image_tel[*,*,0]
  grid_plane = image_grid[*,*,0]
  otf_plane = out_cube[*,*,0]

  print, "RMS fractional error where bright: " $
         , mad((((dplane-plane1)/dplane))[where(dplane gt 0.5)])

  print, "RMS fractional error where bright: " $
         , mad((((dplane-plane2)/dplane))[where(dplane gt 0.5)])


  plot, plane[127,*], yrange=[0.9, 2.1], xrange=[100,150]
  oplot, tel_plane[127,*], color=cgcolor('cyan'), ps=6
  oplot, grid_plane[127,*], color=cgcolor('green'), ps=6
  oplot, otf_plane2[127,*], color=cgcolor('red'), ps=6

  stop

end
