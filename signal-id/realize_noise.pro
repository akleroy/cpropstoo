function realize_noise $
   , noise $
   , template = template $
   , hdr = hdr $
   , bmaj = bmaj $
   , bmin = bmin $
   , bpa = bpa $
   , seed = seed $
   , noconvolve = noconvolve

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ THE NOISE IF IT'S A FILE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(noise) eq 0 then begin
     message, "Need a noise cube or image.", /info
     return, !values.f_nan
  endif

  if size(noise, /type) eq size("hello", /type) then $
     noise = readfits(noise, hdr)

; In the case of 1-d noise require a template
  if n_elements(noise) eq 1 then begin
     if n_elements(template) eq 0 then begin
        message, "Need a template to realize a single noise value.", /info
        return, !values.f_nan
     endif
     if size(template, /type) eq size("hello", /type) then $
        template = readfits(template, temp_hdr)
     noise = noise*((template eq template)*0.+1.)
  endif

; Check that we have a beam if we want convolution with a beam
  if n_elements(hdr) eq 0 and n_elements(bmaj) eq 0 then begin
     message, "Need a header or a beam size for convolution. Convolution disabled.", /info
     noconvolve = 1B
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; REALIZE THE NOISE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
; Generate amplitude "1" noise.
  sz = size(noise)
  if sz[0] eq 2 then $
     unit_noise = randomn(seed, sz[1],sz[2])
  if sz[0] eq 3 then $
     unit_noise = randomn(seed, sz[1],sz[2],sz[3])

; Return here if convolution is unwanted
  if keyword_set(noconvolve) then $
     return, noise*unit_noise
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CONVOLVE WITH THE BEAM
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; If requested, convolve the noise distribution to have the correct
; pixel-to-pixel correlation (set by the beam size).

; Figure out the beam to use
  if n_elements(bmaj) eq 0 then begin
     bmaj = sxpar(hdr, "BMAJ")
     bmin = sxpar(hdr, "BMIN")
     bpa = sxpar(hdr, "BPA")
  endif else begin
     if n_elements(bmin) eq 0 then begin
        bmin = bmaj
        bpa = 0.0
     endif else if n_elements(bpa) eq 0 then begin
        bpa = 0.0
     endif
  endelse

; Convolve with an elliptical Gaussian
  conv_with_gauss $
     , data = unit_noise $
     , hdr = hdr $
     , out_data = unit_noise $
     , target_beam=[bmaj*3600.,bmin*3600.,bpa] $
     , start_beam=[0,0,0]

; Rescale to get the amplitude of the noise back to unity.
; ... diagnostic - remove eventually  
  message, "I want to rescale by a factor of "+str(rescale_value), /info
  rescale_value = 1./stddev(unit_noise)

; Return the noise scaled by the noise map
  return, noise*unit_noise

end
