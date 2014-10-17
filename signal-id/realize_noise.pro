function realize_noise $
   , noise $
   , template = template $
   , hdr = hdr $
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

  if n_elements(hdr) eq 0 then begin
     message, "Need a header.", /info
     return, !values.f_nan
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; REALIZE THE NOISE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
  sz = size(noise)
  if sz[0] eq 2 then $
     this_noise = noise*randomn(seed, sz[1],sz[2])
  if sz[0] eq 3 then $
     this_noise = noise*randomn(seed, sz[1],sz[2],sz[3])

; Return here if convolution is unwanted
  if keyword_set(noconvolve) then $
     return, this_noise
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CONVOLVE WITH THE BEAM
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  bmaj = sxpar(hdr, "BMAJ")
  bmin = sxpar(hdr, "BMIN")
  bpa = sxpar(hdr, "BPA")

  conv_with_ellip_gauss $
     , in_data = this_noise $
     , in_hdr = hdr $
     , out_data = this_noise $
     , cube = sz[0] eq 3 $     
     , target_beam=[bmaj*3600.,bmin*3600.,bpa] $
     , start_beam=[0,0,0]

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RESCALE SO THAT THE AMPLITUDE IS RIGHT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  want_noise = median(noise)
  have_noise = mad(this_noise)
  rescale_value = want_noise/have_noise

  print, "I want to rescale by a factor of ", rescale_value
  this_noise = rescale_value * this_noise

  return, this_noise

end
