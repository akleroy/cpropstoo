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

  conv_with_gauss $
     , data = this_noise $
     , hdr = hdr $
     , out_data = this_noise $
     , target_beam=[bmaj*3600.,bmin*3600.,bpa] $
     , start_beam=[0,0,0] 

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RESCALE SO THAT THE VARIANCE IS RIGHT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; I'm not rescaling the mean because the noise cube gets added to the
; signal cube. This should automatically adjust the mean. The noise
; cube variance does need to be adjusted to match the input noise cube
; characteristics.

if sz[0] eq 3 then begin ; cube case
   for i = 0, n_elements(sz[3]) - 1 do begin
      want_noise = stddev(noise[*,*,i],/double,/nan)
      have_noise = stddev(this_noise[*,*,i],/double,/nan)
      rescale_value = want_noise/have_noise
      this_noise[*,*,i] = rescale_value * this_noise[*,*,i]
   endfor
endif else begin ; image case
   want_noise = stddev(noise,/double,/nan)
   have_noise = stddev(this_noise,/double,/nan)
   rescale_value = want_noise/have_noise
   this_noise = rescale_value * this_noise
endelse

return, this_noise

end
