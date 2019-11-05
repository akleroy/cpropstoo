function calc_conv_beam $
   , start_beam=start_beam $
   , target_beam=target_beam
;+
;-
 
; Now "deconvolve" the starting beam from the target beam to figure
; out what the convolution kernel should look like for this case.
  deconvolve_gauss $
     , meas_maj = target_beam[0] $
     , meas_min = target_beam[1] $
     , meas_pa = target_beam[2] $
     , beam_maj = start_beam[0] $
     , beam_min = start_beam[1] $
     , beam_pa = start_beam[2] $
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
     return, !values.f_nan*[1,1,0]
  endif

  if point eq 1 then begin
     message,'WARNING. The target and starting beam are very close.', /info
  endif

  return, [kernel_bmaj, kernel_bmin, kernel_bpa]

end
