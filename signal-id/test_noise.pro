pro test_noise

;+
;
; Run noise tests for CPROPStoo. The output is intended to be self-explanatory.
;
;-

  print, "---- noise estimate test ----"

  fid_noise = 2.5
  test_vec = randomn(seed, 1e4)*fid_noise
  print, "Input noise vector with rms : ", fid_noise
  
  fit_noise = est_noise(test_vec)
  print, "est_noise gets by default: ", fit_noise

  fit_noise = est_noise(test_vec, /stddev)
  print, "est_noise gets using /stddev: ", fit_noise

  fit_noise = est_noise(test_vec, /negative)
  print, "est_noise gets using negatives: ", fit_noise

  fit_noise = est_noise(test_vec, /iterate)
  print, "est_noise gets using iteration: ", fit_noise

  fit_noise = est_noise(test_vec, /iterate, thresh=3)
  print, "est_noise gets using iteration with 3-sigma rejection: ", fit_noise

  fit_noise = est_noise(test_vec, /iterate, noutlier=10)
  print, "est_noise gets using iteration with 10-outlier rejection: ", fit_noise
  
  print, "---- two-d noise map test ----"

  noise_map = findgen(101) # (fltarr(101)+1.)
  noise_cube = fltarr((size(noise_map))[1], (size(noise_map))[2], 51)
  for i = 0, (size(noise_cube))[3]-1 do noise_cube[*,*,i] = noise_map

  realization = realize_noise(noise_cube)
  make_noise_cube, cube_in = realization $
                   , out_cube = fit_noise $
                   , /twod

  ind = lindgen(n_elements(noise_cube)/10)*10
  plot, noise_cube[ind], fit_noise[ind], ps=3
  equality

  print, "realize_noise + make_noise_cube has this median accuracy reproducing an input map:"  $
         , median(fit_noise/noise_cube)
  print, "realize_noise + make_noise_cube has this rms multiplicative scatter reproducing an input map:" $
         , 10.^stddev(alog10(fit_noise/noise_cube),/nan)-1.

  make_noise_cube, cube_in = realization $
                   , out_cube = box_noise $
                   , box = 21 $
                   , /twod

  ind = lindgen(n_elements(noise_cube)/10)*10
  plot, noise_cube[ind], box_noise[ind], ps=3
  equality

  print, "---- one-d noise spectrum test ----"

  print, "---- cube noise test ----"

  print, "---- realization test ----"

  print, "---- monte carlo test ----"

end
