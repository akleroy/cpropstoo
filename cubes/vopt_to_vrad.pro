function vopt_to_vrad, vopt_kms, reverse=reverse

  c = 2.99792458d10
  
  restfreq = 1.0 ; divides out, I just like it there for clarity

  if keyword_set(reverse) then begin
     vrad_kms = vopt_kms
     nu = restfreq * (1 - vrad_kms*1d5 / c)
     vopt_kms = c * (restfreq - nu)/nu / 1d5
     return, vopt_kms
  endif

  nu = restfreq * (1 + vopt_kms*1d5/c)^(-1.)
  vrad_kms = c *(restfreq - nu)/restfreq / 1d5  
  return, vrad_kms

end
