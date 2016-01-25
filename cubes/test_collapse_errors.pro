pro test_collapse_errors

  vaxis = findgen(101)*100.
  test_spec = 15.*exp(-1.*(vaxis-5000.)^2/2./1d4^2)
  test_cube = fltarr(1000,1000,101)
  for kk = 0, 100 do test_cube[*,*,kk] = test_spec[kk]
  test_cube += randomn(seed,n_elements(test_cube))
  
  collapse_cube $
     , cube=test_cube $
     , vaxis=vaxis $   
     , noise=1.0 $
     , mom0 = mom0 $
     , e_mom0 = e_mom0 $
     , mom1 = mom1 $
     , e_mom1 = e_mom1 $
     , mom2 = mom2 $
     , e_mom2 = e_mom2 $
     , var = var $
     , e_var = e_var $
     , tpeak = tpeak
 
  print, "Moment 0 uncertainty calculated, simulated: ", median(e_mom0), mad(mom0)
  print, "Moment 1 uncertainty calculated, simulated: ", median(e_mom1), mad(mom1)
  print, "Moment 2 uncertainty calculated, simulated: ", median(e_mom2), mad(mom2)

  stop

end
