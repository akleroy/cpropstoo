function get_pixel_scale, hdr
  
  tol = 1e-6
  xyad, hdr, [0,1,0], [0,0,1], ra, dec
  step_x = abs(sphdist(ra[0], dec[0], ra[1], dec[1], /deg))
  step_y = abs(sphdist(ra[0], dec[0], ra[2], dec[2], /deg))
  
  if abs(step_x - step_y) gt tol then begin
     message, "Pixel scale looks different in X and Y. Warning.", /info
     return, sqrt(step_x*step_y)
  endif
  
  return, step_x

end
