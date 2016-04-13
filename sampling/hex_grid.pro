pro hex_grid $
   , ctr_x = ctr_x, ctr_y = ctr_y $
   , spacing = spacing $
   , radec = radec $
   , xout = xout, yout = yout $
   , r_limit = radius $
   , e_limit = edge $
   , center = center

;+
;
; Generate a hexagonal grid pattern of the specified spacing.
;
;-

; Y-SPACING OF THE GRID
  y_spacing = spacing*sin(!dtor*60)

; X-SPACING OF THE GRID
  x_spacing = spacing

; ESTIMATE X AND Y EXTENT
  scale = (n_elements(radius) gt 0) ? radius : (edge / 2.0)
  half_ny = ceil(scale/y_spacing)
  half_nx = ceil(scale/x_spacing)+1

; MAKE THE GRID
  x = findgen(2*half_nx+1) # (fltarr(2*half_ny+1)+1.)
  y = (fltarr(2*half_nx+1.)+1.) # findgen(2*half_ny+1)
  x -= half_nx
  y -= half_ny

; FIGURE OUT THE BOTTOM LEFT CORNER  
  x *= x_spacing
  x += 0.5*x_spacing*((abs(y) mod 2) eq 1)
  y *= y_spacing

; KEEP THE SUBSET THAT MATCHES THE REQUESTED CONDITION
  r = sqrt(x^2 + y^2)
  if n_elements(radius) gt 0 then $
     keep = where(r lt radius, keep_ct) $
  else $
     keep = where(abs(x) le edge/2. and abs(y) le edge/2., keep_ct)

  if keep_ct eq 0 then return

; CREATE OUTPUT ARRAYS
  yout = y[keep] + ctr_y[0]
  if keyword_set(radec) then $
     xout = (x[keep] / cos(!dtor*yout) + ctr_x[0]) $
  else $
     xout = (x[keep] + ctr_x[0])      

end
