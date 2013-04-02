pro ind_to_xyv $
   , ind $
   , x=x $
   , y=y $
   , v=v $
   , sz=sz $
   , cube=cube

;+
;
; Convert from indices in a cube to 1-d indices. Needs the size of the
; cube. Pure convenience.
;
;-

  if n_elements(sz) eq 0 then begin
     if n_elements(cube) gt 0 then $
        sz = size(cube) $
     else begin
        message, "Requires size vector or cube."
        return
     endelse
  endif

  x = ind mod sz[1]
  y = (ind mod (sz[1]*sz[2])) / sz[1]
  v = ind / (sz[1]*sz[2])
  
end
