pro xyv_to_ind $
   , x=x $
   , y=y $
   , v=v $
   , ind=ind $
   , sz=sz $
   , cube=cube

;+
;
; Convert from (x,y,v) to 1-d cube indices. Needs the size of the
; cube. Pure convenience.
;
;-

  if n_elements(sz) eq 0 then begin
     if n_elements(cube) gt 0 then $
        sz = size(cube) $
     else begin
        message, "Requires size vector or cube.", /info
        return
     endelse
  endif

  ind = x + y*sz[1] + v*sz[1]*sz[2]
  
end
