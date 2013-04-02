pro stat_mask $
   , mask $
   , area=area $
   , vwidth=vwidth $
   , volume=volume

;+
; 
; Kick back area, velocity extent, and volume of a mask. Could run
; this with uniq and x,y,v instead. Speed checks make sense if it
; turns into a bottleneck.
;
;-

  sz = size(mask)

  volume = total(mask)
  
  if sz[0] eq 2 then begin
     vwidth = 1
  endif else begin
     vwidth = total(total(total(mask,1),1) ge 1)
  endelse

  if sz[0] eq 2 then begin
     area = volume
  endif else begin
     area = total(total(mask,3) ge 1)
  endelse

end
