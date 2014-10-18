pro view_peaks, peak_map, peaks, header=header
  ; settings
  !p.multi = 0
  loadct, 5

  if n_elements(header) gt 0 then begin
     make_axes, header, raxis=raxis, daxis=daxis, vaxis=vaxis
     disp, peak_map, raxis, daxis, /radec, /sq
     loadct, 0
     oplot, peaks->getRA(), peaks->getDEC(), color=255, ps=1
  endif else begin
     disp, peak_map, /sq
     loadct, 0
     oplot, peaks->getX(), peaks->getY(), color=255, ps=1  
  endelse 
end
