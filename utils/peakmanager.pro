pro peakmanager, filename=filename, peakfile=peakfile, view=view

  if keyword_set(view) then begin
     cube = readfits(filename, hdr)
     peak_map = max(cube,dim=3,/nan)
     peaks = obj_new('peaks', peakfile)
     view_peaks, peak_map, peaks, header=hdr
  endif

  

end
