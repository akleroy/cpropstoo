pro fasthist, data2, log = log, poly = poly, _extra=_extra
;+
; NAME:
;   FASTHIST
; PURPOSE:
;   To create a fast, dumb histogram of the data to determine the
;   basic properties of the data.
;
; CALLING SEQUENCE:
;   FASTHIST, data
;
; INPUTS:
;   DATA -- The data to be histogrammed.
;
; KEYWORD PARAMETERS:
;   LOG -- Set the values to show the logarithm of the histogram.
;          This is the only keyword because it's damn useful.
;
;   IMPORTANT! Note that all extra parameters go to the plot.
;
; OUTPUTS:
;   A nice histogram of the data.
;
; MODIFICATION HISTORY:
;       Concieved in frustration --
;       Fri Jun 1 22:14:38 2001, Erik Rosolowsky <eros@cosmic>
;       Added /poly because it looks nice but autohist sucks --
;       22 Dec 2009 aleroy@nrao.edu
;
;-		



  if n_elements(data2) eq 0 then begin
    message, 'Variable contains no elements.  Foo!', /con
    return
  endif
  data2 = float(data2)

  data = data2[where(data2 eq data2)]
  data = data[where(finite(data))]

  nelts = n_elements(data) 

  if nelts eq 0 then begin
    message, 'No valid data.  Foo!.', /con
    return
  end

  min = min(data)
  max = max(data)

  nbins = ceil(sqrt(float(nelts)))

  binsize = float(max-min)/nbins
  if binsize eq 0 then begin
    message, 'All data are the same and have value: '+string(max), /con
    return
  endif
  x = (findgen(nbins+1))*binsize+min
  h = histogram(float(data), min = min, max = max, binsize = binsize)
  x = [min(x)-binsize, x, max(x)+binsize]
  h = [0, h, 0]
  if keyword_set(log) then begin
     plot, x, h, psym = 10, xtitle = 'Data Values', ytitle = 'Counts', $
           /ylog, yrange = [7e-1, 1.1*max(h)], /yst, _extra=_extra 
  endif else begin
     plot, x, h, psym = 10, xtitle = 'Data Values', ytitle = 'Counts' $
           , _extra=_extra
     if keyword_set(poly) then begin
        for j = 0, n_elements(h)-1 do begin
           a = [x[j] - binsize/2. $
                , x[j] + binsize/2. $
                , x[j] + binsize/2. $
                , x[j] - binsize/2.]
           b = [0., 0., h[j], h[j]]
           polyfill, a, b, orientation=45,noclip=0
        endfor
     endif        
  endelse

  return
end


