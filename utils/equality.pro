pro equality, _extra = ex, npoints = npoints, slope = slope
; NAME:
;    EQUALITY
; PURPOSE:
;    Plot the line of equality (y=x) on the plot.
;
; CALLING SEQUENCE:
;    EQUALITY [,plot_keywords]
;
; INPUTS:
;    None.
;
; KEYWORD PARAMETERS:
;    NPOINTS -- Number of points on the interpolated line. (Default=30)
;    All others passed to the OPLOT routine.
;
; OUTPUTS:
;    None.
;
; MODIFICATION HISTORY:
;
;       Tue May 6 19:13:51 2003, Erik Rosolowsky <eros@cosmic>
;		Written
;
;-


  if n_elements(npoints) eq 0 then npoints = 30
  if n_elements(slope) eq 0 then slope = 1.
  if npoints le 0 then npoints = 1 ; Stupid, stupid rat creatures
  
  bottom = (!x.type ? 1e1^!x.crange[0] : !x.crange[0]) > $
           (!y.type ? 1e1^!y.crange[0] : !y.crange[0])
  top = (!x.type ? 1e1^!x.crange[1] : !x.crange[1]) < $
        (!y.type ? 1e1^!y.crange[1] : !y.crange[1])

  xline = findgen(npoints+1)/npoints*(top-bottom) + bottom
  yline = findgen(npoints+1)/npoints*(top-bottom)*slope + bottom
  oplot, xline, yline, _extra = ex

  return
end

