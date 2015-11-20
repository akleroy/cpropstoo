pro histplot $
   , x_in $
   , y_in $
   , basey = basey $
   , outline = outline $
   , nobars = nobars $
   , lstyle = outline_style $
   , lthick = outline_thick $
   , lcolor = outline_color $
   , fill = fill $
   , fcolor = fill_color $
   , fline = fill_line $
   , fspacing = fill_spacing $
   , fpattern = fill_pattern $
   , forientation = fill_orient $
   , overplot = overplot $
   , _extra = extra

;+
; NAME:
;
; histplot
;
; PURPOSE:
;
; Plot histograms with options to fill the histogram, draw the
; outline, and/or outline individual bars.
;
; CATEGORY:
;
; Display program.
;
; CALLING SEQUENCE:
;
; histplot, x, y, /overplot
;
; INPUTS:
;
; x : the x values of the bins. Ideally as the edges of the bins, in
; which case there are nbins+1 values. If instead there are nbins
; values, the program will assume regular spacing.
;
; y : the values of the histogram in the bin.
;
; OPTIONAL INPUTS:
;
; basey : the y value from which the histogram is drawn, either a
; single value (default 0.0) or a per-bin value.
;
; lstyle, lcolor, lthick : line plotting parameters for the outline
; and the bar
;
; fcolor, fline, fspacing, fpattern, forientation : plotting
; parameters for the polygon fill
;
; KEYWORD PARAMETERS:
;
; *** Note that all extra parameters are passed to the plot call ***
;
; overplot : do not frame the plot
;
; outline : draw the outline of the histogram
;
; fill : fill the histogram
;
; nobars : suppress outlining individual bars
;
;
; OUTPUTS:
;
;
;
; OPTIONAL OUTPUTS:
;
;
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
;-
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND SETUP
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  nbin = n_elements(y_in)

  if n_elements(x_in) ne nbin+1 then begin
     if n_elements(x_in) eq nbin then begin
        message, "Assuming regular bin spacing.", /info
        delta = x_in[1] - x_in[0]
        x = [x_in[0]-delta/2., x_in+delta/2.]
     endif else begin
        message, "Mismatched input vectors.", /info
        return
     endelse
  endif else begin
     x = x_in
  endelse
  
; Default to a histogram centered at 0
  
  if n_elements(basey) eq 0 then $
     basey = 0.0

  if n_elements(basey) ne n_elements(y) then begin
     basey = basey[0]*(fltarr(nbin)+1.0)
  endif

; Deal with not-a-numbers

  y = y_in  
  nan_ind = where(finite(y_in) eq 0, nan_ct)
  if nan_ct ne 0 then begin
     y[nan_ind] = basey[nan_ind]
  endif
  
; Default to drawing the outline
  if n_elements(outline) eq 0 then $
     outline = 1B
  
; Set plotting defaults using !p values  
  if (n_elements(outline_style) eq 0) then $
     outline_style = !p.linestyle
  if (n_elements(outline_thick) eq 0) then $
     outline_thick = !p.thick
  if (n_elements(outline_color) eq 0) then $
     outline_color = !p.color

  if (n_elements(fill_color) eq 0) then $
     fill_color = !p.color
  if (n_elements(fill_orient) eq 0) then $
     fill_orient = 0
  if (n_elements(fill_spacing) eq 0) then $
     fill_spacing = 0
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; MAKE THE VERTICES THAT WILL BE PLOTTED  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  

; Work out the outlines of the whole histogram
  
  outline_x = fltarr(4*nbin+1)
  outline_y = fltarr(4*nbin+1)

; ... start at 0,0  
  outline_x[0] = x[0]
  outline_y[0] = basey[0]

; ... draw the tops of the histogram  
  for ii = 0, nbin-1 do begin
     outline_x[2*ii+1:2*ii+2] = [x[ii], x[ii+1]]
     outline_y[2*ii+1:2*ii+2] = [y[ii], y[ii]]
  endfor

; ... draws the bottoms of the histogram. This won't necessarily be
; plotted but is needed for the polygon plotting.
  for ii = 0, nbin-1 do begin
     outline_x[2*nbin+2*ii+1:2*nbin+2*ii+2] = [x[nbin-1-ii+1], x[nbin-1-ii]]
     outline_y[2*nbin+2*ii+1:2*nbin+2*ii+2] = [basey[nbin-1-ii], basey[nbin-1-ii]]
  endfor  

; Work out a series of polygons for the individual bars  
  bar_x = fltarr(5,nbin)
  bar_y = fltarr(5,nbin)  

  for ii = 0, nbin-1 do begin
     bar_x[*,ii] = [x[ii], x[ii], x[ii+1], x[ii+1], x[ii]]
     bar_y[*,ii] = [basey[ii], y[ii], y[ii], basey[ii], basey[ii]]
  endfor
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; FRAME THE PLOT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  

; All extra parameters go to the framing plot command
  
  if (NOT keyword_set(overplot)) then begin
     old_pmulti = !p.multi       
     plot, x, y, psym = 10, /nodata, _extra = extra
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; FILLL THE HISTOGRAM
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  

  if keyword_set(fill) and keyword_set(fill_line) eq 0 then begin
     if n_elements(fill_pattern) gt 0 then begin
        polyfill, outline_x, outline_y, color=fill_color $
                  , noclip = 0, pattern=fill_pattern        
     endif else begin
        polyfill, outline_x, outline_y, color=fill_color $
                  , noclip = 0
     endelse
  endif

  if keyword_set(fill_line) then begin
     polyfill, outline_x, outline_y, color = fill_color $
               , /line_fill, spacing = fill_spacing $
               , orient = fill_orient, noclip = 0
  endif
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; PLOT THE BARS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  

  if keyword_set(nobars) eq 0 then begin
     for ii = 0, nbin-1 do begin
        plots, bar_x[*,ii], bar_y[*,ii] $
               , linestyle=outline_style $
               , thick=outline_thick $
               , color=outline_color $
               , clip = [!x.crange[0], !y.crange[0], !x.crange[1], !y.crange[1]] $
               , noclip =0
     endfor
  endif
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; PLOT THE OUTLINE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  

  if keyword_set(outline) then begin
     plots, outline_x[0:2*nbin+1], outline_y[0:2*nbin+1] $
            , linestyle=outline_style $
            , thick=outline_thick $
            , color=outline_color $
            , clip = [!x.crange[0], !y.crange[0], !x.crange[1], !y.crange[1]] $
            , noclip =0
  endif
    
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; REDO THE FRAMING PLOT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  

  if (NOT keyword_set(overplot)) then begin
     new_pmulti = !p.multi
     !p.multi = old_pmulti
     plot, x, y, psym = 10, /nodata, _extra = extra, /noerase
     !p.multi = new_pmulti
  endif

end                             ; of histplot

