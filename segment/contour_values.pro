function contour_values $
   , data $
   , allvals = allvals $
   , histeq = histeq $
   , linspace = linspace $
   , logspace = logspace $
   , nlevels = nlevels $
   , nmin = nmin $
   , spacing = spacing $
   , minval = minval $
   , maxval = maxval

;+
; NAME:
;
;   CONTOUR_VALUES
;
; PURPOSE:
;   
; CALLING SEQUENCE:
;
; INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
;
; MODIFICATION HISTORY:
;
;-


; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFINITIONS AND DEFAULTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; MINIMUM NUMBER OF LEVELS
  if n_elements(nmin) eq 0 then $
     nmin = 0

; MINIMUM VALUE
  if n_elements(minval) eq 0 then $
     minval = min(data, /nan)

; MAXIMUM VALUE
  if n_elements(maxval) eq 0 then $
     maxval = max(data, /nan)

; DEFAULT TO LINEARLY SPACED LEVELS
  if keyword_set(allvals) eq 0 and $
     keyword_set(histequal) eq 0 and $
     keyword_set(logspace) eq 0 then begin
     linspace = 1B
  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; RETURN THE UNIQUE, SORTED DATA
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
  
  if (n_elements(data) lt nmin) or keyword_set(allvals) then begin

;    SORT THE DATA AND KEEP UNIQUE, FINITE VALUES
     levels = data[sort(data)]
     keep = where(finite(data), ct)
     if ct eq 0 then $
        return, !values.f_nan
     levels = levels[keep]
     levels = (levels > minval) < maxval
     levels = (levels[uniq(levels)])
     levels = reverse(levels)

;    RETURN
     return, levels

  endif 

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; EVENLY SPACE LEVELS THROUGH THE DATA
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if keyword_set(histequal) then begin

;    TARGET NUMBER OF LEVELS
     if n_elements(nlevels) eq 0 then begin
        nlevels = (100 > nmin)
     endif
     
;    SORT THE DATA AND KEEP UNIQUE, FINITE VALUES
     levels = data[sort(data)]
     keep = where(finite(data), ct)
     if ct eq 0 then $
        return, !values.f_nan
     levels = levels[keep]
     levels = (levels > minval) < maxval
     levels = (levels[uniq(levels)])     

;    EVENLY SPACE CONTOURS THROUGH THE SORTED DATA
     level_index = findgen(nlevels)/(nlevels-1)*n_elements(levels)
     levels = levels[level_index]
     levels = reverse(levels)

;    RETURN
     return, levels
  endif
  
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; SPACE CONTOURS LINEARLY
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if keyword_set(linspace) then begin     

;    TARGET NUMBER OF LEVELS IN THE ABSENCE OF SPACING
     if n_elements(nlevels) eq 0 and n_elements(spacing) eq 0 then begin
        nlevels = (100 > nmin)
     endif

;    SET THE LEVEL SPACING
     if n_elements(spacing) eq 0 then begin        
        spacing = (maxval - minval)/(nlevels-1)
     endif

;    THE LEVELS
     levels = minval + findgen(floor((maxval-minval)/spacing)+1)*spacing
     levels = reverse(levels)
     
;    RETURN
     return, levels

  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; SPACE CONTOURS LOGARITHMICALLY
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if keyword_set(logspace) then begin
    
;    MINIMUM LOG
     if n_elements(minval) eq 0 then $
        minval = 10.^min(alog10(data), /nan)
     
;    MAXIMUM LOG
     if n_elements(maxval) eq 0 then $
        maxval = 10.^max(alog10(data), /nan)
  
;    TARGET NUMBER OF LEVELS IN THE ABSENCE OF SPACING
     if n_elements(nlevels) eq 0 and n_elements(spacing) eq 0 then begin
        nlevels = (100 > nmin)
     endif

;    SET THE LEVEL SPACING
     if n_elements(spacing) eq 0 then begin        
        spacing = (alog10(maxval) - alog10(minval))/(nlevels-1)
     endif

;    THE LEVELS
     levels = alog10(minval) + $
              findgen(floor((alog10(maxval)-alog10(minval))/spacing)+1)*spacing
     levels = reverse(levels)
     levels = 10.^levels
     
;    RETURN
     return, levels     

  endif 

end
