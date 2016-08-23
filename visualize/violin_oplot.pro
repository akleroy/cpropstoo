pro violin_oplot $
   , x, y $
   , midval = midval $
   , scale = scale $
   , horizontal = horizontal $
   , fill = fill $
   , fcolor = fill_color $
   , no_outline = no_outline $
   , lcolor = line_color $
   , quartile = show_quartile $
   , median = show_median $
   , medval_out = medval $
   , low_quart_out = low_quart $
   , high_quart_out = high_quart $
   , _extra = extra 

  q_scale = 0.4
  perc = 0.16

  if keyword_set(horizontal) then begin     

;    Work out the polygon
     poly_x = [x, reverse(x)]
     y_rescale = (y/max(y,/nan))*scale
     nan_ind = where(finite(y_rescale) eq 0, nan_ct)
     if nan_ct gt 0 then y_rescale[nan_ind] = 0.0
     poly_y = [y_rescale, -1.0*reverse(y_rescale)] + midval     

  endif else begin

;    Work out the polygon
     poly_y = [y, reverse(y)]
     x_rescale = (x/max(x,/nan))*scale
     nan_ind = where(finite(x_rescale) eq 0, nan_ct)
     if nan_ct gt 0 then x_rescale[nan_ind] = 0.0
     poly_x = [x_rescale, -1.0*reverse(x_rescale)] + midval     
     
  endelse

; Plot the polygon

  if keyword_set(fill) then begin
     polyfill, [poly_x, poly_x[0]] $
               , [poly_y, poly_y[0]] $
               , /clip, color=fill_color
  endif

; Plot the outline

  if not keyword_set(no_outline) then begin
     oplot, [poly_x, poly_x[0]] $
            , [poly_y, poly_y[0]] $
            , _extra=extra
  endif

; Plot quartile and/or median values

  if keyword_set(show_quartile) or $
     keyword_set(show_median) then begin
     
     if keyword_set(horizontal) then begin

;       Find the quartile values
        sorted_y = y[sort(x)]
        sorted_x = x[sort(x)]
        cumul_y = total(sorted_y, /cumul, /nan)
        cumul_y /= max(cumul_y)

        low_quart = interpol(sorted_x, cumul_y, perc)
        high_quart = interpol(sorted_x, cumul_y, 1.-perc)
        medval = interpol(sorted_x, cumul_x, 0.5)

        q_ind = where((x gt low_quart) and $
                      (x lt high_quart))        

;       Work out the polygon
        quart_x = [x[q_ind], reverse(x[q_ind])]
        y_rescale = (y[q_ind]/max(y,/nan))*scale*q_scale
        nan_ind = where(finite(y_rescale) eq 0, nan_ct)
        if nan_ct gt 0 then y_rescale[nan_ind] = 0.0
        quart_y = [y_rescale, -1.0*reverse(y_rescale)] + midval     
        
     endif else begin

;       Find the quartile values
        sorted_x = x[sort(y)]
        sorted_y = y[sort(y)]
        cumul_x = total(sorted_x, /cumul, /nan)
        cumul_x /= max(cumul_x)

        low_quart = interpol(sorted_y, cumul_x, perc)
        high_quart = interpol(sorted_y, cumul_x, 1.0-perc)
        medval = interpol(sorted_y, cumul_x, 0.5)

        q_ind = where((y gt low_quart) and $
                      (y lt high_quart))        

;       Work out the polygon
        quart_y = [y[q_ind], reverse(y[q_ind])]
        x_rescale = (x[q_ind]/max(x,/nan))*scale*q_scale
        nan_ind = where(finite(x_rescale) eq 0, nan_ct)
        if nan_ct gt 0 then x_rescale[nan_ind] = 0.0
        quart_x = [x_rescale, -1.0*reverse(x_rescale)] + midval     
        
     endelse

; Plot the quartile value

     if keyword_set(show_quartile) then begin

        if keyword_set(fill) then begin
           if n_elements(q_color) eq 0 then $
              q_color = cgcolor('white')
           polyfill, [quart_x, quart_x[0]] $
                     , [quart_y, quart_y[0]] $
                     , /clip, color=q_color
        endif
        
        if not keyword_set(no_outline) then begin
           oplot, [quart_x, quart_x[0]] $
                  , [quart_y, quart_y[0]] $
                     , _extra=extra
        endif

     endif

; Plot the median value

     if keyword_set(show_median) then begin

        if keyword_set(horizontal) then begin
           oplot, [medval], [midval], psym=cgsymcat('filledcircle') $
                  , symsize=1, color=cgcolor('black')
        endif else begin
           oplot, [midval], [medval], psym=cgsymcat('filledcircle') $
                  , symsize=1, color=cgcolor('black')
        endelse

     endif


  endif

end
