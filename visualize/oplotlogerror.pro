pro oplotlogerror $
   , x, y, log_e1, log_e2 $
   , _extra = _extra

  if n_elements(log_e2) eq 0 then begin
     oploterror, x, y, (10.^(log_e1)-1.)*y $
                 , _extra=_extra, /hibar
     oploterror, x, y, (1.-1./10.^(log_e1))*y $
                 , _extra=_extra, /lobar
  endif else begin
     oploterror, x, y, (10.^(log_e1)-1.)*x, (10.^(log_e2)-1.)*y $
                 , _extra=_extra, /hibar
     oploterror, x, y, (1.-1./10.^(log_e1))*x, (10.^(log_e2)-1.)*y $
                 , _extra=_extra, /lobar
  endelse
  
end
