pro oplotlogerror $
   , x, y, log_e $
   , _extra = _extra
  
  oploterror, x, y, (10.^(log_e)-1.)*y $
              , _extra=_extra, /hibar
  oploterror, x, y, (1.-1./10.^(log_e))*y $
              , _extra=_extra, /lobar
  
end
