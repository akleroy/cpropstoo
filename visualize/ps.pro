pro ps $
   , xwin = xwin $
   , ps = ps $
   , _extra = ex $
   , defaults = defaults $
   , toggle = toggle $
   , verbose = verbose $
   , xsize=xsize $
   , ysize=ysize

;+
; NAME:
;   PS
; PURPOSE:
;   Opens and closes the PS device with appropriate flags. 
;
; CALLING SEQUENCE:
;   PS [, ps = ps, x = x, defaults = defaults, toggle = toggle,
;         verbose = verbose, DEVICE KEYWORDS]
;
; INPUTS:
;  NONE
;
; KEYWORD PARAMETERS: 
;  PS -- switch DEVICE to PS
;  X -- switch device to X
;  If both are set, the device will be toggled.
;  If neither, no effect unless called from $MAIN which will message
;  the device name.
;  DEFAULTS -- Use my favorite calls for the PS command.
;  TOGGLE -- Toggles between PS and X
;  VERBOSE -- Forces a message about device name at end of execution.
; OUTPUTS:
;  None.
;
; MODIFICATION HISTORY:
;       Written.
;       Sun Feb 16 14:54:33 2003, Erik Rosolowsky <eros@cosmic>
;-

  if keyword_set(defaults) then begin
     if n_elements(xsize) eq 0 then $
        xsize = 8
     if n_elements(ysize) eq 0 then $
        ysize = 8
     inches = 1b
     color = 1b
     bits_per_pixel = 8
     yoffset = 1
  endif

  name = !d.name

  if (keyword_set(ps) and keyword_set(xwin)) or keyword_set(toggle) then begin 
    if stregex(name, 'X', /bool) then begin
      set_plot, 'ps'
      device, _extra = ex, xsize = xsize, ysize = ysize, $
        inches = inches, color = color, bits_per_pixel = bits_per_pixel, $
        yoffset = yoffset
    endif else begin
      if stregex(name, 'PS', /bool) then device, /close
      set_plot, 'x', _extra = ex
    endelse

  endif else begin

    if keyword_set(ps) then begin
      set_plot, 'ps'
      device, _extra = ex, xsize = xsize, ysize = ysize, $
        inches = inches, color = color, bits_per_pixel = bits_per_pixel, $
        yoffset = yoffset
    endif
    if keyword_set(xwin) then begin
      if stregex(name, 'PS', /bool) then device, /close
      set_plot, 'X', _extra = ex
    endif
  endelse

  help, calls = calls

  if (not keyword_set(ps) and not keyword_set(x)) or $
    keyword_set(verbose) or n_elements(calls) eq 2 then begin
    message, 'Device set to '+!d.name, /con
    if keyword_set(defaults) then message, 'PS Defaults Used', /con
  endif

  return
end
