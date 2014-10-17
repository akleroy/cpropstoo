pro disp, im, xin, yin, min = mn, max = mx, position = pos, $
          _ref_extra = ex, noerase = noerase, noplot = noplot, $
          radec = radec, n_ticks = n_ticks, aspect = aspect, $
          reserve = reserve, squarepix = squarepix, tvtop=tvtop, $
          missing = missval, true=true
;+
; NAME:
;   disp
; PURPOSE:
;   Display pixel images using TVSCL with PLOT-like features.
;
; CALLING SEQUENCE:
;   DISP, image [, xin, yin]
;
; INPUTS:
;   IMAGE - The array to be displayed.  
;   XIN - Vector containing the x-values correponding to the array's
;         first axis
;   YIN - Vector containing the y-values correponding to the array's
;         second axis
;
; KEYWORD PARAMETERS:
;   MIN/MAX - Minimum and maximum values for color scale.
;   NOPLOT - Doesn't plot anything, just passes back axis setup
;            through the keywords.
;   RADEC -- Set this keyword to plot RA and DEC axes with good
;            formatting for the axis labelling.  The plotting routine
;            assumes that the RA is on the X axis and the DEC is on
;            the Y axis.
;   ASPECT -- Force aspect ratio (Y_SIZE/X_SIZE) to be a given value.
;             For a square plot, regardless of device size use ASPECT=1
;   SQUAREPIX -- Forces pixels to be square.  Overrides use of ASPECT keyword.
;   RESERVE -- Reserve the top N colors of the table for plotting.
;   TVTOP -- plot the TV on top
;   MISSING -- color to plot for nans
;
;   Passed to PLOT command:
;   POSITION, XTICKFORMAT, YTICKFORMAT, XTITLE, YTITLE,
;   XCHARSIZE, YCHARSIZE, TITLE, CHARSIZE, CHARTHICK, COLOR, FONT,
;   SUBTITLE, THICK, THICKLEN, [XYZ]THICK, [XYZ]TICKLEN,
;   [XYZ]TICKS
;
; OUTPUTS:
;   NONE (pretty pictures!)
;
; MODIFICATION HISTORY:
;
;       TBD : better handling of xstyle and ystyle (to enable easy
;       contouring); matching axes to middle-of-pixel
;
;       2007 - 2008 leroy@mpia.de : games with missing and plotting order. If
;              you wan the TV to cover the tickmarks use /tvtop
;
;       Fri Apr 18 14:53:24 2003, Adam Leroy <aleroy@astrop>
;       Improved ASPECT keyword.		
;
;       Added
;       RESERVE keyword Sun Oct 20 17:45:38 2002, <eros@master>
;
;       Introduced ASPECT keyword.
;       Wed Sep 18 15:46:43 2002, Adam Leroy <aleroy@astro>
;
;       Added in /NOPLOT keyword.
;       Attempted compatability with !P.MULTI array
;       Mon Jun 10 14:45:15 2002, Erik Rosolowsky <eros@cosmic>
;
;       Trapped passing non 2-d arrays and complex variables.
;       Wed Jul 25 10:13:26 2001, Erik Rosolowsky <eros@cosmic>
;		
;       Allowed min and max values outside data range with appropriate
;       Color table stretches.
;       Tue Jul 10 13:36:55 2001, Erik Rosolowsky <eros@cosmic>
;
;       Added Color Compatibility with PS device. --
;       Thu Jan 25 17:48:46 2001, Erik Rosolowsky <eros@cosmic>
;
;       Initial Documentation -- Thu Oct 5 22:06:32 2000, Erik
;                                Rosolowsky <eros@cosmic>
;
;-

  if n_elements(im) eq 0 then begin
    message, 'No Image to display.  Returning...', /continue
    return
  endif

  if n_elements(size(im, /dim)) ne 2 then begin
    message, 'Image not two dimensions.  Returning...', /continue
    return
  endif

  if size(im, /tname) eq 'COMPLEX' then begin
    message, 'Complex array! Plotting norm.', /con
    im = float(sqrt(conj(im)*im))
  endif
  if not keyword_set(noplot) then $ 
    if ((not keyword_set(noerase)) and (!p.multi[1]*!p.multi[2] le 0)) or $
      ((!p.multi[0] eq 0) and not keyword_set(noerase)) then erase
  if not keyword_set(reserve) then reserve = 0

  reserve = reserve+2 < !d.table_size-2

  image = im
  imsize = size(image)

  if keyword_set(squarepix) then aspect = float(imsize[2])/imsize[1]

  if (n_elements(xin) eq 0) then xin = findgen(imsize[1]+1)
  if (n_elements(yin) eq 0) then yin = findgen(imsize[2]+1)
  x = xin
  y = yin

; Eliminate pathological pixels, set to background color.
  err_pix = where(finite(image) ne 1)
  if err_pix[0] ne -1 then image(err_pix) = min(image, /nan)

  if keyword_set(pos) ne 1 then begin
    plot, [0, 1], /noerase, /nodata, xst = 4, yst = 4
    pos = [!x.window[0], !y.window[0], !x.window[1], !y.window[1]]
  endif
  if (n_elements(x) eq 0) then x = findgen(imsize[1])
  if (n_elements(y) eq 0) then y = findgen(imsize[2])
  if n_elements(mn)  then begin
    subs = where(image le mn)
    if subs[0] ne -1 then image(subs) = mn
  endif else mn = min(image, /nan)
  if n_elements(mx) gt 0 then begin
    subs = where(image ge mx)
    if subs[0] ne -1 then image(subs) = mx
  endif else mx = max(image, /nan)
  sfac = 1

  x0 = !d.x_size*pos[0]
  y0 = !d.y_size*pos[1]
  x1 = !d.x_size*pos[2]
  y1 = !d.y_size*pos[3]

  if n_elements(aspect) gt 0 then begin
    deltay = y1 - y0            ; Y SPACE AVAILABLE
    deltax = x1 - x0            ; X SPACE AVAILABLE

    if (aspect ge 1.) then begin
      x1 = x0 + deltay/aspect
      new_deltax = deltay/aspect ; NEW X SIZE
      if (new_deltax gt deltax) then begin
        scaledown = deltax/new_deltax
        x1 = x0 + deltay/aspect*scaledown
        y1 = y0 + deltay*scaledown
      endif
    endif else begin
      y1 = y0 + deltax*aspect
      new_deltay = deltax*aspect
      if (new_deltay gt deltay) then begin
        scaledown = deltay/new_deltay
        y1 = y0 + deltax*aspect*scaledown
        x1 = x0 + deltax*scaledown
      endif
    endelse

    pos = [x0/!d.x_size, y0/!d.y_size $
           , x1/!d.x_size, y1/!d.y_size]

  endif


  scale = (!d.table_size-reserve)
  bytim = floor((image-mn)*(!d.table_size-reserve)/(mx-mn))
  ind = where(bytim ge (!d.table_size-reserve))
  if total(ind) gt -1 then bytim[ind] = !d.table_size-reserve-1
  ind = where(bytim lt 0)
  if total(ind) gt -1 then bytim[ind] = 0
  bytim = byte(bytim)

  if n_elements(missval) gt 0 then begin
      if err_pix[0] ne -1 then $
        bytim[err_pix] = missval
  endif

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; TV PLOT
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

  if not keyword_set(tvtop) then begin
     if (!d.name eq 'PS') then begin
        if not keyword_set(noplot) then begin
           tv, bytim, x0/!d.x_px_cm, y0/!d.y_px_cm $
               , xsize = (x1-x0)/!d.x_px_cm, $ 
               ysize = (y1-y0)/!d.y_px_cm, /centimeters
        endif
     endif else begin
        bytim = congrid(bytim, ((x1-x0)/sfac), ((y1-y0)/sfac))
        if not keyword_set(noplot) then tv, bytim, x0, y0
     endelse
  endif

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; PLOT CALL
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

  if keyword_set(radec) then begin
    ra_names, x, tick_name = xtn, tick_value = xtv, $
              n_ticks = n_ticks, incr = incrx
    dec_names, y, tick_name = ytn, tick_value = ytv, $
               n_ticks = n_ticks, incr = incry
    xtks = ceil(abs(x[n_elements(x)-1]-x[0])/abs(incrx)+1)
    ytks = ceil(abs(y[n_elements(y)-1]-y[0])/abs(incry)+1)
    plot, x, y, /noerase, /nodata, position = pos, $
          xstyle = 1+4*keyword_set(noplot), ystyle = 1+4*keyword_set(noplot), $
          _extra = ex, xrange = [x[0], x[n_elements(x)-1]], $
          yrange = [y[0], y[n_elements(y)-1]], xtickname = xtn, ytickname = ytn, $
          xticks = n_elements(xtv)-2, $
          yticks = n_elements(ytv)-2, xminor = 4, yminor = 4, ytickv = ytv, $
          xtickv = xtv

  endif else begin
    plot, x, y, /noerase, /nodata, position = pos, $
          xstyle = 1+4*keyword_set(noplot), ystyle = 1+4*keyword_set(noplot), $
          _extra = ex, xrange = [x[0], x[n_elements(x)-1]], $
          yrange = [y[0], y[n_elements(y)-1]]
  endelse

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; TV PLOT (IF REQUESTED ON TOP)
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

  if keyword_set(tvtop) then begin
     if (!d.name eq 'PS') then begin
        if not keyword_set(noplot) then $
           tv, bytim, x0/!d.x_px_cm, y0/!d.y_px_cm $
               , xsize = (x1-x0)/!d.x_px_cm, $ 
               ysize = (y1-y0)/!d.y_px_cm, /centimeters
     endif else begin
        bytim = congrid(bytim, ((x1-x0)/sfac), ((y1-y0)/sfac))
        if not keyword_set(noplot) then tv, bytim, x0, y0
     endelse
  endif

  if keyword_set(noplot) then return 
  if total(!p.multi[1:2]) gt 0 then begin
    if !p.multi[0] le 0 then !p.multi[0] = (!p.multi[1]*!p.multi[2])
    !p.multi[0] = !p.multi[0]-1
  endif
  return
end


