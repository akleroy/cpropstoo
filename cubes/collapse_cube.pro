pro collapse_cube $
   , cube=cube $
   , hdr=hdr $
   , vaxis=vaxis $   
   , mask=mask $
   , noise=noise $
   , mom0 = mom0 $
   , e_mom0 = e_mom0 $
   , mom1 = mom1 $
   , mom2 = mom2 $
   , tpeak = tpeak

;+
; NAME:
;
; collapse_cube
;
; PURPOSE:
;
; Combine a cube, mask, and noise measurement to produce a moment 0
; map, an associated uncertainty map, a moment 1 map, a moment 2 map,
; and a peak map.
;
; CATEGORY:
;
; Data processing / science.
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;
; cube : a data cube
;
; mask : a mask (default: finite values of the cube)
;
; noise : a value or cube of the noise ()
;
; CUBE, MASK, and NOISE need to have matched sizes and are assumed to be
; assigned. MASK and NOISE are optional. CUBE is not.
; 
; hdr : hdr
;
; vaxis : vaxis
;
; Either the header of the cube or its velocity axis. Needed for calculation
; of the moments (mom0 needs dv, mom1 and mom2 need the full axis). One or the
; other is acceptable.
;
; KEYWORD PARAMETERS:
;
; None.
;
; OUTPUTS:
;
; mom0 : zeroth moment, includes a channel width factor (e.g., K -> K km/s)
;
; mom1 : first moment (intensity weighted mean)
;
; mom2 : second moment (intensity weighted RMS velocity scatter)
;
; tpeak : peak intensity map
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
; make_axes, mad, peak_map
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

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SET DEFAULTS / ERROR CHECK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  on_error, 2

; MAKE A DEFAULT MASK
  if n_elements(mask) eq 0 then begin
     mask = finite(cube)
  endif

; WORK OUT A NOISE IF NOT SUPPLIED
  if n_elements(noise) eq 0 then begin
     noise = mad(cube)
  endif

; CHECK FOR VELOCITY INFORMATION
  if n_elements(vaxis) eq 0 then begin
     if n_elements(hdr) eq 0 then begin
        message, 'Need a velocity axis or a header.'
     endif else begin
        make_axes, hdr, vaxis=vaxis, /vonly
     endelse
  endif
  dv = abs(vaxis[1] - vaxis[0])

; ASSUME THAT ANY VELOCITY RESOLUTION > 100 IS IN M/S AND CONVERT TO KM/S
; (... COULD CHECK CTYPE3 INSTEAD)
  if dv gt 1d2 then begin
     vaxis /= 1e3
     dv = abs(vaxis[1] - vaxis[0])
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; COLLAPSE THE CUBE INTO A MOMENT-0 MAP AND NOISE MAP
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  sz = size(cube)

; MAKE THE MOMENT 0 MAP
  mom0 = dv * total(cube*mask,3,/nan)

; UNCERTAINTY ON THE MOMENT 0 MAP  
  e_mom0 = dv * sqrt(total(noise^2*mask,3,/nan))

; MAKE THE MOMENT 1 MAP
  vcube = cube*!values.f_nan
  for i = 0, sz[3]-1 do vcube[*,*,i] = vaxis[i]
  mom1 = total(vcube*mask*cube,3,/nan)/total(mask*cube,3,/nan)

; ... TBD: UNCERTAINTY

; MAKE THE MOMENT 2 MAP
  dvcube = cube*!values.f_nan
  for i = 0, sz[3]-1 do dvcube[*,*,i] = vcube[*,*,i] - mom1
  mom2 = sqrt(total(dvcube^2*mask*cube,3,/nan)/total(mask*cube,3,/nan))

; ... TBD: UNCERTAINTY

; PEAK VALUE MAP
  cube_copy = cube
  outside_mask = where(mask eq 0, outside_ct)
  if outside_ct gt 0 then $
     cube_copy[outside_mask] = !values.f_nan
  tpeak = max(cube_copy, dim=3,/nan)

; ... TBD: UNCERTAINTY / PEAK VEL

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN / CLEANUP
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return

end
   
