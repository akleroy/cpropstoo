pro gauss_model $
   , save_as_idl=save_as_idl $
   , write_pro_file=write_pro_file $
   , outfile=outfile

;+
;
; NAME
;    
;  gauss_model
;
; SYNTAX
;
; gauss_model, /write_pro_file, outfile="calc_gauss_corr.pro"
;
;
; DESCRIPTION
;
; Create a model three dimensional gaussian for use calculating the
; extrapolation factors for a Gaussian. Can save the results as an IDL
; file or write a program that loads them as vectors (compiling this
; should be faster than repeated read/write).
;
; NOTE
;
; This is an infrastructure program and not documented for general
; use.
;
;-

; FWHM OF THE GAUSSIAN
  sig = 20.0

; AXIS SIZE
  nx = 201
  ny = 201
  nz = 201

; CENTER POSITION
  xctr = 100
  yctr = 100
  zctr = 100

; GENERATE AXES
  xax = findgen(nx)-xctr
  yax = findgen(ny)-yctr
  zax = findgen(nz)-zctr
  
; COORDINATE CUBES
  xcube = fltarr(nx,ny,nz)
  ycube = fltarr(nx,ny,nz)
  zcube = fltarr(nx,ny,nz)

  for i = 0, nx-1 do $
     xcube[i,*,*] = xax[i]
  for i = 0, ny-1 do $
     ycube[*,i,*] = yax[i]
  for i = 0, nz-1 do $
     zcube[*,*,i] = zax[i]

; 3-D GAUSSIAN
  intens = exp(-xcube^2/2./sig^2)*exp(-ycube^2/2./sig^2)*exp(-zcube^2/2./sig^2)
  
; LOOP AND MEASURE AS A FUNCTION OF PEAK TO EDGE RATIO
  edge = (findgen(1000)+1.)/1000.*0.9
  n_edge = n_elements(edge)
  peak_to_edge = max(intens)/edge

  sig_1d = fltarr(n_edge)
  sig_2d = fltarr(n_edge)
  area = fltarr(n_edge)
  delta = fltarr(n_edge)
  flux = fltarr(n_edge)

  for i = 0, n_edge-1 do begin 
     counter, i, n_edge, "Considering level "

     mask = intens gt edge[i]

     mom1_x = total(mask*intens*xcube)/total(mask*intens)
     mom1_y = total(mask*intens*ycube)/total(mask*intens)
     mom1_z = total(mask*intens*zcube)/total(mask*intens)

     mom2_x = sqrt(total(mask*intens*(xcube - mom1_x)^2)/total(mask*intens))
     mom2_y = sqrt(total(mask*intens*(ycube - mom1_y)^2)/total(mask*intens))
     mom2_z = sqrt(total(mask*intens*(zcube - mom1_z)^2)/total(mask*intens))

     flux[i] = total(intens*mask)
     sig_1d[i] = mom2_z
     sig_2d[i] = sqrt(mom2_x*mom2_y)     
     area[i] = total(total(mask,3) gt 0)
     delta[i] = total(total(total(mask,1),1) gt 0)
  endfor

; CALCULATE CORRECTION FACTORS
  corr_sig = sig / sig_1d

  delta_halfmax = total(total(total(intens gt 0.5*max(intens),1),1) gt 0)
  corr_delta = delta_halfmax / delta

  area_halfmax = total(total(intens gt 0.5*max(intens),3) gt 0)
  corr_area = area_halfmax / area

  corr_flux = total(intens,/nan) / flux

; SAVE TO AN IDL FILE (IF REQUESTED)
  if keyword_set(save_as_idl) then begin
     save, file="corrfac.idl", peak_to_edge, corr_1d, corr_delta, corr_area, corr_flux
  endif

; WRITE AN IDL PROGRAM (IF REQUESTED)
  if keyword_set(write_pro_file) then begin

     if n_elements(outfile) eq 0 then outfile = "calc_gauss_corr.pro"
     
     openw, 1, outfile
     printf, 1, "pro calc_gauss_corr, peak_to_edge, corr_1d=corr_sig, corr_delta=corr_delta, corr_area=corr_area, corr_flux=corr_flux"
     printf, 1, "p2e_ra = [ $ "
     printf, 1, peak_to_edge[0], " $"
     for i = 1, n_elements(peak_to_edge)-1 do printf, 1, ", ", peak_to_edge[i], " $"
     printf, 1, "] "
     printf, 1, "corr_sig_ra = [ $ "
     printf, 1, corr_sig[0], " $"
     for i = 1, n_elements(peak_to_edge)-1 do printf, 1, ", ", corr_sig[i], " $"
     printf, 1, "] "
     printf, 1, "corr_area_ra = [ $ "
     printf, 1, corr_area[0], " $"
     for i = 1, n_elements(peak_to_edge)-1 do printf, 1, ", ", corr_area[i], " $"
     printf, 1, "] "
     printf, 1, "corr_delta_ra = [ $ "
     printf, 1, corr_delta[0], " $"
     for i = 1, n_elements(peak_to_edge)-1 do printf, 1, ", ", corR_delta[i], " $"
     printf, 1, "] "
     printf, 1, "corr_flux_ra = [ $ "
     printf, 1, corr_flux[0], " $"
     for i = 1, n_elements(peak_to_edge)-1 do printf, 1, ", ", corr_flux[i], " $"
     printf, 1, "] "
     printf, 1, "corr_sig = interpol(corr_sig_ra, p2e_ra, peak_to_edge)"
     printf, 1, "corr_area = interpol(corr_area_ra, p2e_ra, peak_to_edge)"
     printf, 1, "corr_delta = interpol(corr_delta_ra, p2e_ra, peak_to_edge)"
     printf, 1, "corr_flux = interpol(corr_flux_ra, p2e_ra, peak_to_edge)"
     printf, 1, "end"
     close, 1

  endif

  stop

end
