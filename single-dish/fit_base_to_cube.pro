pro fit_base_to_cube $
   , input_file $
   , out_file = output_file $
   , degree = degree $
   , mask = mask_file $
   , show = show
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN FILES
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; TARGET FILE
  if n_elements(output_file) eq 0 then $
     message, 'Need an output file name', /info

; DATA CUBE
  cube = readfits(input_file, hdr)  

  sz = size(cube)

; DEGREE OF POLYNOMIAL TO FIT
  if n_elements(degree) eq 0 then $
     degree = 1

; GET THE MASK, 1=FIT, 0=IGNORE
  if n_elements(mask_file) eq 0 then $
     mask = finite(cube) $
  else begin
;    READ IN THE MASK
     mask = readfits(mask_file, mask_hdr) 

;    ... "INVERT" THE MASK TO GET WHERE ITS OKAY TO FIT
     mask = (mask eq 0) and finite(cube)
  endelse

; EXTRACT THE VELOCITY AXIS FROM THE HEADER
  make_axes, hdr, vaxis=vaxis, /vonly
  vaxis /= 1e3

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; LOOP OVER THE CUBE AND FIT BASELINES TO EACH SPECTRUM
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(show) then begin
     !p.multi=[0,2,2]
  endif

  coeff_ra = fltarr(sz[1],sz[2],degree+1)*!values.f_nan

  pre = cube

  for i = 0, sz[1]-1 do begin
     for j = 0, sz[2]-1 do begin

        spec = cube[i,j,*]

        if total(finite(spec)) eq 0 then continue

;       SMOOTH OUT THE SPECTRUM        
        for k = 0, 2 do $
           spec = smooth(reform(spec),5,/nan)
        
        spec_mask = mask[i,j,*]
        spec_mask_wt = (spec_mask)*1.0
        for k = 0, 2 do $
           spec_mask_wt = smooth(reform(spec_mask_wt),5,/nan)

        mask_ind = where(spec_mask_wt lt 0.75, mask_ct)

        if mask_ct gt sz[3]-10 then $
           continue

        if mask_ct gt 0 then $
           spec[mask_ind] = !values.f_nan

;       ... MEDIAN FILTERING WOULD BE BETTER, BUT IT INTERACTS POORLY WITH
;       BLANKS AND EDGES
;
; for k = 0, 2 do $ spec = median(reform(spec),5)

;       PLOT EVERY 500TH FIT IF REQUESTED
        show_this = (keyword_set(show) and $
                     ((i*sz[2]+j) mod 500) eq 0) ? 1B : 0B
        
;          FIT A BASELINE (EXTRAS GET PASSED TO PLOT)
           coeffs = $
              base_fit(xaxis = vaxis $
                       , yaxis = spec $
                       , degree = degree $
                       , show = show_this $
                       , orig_y = cube[i,j,*] $
                       , fit = fit $
                       , status = status)

           coeff_ra[i,j,*] = coeffs

           base = dblarr(sz[3])
           for k = 0, degree do $
              base += coeffs[k]*vaxis^k

           cube[i,j,*] -= base

     endfor
  endfor

  if keyword_set(show) then begin
     !p.multi=[0,1,1]
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; WRITE OUTPUT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  sxaddpar, hdr, 'HISTORY', 'Fit and subtracted baselines to cube.'

  writefits, output_file, cube, hdr
 
end                             ; of fit_base_to_cube
