function decimate_kernels $
   , kernels_in $
   , cube $
   , all_neighbors = all_neighbors $
   , delta = delta $
   , sigma = sigma $
   , snr = delta_is_snr $
   , minval = minval $
   , merger = merger $
   , minpix = minpix $
   , minarea = minarea $
   , minvchan = minvchan $
   , valid_merger = valid_merger $
   , verbose = verbose $
   , ignore_islands = ignore_islands

; - explore the ability to work on a S/N cube
; - there are issues with NMIN right now... currently hard coded

;+
; NAME:
;
;   DECIMATE_KERNELS
;
; PURPOSE:
;
;   To eliminate kernels based on pixels and area uniquely associated
;   with them, minimal extent in velocity direction, and their contrast
;   with the remainder of the emission (in absolute intensity units).
;
; CALLING SEQUENCE:
;
;
; INPUTS:
;
;
;
; KEYWORD PARAMETERS:
;
;
;
; OUTPUTS:
;
;
;
; REQUIRES: 
;
;   MERGEFIND
;
; MODIFICATION HISTORY:
;
;
;-

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; DEFAULTS AND DEFINITIONS
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

; COPY KERNELS TO ALLOW EDITING/AVOID UNFORTUNATE ACCIDENTS
  kernels = kernels_in

; IS THE CONTRAST CUTOFF IN REAL OR SNR UNITS?
  if keyword_set(delta_is_snr) then $
     delta_is_snr = 1B $
  else $
     delta_is_snr = 0B

; MEAN ABSOLUTE DEVIATION (CONVERTED TO SIGMA) OF DATA IN THE CUBE
  if n_elements(sigma) eq 0 then $
     sigma = mad(cube,/finite)

; MINIMUM ABSOLUTE INTENSITY VALUE FOR A MAXIMUM
  if n_elements(minval) eq 0 then $
     minval = 0.0

; MINIMUM NUMBER OF LEVELS IN THE CUBE (*HARDCODED*)
  if n_elements(nmin) eq 0 then $
     nmin = 100

; CONTRAST CRITERIA
  if n_elements(delta) eq 0 then begin
     if delta_is_snr then $
        delta = 2. $
     else $
        delta = 2.*sigma
  endif

; CONVERT THE DELTA CUTOFF INTO A REAL INTENSITY CUTOFF (NECESSARY ATM)
  if delta_is_snr then $
     cutoff = sigma*delta $
  else $
     cutoff = delta

; DEFAULT REJECTION CRITERIA TO 1
  if n_elements(minpix) eq 0 then $
     minpix = 1

  if n_elements(minarea) eq 0 then $
     minarea = 1

  if n_elements(minvchan) eq 0 then $
     minvchan = 1

; INITIALIZE REJECTION COUNTING
  value_rejects = 0
  delta_rejects = 0
  volume_rejects = 0
  area_rejects = 0
  vchan_rejects = 0

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; REJECT ON ABSOLUTE VALUE OF PIXEL
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

  if finite(minval) then begin
     good_value_ind = where(cube[kernels] ge minval, good_value_ct)
     bad_value_ct = n_elements(kernels) - good_value_ct 
     value_rejects = bad_value_ct
     if bad_value_ct gt 0 then begin
        kernels = kernels[good_value_ind]
     endif
  endif else begin
     value_rejects = 0L
  endelse

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; PRECALCULATE THE MERGER MATRIX
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$

  levels = $
     contour_values( $
     cube $
     , /linspace $
     , spacing=0.2*sigma $
     , nmin=nmin) ; HARDCODED ABOVE

  merger_matrix =  $
     mergefind_approx(cube $
                      , kernels $
                      , levels = levels $
                      , all_neighbors = all_neighbors $
                      , verbose = verbose)     
  merger_matrix_nodiag = merger_matrix
  sz_merge = size(merger_matrix)
  ind = lindgen(sz_merge[1])
  merger_matrix_nodiag[ind,ind] = !values.f_nan

; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$
; LOOP AND REJECT
; &$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$&$  

; SORT KERNELS, WE ATTEMPT TO DECIMATE STARTING FROM LOWEST AND
; WORKING TO HIGHEST
  kernel_value = cube[kernels]
  order = sort(kernel_value)

; DEFINE A MASK WHICH FLAGS KERNELS AS GOOD (1) OR BAD (0)
  nkern = n_elements(kernels)
  valid_kernel = intarr(nkern)+1

; LOOP OVER KERNELS IN ORDER OF INTENSITY
  for i = 0, nkern-1 do begin

     if keyword_set(verbose) then begin
        counter, i, nkern, "Checking validity of kernel "
     endif
     
;    FIND VALID KERNELS
     valid_kernel_ind = where(valid_kernel, valid_ct)
     valid_kernels = kernels[valid_kernel_ind]

;    IF OTHER VALID KERNELS REMAIN, CONTRAST WITH THEM
     if valid_ct gt 1 then begin

;       -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;       CALCULATE WHERE THE CURRENT KERNEL MERGES WITH REMAINING VALID
;       KERNELS
;       -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

        merges = merger_matrix_nodiag[order[i], valid_kernel_ind]
        merge_level = max(merges,/nan)
        if finite(merge_level) eq 0 then begin
           mask = cube gt 0.0
        endif else begin
           unique_lev = min(levels[where(levels gt merge_level)],/nan)
;          mask = cube gt merge_level
           mask = cube gt unique_lev
        endelse
        asgn = label_region(mask, /ULONG)
        stat_mask, asgn eq asgn[kernels[order[i]]] $
                   , volume=npixels, area=area, vwidth=vwidth

;       -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
;       CHECK THE REJECTION CRITERIA FOR THE CURRENT KERNEL
;       -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

;       CONTRAST REJECTION
        if (kernel_value[order[i]]-merge_level) lt cutoff then begin
           delta_rejects = delta_rejects+1
           valid_kernel[order[i]] = 0
        endif

;       VOLUME REJECTION
        if npixels lt minpix then begin
           valid_kernel[order[i]] = 0
           volume_rejects = volume_rejects+1
        endif

;       VELOCITY WIDTH REJECTION
        if vwidth lt minvchan then begin
           valid_kernel[order[i]] = 0
           vchan_rejects = vchan_rejects+1
        endif

;       AREA REJECTION
        if area lt minarea then begin
           valid_kernel[order[i]] = 0
           area_rejects = area_rejects+1
        endif

     endif

  endfor

; RETURN STILL-VALID KERNELS
  valid_kernel_ind = where(valid_kernel, valid_ct)
  valid_kernels = kernels[valid_kernel_ind]

; REPORT DECIMATION
  if keyword_set(verbose) then begin
     message, 'Kernels rejected for value: '+string(value_rejects), /con
     message, 'Kernels rejected for contrast: '+string(delta_rejects), /con
     message, 'Kernels rejected for volume: '+string(volume_rejects), /con
     message, 'Kernels rejected for velocity width: '+string(vchan_rejects), /con
     message, 'Kernels rejected for area: '+string(area_rejects), /con
     message, 'Kernels kept: '+string(n_elements(valid_kernels)), /con
  endif

; SAMPLE MERGER MATRIX
  valid_merger = fltarr(valid_ct, valid_ct)*!values.f_nan
  for i = 0, valid_ct-1 do begin
     for j = 0, valid_ct-1 do begin
        valid_merger[i,j] = merger_matrix[valid_kernel_ind[i], valid_kernel_ind[j]]
        valid_merger[j,i] = merger_matrix[valid_kernel_ind[i], valid_kernel_ind[j]]
     endfor
  endfor

  return, valid_kernels
end

