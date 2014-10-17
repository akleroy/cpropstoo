function mergefind_approx $
   , cube $
   , kernels $
   , levels = levels_in $
   , spacing = spacing $
   , minlevel = minlevel $
   , maxlevel = maxlevel $
   , nlevels = nlevels $
   , nmin = nmin $
   , all_neighbors = all_neighbors $
   , verbose = verbose

;+
; NAME:
;
;   MERGEFIND_APPROX
;
; PURPOSE:
;
;   Isolate the level at which pairs of kernels merge using fixed
;   contours across the cube.
;
; CALLING SEQUENCE:
;
;   merge_matrix = MERGEFIND(cube, index_maxima [,/ALL_NEIGHBORS ,
;   /EVERYPOINT, NLEVELS=nlevels])
;
; INPUTS:
;   CUBE -- A data cube
;   INDEX_MAXIMA -- The indices of the maxima in the data cube.
;
; KEYWORD PARAMETERS:
;   NLEVELS -- The number of levels to divide the range of the
;              datacube into.
;   EVERYPOINT -- Use every point in the data cube to achieve maximum
;                 accuracy in the contour levels.
;   ALL_NEIGHBORS -- Set this keyword to make pixels have 26 neighbors
;                    and not 6.
;
; OUTPUTS:
;   MERGE_MATRIX -- For N kernels, an N X N matrix with the j,k
;                   element being the contour level where the jth
;                   kernel merges with the kth kernel. 
;                
; MODIFICATION HISTORY:
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; CHECK THAT WE HAVE SOME KERNELS TO TRY MERGING.
  kernel_ct = n_elements(kernels)
  if kernel_ct eq 0 then $
    return, !values.f_nan
  
; DEFINE THE LEVELS (NOTE DEPARTURE FROM ORIGINAL CPROPS)
  if (n_elements(levels_in) eq 0) then begin
     levels = $
        contour_values( $
        cube $
        , /linspace $
        , nlevels=100)     
  endif else begin
     levels = levels_in
  endelse
  nlevels = n_elements(levels)

; CHECK THAT THE LEVELS ARE DESCENDING
  if levels[0] lt levels[n_elements(levels)-1] then begin
     message, "Reversing levels", /info
     levels = reverse(levels)
  endif

; MAKE THE MERGER MATRIX, WHICH CONTAINS THE DATA VALUES ON THE DIAGONAL
; AND SHARED CONTOUR INDICES BETWEEN KERNEL ELEMENTS.

  merger = dblarr(kernel_ct, kernel_ct)+!values.f_nan
  merger[indgen(kernel_ct), indgen(kernel_ct)] = cube[kernels]

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; LOOP OVER LEVELS AND FILL MATRIX
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  levels = reverse(levels)
  for i = 0, nlevels-1 do begin

;    GIVE COUNTER
     if keyword_set(verbose) then begin
        counter, i, nlevels, "Merge calculations for "
     endif

;    THE CURRENT LEVEL
     thresh = levels[i]

;    MASK ABOVE THE CURRENT LEVEL
     mask = cube ge thresh

;    GENERATE THE ASSIGNMENT CUBE AT THIS THRESHOLD
     asgn = label_region(mask, all_neighbors = all_neighbors, /ulong)

;    ASSIGNMENTS OF THE INDIVIDUAL KERNELS
     kern_asgn = asgn[kernels]

;    IF NO KERNELS ARE WITHIN THE MASK, CONTINUE
     if max(kern_asgn) eq 0 then $
        continue

;    HISTOGRAM THE ASSIGNMENT VALUES OF KERNELS TO FIND MULTIPLES
     hist_asgn = histogram(kern_asgn $
                           , min = 1 $
                           , max = max(kern_asgn) $
                           , binsize = 1 $
                           , reverse = ri)
     
;    NOTE CASES WHERE MULTIPLE KERNELS ARE ASSIGNED TO THE SAME VALUE
     multiples = where(hist_asgn gt 1, n_multi)

;    FOR ANY MULTIPLES, FILL IN THE MERGER MATRIX
     if n_multi gt 0 then begin

        for j = 0, n_multi-1 do begin         

;         multiples[j] is the bin in the histogram where you have
;         multiple values. Then ri[multiples[j]] is the starting index
;         of the values in reverse_indices the original array that
;         have that value. These run through ri[multiples[j]+1]-1]. So
;         then SHARED_KERN in the following call is a set of kernels
;         sharing a common assignment.

           shared_kern = ri[ri[multiples[j]]:(ri[multiples[j]+1]-1)]
           n_shared = n_elements(shared_kern)

           for ii = 0L, n_shared-1 do begin
              for jj = 0L, n_shared-1 do begin
                 if shared_kern[ii] ne shared_kern[jj] then $
                    merger[shared_kern[ii], shared_kern[jj]] = thresh
              endfor
           endfor
           
        endfor
     endif
     
  endfor

  return, merger
end
