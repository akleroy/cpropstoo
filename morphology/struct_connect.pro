function struct_connect $
   , ndim = ndim $
   , all_neighbors = all_neighbors $
   , xy_only = xy_only $
   , z_only = z_only

;+
;
; NAME:
;
; struct_connect
;
; PURPOSE:
;
; Define structuring elements for simple connectivity for use with the
; morphological operations.
;
; CATEGORY
;
; Image morphology tool
;
; CALLING SEQUENCE:
;
; struct = struct_connect(ndim=3, /all_neighbors, /xy_only, /z_only)
;
; INPUTS:
;
; ndim: number of dimensions. Should be 1, 2, or 3
;
; all_neighbors: if set then corners are included
;
; xy_only: for a three-d element, suppress z connectivity. Useful, for
; example, for growing only in the spatial dimension of a data cube.
;
; z_only: for a three-d element, suppress x-y connectivity. Useful,
; for example, for growing only in the spectral dimension of a data
; cube.
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND ERROR CHECKING
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(ndim) eq 0 then begin
     ndim = 3
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; INITIALIZE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if ndim eq 1 then $
     struct = bytarr(3)+1
  if ndim eq 2 then $
     struct = bytarr(3,3)+1
  if ndim eq 3 then $
     struct = bytarr(3,3,3)+1

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEAL WITH CORNERS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; If the all_neighbors keyword is not set, then suppress connectivity
; at the corners.

  if keyword_set(all_neighbors) eq 0 then begin
     for ii = -1, 1 do begin
        for jj = -1, 1 do begin
           if ndim eq 2 then begin       
              if ii ne 0 and jj ne 0 then begin
                 struct[ii+1,jj+1] = 0B
              endif
           endif
           if ndim eq 3 then begin
              for kk = -1, 1 do begin
                 off_center = ((ii ne 0) + (jj ne 0) + (kk ne 0)) ge 2
                 if off_center then $
                    struct[ii+1,jj+1,kk+1] = 0B
              endfor
           endif
        endfor
     endfor
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ALLOW ONE OR TWO-D ONLY CONNECTIVITY IN THREE DIMENSIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; If requested, suppress connection in either the spectral or spatial
; dimensions of a three dimensional element.  

  if keyword_set(xy_only) and ndim eq 3 then begin
     struct[*,*,0] = 0B
     struct[*,*,2] = 0B
  endif

  if keyword_set(z_only) and ndim eq 3 then begin
     for ii = -1, 1 do $
        for jj = -1, 1 do $
           if ii ne 0 or jj ne 0 then $
              struct[ii+1,jj+1,*] = 0B     
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, struct

end
