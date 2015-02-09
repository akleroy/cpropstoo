function struct_round $
   , rad = rad $
   , cylinder = cylinder $
   , ndim = ndim $
   , xy_only = xy_only $
   , z_only = z_only

;+
;
; NAME:
;
; struct_round
;
; PURPOSE:
;
; Define round structuring elements for use with morphological
; operations. Can make a ball or a cylinder in 2 or 3 dimensions. Some
; versions are going to return trivial results (e.g., /z_only)
;
; METHOD:
;
; Looks at distance from the origin and turns on pixels <= the radius
; away.
;
; CATEGORY
;
; Image morphology tool
;
; CALLING SEQUENCE:
;
; struct = struct_round(rad=rad, /cylinder, /connect, ndim=3, /xy_only, /z_only)
;
; INPUTS:
;
; rad: radius 
;
; ndim: number of dimensions. Should be 1, 2, or 3.
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

  if n_elements(rad) eq 0 then begin
     message, "Require a radius.", /info
     return, !values.f_nan
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; INITIALIZE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  npix = rad*2+1

  if ndim eq 1 then begin
;    The one-d case is trivial, return from here.
     struct = bytarr(npix)+1
     return, struct
  endif
  if ndim eq 2 then $
     struct = bytarr(npix,npix)
  if ndim eq 3 then $
     struct = bytarr(npix,npix,npix)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; IDENTIFY PIXELS INSIDE THE "BALL" STRUCTURING ELEMENT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; This step gives us a ball-like structuring element in either two or
; three dimensions. We will prune it further below.

  for ii = -rad, rad do begin
     for jj = -rad, rad do begin
        if ndim eq 2 then begin       
           if ii*ii+jj*jj le rad*rad then begin
              struct[ii+rad,jj+rad] = 1B
           endif
        endif
        if ndim eq 3 then begin
           for kk = -rad, rad do begin
              if ii*ii+jj*jj+kk*kk le rad*rad then begin
                 struct[ii+rad,jj+rad,kk+rad] = 1B
              endif
           endfor
        endif
     endfor
  endfor
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ALLOW ONE OR TWO-D ONLY CONNECTIVITY AND IMPLEMENT CYLINDERS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; If requested, suppress connection in either the spectral or spatial
; dimensions of a three dimensional element.  

; Suppress all z-connectivity
  if keyword_set(xy_only) and ndim eq 3 then begin
     struct[*,*,0:(npix-1)] = 0B
     struct[*,*,(npix+1):*] = 0B
  endif

; Suppress all xy-connectivity (not super sensible, but valid)
  if keyword_set(z_only) and ndim eq 3 then begin
     for ii = -rad, rad do $
        for jj = -rad, rad do $
           if ii ne 0 or jj ne 0 then $
              struct[ii+rad,jj+rad,*] = 0B     
  endif
  
; Implement a cylinder (all planes resemble the midplane) instead of a
; ball by copying the midplane to the rest of the cube.
  if keyword_set(cylinder) and ndim eq 3 then begin
     for kk = -rad, rad do begin
        struct[*,*,kk] = struct[*,*,0]
     endfor
  endif
  
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, struct

end
