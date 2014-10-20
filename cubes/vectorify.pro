pro vectorify $
   , cube $
   , mask = mask $
   , x = x $
   , y = y $
   , v = v $
   , t = t $
   , id = id $
   , sz = sz $
   , indvec = indvec

;+
; NAME:
;
;  VECTORIFY
;
; PURPOSE: 
;
;  Takes a cube and (optionally) a mask and extracts a vector of
;  position, data, and identifier values from the cube. Useful for
;  grabbing a small subset of data from a largely empty data cube.
;
; CALLING SEQUENCE:
;
; vectorify, cube, mask = mask, x = x, y = y, v = v, t = t, id = id $
;          , sz = sz, indvec=indvec
;
; INPUTS:
;
; CUBE : the three-dimensional data cube to vectorify.
; 
; MASK : a mask identifying regions of interest within the data
; cube. If mask is set, regions with a value of zero in the mask will
; not be included in the output.
;
; KEYWORD PARAMETERS:
;
;
; OUTPUTS:
;
; X, Y, V : the x, y, and v coordinate, in pixels, of each output
; pixel.
;
; T : the data value of each output pixel.
;
; ID : the mask value (if supplied) for each output pixel.
;
; SZ : the size of the data cube. Necessary if you want to reconstruct
; the data cube using, e.g., cubify.
;
; INDVEC : the vector of indices corresponding to each X,Y,V,T pixel.
;
; MODIFICATION HISTORY:
;
; created on Dec 2, 2004.
; 
; June 12, 2014:  Added 2D ability (sets v vector to all zeros)
;-

; GET THE SIZE OF THE CUBE
  sz = size(cube) 
  
; NOW PICK THE ELEMENTS TO VECTORIZE. NON-ZERO VALUES ONLY IF WE HAVE
; A MASK, OTHERWISE THE WHOLE DAMN THING.
  if (n_elements(mask) gt 0) then $
    indvec = where(mask gt 0) else $
    indvec = lindgen(n_elements(cube))

; FIGURE OUT X, Y, V FROM THE INDEX FOR EACH ELEMENT IN THE VECTOR AND
; THEN RECORD THE INTENSITY (DATA VALUE), AND LABEL (MASK VALUE) FOR
; EACH ELEMENT

  ; Original 3D case
  if sz[0] EQ 3 then begin 
     x = indvec mod sz[1]
     y = (indvec mod (sz[1]*sz[2])) / sz[1]
     v = indvec / (sz[1]*sz[2])
     t = cube[indvec]    
     if (n_elements(mask) gt 0) then $
        id = mask[indvec]
     ; 2D CASE
  endif else if sz[0] EQ 2 then begin 
     x = indvec mod sz[1]
     y = indvec/sz[1]
     v = indvec*0
     t = cube[indvec]    
     if (n_elements(mask) gt 0) then $
        id = mask[indvec]
  endif
  
end                             ; OF VECTORIFY
