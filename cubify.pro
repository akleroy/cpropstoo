pro cubify $
   , x = x $
   , y = y $
   , v = v $
   , t = t $
   , size = szin $
   , id = id $
   , indvec = indvec $
   , pad = pad $
   , cube = cube $
   , mask = mask $
   , indcube = indcube $
   , location = location $
   , dim_2d = twod

;+
; NAME:
;
;   CUBIFY
;
; PURPOSE:
;
;   Converts vectorized arrays and IDs into cubes. If a size vector is
;   supplied, then build a cube of that size with (0,0,0) at the
;   bottom left corner. Otherwise, figure out a minimum spanning cube
;   (optionally with padding) that holds the given x, y, and v.
;
; CALLING SEQUENCE:
;
;   CUBIFY, x=x, y=y, v=v, temp=temp, size=size, CUBE = cube, MASK = mask, 
;           INDCUBE = indcube, ID = id, INDVEC = indvec,
;           PAD = pad, LOCATION = location, DIM_2d = dim_2d
;
; INPUTS:
;
;   X,Y,V -- Pixel coordinates (zero indexed) of elements in a datacube
;   T -- Brigthness units to fill cube with.
;
; KEYWORD PARAMETERS:
;
;   SIZE -- Size structure characterizing the datacube with
;           conventions following a call to size(CUBE, /dim), i.e.,
;           the vector has ndim elements with entries (xsize, ysize,
;           vsize).
;
;   PAD -- pad the output cube by this value. Use this if you want to
;          guarantee one or more rows of zeros around the edge of the
;          cube (e.g., to avoid wraps in labeling or shifting).
;
;   ID -- Vector of cloud identifications. If this is set then these
;         values are written into the MASK output. Else MASK holds 1s
;         and 0s indicating where data have been filled in.
;
;   DIM_2D -- Set this flag to force "cubification" to a 2-D matrix.
;           Mostly for compatibility with routines outside the CPROPS
;           distribution. 
;
;   INDVEC -- An optional input that holds the index of the pixel in
;             the original (parent) cube. It will be filled in to the
;             optional output INDCUBE.
;
; OUTPUTS:
;
;   CUBE -- Set this to named variable that will contain the output
;           data cube.
;
;   MASK -- Set this to named variable that will contain an output
;           mask array. If ID is not set, then mask defaults to a BYTE
;           array with a value of 1B where every value of T was
;           set. If ID is set then MASK will hold the assignment
;           values from the indcube.
;
;   INDCUBE -- Set this to a named variable containing the location of
;              a given pixel in the parent cube, which must be
;              supplied by INDVEC. If INDVEC is not specified, then
;              INDCUBE is full of -1.
;
;   LOCATION -- Set this to a named variable that will contain a
;               vector that indicates position of the input
;               (vectorized x, y, v) data in the newly created cube.
;
; MODIFICATION HISTORY:
;
;       Thu Dec 2 12:29:39 2004, Erik Rosolowsky <eros@cosmic>
;		Written.
;
;       Revision and documentation for CPROPSTOO freeze. - aleroy 2014.
;
; TO DO:
;
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; CHECK THAT WE HAVE THE MINIMUM INPUTS
  if n_elements(x) eq 0 or n_elements(y) or n_elements(t) eq 0 then begin
     message, "Not enough information to form cube or image. Require at least {X, Y, T} ", /info
     return
  endif

; CHECK WHETHER WE HAVE INFO ON THE THIRD DIMENSION
  if n_elements(v) eq 0 then begin
     if keyword_set(twod) eq 0 then begin
        message, "Assuming two dimensions.", /info
     endif
  endif

; CHECK WHETHER THE SIZE HAS BEEN FORCED
  if n_elements(szin) gt 0 then begin

;    ERROR CHECKING
     if n_elements(szin) eq 2 then begin
        twod = 1B
     endif else begin
        if n_elements(szin) ne 3 then begin
           message, "Expect 2 or 3 dimensions. Check SIZE parameter.", /info
           return
        endif
     endelse
          
;    INFORMATIONAL
     if keyword_set(pad) then $
        print, 'Keyword PAD has no effect when SZ is supplied. Ignoring.'

;    WITH THE SIZE FORCED, THE MINIMA ARE ZERO BY CONSTRUCTION
     minx = 0
     miny = 0
     minv = 0

;    RECAST SIZE
     sz = szin

  endif else begin

;    IF SIZE IS NOT SUPPLIED FORM THE CUBE TO SPAN THE MINIMUM RANGE
;    OF X, Y, V PLUS ANY PADDING
     message, 'No size information.  Assuming minimum size.', /con

;    FIGURE OUT THE MINIMUM SIZE OF THE (POSSIBLY PADDED) CUBE
     if keyword_set(pad) eq 0 then $
        pad = 0

     sz = [max(x)-min(x)+1+2*pad $
           , max(y)-min(y)+1+2*pad]
     minx = min(x) - pad
     miny = min(y) - pad

     if keyword_set(twod) eq 0 then begin
        sz = [sz, max(y)-min(y)+1+2*pad]
        minv = min(v) - pad
     endif

  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; GENERATE OUTPUTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Generate the cube, mask, index output cube, and the vector of
; locations.

  if keyword_set(twod) then begin

     cube = $
        make_array(sz[0], sz[1], type = size(t, /type))* $
        !values.f_nan

     mask = make_array(sz[0], sz[1], type = size(id, /type) > 1)

     indcube = make_array(sz[0], sz[1], $
                          type = size(indvec, /type) > 1)-1

     location = lonarr(n_elements(x))-1L
     
  endif else begin

     cube = $
        make_array(sz[0], sz[1], sz[2], type = size(t, /type))* $
        !values.f_nan

     mask = make_array(sz[0], sz[1], sz[2], type = size(id, /type) > 1)

     indcube = make_array(sz[0], sz[1], sz[2], $
                          type = size(indvec, /type) > 1)-1

     location = lonarr(n_elements(x))-1L

  endelse

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; FILL OUT OUTPUT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(twod) then begin

;   .......................................
;   TWO DIMENSIONAL CASE
;   .......................................

;    FIND WHERE THE DATA FIT INTO THE IMAGE
     ind = where(((x-minx) lt sz[0]) and $
                 ((x-minx) ge 0) and $
                 ((y-miny) lt sz[1]) and $
                 ((y-miny) ge 0), ct)

     if ct eq 0 then return

;   PLACE THE DATA INTO THE (POSSIBLY PADDED) ARRAY
     cube[x[ind]-minx, y[ind]-miny] = t[ind]

;   FILL OUT THE MASK (WITH THE ASSIGNMENT CUBE IF SUPPLIED, OTHERWISE
;   WITH 1s WHERE WE HAVE DATA)
     if (n_elements(id) eq n_elements(x)) then $
        mask[x[ind]-minx, y[ind]-miny] = id[ind] $
     else $
        mask[x[ind]-minx, y[ind]-miny] = 1b
     
;   IF INDVEC HAS BEEN SUPPLIED THEN FILL OUT THE INDEX CUBE MAPPING
;   BACK TO THE ORIGINAL CUBE
     if (n_elements(indvec) eq n_elements(x)) then $
        indcube[x[ind]-minx, y[ind]-miny] = indvec[ind] $
     else $
        indcube[x[ind]-minx, y[ind]-miny] = -1

;   WORK OUT THE LOCATION OF THE VECTORIZED DATA IN THE NEW CUBE
     indexcube = lindgen(sz[0], sz[1])
     location[ind] = indexcube[x[ind]-minx, y[ind]-miny]

  endif else begin

;   .......................................
;   THREE DIMENSIONAL CASE
;   .......................................

;    FIND WHERE THE DATA FIT INTO THE IMAGE
     ind = where(((x-minx) lt sz[0]) and $
                 ((x-minx) ge 0) and $
                 ((y-miny) lt sz[1]) and $
                 ((y-miny) ge 0) and $
                 ((v-miny) lt sz[2]) and $
                 ((v-miny) ge 0), ct)
     
     if ct eq 0 then return

;   PLACE THE DATA INTO THE (POSSIBLY PADDED) ARRAY
     cube[x[ind]-minx, y[ind]-miny, v[ind]-minv] = t[ind]

;   FILL OUT THE MASK (WITH THE ASSIGNMENT CUBE IF SUPPLIED, OTHERWISE
;   WITH 1s WHERE WE HAVE DATA)
     if (n_elements(id) eq n_elements(x)) then $
        mask[x[ind]-minx, y[ind]-miny, v[ind]-minv] = id[ind] $
     else $
        mask[x[ind]-minx, y[ind]-miny, v[ind]-minv] = 1b

;   IF INDVEC HAS BEEN SUPPLIED THEN FILL OUT THE INDEX CUBE MAPPING
;   BACK TO THE ORIGINAL CUBE
     if (n_elements(indvec) eq n_elements(x)) then $
        indcube[x[ind]-minx, y[ind]-miny, v[ind]-minv] = indvec[ind] $
     else $
        indcube[x[ind]-minx, y[ind]-miny, v[ind]-minv] = -1

;   WORK OUT THE LOCATION OF THE VECTORIZED DATA IN THE NEW CUBE
     indexcube = lindgen(sz[0], sz[1], sz[2])
     location[ind] = indexcube[x[ind]-minx, y[ind]-miny, v[ind]-minv]

  endelse

  return

end

