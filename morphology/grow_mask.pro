function grow_mask $
   , mask_in $                  
   , iters = iters $
   , constraint = constraint $
   , radius = radius_in $
   , xy_only = xy_only $
   , z_only = z_only $
   , no_edges = no_edges $
   , keep_mask = keepmask $
   , all_neighbors = all_neighbors $
   , quiet = quiet

;+
; NAME:
;
; grow_mask
;
; PURPOSE:
;
; Expand a byte mask (binary dilation) in one of three ways:
;
; 1) by a specified radius in pixels
;
; 2) via a set number of iterations. At each iteration, all pixels
; adjacent to the mask enter the mask
;
; 3) expanding the current mask to a larger mask defined by constraint
;
; Mode 1 cannot be used with modes 2 or 3 (though this could in principle be
; added). 
;
; Modes 2 and 3 can be used together, in which case the implementation
; is somewhat different: 
; 
;  - if ONLY mode 3 is used then the program returns all areas of CONSTRAINT
;    that contain part of MASK. This means that parts of mask can be
;    lost. This behavior can be altered by the keyword KEEPMASK.
;
;  - if modes 2 and 3 are used together, MASK is retained in its entirety and
;    the mask is grown into its 'nearest neighbors' ONLY IF they are part of
;    CONSTRAINT.
;
; CATEGORY:
;
; Data analysis tool.
;
; CALLING SEQUENCE:
;
; new_mask = exp_mask(old_mask, iters=iters, constraint=constraint $
;                     radius=radius, /no_edges, /keep_mask, /all_neighbors)
;
; INPUTS:
;
; mask: required, the original mask to expand
;
; one of these is also needed:
;
; iters: iterations to grow the mask into its nearest neighbors
;
; constraint: another mask to 'grow into' or to contrain 'iters'
;
; radius: radial distance to expand the map
;
; OPTIONAL INPUTS:
;
; none
;
; KEYWORD PARAMETERS:
;
; xy_only: a flag that tells the program to operate only in two
; dimensions when considering a three dimensional data set. That is,
; grow only in the spatial dimension.
;
; z_only: a flag that tells the program to operate only in one
; dimension when considering a three dimensional data set. that is,
; grow only in the spectral dimension.
;
; all_neighbors: define diagonals to be adjacent pixels.
;
; keep_mask: ensure that the original mask is part of the final
; mask. This should only be a concern when the constraint supplied
; does not include the original mask.
;
; no_edge: blank the edges of the mask
;
; OUTPUTS:
;
; returns the new mask
;
; OPTIONAL OUTPUTS:
;
; none
;
; COMMON BLOCKS:
;
; none
;
; SIDE EFFECTS:
;
; none
;
; RESTRICTIONS:
;
; none
;
; MODIFICATION HISTORY:
;
; prettied up and generalized - 18 nov 08, leroy@mpia.de
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND ERROR CATCHING
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
; Require that we have an input mask to start with.
  if n_elements(mask_in) eq 0 then begin
     message, 'Requires an input mask. Returning.', /info
     return, !values.f_nan
  endif

; Check that there is something in the original mask.
  if total(mask_in) eq 0 then begin
     if keyword_set(quiet) eq 0 then $
        message, 'Mask empty. Returning empty mask.', /info
     return, mask_in
  endif    

; Initialize the output
  mask_out = mask_in*0B

; Measure dimensions of the mask.
  sz = size(mask_in)

; Check for mutually inconsistent keywords.
  if (n_elements(radius_in) gt 0) AND $
     (n_elements(iters) gt 0) then begin
     message $
        , 'radius cannot be used with iters. Returning.' $
        , /info
     return, mask_in
  endif

  if (n_elements(iters) eq 0) and $
     (n_elements(radius_in) eq 0) and $ 
     (n_elements(constraint) eq 0) then begin
     message $
        , 'No mode selected (iters, radius, constraint). Returning.' $
        , /info
     return, mask_in
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CASE I: ONLY A CONSTRAINT IS SUPPLIED
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; In the case where only a constraint has been supplied, grow the
; original mask into the constraint, iterating until there are no
; changes in the mask.

  if n_elements(constraint) gt 0 and $
     n_elements(iters) eq 0 and n_elements(radius_in) eq 0 then begin

;    This operation returns 0 where the cube has no constraint, 1
;    where only the constraint exists, and 2 where there is both mask
;    and constraint present.
     mask = (mask_in*constraint)+constraint

;    Suppress the edges at this stage
     if keyword_set(no_edges) then $
        zero_edges, mask

;    Identify contiguous non-zero regions inside the new mask.
     regions = label_region(mask gt 0 $
                            , all_neighbors = all_neighbors $
                            , /ulong)

;    If the region definition is empty (it should not be for
;    well-formed input), then finish.
     if total(regions) eq 0 then begin
        mask_out = 0B*mask
        if keyword_set(keep_mask) then $
           mask_out = mask_out or mask_in
        return, mask_out
     endif

;    Use reverse_indices to sort the regions ...
     h = histogram(regions, binsize = 1, min = 1, rev = ri)

;    And include any region that has at least one pixel with value 2
;    in it (meaning that it has a pixel in the original mask that also
;    satisfies the constraint). Reverse indices allows us to quickly
;    map region labels back to pixels in the original mask.
     for k = 0L, n_elements(h)-1 do begin
        inds = ri[ri[k]:(ri[k+1])-1]
        if total(mask[inds] eq 2) ge 1 then $
           mask_out[inds] = 1b
     endfor

;    Return the new mask
     if keyword_set(no_edges) then $
        zero_edges, mask_out
     if keyword_set(keep_mask) then $
        mask_out = mask_out or mask_in
     return, mask_out    
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CASE II: GROW VIA ITERATIONS, WITH OR WITHOUT A CONSTRAINT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Grow the mask via dilation, potentially with a constraint and
; constrained to only one or two dimensions.

  if n_elements(iters) gt 0 then begin

;    Copy the mask     
     mask = mask_in

;    Define a trivial constraint if one is not supplied
     if n_elements(constraint) eq 0 then $
        constraint = mask*0B+1B
     
;    If edge suppression is desired, zero the edges in the constraint
     if keyword_set(no_edges) then $
        zero_edges, constraint

;    Define the structuring element
     struct = struct_connect(ndim=sz[0] $
                             , all_neighbors = all_neighbors $
                             , xy_only = xy_only $
                             , z_only = z_only)

;    Repeatedly dilate and apply the constraint
     for ii = 0, iters-1 do begin
        mask = dilate(mask, struct)
        mask *= constraint
     endfor

;    If requested, ensure that the original mask is included in the
;    output mask.
     if keyword_set(keep_mask) then $
        mask = mask or mask_in
     return, mask

  endif                         ; of iteration part

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CASE III: GROW VIA DILATION USING A SINGLE ROUND ELEMENT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; The final case builds a spherical structuring element and dilates
; using this element.

  if n_elements(radius_in) gt 0 then begin    

;    Copy the mask     
     mask = mask_in

;    Define a trivial constraint if one is not supplied
     if n_elements(constraint) eq 0 then $
        constraint = mask*0B+1B
     
;    If edge suppression is desired, zero the edges in the constraint
     if keyword_set(no_edges) then $
        zero_edges, constraint

;    Build the spherical structuring element
     struct = struct_round(ndim=sz[0] $
                           , rad=radius_in $
                           , xy_only = xy_only $
                           , z_only = z_only)

;    Dilate the mask
     mask = dilate(mask, struct)
     mask *= constraint

;    If requested, ensure that the original mask is included in the
;    output mask.
     if keyword_set(keep_mask) then $
        mask = mask or mask_in

     return, mask

  endif                         ; of radial part

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SHOULD NOT GET HERE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, !values.f_nan

end                             ; of grow_mask
