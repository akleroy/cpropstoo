function grow_mask $
   , mask_in $                  
   , ITERS = iters $
   , CONSTRAINT = constraint $
   , RADIUS = radius_in $
   , TWOD = twod $
   , NO_EDGES = no_edges $
   , KEEP_MASK = keepmask $
   , ALL_NEIGHBORS = all_neighbors $
   , QUIET = quiet

;+
; NAME:
;
; grow_mask
;
; PURPOSE:
;
; Expand a byte mask in one of three ways:
;
; 1) by a specified radius in pixels
;
; 2) a set # of iterations where all pixels adjacent to the mask enter the
; mask
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
; MASK - required, the original mask to expand
;
; one of these is also needed:
;
; ITERS - iterations to grow the mask into its nearest neighbors
; CONSTRAINT - another mask to 'grow into' or to contrain 'iters'
; RADIUS - radial distance to expand the map
;
; OPTIONAL INPUTS:
;
; none
;
; KEYWORD PARAMETERS:
;
; ALL_NEIGHBORS - define diagonals to be adjacent pixels
;
; NO_EDGE - blank the edges of the mask
;
; KEEP_MASK - ensure that the original mask is part of the final mask
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
  
  if n_elements(radius_in) gt 0 AND $
     (n_elements(constraint) gt 0 OR $
      n_elements(iters) gt 0) then begin
     message $
        , 'RADIUS cannot be used with ITERS and/or CONSTRAINT. Returning.' $
        , /info
     return, !values.f_nan
  endif

  if n_elements(mask_in) eq 0 then begin
     message, 'Requires an input mask. Returning.', /info
     return, !values.f_nan
  endif

  if total(mask_in) eq 0 then begin
     if keyword_set(quiet) eq 0 then $
        message, 'Mask empty. Returning empty mask.', /info
     return, mask_in
  endif    

; INITIALIZE THE OUTPUT
  mask_out = mask_in*0B

; MEASURE DIMENSIONS
  sz = size(mask_in)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ONLY A CONSTRAINT SUPPLIED
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(constraint) gt 0 and n_elements(iters) eq 0 then begin
;   0 = NOT CONSTRAINT, 1 = CONSTRAINT, 2 = MASK AND CONSTRAINT
     mask = (mask_in*constraint)+constraint

;   IDENTIFY CONTIGUOUS NON-ZERO REGIONS
     regions = label_region(mask gt 0 $
                            , all_neighbors = all_neighbors $
                            , /ulong)

;   QUIT IF WE'RE EMPTY
     if total(regions) eq 0 then $
        return, !values.f_nan

;   USE reverse_indices TO SORT THE REGIONS ...
     h = histogram(regions, binsize = 1, min = 1, rev = ri)

;   AND INCLUDE ANY REGION THAT INCLUDES AT LEAST ONE TWO IN THE MASK
;   (N.B. 'ge 1' HERE AND 'gt 1' IN dilate_mask)
     for k = 0L, n_elements(h)-1 do begin
        inds = ri[ri[k]:(ri[k+1])-1]
        if total(mask[inds] eq 2) ge 1 then $
           mask_out[inds] = 1b
     endfor

;   ... AND WE'RE OUT.
     if keyword_set(no_edges) then $
        zero_edges, mask_out
     if keyword_set(keep_mask) then $
        mask_out = mask_out or mask_in
     return, mask_out    
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; GROW VIA ITERATIONS, WITH OR WITHOUT A CONSTRAINT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(iters) gt 0 then begin
     
     mask = mask_in
     if n_elements(constraint) eq 0 then $
        constraint = mask*0B+1B
     
     if (sz[0] eq 1) then begin
        for i = 0, iters-1 do begin
           mask = mask or $
                  shift(mask,-1)*constraint or $
                  shift(mask,1)*constraint
           if keyword_set(no_edges) then $
              zero_edges, mask
        endfor
     endif

     if (sz[0] eq 2) then begin
        for i = 0, iters-1 do begin
           for ii=-1,+1 do begin
              for jj=-1,+1 do begin
                 if ((ii eq 0) or (jj eq 0)) then $
                    mask = mask or shift(mask,ii,jj)*constraint $
                 else $
                    if keyword_set(all_neighbors) then $
                       mask = mask or shift(mask,ii,jj)*constraint
                 if keyword_set(no_edges) then $
                    zero_edges,mask
              endfor
           endfor
        endfor
     endif

     if (sz[0] eq 3) then begin
        for i = 0, iters-1 do begin
           for ii = -1, +1 do begin
              for jj = -1, +1 do begin
                 if keyword_set(twod) then begin
                    if (ii eq 0) eq (jj eq 0) then $
                       mask = mask or shift(mask,ii,jj,0)*constraint $
                    else $
                       if keyword_set(all_neighbors) then $
                          mask = mask or shift(mask,ii,jj,0)*constraint
                    if keyword_set(no_edges) then $
                       zero_edges,mask
                 endif else begin
                    for kk = -1, +1 do begin
                       if total((ii eq 0) + (jj eq 0) + (kk eq 0)) eq 2 then $
                          mask = mask or shift(mask,ii,jj,kk)*constraint $
                       else $
                          if keyword_set(all_neighbors) then $
                             mask = mask or shift(mask,ii,jj,kk)*constraint
                       if keyword_set(no_edges) then $
                          zero_edges,mask
                    endfor
                 endelse
              endfor
           endfor
        endfor
     endif

     if keyword_set(keep_mask) then $
        mask = mask or mask_in
     return, mask

  endif                         ; of iteration part

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; GROW RADIALLY
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(radius_in) gt 0 then begin    
     radius = ceil(radius_in)

;   VECTORIZE THE MASK EDGES, DEFINED AS PIXELS THAT ARE 1 BUT BORDER 0
     if sz[0] eq 1 then begin
        edges = $
           where(mask_in and $
                 ((shift(mask_in,-1) + shift(mask_in,+1)) ne 2) $
                 , edge_ct)
     endif
     
     if sz[0] eq 2 then begin
        edges = $
           where(mask_in and $
                 ((shift(mask_in,-1,0) + $
                   shift(mask_in,+1,0) + $
                   shift(mask_in,0,-1) + $
                   shift(mask_in,0,+1)) $
                  ne 4) $
                 , edge_ct)
     endif

     if sz[0] eq 3 then begin
        edges = $
           where(mask_in and $
                 ((shift(mask_in,-1,0,0) + $
                   shift(mask_in,+1,0,0) + $
                   shift(mask_in,0,-1,0) + $
                   shift(mask_in,0,+1,0) + $
                   shift(mask_in,0,0,-1) + $
                   shift(mask_in,0,0,+1)) $
                  ne 6) $
                 ,edge_ct)
     endif

     if edge_ct eq 0 then begin
        message, 'No edges in the cube, returning input mask.', /info
        return, mask_in
     endif
     
     x = edges mod sz[1]
     if sz[0] ge 2 then y = (edges mod (sz[1]*sz[2]))/sz[1]
     if sz[0] ge 3 then z = edges/(sz[1]*sz[2])
     
;   ONE-D CASE
     if sz[0] eq 1 then begin
        for i = -radius, radius do begin
           if (i*i le radius_in*radius_in) then begin                
              newx = ((x+i) > 0) < (sz[1]-1)
              mask_out[newx] = 1B
           endif
        endfor
     endif

;   TWO-D CASE
     if sz[0] eq 2 then begin
        for i = -radius, radius do begin
           for j = -radius, radius do begin
              if ((i*i + j*j) le radius_in*radius_in) then begin
                 newx = ((x+i) > 0) < (sz[1]-1)
                 newy = ((y+j) > 0) < (sz[2]-1)
                 mask_out[newx,newy] = 1B
              endif
           endfor
        endfor
     endif

;   THREE-D CASE
     if sz[0] eq 3 then begin
        for i = -radius, radius do begin
           for j = -radius, radius do begin
              if keyword_set(twod) then begin
                 if ((i*i + j*j) le radius_in*radius_in) then begin
                    newx = ((x+i) > 0) < (sz[1]-1)
                    newy = ((y+j) > 0) < (sz[2]-1)
                    mask_out[newx,newy,z] = 1B
                 endif
              endif else begin
                 for k = -radius, radius do begin
                    if ((i*i + j*j + k*k) le radius_in*radius_in) then begin
                       newx = ((x+i) > 0) < (sz[1]-1)
                       newy = ((y+j) > 0) < (sz[2]-1)
                       newz = ((z+k) > 0) < (sz[3]-1)
                       mask_out[newx,newy,newz] = 1B
                    endif
                 endfor
              endelse
           endfor
        endfor
     endif

;   ALWAYS KEEP THE MASK FOR THIS CASE
     mask_out = mask_out or mask_in

;   GIVING BACK TO THE COMMUNITY...
     if keyword_set(no_edges) then $
        zero_edges, mask_out

     return, mask_out
  endif                         ; of radial part

; SHOULDN'T GET HERE
  return, !values.f_nan

end                             ; of exp_mask
