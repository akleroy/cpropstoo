function perimeter_mask $
   , mask $
   , all_neighbors=all_neighbors $
   , xy_only = xy_only $
   , z_only = z_only $
   , no_edges = no_edges

;+
;
; NAME:
;
; perimeter
;
; PURPOSE:
;
; Return a mask of the perimeter (surface) of a mask.
;
; CATEGORY:
;
; Morphology tool
;
; CALLING SEQUENCE:
;
; perimeter_mask = perimeter_mask(mask, all_neighbors=all_neighbors)
;
; INPUTS:
;
; mask: a mask used to identify signal that will be ignored when
; fitting the noise. Anything that is 0 in the mask is assumed to be
; noise and is used to fit. So this is a *signal* mask.
;
; OPTIONAL INPUTS:

; KEYWORD PARAMETERS:
;
; all_neighbors: define connectivity diagonally.
;
; xy_only: only perform the contrast between mask and the region
; outside in the x-y plane (i.e., only in the spatial part of a data
; cube).
;
; z_only: only perform the contrast between mask and the region
; outside along the z-axis (affects the structuring element used for
; dilation).
;
; no_edges: blank the edges of the mask before the perimeter
; calculation. The operation can get confused around the edge of the
; image, so this is useful in the case of structures in full arrays.
;
; OUTPUTS:
;
; OPTIONAL OUTPUTS:
;
; N/A
;
; COMMON BLOCKS:
;
; N/A
;
; SIDE EFFECTS:
;
; N/A
;
; RESTRICTIONS:
;
; N/A
;
; PROCEDURE:
;
; N/A
;
; EXAMPLE:
;
; N/A
;
; MODIFICATION HISTORY:
; 
; 17-Oct-14 : built
;
;-
  
; If requested, zero the edges of the mask
  if keyword_set(no_edges) then begin
     zero_edges, mask, thick=2
  endif

; Invert the mask to get the currently OFF regions.  
  outside = mask eq 0
  
; Create a structuring element to be used in dilation.
  sz = size(mask)
  struct = $
     defn_connect( $
     ndim=sz[0] $
     , all_neighbors = all_neighbors $
     , xy_only = xy_only $
     , z_only = z_only)
  
; Identify perimeter pixels, which are those where the mask is 1 and
; the dilation of the outside mask is also 1.
  grow_outside = dilate(outside, struct)
  perim = grow_outside and mask

  return, perim

end
