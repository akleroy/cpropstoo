pro make_sampling_points $
   , ra_ctr = ra_ctr $
   , dec_ctr = dec_ctr $
   , max_rad = max_rad $
   , spacing = spacing $
   , mask = in_mask $
   , hdr_mask = in_mask_hdr $
   , overlay = in_overlay $
   , hdr_overlay = in_overlay_hdr $
   , show = show $
   , samp_ra = samp_ra $
   , samp_dec = samp_dec

;+
; NAME:
;
; make_sampling_points
;
; PURPOSE:
;
; Generates a set of {ra, dec} points on a hex grid given a center, a
; spacing, and subject to existing inside of some mask or within some
; radius. Optionally shows them on an image.
;
; CATEGORY:
;
; science tool
;
; CALLING SEQUENCE:
;
; make_sampling_points $
;   , ra_ctr = ra_ctr $
;  , dec_ctr = dec_ctr $
;   , max_rad = max_rad $
;   , spacing = spacing $
;   , mask = in_mask $
;   , hdr_mask = in_mask_hdr $
;   , overlay = in_overlay $
;   , hdr_overlay = in_overlay_hdr $
;   , show = show $
;   , samp_ra = samp_ra $
;   , samp_dec = samp_dec
;
; INPUTS:
;
; ra_ctr, dec_ctr - center of the grid
; 
; max_rad - maximum extent of grid in degrees. Default 1 degree.
;
; spacing - hex grid spacing.
;
; mask - string or image holding a 1s and 0s mask. Only points that
;        sample a pixel with a 1 are valid and kept. Points outside
;        the mask are also dropped. Three dimensional data are
;        collapsed with "or" (i.e., any 1 along the third axis is a 1
;        in the image).
;
; mask_hdr - if a file is not supplied, this holds a header that
;            supplies the mask astrometry.
;
; overlay, overlay_hdr - similar to mask but an image used only to
;                        display the data. Peak intensity is used for
;                        three dimensional data.
;
; show - flag that sets whether an image with the points is displayed.
;
; OUTPUTS:
;
; samp_ra, samp_dec - return arrays of RA, Dec sampling
;                     points. Returns not-a-number if it fails.
;
; MODIFICATION HISTORY:
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  samp_ra = !values.f_nan
  samp_dec = !values.f_nan

; Check for required input
  if (n_elements(ra_ctr) eq 0) or $
     (n_elements(dec_ctr) eq 0) or $
     (n_elements(spacing) eq 0) then begin
     message, 'Needs RA_CTR, DEC_CTR, and SPACING.', /info
     return
  endif

  if n_elements(max_rad) eq 0 then begin
     message, 'Defaulting to a large (one degree) max radius.', /info
     max_rad = 1.0
  endif

; Check if we have a mask.
  have_mask = 0B
  if n_elements(in_mask) gt 0 then begin
     if size(in_mask, /type) eq size("hello", /type) then begin
        fname = in_mask
        fits_read, fname, mask, mask_hdr
     endif else begin
        mask = in_mask
        if n_elements(in_mask_hdr) gt 0 then $
           mask_hdr = in_mask_hdr
     endelse
     have_mask = 1B
  endif

; Collapse the mask to two dimensions if needed.
  if have_mask then begin
     sz_mask = size(mask)
     if sz_mask[0] eq 3 then begin
        message, 'Collapsing mask to two dimensions.', /info
        mask = total(mask,3) ge 1B
        mask_hdr = twod_head(mask_hdr)
     endif
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; GENERATE A GRID
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  hex_grid $
     , ctr_x = ra_ctr $
     , ctr_y = dec_ctr $
     , spacing = spacing $
     , /radec $
     , xout = samp_ra $
     , yout = samp_dec $
     , r_limit = max_rad $
     , /center

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; PARE TO DESIRED SCOPE BASED ON INPUT MAP
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if have_mask then begin
     
     sz_mask = size(mask)

     adxy, mask_hdr, samp_ra, samp_dec, samp_x, samp_y

     samp_x = round(samp_x)
     samp_y = round(samp_y)

     keep = where(samp_x ge 0 and $
                  samp_y ge 0 and $
                  samp_x lt sz_mask[1] and $
                  samp_y lt sz_mask[2], keep_ct)

     if keep_ct eq 0B then begin
        message, 'No sampling points survive inside mask. Returning.', /info
        samp_ra = !values.f_nan
        samp_dec = !values.f_nan
        return
     endif

     samp_ra = samp_ra[keep]
     samp_dec = samp_dec[keep]
     samp_x = samp_x[keep]
     samp_y = samp_y[keep]

     keep = where(mask[samp_x,samp_y], keep_ct)
     
     if keep_ct eq 0B then begin
        message, 'No sampling points survive inside mask. Returning.', /info
        samp_ra = !values.f_nan
        samp_dec = !values.f_nan
        return
     endif

     samp_ra = samp_ra[keep]
     samp_dec = samp_dec[keep]
     samp_x = samp_x[keep]
     samp_y = samp_y[keep]

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; VISUALIZE IF DESIRED
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(show) then begin

;    Check if we have an overlay.
     have_overlay = 0B
     if n_elements(in_overlay) gt 0 then begin
        if size(in_overlay, /type) eq size("hello", /type) then begin
           fname = in_overlay
           fits_read, fname, overlay, overlay_hdr
        endif else begin
           overlay = in_overlay
           if n_elements(in_overlay_hdr) gt 0 then $
              overlay_hdr = in_overlay_hdr
        endelse
        have_overlay = 1B

        sz_overlay = size(overlay)
        if sz_overlay[0] eq 3 then begin
           overlay = max(overlay, dim=3, /nan)
           overlay_hdr = twod_head(overlay_hdr)
        endif

     endif
     
     if have_overlay eq 0 and have_mask eq 1B then begin
        overlay = mask
        overlay_hdr = mask_hdr
        have_overlay = 1B
     endif

     if have_overlay then begin
        adxy, overlay_hdr, samp_ra, samp_dec, samp_x_over, samp_y_over
        loadct, 33
        disp, overlay, /sq, reserve=5, xsty=1, ysty=1
        oplot, [samp_x_over], [samp_y_over], ps=1, color=cgcolor('white'), thick=3
        if have_mask then begin
           contour, mask, /overplot, lev=[1], c_color=cgcolor('white')
        endif
     endif

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return

end
