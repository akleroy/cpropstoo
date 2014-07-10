pro assign_clfind $
   , kernels = kernel_ind $
   , kernfile = kernfile $
   , idlformat = idlformat $
   , data=data $
   , infile=infile $
   , mask=mask_in $
   , inmask=mask_file $
   , trueval=trueval $
   , hdr=hdr $
   , sigma=sigma $
   , outfile=outfile $
   , assign=assign $
   , verbose=verbose $
   , spacing=spacing $
   , minlev=minlev

; NB - assumes equal weighting for velocity and spatial pixel; we may
;      want to add the ability to weight distance in spatial pixels
;      relative to distance in velocity pixels. In the case of a
;      weirdly sampled cube this is a big deal...

; Potentially allow levels set by hand.
;
; Added seeded/unseeded capability.

  compile_opt idl2

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN THE DATA
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(infile) gt 0 then begin
     file_data = file_search(infile, count=file_ct)
     if file_ct eq 0 then begin
        message, "Data not found.", /info
        return
     endif else begin
        data = readfits(file_data, hdr)
     endelse
  endif

  if n_elements(mask_in) eq 0 then begin
     if n_elements(mask_file) gt 0 then begin
        full_file_mask = file_search(mask_file, count=file_ct)
        if file_ct eq 0 then begin
           message, "Mask file not found.", /info
           return
        endif else begin
           mask = readfits(full_file_mask, mask_hdr)
        endelse
     endif else begin
        message, "Defaulting to a mask of finite elements.", /info
        mask = finite(data)
     endelse
  endif else begin
     mask = mask_in
  endelse

; If requested, use only the mask where it equals a certain true
; value. Useful for analyzing only part of an assignment cube, for
; example.
  if n_elements(trueval) ne 0 then begin
     mask = mask eq trueval
  endif

; Return in the case of an empty mask
  if total(mask) eq 0 then begin
     message, "Empty mask. Returning.", /info
     return
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN THE KERNELS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(kernfile) gt 0 then begin
     if keyword_set(idlformat) then begin
        restore, kernfile
     endif else begin
        readcol, kernfile, comment="#" $
                 , format="L,L,L,F,F,F,F" $
                 , kern_xpix, kern_ypix, kern_zpix $
                 , kern_ra, kern_dec, kern_vel, kern_int
        xyv_to_ind, x=kern_xpix, y=kern_ypix, v=kern_zpix $
                    , sz=size(data), ind=kernel_ind
     endelse
  endif

  if n_elements(kernel_ind) ne 0 then begin
     seeded = 1B
  endif else begin
     seeded = 0B
  endelse

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFINITIONS AND DEFAULTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; IF NO SPACING IS DEFINED DEFAULT TO 2 TIMES THE RMS NOISE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if n_elements(spacing) eq 0 then begin
     sigma = mad(data,/finite)
     spacing = 2.0*sigma
  endif

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; SET THE MINIMUM LEVEL TO CONSIDER
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if n_elements(minlev) eq 0 then $
     minlev = min(data[where(mask)])

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; PARE CUBE TO MINIMUM SIZE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
; Convert the data inside the mask to a vector

  vectorify, data $
             , mask = mask $
             , x = x, y = y, v = v, t = t $
             , ind = cubeindex

; Rebuild a minimum-sized cube from the vectorized data.

  szdata = size(data)
  cubify, x=x, y=y, v=v, t=t $
          , cube = minicube $
          , pad = 3 $
          , dim_2d = (szdata[0] eq 2) $
          , indvec = cubeindex $
          , indcube = indcube
  
  if seeded then begin
     minikern = kernel_ind
     for i = 0, n_elements(minikern)-1 do $
        minikern[i] = where(indcube eq kernel_ind[i])
  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; ASSIGNMENT
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; CALCULATE LEVELS TO WORK WITH
  levels = $
     contour_values( $
     minicube $
     , /linspace $
     , spacing=spacing $
     , minval=minlev)
  nlev = n_elements(levels)

; INITIALIZE AN OUTPUT ASSIGNMENT CUBE
  sz = size(minicube)
  miniassign = lonarr(sz[1], sz[2], sz[3])

; EXTRACT KERNEL COORDINATES IN THE MINUM SPANNING CUBE
  if seeded then begin
     ind_to_xyv $
        , minikern $
        , x = minikern_x $
        , y = minikern_y $
        , v = minikern_v $
        , sz=sz
  endif
  
; LOOP OVER LEVELS (HIGH TO LOW)
  for i = 0, nlev-1 do begin 

;    PROGRESS BAR
     if keyword_set(verbose) then begin
        counter, i, nlev, "Clumpfinding for level "
     endif

;    LABEL REGIONS FOR THIS CONTOUR    
     reg = label_region(minicube ge levels[i])
     n_reg = max(reg)
     reg_done = bytarr(n_reg+1) ; ... 0 IS NOT USED, DO 1 INDEX FOR EASE OF REFERENCE

;    IF WE HAVE EXISTING KERNELS FIND THEIR REGIONS AND CREATE A CHECKLIST
     if n_elements(minikern) gt 0 then begin
        kern_reg = reg[minikern]
        kern_done = bytarr(n_elements(minikern))
     endif

;    LOOP OVER ASSIGNMENT FOR EXISTING KERNELS
     for j = 0, n_elements(minikern)-1 do begin

;       If this kernel is done (e.g., from an earlier overlap) or the
;       kernel is in the watershed (i.e., a peak is not in the mask at
;       this level) then continue to the next kernel.

        if kern_reg[j] eq 0 or kern_done[j] eq 1 then $
           continue

;       Handle the special case where there is only one kernel is
;       associated with the region of the current kernel (so that all
;       pixels in this region become associated with that kernel).
        if total(kern_reg eq kern_reg[j]) eq 1 then begin
           ind = where(reg eq kern_reg[j] and miniassign eq 0, n_assign)
           if n_assign eq 0 then $
              continue
           miniassign[ind] = (j+1)
           kern_done[j] = 1B
        endif else begin

;       Otherwise deal with the case where there are shared pixels,
;       assigning based on distance between peak and pixel in the
;       shared region.

;       IMPROVEMENT/SIMPLIFICATION: Note that following many other
;       clumpfind implementations we *only* use the distance to assign
;       to peaks. A potentially important subtlety (that does come up)
;       is to implement friends of friends connection among assigned
;       pixels. This will slow things down and require a change in
;       implementation.

;          FIND UNASSIGNED PIXELS IN THE CURRENT REGION
           shared_ind = where(reg eq kern_reg[j] and miniassign eq 0, n_shared)           

           if n_shared eq 0 then $
              continue

;          GET THE {X, Y, V} COORDINATES FOR THE SHARED PIXELS
           ind_to_xyv $
              , shared_ind $
              , x = shared_x $
              , y = shared_y $
              , v = shared_v $
              , sz=sz

;          FIND THE INDICES OF THE PEAKS IN THIS REGION
           kern_ind = where(kern_reg eq kern_reg[j], n_kern_here)

;          INITIALIZE AN ASSIGNMENT AND DISTANCE OUTPUT
           shared_assign = lonarr(n_shared)
           shared_dist = lonarr(n_shared)
           
;          LOOP OVER SHARED KERNELS
           for k = 0, n_kern_here-1 do begin

;             FIGURE OUT THE DISTANCE BETWEEN EACH PIXEL AND THIS KERNEL
              dist = (shared_x -minikern_x[kern_ind[k]])^2 + $
                     (shared_y -minikern_y[kern_ind[k]])^2 + $
                     (shared_v -minikern_v[kern_ind[k]])^2

              if k eq 0 then begin
;                FOR THE FIRST KERNEL, INITIALIZE THE ASSIGNMENT AND DISTANCE
                 shared_dist = dist
                 shared_assign = shared_assign*0 + kern_ind[k]+1
              endif else begin
;                FOR SUBSEQUENT KERNELS, ASSIGN PIXELS WHERE THE
;                DISTANCE IS LESS THAN THE CURRENT MINIMUM DISTANCE
                 ind = where(dist lt shared_dist, ct)
                 if ct gt 0 then begin
                    shared_dist[ind] = dist[ind]
                    shared_assign[ind] = (kern_ind[k]+1)[0]
                 endif
              endelse
           endfor

;          RECORD THE ASSIGNMENT IN THE MINIASSIGN
           miniassign[shared_ind] = shared_assign

;          NOTE THAT ASSIGNMENT FOR THIS KERNEL IS DONE
           kern_done[kern_ind] = 1B

        endelse
     endfor

;    If we are running the unseeded version then generate new kernels
;    from each of the regions at this level that lack existing
;    assignments (for the first level, with no kernels in place, this
;    will happen automatically).

     if seeded eq 0B then begin

        unassigned = (reg gt 0) and $
                     (miniassign eq 0)
        unassigned_ind = where(unassigned, unassigned_ct)

        if unassigned_ct gt 0 then begin

;          FIGURE OUT WHICH REGIONS REMAIN UNASSIGNED
           unassigned_reg = reg[unassigned_ind]
           unassigned_reg = unassigned_reg[uniq(unassigned_reg, sort(unassigned_reg))]
           n_unassigned_reg = n_elements(unassigned_reg)

;          ADD THE PEAK OF EACH UNASSIGNED REGION TO THE SET OF KERNELS
           for j = 0, n_unassigned_reg-1 do begin

;             FIND THE MAXIMUM (AND ITS INDEX) FOR THIS REGION
              dummy = max((reg eq unassigned_reg[i])*minicube, maxind, /nan)
              
              if dummy eq 0 then $
                 continue

;             CONVERT TO {X, Y, V}
              ind_to_xyv $
                 , maxind $
                 , x = maxind_x $
                 , y = maxind_y $
                 , v = maxind_v $
                 , sz=sz

;             ADD THIS PEAK TO THE LIST OF KERNELS
              if n_elements(minikern) eq 0 then begin
                 minikern = [maxind]
                 minikern_x = [maxind_x]
                 minikern_y = [maxind_y]
                 minikern_v = [maxind_v]
              endif else begin
                 minikern = [minikern, maxind]
                 minikern_x = [minikern_x, maxind_x]
                 minikern_y = [minikern_y, maxind_y]
                 minikern_v = [minikern_v, maxind_v]
              endelse

           endfor ; OF LOOP OVER UNASSIGNED REGIONS

        endif

     endif ; OF GENERATION OF NEW PEAKS

  endfor ; OF LOOP OVER THIS LEVEL

; INITIALIZE ASSIGNMENT CUBE
  sz = size(data)
  assign = lonarr(sz[1], sz[2], sz[3])
  assign_ind = where(miniassign gt 0)
  assign[indcube[assign_ind]] = miniassign[assign_ind]
    
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; OUTPUT
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if n_elements(hdr) gt 0 then begin
     out_hdr = hdr
     sxaddpar, out_hdr, "BUNIT", "ASSIGNMENT"
     writefits, outfile, assign, out_hdr
  endif else begin
     message, "Consider passing a header so that the assignment has astrometry.", /info
     writefits, outfile, assign
  endelse

end

