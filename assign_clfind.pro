pro assign_clfind $
   , kernels = kernel_ind $
   , kernfile = kernfile $
   , idlformat = idlformat $
   , data=data $
   , infile=infile $
   , mask=mask $
   , inmask=inmask $
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

  if n_elements(mask) eq 0 then begin
     if n_elements(inmask) gt 0 then begin
        file_mask = file_search(inmask, count=file_ct)
        if file_ct eq 0 then begin
           message, "Mask file not found.", /info
           return
        endif else begin
           mask = readfits(file_mask, mask_hdr)
        endelse
     endif else begin
        message, "Defaulting to a mask of finite elements.", /info
        mask = finite(data)
     endelse
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
  cubify, x, y, v, t $
          , cube = minicube $
          , pad = 3 $
          , twod = (szdata[0] eq 2) $
          , indvec = cubeindex $
          , indcube = indcube

  minikern = kernel_ind
  for i = 0, n_elements(minikern)-1 do $
     minikern[i] = where(indcube eq kernel_ind[i])

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

; LOOP OVER LEVELS (HIGH TO LOW)
  sz = size(minicube)
  miniassign = lonarr(sz[1], sz[2], sz[3])

  ind_to_xyv $
     , minikern $
     , x = minikern_x $
     , y = minikern_y $
     , v = minikern_v $
     , sz=sz
  
  for i = 0, nlev-1 do begin 
;    GIVE COUNTER
     if keyword_set(verbose) then begin
        counter, i, nlev, "Clumpfinding for level "
     endif
    
     reg = label_region(minicube ge levels[i])

     kern_reg = reg[minikern]
     kern_done = bytarr(n_elements(minikern))

     for j = 0, n_elements(minikern)-1 do begin

        if kern_reg[j] eq 0 or kern_done[j] eq 1 then $
           continue

        if total(kern_reg eq kern_reg[j]) eq 1 then begin
           ind = where(reg eq kern_reg[j] and miniassign eq 0, n_assign)
           if n_assign eq 0 then $
              continue
           miniassign[ind] = (j+1)
           kern_done[j] = 1B
        endif else begin
           shared_ind = where(reg eq kern_reg[j] and miniassign eq 0, n_shared)           
           if n_shared eq 0 then $
              continue
           ind_to_xyv $
              , shared_ind $
              , x = shared_x $
              , y = shared_y $
              , v = shared_v $
              , sz=sz

           kern_ind = where(kern_reg eq kern_reg[j], n_kern_here)

           shared_assign = lonarr(n_shared)
           shared_dist = lonarr(n_shared)
           
           for k = 0, n_kern_here-1 do begin
              dist = (shared_x -minikern_x[kern_ind[k]])^2 + $
                     (shared_y -minikern_y[kern_ind[k]])^2 + $
                     (shared_v -minikern_v[kern_ind[k]])^2
              if k eq 0 then begin
                 shared_dist = dist
                 shared_assign = shared_assign*0 + kern_ind[k]+1
              endif else begin
                 ind = where(dist lt shared_dist, ct)
                 if ct gt 0 then begin
                    shared_dist[ind] = dist[ind]
                    shared_assign[ind] = (kern_ind[k]+1)[0]
                 endif
              endelse
           endfor

           miniassign[shared_ind] = shared_assign

           kern_done[kern_ind] = 1B

        endelse
     endfor
  endfor
  
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

