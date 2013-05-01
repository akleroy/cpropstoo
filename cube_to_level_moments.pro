pro cube_to_level_moments $
   , kernels = kernel_ind $
   , kernfile = kernfile $
   , idlformat = idlformat $
   , data=data $
   , infile=infile $
   , mask=mask $
   , inmask=inmask $
   , outfile=outfile $
   , verbose=verbose

;  CLIPPING
;  OTHER?

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
     file_mask = file_search(inmask, count=file_ct)
     if file_ct eq 0 then begin
        message, "Mask not found.", /info
        return
     endif else begin
        mask = readfits(file_mask, mask_hdr)
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
; GET AN RMS ESTIMATE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if n_elements(sigma) eq 0 then $
     sigma = mad(data)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; WORK THROUGH THE MERGER INFRASTRUCTURE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
       
  if keyword_set(verbose) then begin
     message, "Working out cluster tree.", /info
  endif
  
; PARE CUBE TO MINIMUM SIZE

; ... convert the data inside the mask to a vector

  vectorify, data $
             , mask = mask $
             , x = x, y = y, v = v, t = t $
             , ind = cubeindex

; ... rebuild a minimum-sized cube from the vectorized data.

  szdata = size(data)
  cubify, x, y, v, t $
          , cube = minicube $
          , pad = 3 $
          , twod = (szdata[0] eq 2) $
          , indvec = cubeindex $
          , indcube = indcube

; ... retreive kernel location inside the cube.

  minikern = kernel_ind
  for i = 0, n_elements(minikern)-1 do $
     minikern[i] = where(indcube eq kernel_ind[i])

; CALCULATE LEVELS TO WORK WITH
  levels = $
     contour_values( $
     minicube $
     , /linspace $
     , spacing=0.2*sigma $
     , nmin=nmin)

; WORK OUT MERGER MATRIX
  merger_matrix =  $
     mergefind_approx(minicube $
                      , minikern $
                      , levels=levels $
                      , all_neighbors = all_neighbors $
                      , verbose = verbose)     
  sz_merge = size(merger_matrix)
  ind = lindgen(sz_merge[1])
  ;merger_matrix[ind,ind] = !values.f_nan

; ... invert to get a distance useful for the dendrogram
; calculation. Kernels are closer if they merge at a higher level.
  dist_matrix = max(merger_matrix, /nan) - merger_matrix

; ... place zeros on diagonal (required by cluster functions)
  dist_matrix[ind, ind] = 0.0

  nan_ind = where(finite(dist_matrix) eq 0, nan_ct)
  if nan_ct gt 0 then $
     dist_matrix[nan_ind] = max(dist_matrix)

; DO THE CLUSTER TREE CALCULATION
  clusters = cluster_tree(dist_matrix, linkdistance, linkage=0)
  dendrogram, clusters, linkdistance, outverts, outconn $
              , leafnodes=leafnodes

; REORDER THE KERNELS AND MERGER MATRIX  
  kernel_ind = kernel_ind[leafnodes]
  old_merger_matrix= merger_matrix  
  n_kern = n_elements(kernel_ind)
  for i = 0, n_kern-1 do begin 
     for j = 0, n_kern-1 do begin 
        ;if i eq j then $
        ;   continue 
        merger_matrix[i,j] = $
           old_merger_matrix[leafnodes[i], leafnodes[j]]
     endfor 
  endfor

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CALL THE MEASUREMENT CODE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  n_levels = n_elements(levels)

; INITIALIZE OUTPUT
  moments = replicate(empty_moment_struct(), n_kern, n_levels)

; LOOP OVER LEVELS
  for i = 0, n_levels-1 do begin
       
     if keyword_set(verbose) then begin
        counter, i+1, n_levels, "Moments for contour level "
     endif

;    ... LABEL REGION WITHIN THE MASK AT THIS LEVEL
     this_mask = mask and (data ge levels[i])
     regions = label_region(this_mask)

;    ... EXTRACT PIXELS WITH ASSIGNMENTS
     ind = where(regions ne 0, ct)
     if ct eq 0 then $
        continue

;    ... VECTORIZE (SPEEDS UP SPARSE CASE)
     kern_regions = regions[kernel_ind]
     regions = regions[ind]
     t = data[ind]
     ind_to_xyv, ind, x=x, y=y, v=v, sz=size(data)
     
;    ... INTIALIZE KERNEL CHECKLIST
     done_this_level = bytarr(n_kern)
     
;    ... LOOP OVER KERNELS
     for j = 0, n_kern-1 do begin
        
        if done_this_level[j] then $
           continue

;       ... ... FIND DATA FOR THIS KERNEL
        ind = where(regions eq kern_regions[j], ct)
        if ct eq 0 then continue
             
        this_t = t[ind]
        this_x = x[ind]
        this_y = y[ind]
        this_v = v[ind]
     
;       ... ... WORK OUT PROPERTIES FOR THIS KERNEL       
        this_mom = $
           measure_moments(x=this_x, y=this_y, v=this_v, t=this_t $
                           , /extrap, extarg=0)
        
;       ... ... FIND ALL KERNELS IN THIS REGION        
        this_kern = where(kern_regions eq kern_regions[j], ct_this_kern)

;       ... ... SAVE THE DATA
        for k = 0, ct_this_kern-1 do $
           moments[this_kern[k],i] = this_mom
        
     endfor

  endfor

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SAVE TO DISK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  save, file=outfile, moments, levels, kernel_ind, hdr, old_merger_matrix

end                             ; OF CUBE_TO_LEVEL_MOMENTS
