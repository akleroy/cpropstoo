pro make_merger_tree $
   , data = data $
   , mask = mask $
   , levels = levels $
   , kernel_ind = kernel_ind $
   , merger_matrix = merger_matrix $
   , n_kern = n_kern $
   , verbose = verbose 
   


; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFINITIONS AND DEFAULTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; GET AN RMS ESTIMATE
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if n_elements(sigma) eq 0 then $
     sigma = mad(data,/finite)

 
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
  cubify, x=x, y=y, v=v, t=t $
          , cube = minicube $
          , pad = 3 $
          , dim_2d = (szdata[0] eq 2) $
          , indvec = cubeindex $
          , indcube = indcube

; ... retreive kernel location inside the cube.

  minikern = kernel_ind
  for i = 0, n_elements(minikern)-1 do $
     minikern[i] = where(indcube eq kernel_ind[i])

; CALCULATE LEVELS TO WORK WITH
  if n_elements(levels) EQ 0 then $
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


end
