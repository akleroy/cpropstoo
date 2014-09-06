pro measure_level_moments $
   , data = data $
   , mask = mask $
   , template_data = template_data $
   , template_mask = template_mask $
   , moments = moments $ 
   , levels = levels $
   , kernel_ind = kernel_ind $
   , n_kern = n_kern $
   , verbose = verbose 
   
  compile_opt idl2
        
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CALL THE MEASUREMENT CODE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  n_levels = n_elements(levels)

; INITIALIZE OUTPUT
  dummy = measure_moments(/extrap, extarg=0, do_clip=do_clip $
                          , empty_props = empty_props, /just_empty)
  
  moments = replicate(empty_props, n_kern, n_levels)

; LOOP OVER LEVELS
  for i = 0, n_levels-1 do begin
       
     if keyword_set(verbose) then begin
        counter, i+1, n_levels, "Moments for contour level "
     endif

;    ... LABEL REGION WITHIN THE MASK AT THIS LEVEL
;    Use the template to find the regions if provided
     if n_elements(template_data) GT 0 then begin
        this_mask = template_mask $
                    and (template_data ge levels[i])
        regions = label_region(this_mask)
     endif else begin 
        this_mask = mask and (data ge levels[i])
        regions = label_region(this_mask)
     endelse
;    ... EXTRACT PIXELS WITH ASSIGNMENTS
     ind = where(regions ne 0, ct)
     if ct eq 0 then $
        continue

;    ... VECTORIZE (SPEEDS UP SPARSE CASE)
     kern_regions = regions[kernel_ind]
     regions = regions[ind]
     t = data[ind] ; Always measure this from data 
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
                           , /extrap, extarg=0, do_clip=do_clip $
                           , empty_props = empty_props)

;       ... ... FIND ALL KERNELS IN THIS REGION        
        this_kern = where(kern_regions eq kern_regions[j], ct_this_kern)        

;       .... ... FLIP THE KERNELS IN THIS REGION TO DONE
        if ct_this_kern gt 0 then $
           done_this_level[this_kern] = 1B

;       ... ... SAVE THE DATA
        for k = 0, ct_this_kern-1 do $
           moments[this_kern[k],i] = this_mom

     endfor

  endfor


end
