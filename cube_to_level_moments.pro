pro cube_to_level_moments $
   , kernels = kernel_ind $
   , kernfile = kernfile $
   , idlformat = idlformat $
   , data=data $
   , infile=infile $
   , mask=mask $
   , inmask=inmask $
   , template_data = template_data $ ; USE THIS TO DEFINE LEVEL MOMENTS
   , intemplate_data = intemplate_data $
   , template_mask = template_mask $
   , intemplate_mask = intemplate_mask $
   , outfile=outfile $
   , clip=do_clip $
   , verbose=verbose $
   , levels = levels 

;  CLIPPING --> REALLY SUBTRACT THE CONTOUR OR USE LOWEST VALUE OKAY?
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
  endif else if n_elements(data) eq 0 then begin 
     message, "Must have either infile or data set.", /info
     return
  endif

  if n_elements(inmask) gt 0 then begin
     file_mask = file_search(inmask, count=file_ct)
     if file_ct eq 0 then begin
        message, "Mask not found.", /info
        return
     endif else begin
        mask = readfits(file_mask, mask_hdr)
     endelse
  endif

  if n_elements(intemplate_data) gt 0 then begin
     file_template = file_search(intemplate_data, count=file_ct)
     if file_ct eq 0 then begin
        message, "Template not found.", /info
        return
     endif else begin
        template_data = readfits(file_template, template_hdr)
     endelse
  endif

  if n_elements(intemplate_mask) gt 0 then begin
     file_template_mask = file_search(intemplate_mask, count=file_ct)
     if file_ct eq 0 then begin
        message, "Template mask not found.", /info
        return
     endif else begin
        template_mask = readfits(file_template_mask, template_mask_hdr)
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

 
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; WORK THROUGH THE MERGER INFRASTRUCTURE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  ; IF YOU GIVE A TEMPLATE DATA FILE THEN RUN THE MERGER TREE
  ; AND THE LEVEL PROPS ON THAT INSTEAD

  if n_elements(template_data) GT 0 then begin
     make_merger_tree $
        , data = template_data $
        , mask = template_mask $
        , levels = levels $
        , kernel_ind = kernel_ind $
        , merger_matrix = merger_matrix $
        , n_kern = n_kern $
        , verbose = verbose 
  endif else begin
     make_merger_tree $
        , data = data $
        , mask = mask $
        , levels = levels $
        , kernel_ind = kernel_ind $
        , merger_matrix = merger_matrix $
        , n_kern = n_kern $
        , verbose = verbose 
  endelse


; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CALL THE MEASUREMENT CODE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  measure_level_moments $
     , data = data $
     , mask = mask $
     , template_data = template_data $
     , template_mask = template_mask $
     , moments = moments $
     , levels = levels $
     , kernel_ind = kernel_ind $
     , n_kern = n_kern $
     , verbose = verbose 
     

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SAVE TO DISK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  save, file=outfile, moments, levels, kernel_ind, hdr, merger_matrix

end                             ; OF CUBE_TO_LEVEL_MOMENTS
