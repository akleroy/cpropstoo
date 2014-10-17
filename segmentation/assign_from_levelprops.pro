pro assign_from_levelprops $
   , propfile = propfile $
   , props = props $ 
   , flags = flags $
   , flagfile = flagfile $
   , kernel_ind = kernel_ind $
   , kernfile = kernfile $
   , data = data $
   , infile = infile $
   , mask = mask $
   , inmask = inmask $
   , hdr = hdr $
   , index = index $
   , outfile = outfile $
   , verbose = verbose


;+
;
; NAME:
;
;   ASSIGN_FROM_LEVELPROPS
;
; PURPOSE:
;   To decompose a data cube into the largest coherent regions that match
;   criteria given by an input mask. Requires: a dendrogram property
;   file (or array) and a mask (flags) of the same size, the data cube and
;   mask to which they correspond, and a list of local maxima (kernels).      
;
;
; CALLING SEQUENCE:
;   assign_from_levelprops, propfile=propfile, flagfile=flagfile,
;   kernfile=kernfile, infile=infile, inmask=inmask, outfile=outfile 
;
; INPUTS:
;   PROPFILE --  Path to property array from DENDROGRAM output.
;   PROPS -- (optional) Array of measurements from DENDROGRAM output.
;   FLAGFILE -- Path to byte array with flags for decomposing cube
;               (property mask). A 1B value indicates that this kernel
;               at this threshold level should be included, a 0B value
;               indicates that that kernel at that threshold value
;               shoul dbe excluded. 
;   FLAGS -- (optional) Input flag array.
;   KERNFILE -- Path to the array of kernels (local maxima) in .idl
;               format or in text columns.
;   KERNEL_IND -- (optional) The array of kernels. 
;   INFILE -- The path to the .fits file containing a cube to be decomposed.
;   DATA -- (optional) The cube itself. 
;   INMASK -- (optional) Path to a mask for the .fits cube.
;   MASK -- (optional) The mask itself.
;   HDR -- (optional) .fits header. Must supply this if you do not
;          supply the filepath. 
;             
; KEYWORD PARAMETERS:
;    
;
; OUTPUTS:
;   OUTFILE -- Location (no extensions) to save the new fits file and
;              index file. Will save as OUTFILE.fits and OUTFILE.idl. 
;   INDEX -- The index locations in the property array. 
; MODIFICATION HISTORY:
;
;       Update and documentation -- Mon Nov 25  2013  Stephen Pardy 
;                     <spardy@astro.wisc.edu>
; 
;-

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

  cube_sz = size(data)

  if n_elements(mask) eq 0 then begin
     file_mask = file_search(inmask, count=file_ct)
     if file_ct eq 0 then begin
        message, "Mask not found.", /info
        mask = bytarr(cube_sz[1],cube_sz[2],cube_sz[3])
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

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN THE LEVEL PROPERTIES
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(propfile) gt 0 then $
    restore, propfile

  if n_elements(flagfile) gt 0 then $
     restore, flagfile ,/v
     

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFINITIONS AND DEFAULTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  cube = data*mask 
  index = [!values.f_nan, !values.f_nan]
  assign = lonarr(cube_sz[1],cube_sz[2],cube_sz[3])
  next_assgn = 1


; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; FIND REGIONS 
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  ; LOOP OVER KERNELS
  for i = 0L, n_elements(kernel_ind)-1 do begin
  if keyword_set(verbose) then $
     counter, i, n_elements(kernel_ind)-1, 'KERNEL   '    

  ;  GET FLAGS FOR THIS COLUMN
     this_flag = (reform(flag[i,*]))
     this_flag_ind = where(this_flag, bound_ct)
     
     if bound_ct LT 1 then continue 
  
     lowest_flag = min(levels[this_flag_ind])
     lev = where(levels EQ lowest_flag)
     

     
;  GENERATE THE MASK FOR THIS CLOUD
     this_mask = cube gt lowest_flag
     regions = label_region(this_mask, /ulong)
     this_reg = regions eq regions[kernel_ind[i]]
   ;  COPY TO TOTAL MASK
     ind = where(this_reg and regions NE 0 , num_ct)  
  
    if num_ct GT 0 and num_ct EQ props[i,lev].moments.npix.val then $
     if total(assign[ind] GT 0) NE  num_ct then begin  
       assign[ind] = next_assgn 
       index = [[index], [i, lev]]
       next_assgn += 1
     endif  
  endfor

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; WRITE OUT FILES
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if n_elements(outfile) NE 0 then begin 
     outfits = outfile+".fits"
     outidl = outfile+".idl"
     out_hdr = hdr
     sxaddpar, hdr, "BUNIT", "ASSIGNMENT"
     writefits, outfits, assign, hdr
     save, index, filename=outidl
  endif

end
