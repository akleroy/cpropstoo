pro assign_bound_clouds $
   , kernels = kernel_ind $
   , kernfile = kernfile $
   , idlformat = idlformat $
   , props = props $
   , propfile = propfile $
   , data=data $
   , infile=infile $
   , mask=mask $
   , inmask=inmask $
   , hdr=hdr $
   , outfile=outfile $
   , assign=assign $
   , bound=bound $
   , verbose=verbose  $
   , index = index

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

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN THE LEVEL PROPERTIES
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(propfile) gt 0 then begin
    restore, propfile
 endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFINITIONS AND DEFAULTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  cube = data*mask 
  index = [!values.f_nan, !values.f_nan, !values.f_nan]
  sigma = mad(data)
  if n_elements(levels) Eq 0 then $
     levels = $
     contour_values( $
     data $
     , /linspace $
     , spacing=0.2*sigma $
     , nmin=100)
  
  lum = props.lum.val_extrap
  rad = props.rad_ell.val
  sigv = props.vmom.val
  mvir = props.virmass.val
  mass = props.mass.val
  
  dist = props.dist_pc
  cube_sz = size(cube)
  bound = bytarr(cube_sz[1],cube_sz[2],cube_sz[3])
  assign = lonarr(cube_sz[1],cube_sz[2],cube_sz[3])
 
  G=4.29681E-3 ; pc M_sun^-1 (km/s)^2

  is_bound = mvir/mass LT 2 ;AND mvir/mass GE 1
  next_assgn = 1
  
 
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; CALCULATE Q VALUE 
; should be done before hand...? 
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

 ;WILL USE THE WEIGHTED CENTER POSITION INSTEAD
 ; ind_to_xyv, kernel_ind, sz=cube_sz, x=kernel_x, y=kernel_y, v=kernel_v
 ; xloc = rad*!values.f_nan
 ; yloc = xloc
  ;NOT SURE HOW THE VALUES AND KERNELS ARE INDEXED IN THE NEW STRUCTURE
 ; for i=0,(size(xloc))[1]-1 do begin
 ;   xloc[i,*] = kernel_x[i]
 ;   yloc[i,*] = kernel_y[i]
 ; endfor
   
 xloc = props.moments.mom1_x.val_meas
 yloc = props.moments.mom1_y.val_meas

; ADXY to go x, y -> RA, DEC

  make_axes, hdr, raxis=raxis, daxis=daxis
  readcol, 'freqcurves14feb2013.dat', v1,v2,v3,v4,v5 
;INPUT FILE.... OR HAVE Q VALUES ALREADY?

  ra = interpol(raxis, findgen(935), xloc)
  dec = interpol(daxis, findgen(601), yloc)

;Dynamical center, inclination, and PA from pety et al. 

; Deproject RA, DEC + {incl, pa, ctr} -> rgal, theta

  deproject, ra, dec, [173,21,raxis[461],daxis[303]],$
             rgrid=rgrid,tgrid=tgrid,/vector

; rgal + profile -> Q?

  Radius = rgrid*3600
  Density = (mass)/(!Pi*rad^2)  ; msun/pc^2
  Kappa = interpol(v4,v1,Radius)/1000      
  Q = (sigv*Kappa)/(!Pi*G*Density)

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; FIND REGIONS 
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  ; LOOP OVER KERNELS
  for i = 0L, n_elements(kernel_ind)-1 do begin
  if keyword_set(verbose) then $
     counter, i, n_elements(kernel_ind)-1, 'KERNEL   ' 
  ;  CHECK IF THIS IS EVER BOUND
     if total(is_bound[i,*]) eq 0 OR total(q[i,*] LT 1) EQ 0 then $
        continue

  ;  GET BOUNDEDNESS FOR THIS COLUMN
     this_bound = (reform(is_bound[i,*]))
     this_bound_ind = where(this_bound AND q[i,*] LT 1, bound_ct)
     
     if bound_ct LT 1 then continue 
  
     lowest_bound = min(levels[this_bound_ind])
     lev = where(levels EQ lowest_bound)
     

     
;  GENERATE THE MASK FOR THIS CLOUD
     this_mask = cube gt lowest_bound
     regions = label_region(this_mask, /ulong)
     this_reg = regions eq regions[kernel_ind[i]]
   ;  COPY TO TOTAL MASK
     ind = where(this_reg and regions NE 0 , num_ct)  
  
    if num_ct GT 0 and num_ct EQ props[i,lev].moments.npix.val then $
     if total(bound[ind]) NE  num_ct then begin  
       bound[ind] = 1B
       assign[ind] = next_assgn 
       index = [[index], [i, lev, num_ct]]
       next_assgn += 1
     endif  
  endfor

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; WRITE OUT FILES
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if n_elements(outfile) eq 0 then $
    outfile = 'bound_assignment.fits' 
    
   outfile_a = strmid(outfile,0,strpos(outfile, '.fits'))+'_assign.fits' 
   outfile_b = strmid(outfile,0,strpos(outfile, '.fits'))+'_isbound.fits' 

  
  
  
  out_hdr = hdr
  sxaddpar, hdr, "BUNIT", "ASSIGNMENT"
  writefits, outfile_a, assign, hdr

  out_hdr = hdr
  sxaddpar, hdr, "BUNIT", "IS BOUND"
  writefits, outfile_b, bound, hdr

  

end
