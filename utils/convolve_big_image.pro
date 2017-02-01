pro convolve_big_image $
   , image = image_file $
   , kernel = kernel_file $
   , subimsize = subimsize $
   , overlap = overlap $
   , scratch_root = scratch_root $
   , clobber = clobber $
   , noclean = noclean $
   , outfile = outfile $
   , show = show $
   , _extra = _extra

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SET DEFAULTS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(subimsize) eq 0 then $
     subimsize = 1000L

  if n_elements(overlap) eq 0 then $
     overlap = 300L

  if n_elements(scratch_root) eq 0 then $
     scratch_root = 'cbi_scratch_'

  test = file_search(scratch_root+'*.fits', count=count)
  if count ne 0 and keyword_set(clobber) eq 0 then begin
     message, "Files with the proposed scratch name found and clobber not set.", /info
     return
  endif

  if count ne 0 and keyword_set(clobber) then begin
     spawn, 'rm -rf '+scratch_root+'*.fits'
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CARVE THE IMAGE UP
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  fits_read, image_file, orig_image, orig_hdr
  orig_sz = size(orig_image, /dim)
  n_dim = n_elements(orig_sz)
  
  if n_dim ne 2 then begin
     message, "Only works for two dimensional image.", /info
  endif

  xlo = [0L]
  xhi = ([subimsize]) < (orig_sz[0]-1)
  maxx = orig_sz[0]/(subimsize - overlap)

  ylo = [0L]
  yhi = ([subimsize]) < (orig_sz[1]-1)
  maxy = orig_sz[1]/(subimsize - overlap)

  for ii = 0, maxx-1 do begin
     if max(xhi) eq (orig_sz[0]-1) then $
        continue
     new_xlo = (max(xhi)-overlap) > 0
     new_xhi = (new_xlo + subimsize) < (orig_sz[0]-1)
     xlo = [xlo, new_xlo]
     xhi = [xhi, new_xhi]
  endfor

  for ii = 0, maxy-1 do begin
     if max(yhi) eq (orig_sz[1]-1) then $
        continue
     new_ylo = (max(yhi)-overlap) > 0
     new_yhi = (new_ylo + subimsize) < (orig_sz[1]-1)
     ylo = [ylo, new_ylo]
     yhi = [yhi, new_yhi]
  endfor
  
  nx = n_elements(xlo)
  ny = n_elements(ylo)
  counter = 0L

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; TRAP THE CASE WHERE THE IMAGE IS IN ONE PANE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if nx eq 1 and ny eq 1 then begin

     message $
        , "The image is small, calling CONVOLVE_IMAGE as normal." $
        , /info

     convolve_image $
        , kernel=kernel_file $
        , image=image_file $
        , outfile=outfile $
        , save_kernel=0B
     
     return

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEAL WITH A MORE COMPLEX SITUATION
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  for ii = 0L, nx-1 do begin
     for jj = 0L, ny-1 do begin

        message, "Extracting file "+str(counter), /info

        counter += 1
        scratch_file_name = scratch_root + str(counter) + '.fits'

        hextract, orig_image, orig_hdr $
                  , new_image, new_hdr $
                  , xlo[ii], xhi[ii], ylo[jj], yhi[jj]
        
        writefits, scratch_file_name, new_image, new_hdr

        if n_elements(xlo_use) eq 0 then begin
           if xlo[ii] eq 0 then $
              xlo_use = [xlo[ii]] $
           else $
              xlo_use = [xlo[ii]+(overlap/2)]

           if xhi[ii] eq (orig_sz[0]-1) then $ 
              xhi_use = [xhi[ii]] $
           else $
              xhi_use = [xhi[ii]-(overlap/2)]

           if ylo[jj] eq 0 then $
              ylo_use = [ylo[jj]] $
           else $
              ylo_use = [ylo[jj]+(overlap/2)]

           if yhi[jj] eq (orig_sz[1]-1) then $ 
              yhi_use = [yhi[jj]] $
           else $
              yhi_use = [yhi[jj]-(overlap/2)]
              
        endif else begin
           if xlo[ii] eq 0 then $
              xlo_use = [xlo_use, xlo[ii]] $
           else $
              xlo_use = [xlo_use, xlo[ii]+(overlap/2)]

           if xhi[ii] eq (orig_sz[0]-1) then $ 
              xhi_use = [xhi_use, xhi[ii]] $
           else $
              xhi_use = [xhi_use, xhi[ii]-(overlap/2)]

           if ylo[jj] eq 0 then $
              ylo_use = [ylo_use, ylo[jj]] $
           else $
              ylo_use = [ylo_use, ylo[jj]+(overlap/2)]

           if yhi[jj] eq (orig_sz[1]-1) then $ 
              yhi_use = [yhi_use, yhi[jj]] $
           else $
              yhi_use = [yhi_use, yhi[jj]-(overlap/2)]
              
        endelse

     endfor
  endfor

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RUN THE CONVOLUTION ON EACH PIECE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  for nn = 1, counter do begin

     message, "Convolving file "+str(nn), /info

     scratch_file_in = scratch_root + str(nn)+'.fits'
     scratch_file_out = scratch_root + str(nn) + '_conv.fits'
     
     convolve_image $
        , kernel=kernel_file $
        , image=scratch_file_in $
        , outfile=scratch_file_out $
        , save_kernel=0B

  endfor  

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; STITCH THE IMAGE BACK TOGETHER
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
  rms = mad(orig_image)
  new_image = fltarr(orig_sz[0], orig_sz[1])

  for nn = 1, counter do begin

     message, "Re-stitching file "+str(nn), /info

     scratch_file = scratch_root + str(nn) + '_conv.fits'     

     fits_read, scratch_file, image_piece, scratch_hdr

     hastrom, image_piece, scratch_hdr, orig_hdr $
              , interp=2, cubic=-0.5, missing=!values.f_nan
     
     new_image[xlo_use[nn-1]:xhi_use[nn-1], ylo_use[nn-1]:yhi_use[nn-1]] = $
        image_piece[xlo_use[nn-1]:xhi_use[nn-1], ylo_use[nn-1]:yhi_use[nn-1]]

     if keyword_set(show) then begin
        !p.multi=[0,1,1]
        disp, new_image, /sq, max=3*rms, min=-3.*rms
        !p.multi=[0,1,1]
     endif

  endfor

  nan_ind = where(finite(orig_image) eq 0 or (orig_image eq 0), nan_ct)
  if nan_ct gt 0 then new_image[nan_ind] = !values.f_nan

  writefits, outfile, new_image, orig_hdr

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CLEAN UP SCRATCH FILES
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(noclean) eq 0 then begin
     message, 'Removing scratch files.', /info
     spawn, 'rm -rf '+scratch_root+'*.fits'
  endif

end
   
