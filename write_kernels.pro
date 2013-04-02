pro write_kernels $
   , kernel_ind $
   , sz = sz $
   , cube = cube $
   , hdr = hdr $
   , text_file = text_file $
   , idl_file = idl_file $
   , merger = merger_matrix

;+
;
; Write a collection of "kernels" to disk. These kernels are local
; maxima used for further analysis by CPROPS or dendro.
;
;-

  if n_elements(sz) eq 0 and n_elements(cube) eq 0 then begin
     message, "Need the cube or a size to put the kernels in context."
     return
  endif
  
  if n_elements(sz) eq 0 then begin
     sz = size(cube)
  endif

  ind_to_xyv, kernel_ind, x=xpix, y=ypix, v=zpix, sz=sz

  if n_elements(cube) ne 0 then begin
     intens = cube[xpix, ypix, zpix]
  endif else begin
     intens = xpix*!values.f_nan
  endelse

  if n_elements(hdr) ne 0 then begin
     xyad, hdr, xpix, ypix, ra, dec
     make_axes, hdr, vaxis=vaxis, /vonly
     vel = vaxis[zpix]
  endif else begin
     ra = xpix*!values.f_nan
     dec = xpix*!values.f_nan     
     vel = xpix*!values.f_nan
  endelse

  if n_elements(text_file) gt 0 then begin
     openw, 1, text_file
     printf, 1, "# file: "
     printf, 1, "# Column 1: x [pix]"
     printf, 1, "# Column 2: y [pix]"
     printf, 1, "# Column 3: z [pix]"
     printf, 1, "# Column 4: R.A. [deg]"
     printf, 1, "# Column 5: Dec. [deg]"
     printf, 1, "# Column 6: Velocity [km/s]"
     printf, 1, "# Column 7: Intensity"
     for i = 0, n_elements(kernel_ind)-1 do begin
        line = ''
        line += str(xpix[i])+' '
        line += str(ypix[i])+' '
        line += str(zpix[i])+' '
        line += str(ra[i])+' '
        line += str(dec[i])+' '
        line += str(vel[i])+' '
        line += str(intens[i])
        printf, 1, line
     endfor
     close, 1
  endif

  if n_elements(idl_file) gt 0 then begin
     save, file=idl_file $
           , kernel_ind, xpix, ypix, zpix, ra, dec, vel, intens, merger_matrix
  endif

end
