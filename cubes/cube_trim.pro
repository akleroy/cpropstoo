pro cube_trim $
   , data = in_data $
   , hdr_in = in_hdr $
   , out_cube = out_cube $
   , out_hdr = out_hdr $
   , outfile = out_file $
   , quiet = quiet
  
  if size(in_data, /type) eq size("hello", /type) then begin
     fname = in_data
     fits_read, fname, data, hdr
  endif else begin
     data = in_data     
     if n_elements(in_hdr) gt 0 then $
        hdr = in_hdr
  endelse
  
  fin_ind = where(finite(data))
  sz = size(data)
  ind_to_xyv $
     , fin_ind $
     , x=x $
     , y=y $
     , v=v $
     , sz=sz
  xmin = min(x, /nan)
  xmax = max(x, /nan)
  ymin = min(y, /nan)
  ymax = max(y, /nan)
  if keyword_set(quiet) eq 0 then begin
     print, "X: From size "+str(sz[1])+" x = "+str(xmin)+" - "+str(xmax)
     print, "Y: From size "+str(sz[2])+" y = "+str(ymin)+" - "+str(ymax)
  endif

  if sz[0] eq 2 then begin
     hextract, data, hdr, out_cube, out_hdr $
               , xmin, xmax, ymin, ymax, /silent
     if n_elements(out_file) gt 0 then begin
        writefits, out_file, out_cube, hdr
     endif
     return
  endif  
  
  vmin = min(v, /nan)
  vmax = max(v, /nan)
  if keyword_set(quiet) eq 0 then begin
     print, "V: From size "+str(sz[3])+" v = "+str(vmin)+" - "+str(vmax)
  endif

  out_cube = $
     extract_planes( $
     cube=data $
     , hdr=hdr $
     , from_plane=vmin $
     , to_plane=vmax $
     , new_hdr=new_hdr)
  hdr = new_hdr
  data = out_cube
  
  out_cube = $
     cube_hextract( $
     cube_in = data $
     , hdr_in = hdr $
     , hdr_out = out_hdr $
     , x0 = xmin $
     , x1 = xmax $
     , y0 = ymin $
     , y1 = ymax $
     , noreturn=noreturn $
                  )
  
  if n_elements(out_file) gt 0 then begin
     writefits, out_file, out_cube, out_hdr
  endif

end
