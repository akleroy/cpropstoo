pro moments_to_props $
   , indata = indata $
   , infile = infile $
   , hdrfile = hdrfile $
   , inhdr = inhdr $
   , dist = dist $
   , alpha = alpha $
   , rmstorad = rmstorad $
   , chantosig = chantosig $
   , vircoeff = vircoeff $
   , verbose = verbose $
   , text_file = text_file $
   , idl_file = idl_file $
   , fits_file = fits_file

  compile_opt idl2

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; DEFAULTS & DEFINITIONS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
  
; DISTANCE
  if n_elements(dist) eq 0 then begin
     message, "Need a distance to derive properties.", /info
     return
  endif

; INPUT MOMENT DATA
  if n_elements(indata) eq 0 then begin

     if n_elements(infile) gt 0 then begin
        restore, infile
     endif else begin
        message, "Need input moments data or file", /info
     endelse

  endif  

; INPUT HEADER FILE
  if n_elements(inhdr) eq 0 then begin

     if n_elements(hdrfile) gt 0 then begin
        hdr = headfits(hdrfile)
     endif else begin
        message, "Need input header data or file to extract header.", /info
     endelse

  endif  

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; LOOP OVER MOMENTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
 
  sz = size(moments)

  n_mom = n_elements(moments)
  
  for i = 0, n_mom-1 do begin
       
     if keyword_set(verbose) then begin
        counter, i+1, n_mom, "Properties for moment "
     endif

     this_prop = calc_props_from_moments( $
                 moments[i] $
                 , empty_props = empty_props $
                 , hdr = hdr $
                 , astr = astr $
                 , vaxis = vaxis $
                 , dist = dist $
                 , alpha = alpha $
                 , bmaj = bmaj $
                 , bmin = bmin $
                 , bpa = bpa $
                 , rmstorad = rmstorad $
                 , chantosig = chantosig $
                 , vircoeff = vircoeff)

     if n_elements(props) eq 0 then begin

        if sz[0] eq 1 then begin
           props = replicate(empty_props, sz[1])
        endif

        if sz[0] eq 2 then begin
           props = replicate(empty_props, sz[1], sz[2])
        endif

     endif
     
     props[i] = this_prop

  endfor

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; OUTPUT
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if n_elements(idl_file) gt 0 then begin
     if n_elements(levels) gt 0 then begin
        save, file=idl_file, props, levels, kernel_ind, hdr, merger_matrix
     endif else begin
        save, file=idl_file, props
     endelse
  endif

  if n_elements(text_file) gt 0 then begin
     ; write to csv is super easy 
     ; need to tweak this
     write_csv, text_file, props, header=tag_names(props)
  endif
  
  if n_elements(fits_file) gt 0 then begin 
     mwrfits, props, fits_file, /create
  endif 

end                             ; OF MOMENTS_TO_PROPS
