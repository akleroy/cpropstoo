pro moments_to_props $
   , indata = indata $
   , infile = infile $
   , outfile = outfile $ ; should be deprecated ? 
   , hdrfile = hdrfile $
   , inhdr = inhdr $
   , dist = dist $
   , alpha = alpha $
   , rmstorad = rmstorad $
   , chantosig = chantosig $
   , vircoeff = vircoeff $
   , verbose = verbose $
   , text_file = text_file $ ; give a directory and file stem here 
   , idl_file = idl_file

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

  if n_elements(outfile) gt 0 or $
     n_elements(idl_file) gt 0 then begin

     if n_elements(levels) gt 0 then begin
        save, file=outfile, props, moments, levels, kernel_ind, hdr, merger_matrix
     endif else begin
        save, file=outfile, props, moments
     endelse

  endif

  if n_elements(text_file) gt 0 then begin
     ; check to see if the text_file contains .txt extension
     ; remove it if it does (we will add these later)
     if strmatch(text_file, '*.txt') then $
        text_file = "_"+strmid(text_file, 3,/REVERSE_OFFSET)
     write_props_text, props,  file=text_file
  endif
  
end                             ; OF MOMENTS_TO_PROPS
