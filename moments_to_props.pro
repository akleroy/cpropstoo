pro moments_to_props $
   , indata = indata $
   , infile = infile $
   , outfile = outfile $
   , hdrfile = hdrfile $
   , inhdr = inhdr $
   , dist = dist $
   , xco = xco $
   , rmstorad = rmstorad $
   , chantosig = chantosig $
   , vircoeff = vircoeff $
   , verbose = verbose

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

  if sz[0] eq 1 then begin
     props = replicate(empty_cloud_struct(), sz[1])
  endif

  if sz[0] eq 2 then begin
     props = replicate(empty_cloud_struct(), sz[1], sz[2])
  endif

  n_mom = n_elements(moments)
  
  for i = 0, n_mom-1 do begin
       
     if keyword_set(verbose) then begin
        counter, i+1, n_mom, "Properties for moment "
     endif

     this_prop = calc_props_from_moments( $
                 moments[i] $
                 , hdr = hdr $
                 , astr = astr $
                 , vaxis = vaxis $
                 , dist = dist $
                 , xco = xco $
                 , bmaj = bmaj $
                 , bmin = bmin $
                 , bpa = bpa $
                 , rmstorad = rmstorad $
                 , chantosig = chantosig $
                 , vircoeff = vircoeff)
     
     props[i] = this_prop

  endfor

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&
; OUTPUT
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&

  if n_elements(outfile) gt 0 then begin
     if n_elements(levels) gt 0 then begin
        save, file=outfile, props, levels, kernel_ind, hdr, merger_matrix
     endif else begin
        save, file=outfile, props
     endelse
  endif
  
end                             ; OF MOMENTS_TO_PROPS
