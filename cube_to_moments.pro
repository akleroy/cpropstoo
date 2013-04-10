pro cube_to_moments $
   , data=data $
   , infile=infile $
   , assign=assign $
   , inassign=inassign $
   , hdr=hdr $
   , outfile=outfile $
   , verbose=verbose $
   , cloudlist=cloudlist

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
        data = readfits(file_data, hdr, /silent)
     endelse
  endif

  if n_elements(assign) eq 0 then begin
     file_assign = file_search(inassign, count=file_ct)
     if file_ct eq 0 then begin
        message, "Assignment cube not found.", /info
        return
     endif else begin
        assign = readfits(file_assign, assign_hdr, /silent)
     endelse
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CALL THE MEASUREMENT CODE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; EXTRACT PIXELS WITH ASSIGNMENTS
  ind = where(assign ne 0, ct)
  if ct eq 0 then begin
     message, "No valid assignments.", /info
     return
  endif  

; GENERATE A CLOUD LIST IF NOT SUPPLIED
  if n_elements(cloudlist) eq 0 then begin
     cloudlist = assign[ind]
     cloudlist = cloudlist[sort(cloudlist)]
     cloudlist = cloudlist[uniq(cloudlist)]
  endif

; VECTORIZE (SPEEDS UP SPARSE CASE)
  assign = assign[ind]
  t = data[ind]
  ind_to_xyv, ind, x=x, y=y, v=v, sz=size(data)

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CALL THE MEASUREMENT CODE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; LOOP OVER CLOUDS
  nclouds = n_elements(cloudlist)

  for i = 0, nclouds-1 do begin
       
     if keyword_set(verbose) then begin
        counter, i+1, nclouds, "Moments for cloud "
     endif

     ind = where(assign eq cloudlist[i], ct)
     if ct eq 0 then continue    

     this_t = t[ind]
     this_x = x[ind]
     this_y = y[ind]
     this_v = v[ind]
     
     this_mom = $
        measure_moments(x=this_x, y=this_y, v=this_v, t=this_t $
                        , /extrap, extarg=0)

     this_mom.peaknum = cloudlist[i]

     if n_elements(moments) eq 0 then begin
        moments = [this_mom]        
     endif else begin
        moments = [moments, this_mom]
     endelse

  endfor

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SAVE TO DISK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  save, file=outfile, moments

end                             ; OF CUBE_TO_MOMENTS
