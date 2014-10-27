pro extract_spectra $
   , data=data $
   , infile=infile $
   , assign=assign $
   , inassign=inassign $
   , spectra=spectra $
   , hdr=hdr $
   , vel=vaxis

;+
; Purpose: extract spectra from regions in an assignment cube
;
; Input:
;       data: fits file with data cube
;
;       assign: fits file with assignment cube
;
;       infile: cube
;
;       inassign: assignment cube
;
;       hdr: header for cube.
;
;       vel: velocity axis
;
; Output:
;
;       IDL save file with spectra in it as an array
;
; Date          Programmer              Description of Changes
;----------------------------------------------------------------------
; 10/16/2014    A.A. Kepley             Original Code
;
;     
;-

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

; get velocity information
  make_axes,hdr, vaxis=vaxis,/vonly

; loop through the clumps, extract data from clump, and sum the clump to get spectra
  max_assign = max(assign)
  spectra = replicate({peaknum:0,$
                       vel:dblarr(n_elements(vaxis)),$
                       t:dblarr(n_elements(vaxis))},max_assign)

  for i = 0, max_assign - 1 do begin

     idx = where(assign eq i+1, count)

     mask = assign*0.0
     mask[idx] = 1.0

     xx_sum = total(mask*data,1,/double,/nan)
     myspec = total(xx_sum,1,/double,/nan)/count

     spectra[i].peaknum = i+1
     spectra[i].t = myspec 
     spectra[i].vel = vaxis

  endfor


end
