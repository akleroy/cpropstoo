pro extract_spectra $
   , data=data $
   , infile=infile $
   , assign=assign $
   , inassign=inassign $
   , inprops=inprops $
   , hdr=hdr $
   , doaverage = doaverage $
   , idl_file=idl_file 

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
;       inprops: idl save file with input props structure
;
;       hdr: header for cube.
;
;       doaverage: average the spectra together. Otherwise, the sum of
;       the spectra are returned.
;
; Output:
;
;       idl_file: IDL save file with spectra added to props structure
;
; Date          Programmer              Description of Changes
;----------------------------------------------------------------------
; 10/16/2014    A.A. Kepley             Original Code
; 10/29/2014    A.A. Kepley             double-check on code + added
;                                       spectra to props structure.
;
;     
;-

; read in files or cubes
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

; read in old props
  if n_elements(inprops) eq 0 then begin
     message,/info,"need input props structure (inprops=)"
     return
  endif else begin
     restore,inprops,/verb
  endelse

; average spectra or total spectra
  if n_elements(doaverage) eq 0 then doaverage = 0

; get velocity information
  make_axes,hdr, vaxis=vaxis,/vonly
  nchan = n_elements(vaxis)

; initialize the output structure
  nassign = max(assign)
  for i = 0, nassign - 1 do begin
     this_props_spectra = add_spectra_fields(props[i],nchan)    
     this_props_spectra = alphabetize_struct(this_props_spectra)
     if i eq 0 then  begin
        props_spectra = this_props_spectra
     endif else begin
        props_spectra = [props_spectra,this_props_spectra]
     endelse
  endfor

; loop through the clumps, extract data from clump, and sum the clump to get spectra
  for i = 0, nassign - 1 do begin

     idx = where(assign eq i+1, count)

     mask = assign*0.0
     mask[idx] = 1.0

     xx_sum = total(mask*data,1,/double,/nan)
     this_spec = total(xx_sum,1,/double,/nan)

     if doaverage ge 1 then this_spec = this_spec/props_spectra[i].area 

     props_spectra[i].spectra = this_spec
     props_spectra[i].velocity = vaxis

  endfor

  ; write out the spectra structure
  props = props_spectra 
  save, /verb, props, filename=idl_file

end
