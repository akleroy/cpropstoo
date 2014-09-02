pro find_kernels_from_assign $
   , data=data $
   , infile=infile $
   , assign=assign_in $
   , inassign=assign_file $
   , hdr=hdr $
   , kernels = kernels $
   , idl_out = idl_out $
   , text_out = text_out $
   , verbose = verbose


;+
;
; NAME:
;
;   FIND_KERNELS_FROM_ASSIGN
;
; PURPOSE:
;
;   Extracts a set of kernels (local maxima) from an assignment
;   cube. Writes the output to an IDL or text file.
;
; CALLING SEQUENCE:
;    
;
; INPUTS:
;
;   INFILE -- Path to a .fits cube.
;   DATA -- (optional) data cube.  
;
;   INASSIGN -- Path to .fits assignment cube (must be same size as data)    
;   ASSIGN -- (optional) assignment cube.
;
;   HDR -- (optional) .fits Header (required if no filepath is specified).   
;             
; KEYWORD PARAMETERS:
;
; OUTPUTS: 
;
;   KENELS -- array of local maxima.
;   IDL_OUT -- an IDL save file
;   TEXT_OUT -- a text file
;
; MODIFICATION HISTORY:
; 
;-

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

  if n_elements(assign_in) eq 0 then begin
     if n_elements(assign_file) gt 0 then begin
        full_file_assign = file_search(assign_file, count=file_ct)
        if file_ct eq 0 then begin
           message, "Assignment file not found.", /info
           return
        endif else begin
           assign = readfits(full_file_assign, assign_hdr)
        endelse
     endif else begin
        message, "Need an assignment cube to proceed. Returning", /info
        return
     endelse
  endif else begin
     assign = assign_in
  endelse

; Return in the case of an empty mask
  if total(assign) eq 0 then begin
     message, "Empty assignment cube. Returning.", /info
     return
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SET DEFAULTS AND DEFINTIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  szdata = size(data)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DEFAULT OUTPUT NAMES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if n_elements(text_out) eq 0 then $
     text_out = "lmax.txt"

  if n_elements(idl_out) eq 0 then $
     idl_out = "lmax.idl"

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; IDENTIFY KERNELS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  nreg = max(assign)
  kernels = lonarr(nreg)
  found = bytarr(nreg)

  for i = 1, nreg do begin
     if total(assign eq i) eq 0 then $
        continue
     dummy = max((assign eq i)*data, maxind, /nan)
     kernels[i-1] = maxind
     found[i-1] = 1B
  endfor

  keep = where(found, keep_ct)
  if keep_ct eq 0 then begin
     message, "No valid regions found.", /info
     kernels = -1
     return
  endif
  kernels = kernels[keep]

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; WRITE TO DISK
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  write_kernels $
     , kernels $
     , sz = size(data) $
     , cube = data $
     , hdr = hdr $
     , text_file = text_out $
     , idl_file = idl_out $
     , merger = merger_matrix
  
  return

end

