pro cube_hastrom $
;  Input and output
   , data = data_in $
   , hdr_in = hdr_in $
   , orig_vaxis = orig_vaxis $
   , outfile = file_out $
   , outcube = output $
   , outhdr = hdr_out $
;  Target astrometry and velocity axis
   , target_hdr = target_hdr $   
   , vaxis = target_vaxis $
;  How to do the positional part of the interpolation
   , pinterp=pinterp $
   , pmissing=pmissing $
;  How to do the velocity part of the interpolation
   , vinterp=vinterp $
   , vmissing=vmissing $
;  Define operations to be carried out
   , operation=operation $
   , verbose = verbose $
   , quiet=quiet $
   , _extra = _extra

;+
; NAME:
;
; cube_hastrom
;
; PURPOSE:
;
; Accept a file name or a cube + header and a target header containing
; WCS astrometry. Align the input cube to the target astrometry and either
; return the cube or write it to a new file.
;
; CATEGORY:
;
; Science program (astrometry utility).
;
; CALLING SEQUENCE:
;
; INPUTS:
;
; OPTIONAL INPUTS:
;
; KEYWORD PARAMETERS:
;
; OUTPUTS:
; 
; The newly-aligned cube.
;
; OPTIONAL OUTPUTS:
;
; An updated header (hdr_out) and a file written to disk (file_out).
;
; COMMON BLOCKS:
;
; None.
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURES USED:
;
; Needs IDLAstro installed.
;
; MODIFICATION HISTORY:
;
; documented - 09 apr 08 leroy@mpia.de
;
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFAULTS AND DEFINITIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Figure out what operation we're performing
  
  if n_elements(operation) eq 0 then begin
     op = 'BOT'
  endif else begin
     valid_op = ['POS','VEL','BOT']
     op = strupcase(strmid(operation,0,3))
     if total(op eq valid_op) eq 0 then begin        
        message, 'Invalid choice. Defaulting to both (position + velocity).', /info
     endif
  endelse

; Require a target astrometry and/or velocity axis

  if op eq 'POS' or op eq 'BOT' then begin
     if n_elements(target_hdr) eq 0 then begin
        message, 'Require a target astrometry for astrometric'+$
                 ' alignment. Returning', /info
        return
     endif     
  endif

  if op eq 'VEL' then begin
     if n_elements(target_hdr) eq 0 and $
        n_elements(target_vaxis) eq 0 then begin
        message, 'Require a target velocity axis for velocity'+$
                 ' alignment. Returning', /info
        return
     endif          
  endif

; Read in the cube to be aligned if a file name is supplied. If no
; file name is supplied then make sure there's a header.

  if size(data_in, /type) eq size("hello", /type) then begin
     fname = data_in
     fits_read, fname, data, hdr
  endif else begin
     data = data_in
     if n_elements(hdr_in) gt 0 then begin
        hdr = hdr_in 
     endif else begin
        if op eq 'VEL' then begin
           if n_elements(orig_vaxis) eq 0 then begin
              message, "Velocity alignment requires original"+ $
                       "velocity axis or header. Returning.", /info
              return
           endif
        endif else begin
           message, "Alignment requires coordinate information"+$
                    ". Returning.", /info
           return
        endelse
     endelse
  endelse

; Defaults for the positional interpolation

  if op eq 'BOT' or op eq 'POS' then begin

     if n_elements(pmissing) eq 0 then $
        pmissing = !values.f_nan

     if n_elements(pinterp) eq 0 then $
        pinterp = 2

     print, pinterp, pmissing
     print, pinterp, pmissing

  endif

; Defaults for the velocity interpolation

  if op eq 'BOT' or op eq 'VEL' then begin

     if n_elements(vmissing) eq 0 then $
        vmissing = !values.f_nan

     if n_elements(vinterp) eq 0 then $
        vinterp = 1

     valid_vinterp = [1,2,3]
     if total(valid_vinterp eq vinterp) eq 0 then begin
        message, "Defaulting to linear interpolation "+ $
                 "for velocity regridding", /info
        vinterp = 1
     endif

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; WORK OUT THE TARGET ASTROMETRY
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Note the zie of the input data

  sz = size(data)

; Deal with common radio convention issue

  if n_elements(hdr) ne 0 then begin
     hdr = gls_to_sfl(hdr)
  endif

; Define the output size

  if op eq 'POS'  or op eq 'BOT' then begin
     nx = sxpar(target_hdr, 'NAXIS1')
     ny = sxpar(target_hdr, 'NAXIS2')
     if op eq 'POS' then nz = sz[3]
  endif
  
; Make sure that we have a target velocity axis

  if op eq 'VEL' or op eq 'BOT' then begin
     if n_elements(target_vaxis) eq 0 then begin
        make_axes, target_hdr, vaxis=target_vaxis, /vonly
     endif
     nz = n_elements(target_vaxis)
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ASTROMETRIC ALIGNMENT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
; Do the astrometric alignment by looping plane by plane over the cube
; and re-running hastrom. Inefficient - if speed becomes a real issue
; then we should look at extracting the under-the-hood interpolation
; code from hastrom.

  if (op eq 'POS') or (op eq 'BOT') then begin

     if keyword_set(quiet) eq 0 then $
        message, "... performing astrometric alignment.", /info

;    Initialize the output for this part

     output = fltarr(nx,ny,sz[3])*!values.f_nan

;    Copy the original and target header and collapse them to two
;    dimensions so that they can be used with hastrom

     hdr_copy = twod_head(hdr)
     target_hdr_copy = twod_head(target_hdr)
     
     for kk = 0, sz[3]-1 do begin

        if keyword_set(quiet) eq 0 then $
           counter, kk+1, nz, 'CUBE_HASTROM: plane '
        
;       Copy the header and the channel map

        new_hdr_copy = hdr_copy
        this_plane = data[*,*,kk]

;       Do the interpolation

        hastrom, this_plane, new_hdr_copy, target_hdr_copy $
                 , missing=pmissing $
                 , interp=pinterp, cubic=-0.5

;       Save the result in the data cube

        output[*,*,kk] = this_plane

     endfor

;    Make an updated header

     hdr_out = new_hdr_copy

     sxaddpar, hdr_out,'NAXIS',sxpar(hdr,'NAXIS')

     n3 = sxpar(hdr,'NAXIS3',count=ct)
     if (ct gt 0) then sxaddpar,hdr_out,'NAXIS3',n3,after='NAXIS2'

     n4 = sxpar(hdr,'NAXIS4',count=ct)
     if (ct gt 0) then sxaddpar,hdr_out,'NAXIS4',n4,after='NAXIS3'     

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; VELOCITY ALIGNMENT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; Do the velocity alignment using the shuffle routine to avoid
; double-coding. The operation is the same, though there is a slight
; gain in overhead. The call is a bit different depending on whether
; we have already done astrometric alignment.

  if (op eq 'VEL') or (op eq 'BOT') then begin

     if keyword_set(quiet) eq 0 then $
        message, "... performing velocity alignment.", /info
     
;    Make sure we have the original velocity axis as a vector

     if n_elements(orig_vaxis) eq 0 then begin
        make_axes, hdr, vaxis=orig_vaxis, /vonly
     endif

     if (op eq 'BOT') then begin
        output_copy = $
           shuffle ( $
           spec = output $
           , vaxis = orig_vaxis $
           , zero = 0.0 $
           , target_vaxis = target_vaxis $
           , interp = vinterp $
           , missing = vmissing $
           , quiet = quiet)
        output = output_copy
     endif else begin
        output = $
           shuffle ( $
           spec = data $
           , vaxis = orig_vaxis $
           , zero = 0.0 $
           , target_vaxis = target_vaxis $
           , interp = vinterp $
           , missing = vmissing $
           , quiet = quiet)
     endelse

;    Update the header
     if n_elements(target_hdr) gt 0 then begin
        sxaddpar, hdr_out, 'CTYPE3', sxpar(target_hdr, 'CTYPE3')
        sxaddpar, hdr_out, 'CUNIT3', sxpar(target_hdr, 'CUNIT3')
        sxaddpar, hdr_out, 'SPECSYS', sxpar(target_hdr, 'SPECSYS')
     endif
     sxaddpar, hdr_out, 'CRVAL3', target_vaxis[0]
     sxaddpar, hdr_out, 'CRPIX3', 1.0
     sxaddpar, hdr_out, 'CDELT3', target_vaxis[1]-target_vaxis[0]
     sxaddpar, hdr_out, 'NAXIS3', n_elements(target_vaxis)

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; OUTPUT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; If requrested, write the cube to disk

  if n_elements(file_out) gt 0 then begin

     writefits,file_out,output,hdr_out

  endif

end
