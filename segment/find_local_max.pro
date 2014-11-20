pro find_local_max $
   , data=data $
   , infile=infile $
   , mask=mask_in $
   , inmask=mask_file $
   , trueval=trueval $
   , hdr=hdr $
   , friends = friends $
   , specfriends = specfriends $
   , minpix = minpix $
   , minvchan = minvchan $
   , minarea = minarea $
   , all_neighbors = all_neighbors $
   , kernels = kernels $
   , delta = delta $
   , snr = delta_is_snr $
   , rmsfile = rmsfile $
   , inrms = rms $
   , minval = minval $
   , nodecimate = nodecimate $
   , justdecimate = justdecimate $
   , idl_out = idl_out $
   , text_out = text_out $
   , verbose = verbose


;+
;
; NAME:
;
;   FIND_LOCAL_MAX
;
; PURPOSE:
;   Generates a set of kernels (local maxima) from a CO data cube and
;   initial max. Kernels are found using the brightest point in a
;   search box with size friends by friends by specfiends.    
;
; CALLING SEQUENCE:
;    
;
; INPUTS:
;   INFILE -- Path to a .fits cube.
;   DATA -- (optional) CO Cube.  
;   INMASK -- Path to .fits mask (must be same size as data)    
;   MASK -- (optional) CO byte mask.
;   TRUEVAL -- (optional) truth value of mask. Defaults to >= 1.
;   HDR -- (optional) .fits Header (required if no filepath is specified). 
;   text_out -- Text file to save kernel locations in ascii format.
;   idl_out -- IDL file (.sav or .idl) to save kernel
;              locations in IDL format (variable name = kernel_ind).
;             
; KEYWORD PARAMETERS:
;   FRIENDS -- (optional) Pixels to search over in the x-y plane. Total search
;              box length is 2*Friends+1. Default is friends=3
;   SPECFRIENDS -- (optional) Pixels to search over in the v plane. Total search
;                  box length is 2*Specfriends+1. Default is
;                  specfriends=1
;
; OUTPUTS: 
;   KERNELS -- Array of local maxima.

; MODIFICATION HISTORY:
;      Originally written by Adam Leroy and Erik Rosolowsky.
;
;
;      Some documentation -- Mon Nov 25, 2013  Stephen Pardy 
;                     <spardy@astro.wisc.edu>
; 
;-



;+
;
; TBD:
;
; - proper island handling in the decimation
; - default mask creation call? debateable
; - noise assumed homogeneous?
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

  if n_elements(mask_in) eq 0 then begin
     if n_elements(mask_file) gt 0 then begin
        full_file_mask = file_search(mask_file, count=file_ct)
        if file_ct eq 0 then begin
           message, "Mask file not found.", /info
           return
        endif else begin
           mask = readfits(full_file_mask, mask_hdr)
        endelse
     endif else begin
        message, "Defaulting to a mask of finite elements.", /info
        mask = finite(data)
     endelse
  endif else begin
     mask = mask_in
  endelse

; If requested, use only the mask where it equals a certain true
; value. Useful for analyzing only part of an assignment cube, for
; example.
  if n_elements(trueval) ne 0 then begin
     mask = mask eq trueval
  endif

; Return in the case of an empty mask
  if total(mask) eq 0 then begin
     message, "Empty mask. Returning.", /info
     return
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; SET DEFAULTS AND DEFINTIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  szdata = size(data)

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; ERROR TRAP
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if n_elements(kernels) eq 0 and $
     keyword_set(just_decimate) then begin
     message, "Cannot decimate without a kernel list.", /info
  endif

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; SIGNAL-TO-NOISE AND RMS UNITS
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; DEFINE UNITS OF DELTA (S/N OR REAL)
  if n_elements(delta_is_snr) eq 0 then $
     delta_is_snr = 1B

  if n_elements(rmsfile) gt 0 then begin
     file_rms = file_search(rmsfile, count=file_ct)
     if file_ct eq 0 then begin
        message, "Noise file not found.", /info
        return
     endif else begin
        rms = readfits(file_rms, rms_hdr)
     endelse
  endif else begin
     if n_elements(rms) eq 0 then begin        
        rms = mad(data, /finite)
     endif
  endelse

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; SEARCH CRITERIA ...
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; SPATIAL SEARCH AREA IN PIXELS
  if n_elements(friends) eq 0 then $
     friends = 3

; THE SEARCH AREA ALONG THE THIRD DIMENSION IN PIXELS
  if n_elements(specfriends) eq 0 then $
     specfriends = 1
  if szdata[0] eq 2 then $
     specfriends = 0

; DEFINE CONNECTEDNESS
  if not(keyword_set(all_neighbors)) then $
     all_neighbors = 0b

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; REJECTION CRITERIA ...
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; CONTRAST WITH MERGE LEVEL
  if n_elements(delta) eq 0 then $
     delta = 3

; MINIMUM VOLUME
  if n_elements(minval) eq 0 then $
     minval = 0

; MINIMUM VOLUME
  if n_elements(minpix) eq 0 then $
     minpix = 4

; MINIMUM AREA
  if n_elements(minarea) eq 0 then $
     minarea = 1

; MINIMUM SPECTRAL EXTENT
  if n_elements(minvchan) eq 0 then $
     minvchan = 1

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DEFAULT OUTPUT NAMES
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if n_elements(text_out) eq 0 then $
     text_out = "lmax.txt"

;  if n_elements(idl_out) eq 0 then $
  if n_elements(idl_out_wmm) eq 0 then $
     idl_out = "lmax.idl"

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; PARE CUBE TO MINIMUM SIZE
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
; Convert the data inside the mask to a vector. Do the same for the
; noise if we have a cube.

  vectorify, data $
             , mask = mask $
             , x = x, y = y, v = v, t = t $
             , ind = cubeindex

  if n_elements(rms) eq n_elements(data) then begin
     vectorify, rms $
                , mask = mask $
                , x = x, y = y, v = v, t = e_t $
                , ind = cubeindex     
  endif else begin
     if n_elements(rms) ne 1 then begin
        message, "Size of rms is unexpected.", /info
        return
     endif
  endif

; Rebuild a minimum-sized cube from the vectorized data.

  cubify, x=x, y=y, v=v, t=t $
          , cube = minicube $
 ;         , pad = (friends > specfriends) $
          , pad = 3 $
          , dim_2d = (szdata[0] eq 2) $
          , indvec = cubeindex $
          , indcube = indcube

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; IDENTIFY KERNELS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; This step runs unless the "justdecimate" flag is set, in which case
; it is assumed that kernels have been supplied by the user.

  if keyword_set(justdecimate) eq 0 then begin
     sub_kernels = alllocmax(minicube $
                             , friends = friends $
                             , specfriends = specfriends $
                             , verbose = verbose)
  endif

; This step runs unless the "nodecimate" flag is set. The
; subroutine rejects all but significant kernels, defined by a set of
; user-tunable quantities.

  if keyword_set(nodecimate) eq 0 then begin
     sigma=mad(data,/finite) ; estimate here, since RMS of masked cube may be different
;     print,"Calling decimate_kernels with sigma value: ",sigma
     sub_kernels = $
        decimate_kernels(sub_kernels $
                         , minicube $
                         , all_neighbors = all_neighbors $
                         , delta = delta $
                         , snr = delta_is_snr $
                         , sigma = sigma $
                         , minval = minval $
                         , minpix = minpix $
                         , minarea = minarea $
                         , minvchan = minvchan $
                         , verbose = verbose $
                         , valid_merger = merger_matrix)
  endif

  kernels = indcube[sub_kernels]

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
