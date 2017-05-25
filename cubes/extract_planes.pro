function extract_planes $
   , cube=in_cube $
   , hdr=in_hdr $
   , from_plane=from_plane $
   , to_plane=to_plane $
   , by_value=by_value $
   , outfile=outfile $
   , new_hdr=new_hdr

;+
; NAME:
;
; extract_planes
;
; PURPOSE:
;
; Extract a subset of velocity planes from a cube while also updating
; the header. Accepts the range in either channel numbers or data
; units. Assumes that the spectral axis is the third axis.
;
; CATEGORY:
;
; cube utility
;
; CALLING SEQUENCE:
;
; new_cube = extract_planes(cube=cube, hdr=hdr, from=10, to=50, $
;                           outfile="new_cube.fits", new_hdr=out_hdr, $
;                           /by_value)
;
; INPUTS:
;
; cube: A file name or data cube.
;
; hdr: the header, needed if no file name is supplied
;
; from_plane: the starting plane
;
; to_plane: the ending plane
;
; OPTIONAL INPUTS:
;
; outfile: the output file
;
; KEYWORD PARAMETERS:
;
; by_value: the planes are given in value units
;
; OUTPUTS:
;
; a smaller data cube
;
; OPTIONAL OUTPUTS:
;
; new_hdr: the output header
;
; a data cube written to outfile
;
; COMMON BLOCKS:
;
;
;
; SIDE EFFECTS:
;
;
;
; RESTRICTIONS:
;
;
;
; PROCEDURE:
;
;
;
; EXAMPLE:
;
;
;
; MODIFICATION HISTORY:
;
; drafted - Dec 14
;
;-  

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; READ IN OR COPY DATA
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

; Attempt to smartly detect whether the file is a string (FITS file
; name) or data. Error catch the need for a header.

  if size(in_cube, /type) eq size("hello", /type) then begin
     fname = in_cube
     fits_read, fname, cube, hdr
  endif else begin
     cube = in_cube
     if n_elements(in_hdr) gt 0 then $
        hdr = in_hdr
  endelse

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; EXTRACT THE PLANES OF INTEREST
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  make_axes, /vonly, hdr, vaxis=vaxis

  if keyword_set(by_value) then begin
     chan = findgen(n_elements(vaxis))
     to_plane_chan = round(interpol(chan, vaxis, to_plane))
     from_plane_chan = round(interpol(chan, vaxis, from_plane))
  endif else begin
     to_plane_chan = to_plane
     from_plane_chan = from_plane
  endelse

; First error check and then extract the planes of interest.

  sz = size(cube,/dim)
  if from_plane_chan lt 0 or to_plane_chan ge sz[2] then begin
     message, "Must select TO and FROM planes within the cube.", /info
  endif

  if from_plane_chan gt to_plane_chan then begin
     message, "Order of TO and FROM reversed?", /info
  endif

  cube = cube[*,*,from_plane_chan:to_plane_chan]

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; DEAL WITH THE HEADER
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
  
  new_hdr = hdr
  sxaddpar, new_hdr, 'NAXIS3', n_elements(vaxis)
  sxaddpar, new_hdr, 'CRPIX3', 1.0, 'Overwritten by EXTRACT_PLANES (IDL)'
  sxaddpar, new_hdr, 'CRVAL3', vaxis[from_plane_chan] $
            , 'Overwritten by EXTRACT_PLANES (IDL)'
  sxaddpar, new_hdr, 'CDELT3', (vaxis[1]-vaxis[0]) $
            , 'Overwritten by EXTRACT_PLANES (IDL)'

; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
; WRITE TO DISK IF REQUESTED
; -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=

  if n_elements(outfile) gt 0 then begin
     writefits, outfile, cube, new_hdr
  end

  return, cube

end
