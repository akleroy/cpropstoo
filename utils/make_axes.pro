pro make_axes $
   , h $
   , raxis = raxis $
   , daxis = daxis $
   , vaxis = vaxis $
   , astrom = astrom $
   , rimg = rimg $
   , dimg = dimg $
   , quiet = quiet $
   , novec=novec $
   , vonly = vonly $
   , simple=simple

;+
; NAME:
;   make_axes
;
; PURPOSE:
;
;   Makes axis arrays using 'exast' and 'xy2ad.' Returns units of decimal
;   degrees. Requires a .FITS header and gives the option of returning
;   coordinates for each point in the image.
;
; CALLING SEQUENCE:
;
;   make_axes, h $
;            , raxis=raxis, daxis=daxis $
;            , vaxis=vaxis $
;            , rimg=rimg, dimg=dimg $
;            , astrom = astrom $
;            , /quiet $
;            , /novec $
;            , /vonly $
;            , /simple
;
; INPUTS: 
;
;   H - a single header structure (e.g. one returned by 'readfits')
;       KEYWORD PARAMATERS: none
;
; OUTPUTS
;
;   RAXIS - the right ascension axis pulled from the central row of
;           the image.
;   DAXIS - the declination axis pulled from the central column of the
;           image.
;   VAXIS - the velocity/z axis.
;   RIMG  - the right ascension of each point in the image.
;   DIMG  - the declination of each point in the image.
;
; MODIFICATION HISTORY:
;
;       Initial Documentation - Thu Nov 2
;				Adam Leroy <aleroy@mars.berkeley.edu>
;       Got rid of silly use of 'cube' and allowed axis construction
;         using only the header. Still accepts cube as an argument,
;         but does not use it.
;                               AL, 7/30/03
;                               <aleroy@astro.berkeley.edu>
;       Renamed routine 'make_axes' and scrapped backwards
;       compatibility. No longer takes cube argument and now has
;       optional arguments to return the full RA and DEC images (2d
;       fields) in addition to the axes.
;                               AL, 7/30/03
;                               <aleroy@astro.berkeley.edu>
;
;       finally patched in the GLS/SFL thing.
;        - 1 Oct 2007 - leroy@mpia-hd.mpg.de
;
;       vectorized ?and sped things way way up? - 29 Nov 2007 leroy@mpia.de
;
;       added option to skip RA/DEC with /VONLY - 12 Dec 2008
;-

; PULL THE IMAGE/CUBE SIZES FROM THE HEADER
  naxis  = sxpar(h, 'NAXIS')
  naxis1 = sxpar(h, 'NAXIS1')
  naxis2 = sxpar(h, 'NAXIS2')
  naxis3 = sxpar(h, 'NAXIS3')

; USE 'extast' TO EXTRACT A FITS ASTROMETRY STRUCTURE
  extast, h, astrom
  
; IF DATASET IS A CUBE THEN WE MAKE THE THIRD AXIS IN THE SIMPLEST WAY
; POSSIBLE (NO COMPLICATED ASTROMETRY WORRIES FOR FREQUENCY
; INFORMATION)
  if (naxis ge 3) then begin
;   GRAB THE RELEVANT INFORMATION FROM THE ASTROMETRY HEADER
    cd = astrom.cd
    crpix = astrom.crpix
    cdelt = astrom.cdelt
    crval = astrom.crval
    
;   MAKE THE VELOCITY AXIS (WILL BE M/S)
    v = findgen(naxis3)
    vdif = v - (sxpar(h, 'CRPIX3')-1)
    vaxis = (vdif * sxpar(h, 'CDELT3')+ sxpar(h, 'CRVAL3'))
 endif

; CUT OUT HERE IF WE ONLY WANT VELOCITY INFO
  if keyword_set(vonly) then $
     return

; IF 'SIMPLE' IS CALLED THEN DO THE REALLY TRIVIAL THING:
  if keyword_set(simple) then begin
     message, 'Using simple aproach to make axes.', /info
     message, 'BE SURE THIS IS WHAT YOU WANT! It probably is not.', /info
     raxis = findgen(naxis1)
     rdif = raxis - (sxpar(h, 'CRPIX1')-1)
     raxis = (rdif * sxpar(h, 'CDELT1')+ sxpar(h, 'CRVAL1'))
     
     daxis = findgen(naxis2)
     ddif = daxis - (sxpar(h, 'CRPIX1')-1)
     daxis = (ddif * sxpar(h, 'CDELT1')+ sxpar(h, 'CRVAL1'))

     rimg = raxis # (fltarr(naxis2) + 1.)
     dimg = (fltarr(naxis1) + 1.) # daxis
     return
  endif
  
; OBNOXIOUS SFL/GLS THING
  glspos = strpos(astrom.ctype[0],'GLS')
  if glspos ne -1 then begin
      ctstr=astrom.ctype[0]
      strput,ctstr,'SFL',glspos
      astrom.ctype[0] = ctstr
      message,'Replaced GLS with SFL; ctype[0] now ='+astrom.ctype[0] $
        ,/informational
  endif
  glspos = strpos(astrom.ctype[1],'GLS')
  if glspos ne -1 then begin
      ctstr=astrom.ctype[1]
      strput,ctstr,'SFL',glspos
      astrom.ctype[1] = ctstr
      message,'Replaced GLS with SFL; ctype[0] now = '+astrom.ctype[1] $
        ,/informational
  endif

; CALL 'xy2ad' TO FIND THE RA AND DEC FOR EVERY POINT IN THE IMAGE
  if keyword_set(novec) then begin
     rimg = dblarr(naxis1, naxis2)
     dimg = dblarr(naxis1, naxis2)     
     for i = long(0), naxis1-1 do begin
        j = lindgen(naxis2)
        if (astrom.ctype[0] eq 'RA---GSS') then $
           gsssxyad, astrom, double(i), double(j),  ra, dec else $
              xy2ad, double(i), double(j), astrom, ra, dec
        rimg[i, *] = ra
        dimg[i, *] = dec
        if (NOT keyword_set(quiet)) then $
           counter, i+1, naxis1, 'Row '
     endfor
  endif else begin
     ximg = findgen(naxis1) # (fltarr(naxis2) + 1.)
     yimg = (fltarr(naxis1) + 1.) # findgen(naxis2)
     if (astrom.ctype[0] eq 'RA---GSS') then $
        gsssxyad, astrom, ximg, yimg, rimg_new, dimg_new else $
           xy2ad, ximg, yimg, astrom, rimg_new, dimg_new
     rimg = rimg_new
     dimg = dimg_new
  endelse

; GET AXES FROM THE IMAGES. USE THE CENTRAL COLUMN AND CENTRAL ROW
  raxis = reform(rimg[*, naxis2/2])
  daxis = reform(dimg[naxis1/2, *])
  
end                             ; of make_axes
