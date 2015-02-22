pro deproject, ra, dec, galpos, RGRID = rgrid, TGRID = tgrid $
               , XGRID = deproj_x, YGRID = deproj_y, VECTOR = vector, GAL=gal
                  
;+ 
; NAME: deproject
; 
; PURPOSE: 
; Takes a center, position angle and inclination and computes
; deprojected radii and projected angle.
;
; CALLING SEQUENCE:
; deproject, ra, dec, galpos, RIMG = rimg, DIMG = dimg $
;          , RGRID = rgrid, TGRID = tgrid $
;          , XGRID = deproj_x, YGRID = deproj_y,GAL=gal
;
; INPUTS:
;
;   RA - the right ascenscion array corresponding to the x-axis. Can
;        also be an image if curvature is important.
;   DEC - the declination array corresponding to the y-axis. Can also
;         be an image if curvature is important.
;   GALPOS - the 'standard' galaxy position/orientation array
;            [vlsr, pa, inc, xctr, yctr] (all in degrees & km/s)
;            or just [pa, inc, xctr, yctr] if 4 elements
;
; OUTPUTS:
;
;   RGRID - the grid of galactocentric radii
;   TGRID - the grid of angle from P.A.
;   XGRID - the grid of deprojected X-values
;   YGRID - the grid of deprojected Y-values
;
; KEYWORDS 
;
;   VECTOR - tells DEPROJECT to return vectors matched in size to RA and DEC,
;            useful e.g. for radial profiles.
;
;
; MODIFICATION HISTORY:
;
;   Written by 
;    Adam Leroy < aleroy@mars.berkeley.edu > Thurs Jan 11, 2001
;   Dramatically altered by
;    Adam Leroy < aleroy@mars.berkeley.edu> Thurs, Jan 18, 2001
;   Hepped up on crack by
;    Adam Leroy < aleroy@mars.berkeley.edu> Mon, Feb 26, 2001   
;   Stripped down to its skivvies by
;    Adam Leroy < aleroy@mars.berkeley.edu> Wed, Mar 14, 2001
;   Sign error tracked down and ground beneath the boot of
;    Adam Leroy < aleroy@mars.berkeley.edu> Mon, Apr 2, 2001
;   Small concession to the curvature of the sky made by
;    Adam Leroy < aleroy@astro.berkeley.edu> Wed, Oct 22, 2003
;   Added ability to work on vectors only (untested)
;    Adam Leroy < leroy@mpia-hd.mpg.de Mon, Oct 1, 2007
;   Eats galaxy structures and takes 4 element galpo
;    Adam Leroy < leroy@mpia-hd.mpg.de Mon, Apr 7, 2008
;-

; EXPAND THE GALAXY ORIENTATION VECTOR

  if n_elements(gal) gt 0 then begin
     pa = gal.posang_deg*!dtor
     inc = gal.incl_deg*!dtor
     xctr= gal.ra_deg
     yctr = gal.dec_deg
  endif else begin
      if n_elements(galpos) eq 5 then begin
          vlsr = galpos[0]
          pa   = galpos[1]*!dtor
          inc  = galpos[2]*!dtor
          xctr = galpos[3]
          yctr = galpos[4]
      endif else begin
          pa   = galpos[0]*!dtor
          inc  = galpos[1]*!dtor
          xctr = galpos[2]
          yctr = galpos[3]
      endelse
  endelse

  rsz = size(ra)
  dsz = size(dec)

  if (rsz[0] eq 1 and (NOT keyword_set(vector)) ) then begin
; IF THE USER HAS SUPPLIED ARRAYS AND NOT IMAGES THEN MAKE 2-D GRID,
; ONE CONTAINING RA AND ONE CONTAINING DEC
      rimg = (fltarr(n_elements(dec))+1.) ## ra
      dimg = dec ## (fltarr(n_elements(ra))+1.)
  endif else begin
      rimg = ra
      dimg = dec
  endelse

; RECAST THE RA AND DEC ARRAYS IN TERMS OF THE CENTERS
; ARRAYS ARE NOW IN DEGREES FROM CENTER
  xgrid = (rimg - xctr)*cos(!dtor*yctr)
  ygrid = (dimg - yctr)
  
; ROTATION ANGLE (ROTATE YOUR X-AXIS UP TO THE MAJOR AXIS)
  rotang =  -1.*(pa - 1.0*!pi/2.)

; MAKE 2-D GRIDS FOR ROTATED X AND Y
  deproj_x = xgrid * cos(rotang) + ygrid * sin(rotang)
  deproj_y = ygrid * cos(rotang) - xgrid * sin(rotang)
  
; REMOVE INCLINATION EFFECT
  deproj_y = deproj_y / cos(inc)
  
; MAKE GRID OF DEPROJECTED DISTANCE FROM THE CENTER
  rgrid = sqrt(deproj_x^2 + deproj_y^2)
  
; MAKE GRID OF ANGLE W.R.T. PA
  tgrid = atan(deproj_y, deproj_x)

end ; of deproject






