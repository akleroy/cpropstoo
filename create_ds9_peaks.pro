pro create_ds9_peaks, lmaxfile, region_file_name = region_file_name, $
                      pixels = pixels, $
                      type = type, $
                      kms = kms
;
; Purpose: Create a ds9 region file for the cprops list of maxima. This file can
; be used in ds9 and aplpy (and maybe casaviewer).

; Input: 
;
;       lmaxfile: idl sav file with the properties of the maxima.
;
;       region_file_name: filename for region file
;
;       pixels: output region in pixels, otherwise do actual
;               values. Currently you need to set this to true to
;               display if creating rapv or decpv maps.
;
;       type: 0 = radec,  1 = rapv, 2 = decpv
;       
;       kms: output velocities as km/s instead of m/s
;               
;
; Output:
;
;       region file for ds9
;
; To Do:
;       -- allow user to select color and shape of point.
;       
; 
;
; Date          Programmer      Description of Changes
;----------------------------------------------------------------------
; 6/27/2014     A.A. Kepley     Original Code
; 9/25/2014     A.A. Kepley     props contains moments, so
;                               don't need to add both.
; 9/25/2014     A.A. Kepley     Modified to plot maxima instead of
;                               region centers. Procedure given a
;                               different name.

; setting defaults
if n_elements(lmaxfile) eq 0  then begin
   message,/info,'create_ds9_peaks,lmaxfile'
   return
end

if n_elements(region_file_name) eq 0 then region_file_name='ds9.reg'
if n_elements(pixels) eq 0 then pixels = 0
if n_elements(type) eq 0 then begin
   type=0
   message,/info,"Assuming that you want RA, Dec positions. Change type parameter generate positions for RA/Vel or Dec/Vel"
endif
if n_elements(kms) eq 0 then kms = 0

; restoring maxima file
restore,/verb,lmaxfile

; setting coordinate system
if pixels then begin
   coordsys = 'physical'
endif else begin
   coordsys = 'fk5 '; assuming J2000 here. probably a bad assumption. get from somewhere else?
endelse

if kms then begin
   vel_factor = 1.0 ; output km/s
endif else begin
   vel_factor = 1000.0 ; output m/s
endelse

; setting the point style. Could make this a option later.
pointstyle = 'point=x'

; setting up which values to print
if pixels gt 0 then begin
   case type of
      0: begin
         xx = xpix
         yy = ypix

      end
      1: begin
         xx = xpix
         yy = zpix

      end
      2: begin
         xx = ypix
         yy = zpix
      end
   endcase
endif else begin
   case type of
      0: begin
         xx = ra
         yy  = dec
      end
      1: begin
         xx = ra
         yy  = vel * vel_factor
      end
      2: begin 
         xx = dec
         yy  = vel * vel_factor
      end
   endcase
endelse

; opening file
openw,lun,region_file_name,/get_lun

; printing header
printf, lun, '# Region file format: DS9 version 4.0'
printf, lun, 'global color=magenta font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
printf, lun, coordsys

; writing data
n_regions = n_elements(xx)
for i = 0, n_regions - 1 do begin
   pointlabel = "text={" + string(i+1,format='(I2)') + "}"
   printf, lun, 'point(' + string(xx[i],format='(F15.7)') + ',' +$
           string(yy[i],format='(F15.7)') +  ') #' $
           + pointstyle + ' ' + pointlabel
end

; closing file
free_lun,lun

end
