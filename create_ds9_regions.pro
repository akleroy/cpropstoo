pro create_ds9_regions, props, region_file_name = region_file_name,$
                        pixels = pixels,$
                        type = type, $
                        ellfit = ellfit

; Purpose: Create a ds9 region file to output cprops regions. This file can
; be used in ds9 and aplpy (and maybe casaviewer).

; Input: 
;
;       props: props structure 
;
;
;       region_file_name: filename for region file
;
;       pixels: output region in pixels, otherwise do actual
;       values. If doing rapv or depv region file, set this.
;
;       type: 0 = radec,  1 = rapv, 2 = decpv
;
;       ellfit: use ellfit values for ra/dec sizes. Otherwise use moments.
;;               
;
; Output:
;
;       region file for ds9
;
; To Do:
;       -- allow user to select color of region.

;
; Date          Programmer      Description of Changes
;----------------------------------------------------------------------
; 6/27/2014     A.A. Kepley     Original Code
; 9/25/2014     A.A. Kepley     props contains moments, so
;                               don't need to add both.
; 9/25/2014     A.A. Kepley     Modified for choice of region

; setting defaults
if n_elements(props) eq 0  then begin
   message,/info,'create_ds9_regions,props'
   return
end

if n_elements(region_file_name) eq 0 then region_file_name='ds9.reg'
if n_elements(pixels) eq 0 then pixels = 0
if n_elements(type) eq 0 then begin
   type=0
   message,/info,"Assuming that you want RA, Dec positions. Change type parameter generate positions for RA/Vel or Dec/Vel"
endif
if n_elements(ellfit) eq 0 then ellfit = 0
   
; setting coordinate system
if pixels then begin
   coordsys = 'physical'
   majaxislabel = ''
endif else begin
   coordsys = 'fk5 '; assuming J2000 here. probably a bad assumption. get from somewhere else?
   majaxislabel = '"'
endelse

; setting up the values to print. The calculations are done into
; pixels an converted to degrees as needed.
case type of

   0: begin

      if ellfit then begin
         majoraxis = props.ellfitmaj_halfmax  
         minoraxis = props.ellfitmin_halfmax 
         posang = (props.ellfitposang_halfmax) / !dtor
      endif else begin
         majoraxis =  props.mom2maj * props.sig_to_fwhm  ;; xxx I'm not sure this is right, it's closish
         minoraxis = props.mom2min * props.sig_to_fwhm  ;halfmax
         posang = props.momposang / !dtor
      endelse

      if pixels then begin
         xx = props.mom1x
         yy = props.mom1y
      endif else begin
         ;; convert if degrees are requested
         xx = props.xpos
         yy = props.ypos         
         majoraxis = (majoraxis * props.degperpix) * 3600.00 ; arcsec
         minoraxis = (minoraxis * props.degperpix) * 3600.00 ; arcsec
      endelse

   end

   1: begin
      
      ;; AAK: fix this up later to be right. really what I want is the
      ;; extent along the ra or dec axis. I think I can do this with a
      ;; straightforward application of some math.
      
      if ellfit then begin
         majoraxis = props.ellfitmaj_halfmax
         minoraxis = props.deltav_halfmax
         posang = dblarr(n_elements(majoraxis)) * 0.0
      endif else begin
         ; use moments here
      endelse

      if pixels then begin
         xx =  props.mom1x
         yy = props.mom1v
      endif else begin
         xx = props.xpos
         yy = props.vpos
         majoraxis = (majoraxis * props.degperpix) * 3600.0 ; arcsec
         minoraxis = minoraxis * chanwidth_to_kms * 1000.0 ; m/s
      endelse
      
   end

   2: begin
      ;; AAK: fix this up later to be right. really what I want is the
      ;; extent along the ra or dec axis. I think I can do this with a
      ;; straightforward application of some math.
      
      if ellfit then begin
         majoraxis = props.ellfitmaj_halfmax
         minoraxis = props.deltav_halfmax
         posang = dblarr(n_elements(majoraxis)) * 0.0
      endif else begin
         ; use moments here
      endelse

      if pixels then begin
         xx =  props.mom1y
         yy = props.mom1v
      endif else begin
         xx = props.ypos
         yy = props.vpos
         majoraxis = (majoraxis * props.degperpix) * 3600.0 ; arcsec
         minoraxis = minoraxis * chanwidth_to_kms * 1000.0 ; m/s
      endelse

    
   end
endcase


; opening file
openw,lun,region_file_name,/get_lun

; printing header
printf, lun, '# Region file format: DS9 version 4.0'
printf, lun, 'global color=magenta font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
printf, lun, coordsys

; writing data
n_regions = n_elements(xx)
for i = 0, n_regions - 1 do begin
   pointlabel = "text={" + string(props[i].peaknum,format='(I2)') + "}"
   printf, lun, 'ellipse(' + $
           string(xx[i],format='(F15.7)') + ',' + $ ;x position
           string(yy[i],format='(F15.7)') + ',' + $ ;y position
           string(majoraxis[i],format='(F15.7)') + majaxislabel + ' ,' + $ ; major axis
           string(minoraxis[i],format='(F15.7)') + majaxislabel + ' ,' +  $ ; minor axis
           string(posang[i],format='(F15.7)') + ') # ' + pointlabel
end

; closing file
free_lun,lun

end
