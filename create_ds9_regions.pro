pro create_ds9_regions, props, region_file_name = region_file_name,$
                        type = type, $
                        color=color

; Purpose: Create a ds9 region file to output cprops regions.  This
; file can be used in ds9 and aplpy (and maybe casaviewer). Right now
; I'm using the ellfit values for the regions. It's seems to produce a
; good result and is fairly simple. 

; The regions are output as pixels for now.

; Input: 
;
;       props: props structure 
;
;
;       region_file_name: filename for region file
;
;
;       type: 0 = radec,  1 = rapv, 2 = decpv
;
;       color: string recognized by ds9. options include white,
;       black, red, green, blue, cyan, magenta, and yellow.
;
;
; Output:
;
;       region file for ds9
;
; To Do:
;       -- allow the user to overlay other types of regions.
;       -- convert to angular units.

;
; Date          Programmer      Description of Changes
;----------------------------------------------------------------------
; 6/27/2014     A.A. Kepley     Original Code
; 9/25/2014     A.A. Kepley     props contains moments, so
;                               don't need to add both.
; 9/25/2014     A.A. Kepley     Modified for choice of region
; 9/26/2014     A.A. Kepley     Simplified and corrected. Also added
;                               color choice
; 9/30/2014     A.A. Kepley     Fixed logic error 

; setting defaults
if n_elements(props) lt 1  then begin
   message,/info,'create_ds9_regions,props'
   return
end

if n_elements(region_file_name) eq 0 then region_file_name='ds9.reg'
if n_elements(type) eq 0 then begin
   type=0
   message,/info,"Assuming that you want RA, Dec positions. Change type parameter generate positions for RA/Vel or Dec/Vel"
endif
if n_elements(color) eq 0 then color='green'


; setting coordinate system
coordsys = 'physical'
majaxislabel = ''

; setting up the values to print.
case type of

   ; RA DEC
   0: begin

      xx = props.mom1x
      yy = props.mom1y
      
      majoraxis = props.ellfitmaj_halfmax  
      minoraxis = props.ellfitmin_halfmax 
      posang = (props.ellfitposang_halfmax) / !dtor

   end

   ; RA/Vel
   1: begin

      xx =  props.mom1x
      yy = props.mom1v
      
      majoraxis = abs(props.ellfitmaj_halfmax * cos(props.ellfitposang_halfmax) - props.ellfitmin_halfmax * sin(props.ellfitposang_halfmax))
      minoraxis = props.deltav_halfmax
      posang = dblarr(n_elements(majoraxis)) * 0.0
      
   end

   ; Dec/Vel
   2: begin

      xx =  props.mom1y
      yy = props.mom1v

      majoraxis = abs(props.ellfitmaj_halfmax * sin(props.ellfitposang_halfmax) + props.ellfitmin_halfmax * cos(props.ellfitposang_halfmax))
      minoraxis = props.deltav_halfmax
      posang = dblarr(n_elements(majoraxis)) * 0.0

   end
endcase


; opening file
openw,lun,region_file_name,/get_lun

; printing header
printf, lun, '# Region file format: DS9 version 4.0'
printf, lun, 'global color=' + string(color)+ ' font="helvetica 10 normal" select=1 highlite=1 edit=1 move=1 delete=1 include=1 fixed=0 source'
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
