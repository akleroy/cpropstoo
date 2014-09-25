pro create_ds9_regions_clumps, props, region_file_name = region_file_name,$
                        pixels=pixels,$
                        type = type

; Purpose: Create a ds9 region file for cprops output. This file can
; be used in ds9 and aplpy (and maybe casaviewer).

; Input: 
;
;       props: props structure 
;
;
;       region_file_name: filename for region file
;
;       pixels: output region in pixels, otherwise do actual values
;
;       type: 0 = radec,  1 = rapv, 2 = decpv
;               
;
; Output:
;
;       region file for ds9
;
; To Do:
;       -- allow user to select color and shape of region.
;       -- plot size ellipse instead of point?
; 
;       -- I think I want to modify this to plot the stuff for the
;          lmax idl file rather than the outgoing regions. Really
;          that's what I'm looking at here.
;
; Date          Programmer      Description of Changes
;----------------------------------------------------------------------
; 6/27/2014     A.A. Kepley     Original Code
; 9/25/2014     A.A. Kepley     props contains moments, so
;                               don't need to add both.

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
   
; setting coordinate system
if pixels then begin
   coordsys = 'physical'
endif else begin
   coordsys = 'fk5 '; assuming J2000 here. probably a bad assumption. get from somewhere else?
endelse

; setting the point style. Could make this a option later.
pointstyle = 'point=x'

; setting up which values to print
if pixels gt 0 then begin
   case type of
      0: begin
         xx = props.mom1x
         yy = props.mom1y

      end
      1: begin
         xx =  props.mom1x
         yy = props.mom1v

      end
      2: begin
         xx =  props.mom1y
         yy = props.mom1v
      end
   endcase
endif else begin
   case type of
      0: begin
         xx = props.xpos
         yy  = props.ypos
      end
      1: begin
         xx = props.xpos
         yy  = props.vpos
      end
      2: begin 
         xx = props.ypos
         yy  = props.vpos
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
