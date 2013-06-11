pro reversect, update_common=update_common
;+
; NAME:
;   REVERSECT
; PURPOSE:
;   Reverses the sense of the color table.
;
; CALLING SEQUENCE:
;   REVERSECT
;
; INPUTS:
;   none
;
; KEYWORD PARAMETERS:
;   none
;
; OUTPUTS:
;   none
;
; MODIFICATION HISTORY:
;      Documented. 
;       Wed Nov 21 12:20:42 2001, Erik Rosolowsky <eros@cosmic>
;
;		
;
;-

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

tvlct, r, g, b, /get
r = reverse(r)
g = reverse(g)
b = reverse(b)
tvlct, r, g, b

if keyword_set(update_common) then begin
   r_orig = r
   g_orig = g
   b_orig = b
   r_curr = r
   g_curr = g
   b_curr = b
endif



  return
end
