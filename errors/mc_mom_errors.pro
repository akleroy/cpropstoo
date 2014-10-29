pro mc_mom_errors $
   , in_file = in_file $
   , in_assign = in_assign $
   , in_noise = in_noise $
   , n_sim = n_sim  $
   , out_props_sim = out_props_sim $
   , dist = dist


;+
; mc_mom_errors
;
; Purpose: monte carlo errors for moment 
;
; Input: 
;       in_file: input data cube
;
;       in_assign: assignment cube for calculating moments
;
;       in_noise: input noise cube
;
;       n_sim: number of simulations to run
;
; Output:
;
;       out_props_sim: idl save file with props from each simulation
;
; Notes:
;       
;       Right now I'm just monte-carloing over the props inputs, but I
;       could add things like distance and alpha if necessary. It
;       might make this program too complex though. May be worth
;       separating out the distance monte carlo in case someone wanted
;       to do that one separately. The results could just be added
;       together in quadrature.
;
; Date          Programmer              Description of Changes
; ----------------------------------------------------------------------
; 10/6/2014     A.A. Kepley             Original Code
; 10/29/2014    A.A. Kepley             Modified for new
;                                       add_noise_to_cube.pro 
;-

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; READ IN                                               
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if n_elements(in_file) gt 0 then begin
     if not file_test(in_file, /read) then begin
        message, in_file+' is not accessible.', /con
        return
     endif
     cube = readfits(in_file, hdr)
  endif

  if not n_elements(hdr) GT 0 then $
     return

  if n_elements(in_assign) gt 0 then begin
     if not file_test(in_assign, /read) then begin
        message, in_assign+' is not accessible.', /con
        return
     endif
     assign = readfits(in_assign, assign_hdr)
  endif

  if n_elements(in_noise) gt 0 then begin
     if not file_test(in_noise,/read) then begin
        message, in_noise+' is not accessible.', /con
        return
     endif
     noise = readfits(in_noise,noise_hdr)
  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; Run MONTE CARLOs
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  
  
  ; initialize seed
  seed = float(systime(1))

  for i = 0, n_sim - 1 do begin

     add_noise_to_cube $
        , cube = cube $
        , hdr = hdr $
        , out_file = out_file $
        , out_cube = out_cube $
        , seed = seed $
        , noise = noise
     
     cube_to_moments $
        , data = out_cube $
        , assign = assign $
        , hdr = hdr $
        , moments = moments

     moments_to_props $
        , inhdr = hdr $
        , indata = moments $
        , props = props $
        , dist = dist $  
        , /verbose

     ; add simulation to array of props structures
     if i eq 0 then begin
        props_sim = props
     endif else begin
        props_sim = [[props_sim],[props]]
     endelse
      
  endfor

;&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; Save structure for future calculations
;&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  save, props_sim, filename=out_props_sim

end
   
