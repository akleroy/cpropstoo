pro add_noise_to_cube $
   , in_file = in_file $
   , cube = cube $
   , hdr = hdr $
   , out_file = out_file $
   , out_cube = out_cube $
   , seed = seed $
   , gain = gain $
   , addnoise = addnoise $
   , addgain = addgain $
   , noise_file = noise_file $
   , psf = psf


;+
;
; NAME:
;
;   ADD_NOISE_TO_CUBE
;
; PURPOSE:
;   To take a data cube (from disk or in array) and add a gain noise
;   and random noise (useful for Montecarlo errors). Will convolve
;   with a given PSF or generate one from the header.     
;
;
; CALLING SEQUENCE:
;   add_noise_to_cube, in_file=filename, out_file=noisyfilename 
;   /addgain, /addnoise [, noise_file=noise_file]  
;
; INPUTS:
;   CUBE -- data cube to add noise.
;   HDR -- fits header, required if passing an array
;   IN_FILE -- data on disk to read in.  
;   PSF -- (optional) Custom PSF.
;   SEED -- (optional) Random number generator seed, if you want
;           repeatable "randomness." Default is the system time. 
;   GAIN -- (optional) The percentage gain. 
;             
; KEYWORD PARAMETERS:
;   ADDNOISE -- (optional) if set this will add random noise at
;               the same noise level.
;   ADDGAIN -- (optional) if set this will add a flux gain term to the data.
;   OUT_FILE -- (optional) Variable specifying a filepath to save the noisy cube. 
;
; OUTPUTS:
;   OUT_CUBE -- Variable that will hold the new (noisy) cube
;
; MODIFICATION HISTORY:
;
;       More documentation -- Mon Nov 25  2013  Stephen Pardy
;                     <spardy@astro.wisc.edu>
; 
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
   
  sz = size(cube)
     
  if not keyword_set(seed) then $
        seed = float(systime(1))

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ADD RANDOM CALIBRATION GAIN
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(addgain) then begin 

     sd = seed
     if not keyword_set(gain) then $ 
        gain = randomn(sd, /normal)/10. 
     
     out_cube = cube*(1+gain)

  endif

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; ADD CONVOLVED NOISE 
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  if keyword_set(addnoise) then begin

     sd = seed
     noise = randomn(sd, sz[1],sz[2],sz[3],/normal)
    
     if n_elements(psf) EQ 0 then begin 
        bmaj = sxpar(hdr, "BMAJ")
        bmin = sxpar(hdr, "BMIN")
     
        extast, hdr, astr
        xy2ad, [0,1], [0,0], astr, ra, dec
        degperpix = sphdist(ra[0], dec[0], ra[1], dec[1], /deg)
     
        beamfwhm_deg = sqrt(bmaj*bmin)
        beamfwhm_pix = beamfwhm_deg / degperpix
     
        psf = psf_gaussian(npixel=(3*beamfwhm_pix+1)*[1,1],$
                        fwhm=beamfwhm_pix*[1,1]) 
      endif 
  
     for i=0,sz[3]-1 do begin
        plane = noise[*,*,i]
        new_plane = convolve(plane, psf)
        noise[*,*,i] = new_plane
     endfor   
     
     rms = mad(cube)
     current_rms = mad(noise)
     noise = rms*noise/current_rms
     
     out_cube = cube + noise

  endif 

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; OUTPUT
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%


sxaddpar, hdr, 'RSEED', seed, 'Random seed value'
if n_elements(out_file) GT 0 then $
   writefits, out_file, out_cube, hdr

if keyword_set(noise_file) then $
   writefits, noise_file, noise_cube


end
