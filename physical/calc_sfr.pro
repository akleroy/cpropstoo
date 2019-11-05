function calc_sfr $
   , ha=ha $
   , aha=aha $
   , fuv=fuv $
   , nuv=nuv $
   , ir24=ir24 $
   , nhi=nhi $
   , nh2=nh2 $
   , dgr=dgr $
   , umin=umin $
   , emis24=emis24 $
   , i24totir=i24totir $
   , incl=incl

;+
;
; SFR calculations pre-Herschel. Intended to replicate Leroy+ 12, and
; Leroy+ 13. This needs checking, as its hacked together from several
; programs.
;
;-

  @constants.bat
  nan = !values.f_nan

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; DEFINITIONS AND TUNING PARAMETERS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; ADDITIONAL WEIGHT TO PLACE ON 24 RELATIVE TO CALZETTI07 FOR HA
  w_ha = 1.0
  w_ha_scaled = 1.3
  w_ha_gas_umin = 1.3

; ADDITIONAL WEIGHT TO PLACE ON 24 RELATIVE TO CALZETTI07 FOR FUV
  w_fuv = 1.3
  w_fuv_scaled = 1.55
  w_fuv_gas_umin = 1.55

; ADDITIONAL WEIGHT TO PLACE ON 24 RELATIVE TO CALZETTI07 FOR 24 ALONE
  w_24 = 1.9

; NUMBERS FOR THE UV+TIR CALCULATION
  nu24 = c/24.0d-4
  nufuv = c/1528d-8

; INCLINATION CORRECTION
  if n_elements(incl) gt 0 then begin
     incl_corr = cos(!dtor*incl)
  endif else begin
     message, 'Defaulting to no inclination correction.', /info
     incl_corr = 1.0
  endelse

; DEFAULT EXTINCTION
  if n_elements(aha) eq 0 then begin
     message, 'Defaulting to AHA = 1 mag.', /info
     aha = 1.0
  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; MAKE SURE WE HAVE SOMETHING
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%  

  if n_elements(fuv) gt 0 then begin
     n_pts = n_elements(fuv)
  endif else if n_elements(ha) gt 0 then begin
     n_pts = n_elements(ha)
  endif else if n_elements(ir24) gt 0 then begin
     n_pts = n_elements(ir24)
  endif else begin
     message, "No valid data points - need at least FUV, HA, or 24! Returning.", /info
     return, nan
  endelse

  if n_elements(fuv) eq 0 then $
     fuv = replicate(nan, n_pts)

  if n_elements(ha) eq 0 then $
     ha = replicate(nan, n_pts)

  if n_elements(ir24) eq 0 then $
     ir24 = replicate(nan, n_pts)

  if n_elements(nuv) eq 0 then $
     nuv = replicate(nan, n_pts)

  if n_elements(nhi) eq 0 then $
     nhi = replicate(nan, n_pts)

  if n_elements(nh2) eq 0 then $
     nh2 = replicate(nan, n_pts)

  if n_elements(dgr) eq 0 then $
     dgr = replicate(nan, n_pts)

  if n_elements(umin) eq 0 then $
     umin = replicate(nan, n_pts)

  if n_elements(emis24) eq 0 then $
     emis24 = replicate(nan, n_pts)

  if n_elements(i24totir) eq 0 then $
     i24totir = replicate(nan, n_pts)

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; AN EMPTY STRUCTURE TO HOLD OUR RESULTS
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
  
  empty_sfr = { $
              just_ha:nan, $
              just_fuv:nan, $
              just_ir24:nan, $
              just_ir24_hi_sn:nan, $
              just_ir24_hi_umin:nan, $
              just_ir24_hi_scaled:nan, $
              just_ir24_gas_sn:nan, $
              just_ir24_gas_umin:nan, $
              just_ir24_gas_scaled:nan, $
              ha_avg:nan, $
              ha_1mag:nan, $
              ha24:nan, $
              ha24_hi_sn:nan, $
              ha24_hi_umin:nan, $
              ha24_hi_scaled:nan, $
              ha24_gas_sn:nan, $
              ha24_gas_umin:nan, $
              ha24_gas_scaled:nan, $
              fuv24:nan, $
              fuv24_hi_sn:nan, $
              fuv24_hi_umin:nan, $              
              fuv24_hi_scaled:nan, $              
              fuv24_gas_sn:nan, $              
              fuv24_gas_umin:nan, $              
              fuv24_gas_scaled:nan, $              
              irx_beta:nan, $
              fuvtir_early:nan, $
              fuvtir_mid:nan, $
              fuvtir_late:nan, $
              ha_gas:nan, $
              fuv_gas:nan, $              
              ha_gasdgr:nan, $
              fuv_gasdgr:nan, $              
              ir24:nan, $
              med:nan, $
              meanlog:nan, $
              mad:nan, $
              stddev:nan, $
              high:nan, $
              hind:-1, $
              low:nan, $
              lind:-1 $
              }

; COPY IT
  sf_struct = replicate(empty_sfr, n_pts)

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; CALCULATE PLAUSIBLE CIRRUS CONTAMINATION OF 24 MICRON
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; DESERT 1990, MJY/SR/1E20
  ir25_per_nh = 3.96d-2

; DRAINE AND LI, DEPENDING ON QPAH 1.0 - 1.8E-2
  dl07_ir24_per_nh = 1.7d-2

; RAFIKOV 2001 SOLAR NEIGHBORHOOD NUMBERS
  mstar_sn = 35.
  nhi_sn = 10.d/1.36*ms/mh/pc/pc

; FIDUCIAL IR COLOR
  fid_dgr = 1./136.

; BEST FIT SCALING OF UMIN
  u_scale = 0.50

; BUILD AN EMPTY STRUCTURE TO HOLD THE CIRRUS
  empty_cirrus_struct = $
     { $
     hi_sn:nan, $
     hi_umin:nan, $
     hi_scaled:nan, $
     gas_sn:nan, $
     gas_umin:nan, $
     gas_scaled:nan $
     }
  cirrus_struct = replicate(empty_cirrus_struct, n_pts)

  cirrus_struct.hi_sn = emis24*dgr*(nhi*mh/ms*1.36*pc*pc)*(1.0/umin)
  cirrus_struct.hi_umin = emis24*dgr*(nhi*mh/ms*1.36*pc*pc)
  cirrus_struct.gas_sn = emis24*dgr*((nhi+2.0*nh2)*mh/ms*1.36*pc*pc)*(1.0/umin)
  cirrus_struct.gas_umin = emis24*dgr*((nhi+2.0*nh2)*mh/ms*1.36*pc*pc)

; SCALE THEM TO MATCH OUR BEST FIT TO THE OBSERVED DISTRIBUTION
  cirrus_struct.hi_scaled = $
     cirrus_struct.hi_umin*u_scale
  cirrus_struct.gas_scaled = $
     cirrus_struct.gas_umin*u_scale

; SUBTRACT THE CIRRUS FROM THE 24 MICRON 
  ir24_sub_hi_sn = (ir24 - cirrus_struct.hi_sn) > 0
  ir24_sub_hi_umin = (ir24 - cirrus_struct.hi_umin) > 0
  ir24_sub_hi_scaled = (ir24 - cirrus_struct.hi_scaled) > 0
  ir24_sub_gas_sn = (ir24 - cirrus_struct.gas_sn) > 0
  ir24_sub_gas_umin = (ir24 - cirrus_struct.gas_umin) > 0
  ir24_sub_gas_scaled = (ir24 - cirrus_struct.gas_scaled) > 0

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; APPLY TONY WONG'S GAS->HYBRID METHOD
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; H COLUMN TO ACHIEVE ONE MAGNITUDE OF E(B-V) REDDENING
  onemag = 5.8e21

; EXTINCTION DUE TO HALF THE HI IN FOREGROUND
  half_hi_fg = (nhi/onemag/2.0*2.325) > 0

; EXTINCTION DUE TO THE H2
  aha_h2_only = (nh2*2.0/onemag*2.325) > 0
; ... FOR THE CASE OF A SMOOTH MIX WITH THE HALPHA
  tau_h2 = aha_h2_only/1.086
  aha_h2_smooth = 2.5*alog(tau_h2/(1.0-exp(-1.0*tau_h2)))
; TOTAL EXTINCTION
  aha_gas = half_hi_fg+aha_h2_smooth
  auv_gas = aha_gas * (8.24/2.325)

; COULD SCALE THIS BY THE DGR IF WE BELIEVE THE DL07 NORMALIZATION... 
  fid_dgr = 0.01                ; APPROXIMATE, SHOULD BE 0.0074
  aha_scaled = aha_gas * (dgr/fid_dgr)
  auv_scaled = aha_gas * (8.24/2.325)

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; A FULL SET OF RECIPES
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; WORK OUT THE PARTS SEPARATELY
  just_ha = $
     ha_to_sfsd(ha) * incl_corr
  just_fuv = $
     fuv_to_sfsd(fuv) * incl_corr
  just_ir24 = $
     ir24_to_sfsd(ir24) * incl_corr

  just_ir24_hi_sn = $
     ir24_to_sfsd(ir24_sub_hi_sn) * incl_corr
  just_ir24_hi_umin = $
     ir24_to_sfsd(ir24_sub_hi_umin) * incl_corr
  just_ir24_hi_scaled = $
     ir24_to_sfsd(ir24_sub_hi_scaled) * incl_corr

  just_ir24_gas_sn = $
     ir24_to_sfsd(ir24_sub_gas_sn) * incl_corr
  just_ir24_gas_umin = $
     ir24_to_sfsd(ir24_sub_gas_umin) * incl_corr
  just_ir24_gas_scaled = $
     ir24_to_sfsd(ir24_sub_gas_scaled) * incl_corr

; SAVE COMPONENTS IN STRUCTURE
  sf_struct.just_ha = just_ha
  sf_struct.just_fuv = just_fuv
  sf_struct.just_ir24 = just_ir24

  sf_struct.just_ir24_hi_sn = just_ir24_hi_sn
  sf_struct.just_ir24_hi_umin = just_ir24_hi_umin  
  sf_struct.just_ir24_hi_scaled = just_ir24_hi_scaled

  sf_struct.just_ir24_gas_sn = just_ir24_gas_sn
  sf_struct.just_ir24_gas_umin = just_ir24_gas_umin
  sf_struct.just_ir24_gas_scaled = just_ir24_gas_scaled

; HALPHA + AVERAGE EXTINCTION
  sf_struct.ha_avg = just_ha*10.^(aha/2.5)
  sf_struct.ha_1mag = just_ha*10.^(1.0/2.5)

; HALPHA + 24
  sf_struct.ha24 = just_ha + w_ha * just_ir24

  sf_struct.ha24_hi_sn = just_ha + w_ha * just_ir24_hi_sn
  sf_struct.ha24_hi_umin = just_ha + w_ha * just_ir24_hi_umin
  sf_struct.ha24_hi_scaled = just_ha + w_ha * just_ir24_hi_scaled

  sf_struct.ha24_gas_sn = just_ha + w_ha_gas_umin * just_ir24_gas_sn
  sf_struct.ha24_gas_umin = just_ha + w_ha_gas_umin * just_ir24_gas_umin
  sf_struct.ha24_gas_scaled = just_ha + w_ha_scaled * just_ir24_gas_scaled

; FUV + 24
  sf_struct.fuv24 = just_fuv + w_fuv * just_ir24

  sf_struct.fuv24_hi_sn = just_fuv + w_fuv * just_ir24_hi_sn
  sf_struct.fuv24_hi_umin = just_fuv + w_fuv * just_ir24_hi_umin
  sf_struct.fuv24_hi_scaled = just_fuv + w_fuv * just_ir24_hi_scaled

  sf_struct.fuv24_gas_sn = just_fuv + w_fuv_gas_umin * just_ir24_gas_sn
  sf_struct.fuv24_gas_umin = just_fuv + w_fuv_gas_umin * just_ir24_gas_umin
  sf_struct.fuv24_gas_scaled = just_fuv + w_fuv_scaled * just_ir24_gas_scaled

; JUST IR24
  sf_struct.ir24 = just_ir24 * w_24

; USING THE GAS TO ESTIMATE EXTINCTION
  sf_struct.ha_gas = just_ha*10.^(aha_gas/2.5)
  sf_struct.fuv_gas = just_ha*10.^(auv_gas/2.5)
  sf_struct.ha_gasdgr = just_ha*10.^(aha_scaled/2.5)
  sf_struct.fuv_gasdgr = just_ha*10.^(auv_scaled/2.5)

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; IRX BETA
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  fuv_m_nuv = $
     -2.5*alog10(fuv/nuv)

; SALIM 07 ("BLUE GALAXIES")
  afuv_irx = $
     (2.99*(fuv_m_nuv) + 0.27)*(fuv_m_nuv lt 0.9) + $
     (2.96)*(fuv_m_nuv ge 0.9)
  afuv_irx = (afuv_irx > 0.0)
  sf_struct.irx_beta = just_fuv * 10.^(afuv_irx/2.5)

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; A SIMPLE UV + IR APPROACH USING THE LOCAL 24-TO-TIR RATIO
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; FOLLOWS CORTESE+ 08
  a_early = [0.02025,0.06107,0.07212,0.10588,-0.01517]
  a_mid = [0.42168,0.74191,0.44624,0.04332,-0.03362]
  a_late = [0.50994,0.88311,0.53315,0.04004,-0.04883]

  if n_elements(i24totir) gt 0 then begin
     ltir = nu24*ir24/i24totir*1d-17    ; MJy/sr -> erg/s/cm2/sr
     lfuv = nufuv*fuv*1d-17      ; MJy/sr -> erg/s/cm2/sr
     fuvtotir = alog10(ltir/lfuv)
     afuv_cort08_late = a_late[0] + fuvtotir*0.0
     afuv_cort08_mid = a_mid[0] + fuvtotir*0.0
     afuv_cort08_early = a_early[0] + fuvtotir*0.0
     for i = 1L, n_elements(a_late)-1 do begin
        afuv_cort08_late += a_late[i]*fuvtotir^(1.0*i)
        afuv_cort08_mid += a_mid[i]*fuvtotir^(1.0*i)
        afuv_cort08_early += a_early[i]*fuvtotir^(1.0*i)
     endfor
     sf_struct.fuvtir_early = just_fuv*10.^(afuv_cort08_early/2.5)
     sf_struct.fuvtir_mid = just_fuv*10.^(afuv_cort08_mid/2.5)
     sf_struct.fuvtir_late = just_fuv*10.^(afuv_cort08_late/2.5)
  endif

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; COMPOSITE MEASUREMENT
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  for i = 0L, n_pts-1 do begin
     
     vec = [sf_struct[i].ha_1mag, $
            sf_struct[i].ha24, $
            sf_struct[i].ha24_hi_scaled, $
            sf_struct[i].ha24_gas_umin, $
            sf_struct[i].ha24_gas_scaled, $
            sf_struct[i].fuv24, $
            sf_struct[i].fuv24_hi_scaled, $
            sf_struct[i].fuv24_gas_umin, $
            sf_struct[i].fuv24_gas_scaled, $
            sf_struct[i].irx_beta, $
            sf_struct[i].fuvtir_late, $
            sf_struct[i].fuvtir_mid]

     vec = vec[sort(vec)]

;    MEDIAN
     sf_struct[i].med = median(vec, /even)

;    MEAN LOG
     sf_struct[i].meanlog = 10.^(mean(alog10(vec), /nan))

;    MAD
     sf_struct[i].mad = mad(alog10(vec))

;    STDDEV
     sf_struct[i].stddev = stddev(alog10(vec),/nan)
          
;    MIN + 1
     sf_struct[i].low = vec[1]

;    MAX - 1
     sf_struct[i].high = vec[n_elements(vec)-2]

  endfor

; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; RETURN
; %&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

  return, sf_struct

end 
