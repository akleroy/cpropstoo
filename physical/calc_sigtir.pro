function calc_sigtir $
   , i24 = i24 $
   , i70 = i70 $
   , i100 = i100 $
   , i160 = i160 $
   , i250 = i250

; Implements Table 3 of Galametz+ 13

; I've implemented the surface density conversions, the luminosity
; conversions are very similar, though not identical.

; CHECK WHICH BANDS WE HAVE
  have24 = n_elements(i24) ne 0
  have70 = n_elements(i70) ne 0
  have100 = n_elements(i100) ne 0
  have160 = n_elements(i160) ne 0
  have250 = n_elements(i250) ne 0

; CONSTANTS
  c = 2.99792458d10
  pc = 3.0857d18
  nu24 = c/24.0d-4
  nu70 = c/70.0d-4
  nu100 = c/100.0d-4
  nu160 = c/160.0d-4
  nu250 = c/250.0d-4

; MJY/SR -> W/KPC^2
  fac24 = nu24*1d-17*1d-7*4.0d*!pi*(pc*1d3)^2
  fac70 = nu70*1d-17*1d-7*4.0d*!pi*(pc*1d3)^2
  fac100 = nu100*1d-17*1d-7*4.0d*!pi*(pc*1d3)^2
  fac160 = nu160*1d-17*1d-7*4.0d*!pi*(pc*1d3)^2
  fac250 = nu250*1d-17*1d-7*4.0d*!pi*(pc*1d3)^2

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; MONOCHROMATIC CONVERSIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; TBD

; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%
; HYBRID CONVERSIONS
; &%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%&%

; 24+70
  if have24 and have70 and $
     (have100 eq 0) and (have160 eq 0) AND (have250 eq 0) then begin
     s_tir = $
        (3.925)*i24*fac24 + $
        (1.551)*i70*fac70
     return, s_tir
  endif

; 24 + 100
  if have24 and have100 and $
     (have70 eq 0) and (have160 eq 0) AND (have250 eq 0) then begin
     s_tir = $
        (2.421)*i24*fac24 + $
        (1.410)*i100*fac100
     return, s_tir
  endif

; 24 + 160
  if have24 and have160 and $
     (have70 eq 0) and (have100 eq 0) AND (have250 eq 0) then begin
     s_tir = $
        (3.854)*i24*fac24 + $
        (1.373)*i160*fac160
     return, s_tir
  endif

; 24 + 250
  if have24 and have250 and $
     (have70 eq 0) and (have100 eq 0) AND (have160 eq 0) then begin
     s_tir = $
        (5.179)*i24*fac24 + $
        (3.196)*i250*fac250
     return, s_tir
  endif

; 70+100
  if have70 and have100 and $
     (have24 eq 0) and (have160 eq 0) AND (have250 eq 0) then begin
     s_tir = $
        (0.458)*i70*fac70 + $
        (1.444)*i100*fac100
     return, s_tir
  endif

; 70+160

; 70+250

; 100+160

; 100+250

; 160+250

; 24+70+100
  if have24 and have70 and have100 and $
     (have160 eq 0) and (have250 eq 0) then begin
     s_tir = $
        (2.162)*i24*fac24 + $
        (0.185)*i70*fac70 + $
        (1.319)*i100*fac100
     return, s_tir
  endif

; 24+70+160

; 24+70+250

; 24+100+160

; 24+100+250

; 24+160+250

; 70+100+160

; 70+100+250

; 70+160+250
  if have70 and have160 and have250 and $
     (have24 eq 0) and (have100 eq 0) then begin
     s_tir = $
        (1.018)*i70*fac70 + $
        (1.068)*i160*fac160 + $
        (0.402)*i250*fac250
     return, s_tir
  endif

; 100+160+250

; 24+70+100+160

; 24+70+100+250

; 24+70+160+250
  if have24 and have70 and have160 and have250 and $
     (have100 eq 0) then begin
     s_tir = $
        (2.119)*i24*fac24 + $
        (0.688)*i70*fac70 + $
        (0.995)*i160*fac160 + $
        (0.354)*i250*fac250
     return, s_tir
  endif

; 24+100+160+250
  if have24 and have100 and have160 and have250 and $
     (have70 eq 0) then begin
     s_tir = $
        (2.643)*i24*fac24 + $
        (0.836)*i100*fac100 + $
        (0.357)*i160*fac160 + $
        (0.791)*i250*fac250
     return, s_tir
  endif

; 70+100+160+250

; 24+70+100+160+250
  if have24 and have70 and have100 and have160 and have250 $
     then begin
     s_tir = $
        (2.013)*i24*fac24 + $
        (0.508)*i70*fac70 + $
        (0.393)*i100*fac100 + $
        (0.599)*i160*fac160 + $
        (0.680)*i250*fac250
     return, s_tir
  endif

end
